module Main where

import Control.Monad.Catch (handle)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (execStateT)
import Data.Text (pack, unpack, strip)
import Language.Lyapas.Interpret (_tau, ExecutionState (..), runFunction, emptyState, InterpretError (..))
import Language.Lyapas.Parse (functionBody, program)
import Language.Lyapas.Syntax (Program (..), FunctionName (..))
import Language.Lyapas.Test (execFile)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStr)
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)

import qualified Data.Text.IO as TIO


main :: IO ()
main = getArgs >>= \case
    [] -> runInteractive
    files -> mapM_ execFile files

runInteractive :: IO ()
runInteractive =
    let toplevelState = emptyState
        toplevelReader = ExecutionState
            { _programFunctions = []
            , _traceback = ["INTERACTIVE"]
            }
    in liftIO $ putStrLn "nlyapas v0.1.0\nType \":help\" for help"
    >> runInputT defaultSettings (loop toplevelState toplevelReader)
    where
        loop tlState tlReader = getInputLine ">>> " >>= \case
            Nothing -> outputStr "goodbye\n"
            Just (':':cmd) -> do
                (tlState', tlReader') <- runCmd cmd tlState tlReader
                loop tlState' tlReader'
            Just line -> do
                (tlState', tlReader') <- runLyapas line tlState tlReader
                loop tlState' tlReader'
        --
        runLyapas line tlState tlReader = case parse functionBody "INPUT" (pack line) of
            Left err -> do
                outputStr . errorBundlePretty $ err
                pure (tlState, tlReader)
            Right body -> do
                tlState' <- liftIO $ handle (interpretError tlState) $
                    runReaderT (execStateT (runFunction body) tlState) tlReader
                outputStr $ "τ = " <> show (_tau tlState') <> "\n"
                pure (tlState', tlReader)
        --
        runCmd ('l':'o':'a':'d':' ':file') s r = do
            let file = unpack . strip . pack $ file'
            outputStr $ "Opening '" <> file <> "'\n"
            text <- liftIO $ TIO.readFile file
            case parse program file text of
                Left err -> do
                    outputStr . errorBundlePretty $ err
                    pure (s, r)
                Right (Program funcs) ->
                    let oldFuncs = _programFunctions r
                        r' = r {_programFunctions = oldFuncs <> funcs}
                    in outputStr "ok\n" >> pure (s, r')
        runCmd _any s r = do
            outputStr $ "Available commands:"
                <> "\n  :help - display this message"
                <> "\n  :load FILE - load functions from file"
                <> "\n"
            pure (s, r)
        interpretError x err =
            let (text, trace) = case err of
                    NotFound s t -> ("NotFound: " <> s, t)
                    TypeError s t -> ("TypeError: " <> s, t)
                    IndexError s t -> ("IndexError: " <> s, t)
                    NameError s t -> ("NameError: " <> s, t)
                    ValueError s t -> ("ValueError: " <> s, t)
                    NotImplemented s t -> ("NotImplemented: " <> s, t)
            in (TIO.putStrLn text) >> putTrace trace >> pure x
        putTrace ((FunctionName n):ns) = do
            TIO.putStrLn $ "Required from \"" <> n <> "\""
            putTrace ns
        putTrace [] = pure ()


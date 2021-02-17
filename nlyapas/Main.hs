{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.Catch (handle, try, displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (execStateT)
import Data.Text (Text, pack, unpack, strip)
import Language.Lyapas.Interpret (_tau, FunctionState (..), ExecutionState (..), runFunction, emptyState, InterpretError (..))
import Language.Lyapas.Parse (functionBody, program)
import Language.Lyapas.Syntax (Program (..), FunctionName (..), pretty)
import Language.Lyapas.Test (execFile)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStr, outputStrLn)
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
            Nothing -> outputStrLn "goodbye"
            Just (':':cmd) -> do
                (tlState', tlReader') <- runCmd cmd tlState tlReader
                loop tlState' tlReader'
            Just line -> do
                tlState' <- runLyapas (pack line) tlState tlReader
                loop tlState' tlReader

runLyapas :: Text -> FunctionState -> ExecutionState -> InputT IO FunctionState
runLyapas line tlState tlReader = case parse functionBody "INPUT" line of
    Left err -> do
        outputStr . errorBundlePretty $ err
        pure tlState
    Right body -> do
        tlState' <- liftIO $ handle (interpretError tlState) $
            runReaderT (execStateT (runFunction body) tlState) tlReader
        outputStrLn $ "τ = " <> show (_tau tlState')
        pure tlState'
    where
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


runCmd :: String -> FunctionState -> ExecutionState -> InputT IO (FunctionState, ExecutionState)
runCmd ('l':'o':'a':'d':' ':file) s r = (s,) <$> loadFile r file
runCmd ('l':' ':file) s r = (s,) <$> loadFile r file
runCmd ("list") s r = listFunctions r >> pure (s, r)
runCmd _any s r = do
    outputStr $ "Available commands:"
        <> "\n  :help - display this message"
        <> "\n  :load FILE - load functions from file"
        <> "\n  :l FILE - shorthand for :load"
        <> "\n  :list - list all available functions"
        <> "\n"
    pure (s, r)

loadFile :: ExecutionState -> FilePath -> InputT IO ExecutionState
loadFile r file' =
    let file = unpack . strip . pack $ file'
    in (liftIO . try . TIO.readFile $ file) >>= \case
        Left e -> do
            outputStrLn $ "Loading failed: " <> displayException (e :: IOError)
            pure r
        Right text -> case parse program file text of
            Left err -> do
                outputStr . errorBundlePretty $ err
                pure r
            Right (Program funcs) ->
                let oldFuncs = _programFunctions r
                    r' = r {_programFunctions = oldFuncs <> funcs}
                in outputStrLn "ok" >> pure r'

listFunctions :: ExecutionState -> InputT IO ()
listFunctions ExecutionState {_programFunctions} =
    mapM_ (outputStrLn . unpack . pretty) _programFunctions

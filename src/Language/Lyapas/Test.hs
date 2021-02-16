module Language.Lyapas.Test
    ( execProgram, execParagraph, execFunction, execFile
    ) where

import Control.Monad.Catch (handle)
import Data.Text (Text)
import Language.Lyapas.Interpret (runProgram, InterpretError (..))
import Language.Lyapas.Parse (program, functionBody, paragraph)
import Language.Lyapas.Syntax (Function (..), Program (..), Paragraph (..), FunctionName (..))
import Text.Megaparsec (parse, errorBundlePretty)

import qualified Data.Text.IO as TIO

execProgram :: Text -> IO ()
execProgram p = case parse program "ARG" p of
    Left err -> putStr . errorBundlePretty $ err
    Right body -> runProgram body

execParagraph :: Text -> IO ()
execParagraph p = case parse paragraph "ARG" p of
    Left err -> putStr . errorBundlePretty $ err
    Right body ->
        let par = Paragraph "ENTRY" body
            func = Function "main" [] [] [par]
            prog = Program [func]
        in runProgram prog

execFunction :: Text -> IO ()
execFunction p = case parse functionBody "ARG" p of
    Left err -> putStr . errorBundlePretty $ err
    Right body ->
        let func = Function "main" [] [] body
            prog = Program [func]
        in runProgram prog

execFile :: FilePath -> IO ()
execFile path = handle interpretError $ do
    content <- TIO.readFile path
    case parse program path content of
        Left err -> putStr . errorBundlePretty $ err
        Right body -> runProgram body
    where
        interpretError :: InterpretError -> IO ()
        interpretError err =
            let (text, trace) = case err of
                    NotFound s t -> ("NotFound: " <> s, t)
                    TypeError s t -> ("TypeError: " <> s, t)
                    IndexError s t -> ("IndexError: " <> s, t)
                    NameError s t -> ("NameError: " <> s, t)
                    ValueError s t -> ("ValueError: " <> s, t)
                    NotImplemented s t -> ("NotImplemented: " <> s, t)
            in TIO.putStrLn text >> putTrace trace
        putTrace :: [FunctionName] -> IO ()
        putTrace ((FunctionName n):ns) = do
            TIO.putStrLn $ "Required from \"" <> n <> "\""
            putTrace ns
        putTrace [] = pure ()


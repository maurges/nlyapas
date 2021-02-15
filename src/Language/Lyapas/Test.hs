module Language.Lyapas.Test
    ( execProgram, execParagraph, execFunction, execFile
    ) where

import Control.Monad.Catch (handle)
import Data.Text (Text)
import Language.Lyapas.Interpret (runProgram, InterpretError (..))
import Language.Lyapas.Parse (program, functionBody, paragraph)
import Language.Lyapas.Syntax (Function (..), Program (..), Paragraph (..))
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
        interpretError = TIO.putStrLn . \case
            NotFound s -> "NotFound: " <> s
            TypeError s -> "TypeError: " <> s
            IndexError s -> "IndexError: " <> s
            NameError s -> "NameError: " <> s
            NotImplemented s -> "NotImplemented: " <> s

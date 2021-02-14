module Language.Lyapas.Test
    ( execProgram, execParagraph, execFunction
    ) where

import Data.Text (Text)
import Language.Lyapas.Interpret (runProgram)
import Language.Lyapas.Parser (program, functionBody, paragraph)
import Language.Lyapas.Syntax (Function (..), Program (..), Paragraph (..))
import Text.Megaparsec (parse, errorBundlePretty)

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

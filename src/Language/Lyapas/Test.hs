module Language.Lyapas.Test
    ( execProgram, execParagraph
    ) where

import Data.Text (Text)
import Language.Lyapas.Interpret (runProgram)
import Language.Lyapas.Parser (program, paragraph)
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

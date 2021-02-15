{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Language.Lyapas.Parse
--     (
--     ) where
where

import Control.Applicative (liftA2, liftA3)
-- import Control.Applicative.Permutations (runPermutation, toPermutationWithDefault)
import Control.Monad (void)
import Data.Bits (shiftL)
-- import Data.Default.Class (def)
import Data.Int (Int64)
import Data.List (foldl')
-- import Data.Maybe (fromMaybe)
import Data.MonoTraversable (opoint)
import Data.Text (Text, cons, pack)
import Data.Void (Void)

import qualified Data.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Lyapas.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, hspace)


type DomainError = Void -- may be other in the future
type Parser = Parsec DomainError Text

----------
-- Лексинг


commentPrefix :: Parser ()
commentPrefix = void $ string "***" -- *> notFollowedBy (char '#') -- '#' начинает прагму -- пока без экстеншнов

skipLineComment :: Parser ()
skipLineComment = commentPrefix *> void (takeWhileP (Just "character") (/= '\n'))

space, hspace :: Parser ()
space = L.space
    space1
    skipLineComment
    empty -- парсер многострочных комментариев (у нас их нет)
hspace = L.space
    hspace1
    skipLineComment
    empty -- парсер многострочных комментариев (у нас их нет)

lexeme, hlexeme :: Parser a -> Parser a
lexeme = L.lexeme space
hlexeme = L.lexeme hspace

symbol, hsymbol :: Text -> Parser Text
symbol = L.symbol space
hsymbol = L.symbol hspace

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented space

braced, hbraced :: Parser a -> Parser a
braced = between (symbol "(") (symbol ")")
hbraced = between (symbol "(") (hsymbol ")")

keyword, keywordNonDigit :: Parser Char
keyword = alphaNumChar <|> char '_'
keywordNonDigit = letterChar <|> char '_'


----------
-- Парсинг

program :: Parser Program
program = do
    optional shabangLine
    space
    p <- Program <$> many function
    eof
    pure p
    where
        shabang = string "#!"
        shabangLine = shabang *> anySingle `skipManyTill` (void eol <|> eof)

{- -- опять экстеншны, пока без них
extProgramParams :: Parser ExtProgramParams
extProgramParams =
    ExtProgramParams
        <$> (fmap concat . many $ pragma disableFeature langFeatures)
        <*> (fromMaybe def <$> optional (pragma variableSizeName pVariableSize))
        <*> many (pragma importName import_)
    where
        disableFeature = hlexeme "disable" <|> hlexeme "отключить"
        variableSizeName = hlexeme "default" <|> hlexeme "дефолтные"
        importName = hlexeme "import" <|> hlexeme "импорт"

pragma :: Parser name -> Parser a -> Parser a
pragma name value = nonIndented $ do
    hsymbol "#" <|> hsymbol "***#"
    hlexeme name
    hlexeme value

langFeatures :: Parser [LangFeature]
langFeatures = braced $ sepBy langFeature (symbol ",")

langFeature :: Parser LangFeature
langFeature
    = lexeme "random" *> pure RandomOperator
  <|> lexeme "console" *> pure ConsoleIO
  <|> lexeme "heap" *> pure LargeComplex
  <|> lexeme "рандом" *> pure RandomOperator
  <|> lexeme "коносоль" *> pure ConsoleIO
  <|> lexeme "кучу" *> pure LargeComplex


pVariableSize :: Parser VariableSize
pVariableSize
    = lexeme "int8" *> pure Signed8
  <|> lexeme "int16" *> pure Signed16
  <|> lexeme "int32" *> pure Signed32
  <|> lexeme "int64" *> pure Signed64
  <|> lexeme "unsigned8" *> pure Unsigned8
  <|> lexeme "unsigned16" *> pure Unsigned16
  <|> lexeme "unsigned32" *> pure Unsigned32
  <|> lexeme "unsigned64" *> pure Unsigned64
  <|> lexeme "big" *> pure BigNumbers
  <|> lexeme "большие" *> pure BigNumbers

import_ :: Parser Import
import_ = do
    ident <- importIdentifier
    let including' = fmap (\l -> Left (True, l)) including
    let excluding' = fmap (\l -> Left (False, l)) excluding
    let renamer' = fmap Right renamer
    (try including' <|> try excluding' <|> renamer') >>= \case
        Left (True, list) -> pure $ ImportIncluding ident list
        Left (False, list) -> pure $ ImportExcluding ident list
        Right name -> pure $ ImportQualified ident name
    where
        including = importList
        excluding = (hlexeme "hiding" <|> hlexeme "кроме") *> importList
        renamer = (hlexeme "as" <|> hlexeme "как") *> importRenamer
        importList = braced $ sepBy importItem (symbol ",")
        importItem = try (fmap Left functionName) <|> fmap Right (braced operatorName)

importIdentifier :: Parser ImportIdentifier
importIdentifier = fail "Imports are not implemented yet"

importRenamer :: Parser ImportRenamer
importRenamer = fail "Imports are not implemented yet"
-}

functionName :: Parser FunctionName
functionName = lexeme $ FunctionName <$>
    liftA2 cons keywordNonDigit (pack <$> many keyword)

varName :: Parser VarName
varName = lexeme $ VarName . opoint <$> charCategory Char.LowercaseLetter

complexIdentifier :: Parser ComplexIdentifier
complexIdentifier = ComplexIdentifier . pack <$> some digitChar

complexName :: Parser ComplexName
complexName = lexeme $
    ((char 'F' *> pure ShortComplex) <|> (char 'L' *> pure LongComplex))
    <*> complexIdentifier

shortComplex :: Parser ComplexName
shortComplex = complexName >>= \case
    x@(ShortComplex _) -> pure x
    _ -> fail "Expected short complex"

paragraphName :: Parser ParagraphName
paragraphName = lexeme $ ParagraphName . pack <$> some digitChar

stringLiteral :: Parser StringLiteral
stringLiteral = fmap (StringLiteral . pack) $ do
    char '\''
    str <- many stringChar
    char '\''
    pure str
    where
        stringChar = escaped <|> noneOf ['\'']
        escaped = char '\\' *> choice parseAndReplaces
        parseAndReplaces = map (\(code, replace) -> char code *> pure replace) replaces
        replaces = [('\\', '\\'), ('n', '\n'), ('\'', '\'')]

decimal, hexadecimal, octal, binary :: Parser Int64
decimal = lexeme L.decimal
hexadecimal = numberParser 16 'h' $
    digitChar <|> oneOf ("ABCDEF" :: String)
octal = numberParser 8 'o' $
    oneOf ("01234567" :: String)
binary = numberParser 2 'b' $ oneOf ['0', '1']

numberParser :: Int64 -> Char -> Parser Char -> Parser Int64
numberParser base postfix digit = lexeme $ do
    x <- some digit
    char postfix
    pure $ foldl' step 0 x
    where
        step a c = a * base + fromIntegral (Char.digitToInt c)

singleOneConstant :: Parser Int64
singleOneConstant = do
    char 'I'
    shift <- decimal
    pure $ 1 `shiftL` fromIntegral shift

charConstant :: Parser Int64
charConstant = lexeme $ do
    char '\''
    c <- stringChar
    char '\''
    pure . fromIntegral . Char.ord $ c
    where
        stringChar = escaped <|> noneOf ['\'']
        escaped = char '\\' *> choice parseAndReplaces
        parseAndReplaces = map (\(code, replace) -> char code *> pure replace) replaces
        replaces = [('\\', '\\'), ('n', '\n'), ('\'', '\'')]

constant :: Parser Int64
constant = choice [decimal, hexadecimal, octal, binary, singleOneConstant, charConstant]

operand :: Parser Operand
operand
      = try (fmap Constant constant) -- try because it overlaps with complexes
    <|> (symbol "Z" *> pure OverflowValue)
    <|> (symbol "T" *> pure TimeValue)
    <|> (symbol "X" *> pure (MutableOperand . IdentVar $ "X"))
    <|> (symbol "I" *> fmap UnitVector varName)
    <|> fmap MutableOperand identifier

identifier :: Parser Identifier
identifier = lexeme $
        fmap IdentVar varName
    <|> liftA2 IdentComplexElement complexName complexIndex
    <|> fmap IdentComplexSize (char 'Q' *> complexIdentifier)
    <|> fmap IdentComplexCap (char 'S' *> complexIdentifier)

complexIndex :: Parser Operand
complexIndex
      = fmap Constant (symbol "." *> constant)
    <|> fmap (MutableOperand . IdentVar) varName


function :: Parser Function
function = do
    name <- functionName
    --
    (rargs, wargs) <- hbraced $ do
        rargs <- sepBy argument (symbol ",")
        symbol "/"
        wargs <- sepBy argument (symbol ",")
        pure (rargs, wargs)
    --
    {- -- экстеншны
    mbExtParams <- optional $ runPermutation $ ExtFunctionParams
        <$> toPermutationWithDefault
            False
            ((hsymbol "public" <|> hsymbol "публично") *> pure True)
        <*> toPermutationWithDefault
            Nothing
            (Just <$> ((hsymbol "operator" <|> hsymbol "оператор") *> hbraced operatorName))
        <*> toPermutationWithDefault
            True
            (hsymbol "nomangle" *> pure False)
    let extParams = fromMaybe def mbExtParams
    -}
    space
    --
    body <- functionBody
    symbol "**" *> notFollowedBy (char '*')
    --
    pure $ Function name rargs wargs body

argument :: Parser Argument
argument = fmap ArgVar varName <|> fmap ArgComplex complexName

functionBody :: Parser [Paragraph]
functionBody = do
    first <- Paragraph "ENTRY" <$> paragraph
    rest <- many $ do
        pname <- fmap (ParagraphName . pack) $ lexeme $ char '§' *> some digitChar
        Paragraph pname <$> paragraph
    pure $ first:rest

paragraph :: Parser [Statement]
paragraph = many $ choice
    -- сперва многозначный парс
    -- обнуление
    [ try $ unary 'O' ":set-null" -- пересекается с комплексом
    , char 'O' *> fmap (Compute . ComplexNullary ":set-null") complexName
    -- почти одинаковый синтаксис с умножением
    , Compute <$> functionCall
    -- почти одинаковый синтаксис с делением
    , try $ char '/' *> fmap Compute ioOperation
    -- передача значения
    , unary '⁻' ":set-full"
    , unary '⇒' ":set-assign"
    , Compute . SetTau <$> operand
    , swapComplexes
    , swapVars
    -- логические и арифметические операции
    , nullary '!' ":rightmost-one"
    , nullary '¬' ":bit-negate"
    , nullary '%' ":bit-weight"
    , unary '∨' ":disjunction"
    , unary '&' ":conjunction"
    , unary '⊕' ":xor"
    , unary '<' ":left-shift"
    , unary '>' ":right-shift"
    , unary '+' ":add"
    , unary '-' ":sub"
    , try $ unary '*' ":mul" -- у звёздочки слишком много значений
    , unary '/' ":div"
    , unary ';' ":mod"
    , unary '∆' ":inc"
    , unary '∇' ":dec"
    -- операции перехода
    , goto "→" Goto
    , goto "↪" GoZero
    , goto "↦" GoNotZero
    , fmap Control $ symbol "↑" *> conditionOrEnumerate
    -- операции над комплексами
    , fmap Compute $ char '@' *> complexOperation
    ]
  where
    nullary sym name =
        fmap Compute $ lexeme (char sym) *> pure (Nullary name)
    unary sym name = fmap Compute $ lexeme (char sym) *> fmap (Unary name) operand
    swapVars = fmap Compute $ symbol "⇔" *>
        (braced $ liftA2 SwapVars (lexeme identifier) (lexeme identifier))
    swapComplexes = fmap Compute $ symbol "⇔" *>
        (braced $ liftA3 SwapComplex (lexeme complexName) (lexeme complexIndex) (lexeme complexIndex))
    goto sym op = symbol sym *> (Control . op <$> paragraphName)
    conditionOrEnumerate
          = (symbol "X" *> liftA3 GoEnumerateZeros paragraphName identifier identifier)
        <|> (liftA2 GoCondition (braced condition) paragraphName)

condition :: Parser Condition
condition = liftA3 Condition operand comparison operand

comparison :: Parser Comparison
comparison = choice . map (\(s, o) -> symbol s *> pure o) $
    [ ("=", CompEq)
    , (">", CompG)
    , ("<", CompL)
    , ("≠", CompNE)
    , ("≥", CompGE)
    , ("≤", CompLE)
    ]

functionCall :: Parser ComputeStatement
functionCall = do
    name <- try $ do
      char '*' -- no space after
      name <- functionName
      lookAhead $ char '('
      pure name
    (rargs, wargs) <- braced $ do
        rargs <- sepBy anyArument (symbol ",")
        symbol "/"
        wargs <- sepBy argument (symbol ",")
        pure (rargs, wargs)
    pure $ FunctionCall name rargs wargs
    where
        anyArument = try (fmap Right operand) <|> fmap Left complexName
        -- try because of overlap between complex name and complex element

complexOperation :: Parser ComputeStatement
complexOperation = choice
    [ symbol "+" *> liftA2 (ComplexUnary ":complex-create") complexName (braced operand)
    , symbol "-" *> complexNullary ":complex-delete"
    , symbol "%" *> complexNullary ":complex-shrink"
    , symbol ">" *> complexNullary ":complex-push"
    , symbol "<" *> complexNullary ":complex-pop"
    , lookAhead (char '\'') *> complexString
    -- not implemented: @#, вставка и поп с произвольного индекса
    ]
  where
    complexNullary name = ComplexNullary name <$> complexName
    complexString = do
        str <- stringLiteral
        symbol ">"
        name <- shortComplex
        pure $ ComplexSymbols ":complex-append" name str

ioOperation :: Parser ComputeStatement
ioOperation = try complexIO <|> stringOut where
    stringOut = do
        str <- stringLiteral
        symbol ">C"
        pure $ StringLiteralStatement ":print-string" str
    complexIO = do
        name <- shortComplex
        choice
            [ symbol ">C" *> pure (ComplexNullary ":complex-print" name)
            , symbol "<C" *> pure (ComplexNullary ":complex-read" name)
            ]


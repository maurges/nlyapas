{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Language.Lyapas.Syntax where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)
import Data.Sequences (unpack, pack)


data Program = Program [Function]
    deriving (Eq, Ord, Show)

data Function = Function FunctionName
                         [Argument] -- ^ Pass by value
                         [Argument] -- ^ Pass by reference
                         [Paragraph] -- ^ First (unnamed) paragraph has special name
    deriving (Eq, Ord, Show)

data Argument = ArgVar VarName | ArgComplex ComplexName
    deriving (Eq, Ord, Show)

data Paragraph = Paragraph ParagraphName [Statement]
    deriving (Eq, Ord, Show)

data Statement = Compute ComputeStatement | Control ControlStatement
    deriving (Eq, Ord, Show)

data ComputeStatement
    = SetTau Operand
    | Nullary OperatorName
    | Unary OperatorName Operand
    | SwapVars Identifier Identifier
    | SwapComplex ComplexName Operand Operand
    | ComplexNullary OperatorName ComplexName
    | ComplexUnary OperatorName ComplexName Operand
    | ComplexSymbols OperatorName ComplexName StringLiteral
    | StringLiteralStatement OperatorName StringLiteral
    | FunctionCall FunctionName [Either ComplexName Operand] [Either ComplexName Identifier]
    deriving (Eq, Ord, Show)

data ControlStatement
    = Goto ParagraphName
    | GoNotZero ParagraphName
    | GoZero ParagraphName
    | GoCondition Condition ParagraphName
    | GoEnumerateZeros ParagraphName Identifier Identifier
    -- переход по времени?
    deriving (Eq, Ord, Show)

data Operand
    = Constant Int64
    | OverflowValue
    | TimeValue
    | UnitVector VarName
    | MutableOperand Identifier
    deriving (Eq, Ord, Show)

data Identifier
    = IdentVar VarName
    | IdentComplexElement ComplexName Operand
    | IdentComplexSize ComplexIdentifier
    | IdentComplexCap ComplexIdentifier
    deriving (Eq, Ord, Show)

data Condition = Condition Operand Comparison Operand
    deriving (Eq, Ord, Show)

data Comparison
    = CompEq -- | a = b
    | CompNE -- | a /= b
    | CompG  -- | a > b
    | CompGE -- | a >= b
    | CompL  -- | a < b
    | CompLE -- | a <= b
    deriving (Eq, Ord, Show)

data ComplexName
    = ShortComplex ComplexIdentifier
    | LongComplex ComplexIdentifier
    deriving (Eq, Ord, Show)

newtype FunctionName = FunctionName Text  deriving (Eq, Ord, Show, IsString, Hashable)
newtype VarName = VarName Text  deriving (Eq, Ord, Show, IsString, Hashable)
newtype ComplexIdentifier = ComplexIdentifier Text  deriving (Eq, Ord, Show, IsString, Hashable)
newtype ParagraphName = ParagraphName Text  deriving (Eq, Ord, Show, IsString, Hashable)
newtype OperatorName = OperatorName Text  deriving (Eq, Ord, Show, IsString, Hashable)
newtype StringLiteral = StringLiteral Text  deriving (Eq, Ord, Show, IsString, Hashable)

{-
-----------------------
-- Extensions to lyapas

data ExtProgramParams = ExtProgramParams
    -- | Выключить фичи языка, если нужно уменьшить библиотеку, с которой нужно
    -- линковаться (для системного программирования)
    { disableFeatures :: [LangFeature]
    -- | Выставить размер для тау, именованных переменных и ячейки больших комплексов
    , variableSize :: VariableSize
    -- | Возможность импорта функций и операторов из других модулей
    , imports :: [Import]
    } deriving (Eq, Ord, Show)

data ExtFunctionParams = ExtFunctionParams
    { public :: Bool
    , operator :: Maybe OperatorName
    , mangleName :: Bool
    } deriving (Eq, Ord, Show)

data Import
    = ImportIncluding ImportIdentifier [Either FunctionName OperatorName]
    | ImportExcluding ImportIdentifier [Either FunctionName OperatorName]
    | ImportQualified ImportIdentifier ImportRenamer
    deriving (Eq, Ord, Show)

newtype ImportIdentifier = ImportIdentifier Text -- TODO: didn't think this out
    deriving (Eq, Ord, Show)
newtype ImportRenamer = ImportRenamer Text -- TODO: didn't think this out
    deriving (Eq, Ord, Show)
moduleAsItself :: ImportIdentifier -> ImportRenamer
moduleAsItself (ImportIdentifier x) = ImportRenamer x

data VariableSize
    = Signed8   | Signed16   | Signed32   | Signed64
    | Unsigned8 | Unsigned16 | Unsigned32 | Unsigned64
    | BigNumbers
    deriving (Eq, Ord, Show)

data LangFeature
    -- | Оператор генерации случайного числа
    = RandomOperator
    -- | Ввод-вывод из консоли
    | ConsoleIO
    -- | Комплексы, которые нельзя аллоцировать на стеке, а только на куче
    | LargeComplex
    deriving (Eq, Ord, Show)


instance Default ExtProgramParams where
    def = ExtProgramParams
        { disableFeatures = []
        , variableSize = def
        , imports = []
        }

instance Default ExtFunctionParams where
    def = ExtFunctionParams
        { public = True
        , operator = Nothing
        , mangleName = False
        }

instance Default VariableSize where
    def = Unsigned32 -- FIXME: i'm not sure here
-}

pattern OP :: String -> OperatorName
pattern OP str <- (unpack . unOperatorName -> str)
    where OP = OperatorName . pack

unOperatorName :: OperatorName -> Text
unOperatorName (OperatorName n) = n


--------------------------------------------------------------
-- Class for pretty printing syntax elements in error messages


class Pretty a where
    pretty :: a -> Text

instance Pretty Function where
    pretty (Function (FunctionName name) rargs wargs _) =
        name <> "("
        <> sepBy ", " (map pretty rargs) <> " / "
        <> sepBy ", " (map pretty wargs)
        <> ")"
        where
            sepBy _sep [] = ""
            sepBy _sep [x] = x
            sepBy sep (x:xs) = x <> sep <> sepBy sep xs

instance Pretty Argument where
    pretty (ArgVar (VarName name)) = name
    pretty (ArgComplex (ShortComplex (ComplexIdentifier ident))) = "F" <> ident
    pretty (ArgComplex (LongComplex (ComplexIdentifier ident))) = "L" <> ident

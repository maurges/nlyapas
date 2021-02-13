{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Lyapas.Syntax where

import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)


data Program = Program [Function]
    deriving (Eq, Show)

data Function = Function FunctionName
                         [Argument] -- ^ Pass by value
                         [Argument] -- ^ Pass by reference
                         [Paragraph] -- ^ First (unnamed) paragraph has special name
    deriving (Eq, Show)

data Argument = ArgVar VarName | ArgComplex ComplexName
    deriving (Eq, Show)

data Paragraph = Paragraph ParagraphName [Statement]
    deriving (Eq, Show)

data Statement = Compute ComputeStatement | Control ControlStatement
    deriving (Eq, Show)

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
    | FunctionCall FunctionName [Either ComplexName Operand] [Argument]
    deriving (Eq, Show)

data ControlStatement
    = Goto ParagraphName
    | GoNotZero ParagraphName
    | GoZero ParagraphName
    | GoCondition Condition ParagraphName
    | GoEnumerateZeros ParagraphName Identifier Identifier
    -- переход по времени?
    deriving (Eq, Show)

data Operand
    = Constant Int64
    | OverflowValue
    | TimeValue
    | UnitVector VarName
    | MutableOperand Identifier
    deriving (Eq, Show)

data Identifier
    = IdentVar VarName
    | IdentComplexElement ComplexName Operand
    | IdentComplexSize ComplexIdentifier
    | IdentComplexCap ComplexIdentifier
    deriving (Eq, Show)

data Condition = Condition Operand Comparison Operand
    deriving (Eq, Show)

data Comparison
    = CompEq -- | a = b
    | CompNE -- | a /= b
    | CompG  -- | a > b
    | CompGE -- | a >= b
    | CompL  -- | a < b
    | CompLE -- | a <= b
    deriving (Eq, Show)

data ComplexName
    = ShortComplex ComplexIdentifier
    | LongComplex ComplexIdentifier
    deriving (Eq, Show)

newtype FunctionName = FunctionName Text  deriving (Eq, Show)
newtype VarName = VarName Text  deriving (Eq, Show, IsString)
newtype ComplexIdentifier = ComplexIdentifier Text  deriving (Eq, Show)
newtype ParagraphName = ParagraphName Text  deriving (Eq, Show, IsString)
newtype OperatorName = OperatorName Text  deriving (Eq, Show, IsString)
newtype StringLiteral = StringLiteral Text  deriving (Eq, Show)

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
    } deriving (Eq, Show)

data ExtFunctionParams = ExtFunctionParams
    { public :: Bool
    , operator :: Maybe OperatorName
    , mangleName :: Bool
    } deriving (Eq, Show)

data Import
    = ImportIncluding ImportIdentifier [Either FunctionName OperatorName]
    | ImportExcluding ImportIdentifier [Either FunctionName OperatorName]
    | ImportQualified ImportIdentifier ImportRenamer
    deriving (Eq, Show)

newtype ImportIdentifier = ImportIdentifier Text -- TODO: didn't think this out
    deriving (Eq, Show)
newtype ImportRenamer = ImportRenamer Text -- TODO: didn't think this out
    deriving (Eq, Show)
moduleAsItself :: ImportIdentifier -> ImportRenamer
moduleAsItself (ImportIdentifier x) = ImportRenamer x

data VariableSize
    = Signed8   | Signed16   | Signed32   | Signed64
    | Unsigned8 | Unsigned16 | Unsigned32 | Unsigned64
    | BigNumbers
    deriving (Eq, Show)

data LangFeature
    -- | Оператор генерации случайного числа
    = RandomOperator
    -- | Ввод-вывод из консоли
    | ConsoleIO
    -- | Комплексы, которые нельзя аллоцировать на стеке, а только на куче
    | LargeComplex
    deriving (Eq, Show)


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

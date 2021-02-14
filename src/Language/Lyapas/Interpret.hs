{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Lyapas.Interpret
    ( runProgram
    , runFunction
    , runParagraph
    ) where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.Catch (throwM)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bits (shiftL, shiftR, (.|.), (.&.), testBit, countTrailingZeros, finiteBitSize, complement, popCount, xor)
import Data.ByteString (ByteString)
import Data.Containers (mapFromList, lookup, insertMap, member, deleteMap)
import Data.HashMap.Strict (HashMap)
import Data.Int (Int64)
import Data.MonoTraversable (olength, olength64, Element)
import Data.Sequences (index, fromList, IsSequence, Index, take, snoc)
import Data.Text (Text, pack, unpack)
import Data.Vector.Storable (Vector)
import Optics.At () -- instances for bytestrings and vectors
import Optics.Core (ix, (%), (&), (.~), _1, _2) -- orphans for bytestrings and vectors
import Optics.Lens (Lens')
import Optics.State (use, preuse)
import Optics.State.Operators ((%=), (.=))
import Optics.TH (makeLenses)
import System.Clock (Clock (Realtime), getTime, sec)
import System.Random (randomIO, setStdGen, mkStdGen)

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.IO as TIO

import Language.Lyapas.Syntax
import Prelude hiding (lookup, take)


data FunctionState = FunctionState
    { _tau :: Int64
    , _vars :: HashMap VarName Int64
    , _longs :: HashMap ComplexIdentifier ((Vector Int64), Int) -- ^ snd is capacity
    , _shorts :: HashMap ComplexIdentifier (ByteString, Int) -- ^ snd is capacity
    , _overflow :: Int64
    } deriving (Eq, Show)
makeLenses ''FunctionState

data ArgumentMap = ArgumentMap
    { argVars :: [(Text, Text)]
    , argLongs :: [(Text, Text)]
    , argShorts :: [(Text, Text)]
    } deriving (Eq, Show)

type ProgramFunctions = [Function]

type LyapasT m = StateT FunctionState (ReaderT ProgramFunctions m)

data InterpretError
    = NotFound Text
    | TypeError Text
    | IndexError Text
    | NameError Text
    deriving (Eq, Show)
instance Exception InterpretError


runProgram :: Program -> IO ()
runProgram (Program funcs) = do
    func <- case funcs of
        [] -> throwM . NotFound $ "Program has no functions to run"
        (Function _ [] [] body):_ -> pure body
        (Function (FunctionName name) _ _ _):_ -> throwM . TypeError $
            "Function " <> name <> " (first function in program) should have no arguments"
    let initialState = FunctionState
            { _tau = 0
            , _vars = mapFromList []
            , _longs = mapFromList []
            , _shorts = mapFromList []
            , _overflow = 0
            }
    void $ runReaderT (runStateT (runFunction func) initialState) funcs

-- | Prepare new "stack frame" for function, run it, and update outer stack
-- frame on exit
callFunction :: Function -> LyapasT IO ()
callFunction = error "call function"

-- | Run function in current "stack frame"
runFunction :: [Paragraph] -> LyapasT IO ()
runFunction [] = pure ()
runFunction allParagraphs = go allParagraphs where
    go [] = pure ()
    go (p:ps) = runParagraph p >>= \case
        Nothing -> go ps
        Just jumpTo ->
            case dropWhile ((/= jumpTo) . paragraphName) allParagraphs of
                [] -> throwM . NotFound $ "Paragraph " <> tshow jumpTo <> " does not exist"
                ps' -> go ps'
    paragraphName (Paragraph name _) = name

runParagraph :: Paragraph -> LyapasT IO (Maybe ParagraphName)
runParagraph p = runExceptT (run p) >>= \case
    Left goto -> pure $ Just goto
    Right () -> pure Nothing
  where
    run (Paragraph _name sts) = mapM_ runStatement sts
    runStatement = \case
        Compute c -> lift $ runCompute c
        Control c -> runControl c

runControl :: ControlStatement -> ExceptT ParagraphName (LyapasT IO) ()
runControl = \case
    Goto name -> goto name
    GoNotZero name -> use tau >>= \case
        0 -> pure ()
        _ -> goto name
    GoZero name -> use tau >>= \case
        0 -> goto name
        _ -> pure ()
    GoCondition (Condition lo c ro) name -> do
        l <- lift $ getOperand lo
        r <- lift $ getOperand ro
        let comp = case c of
                CompEq -> (==)
                CompNE -> (/=)
                CompG  -> (>)
                CompGE -> (>=)
                CompL  -> (<)
                CompLE -> (<=)
        if l `comp` r
            then goto name
            else pure ()
    GoEnumerateZeros name val enumerator -> do
        v <- lift $ getIdentifier val
        let oneIndex = countTrailingZeros v
        if oneIndex == finiteBitSize v -- all are zeroes
            then do
                lift $ setIdentifier enumerator 0
                goto name
            else lift $ setIdentifier enumerator (fromIntegral oneIndex)
    where
    goto = ExceptT . pure . Left

runCompute :: ComputeStatement -> LyapasT IO ()
runCompute = \case
    SetTau op -> do
        val <- getOperand op
        tau .= val
    SwapVars i1 i2 -> do
        val1 <- getIdentifier i1
        val2 <- getIdentifier i2
        setIdentifier i1 val2
        setIdentifier i2 val1
    SwapComplex name op1 op2 ->
        let i1 = IdentComplexElement name op1
            i2 = IdentComplexElement name op2
        in runCompute $ SwapVars i1 i2
    StringLiteralStatement (OP":print-string") (StringLiteral str) ->
        liftIO $ TIO.putStr str
    --
    Nullary (OP":rightmost-one") -> nullary (fromIntegral . countTrailingZeros)
    Nullary (OP":bit-negate") -> nullary complement
    Nullary (OP":bit-weight") -> nullary (fromIntegral . popCount)
    --
    Unary (OP":set-null") op -> unarySet op (\_t _old -> 0)
    Unary (OP":set-full") op -> unarySet op (\_t _old -> complement 0)
    Unary (OP":set-assign") op -> unarySet op (\t _old -> t)
    --
    Unary (OP":disjunction") op -> unary op (.|.)
    Unary (OP":conjunction") op -> unary op (.&.)
    Unary (OP":xor") op -> unary op xor
    Unary (OP":left-shift") op -> unary op (\t x -> t `shiftL` fromIntegral x)
    Unary (OP":right-shift") op -> unary op (\t x -> t `shiftR` fromIntegral x)
    Unary (OP":add") op -> unary op (+)
    Unary (OP":sub") op -> unary op (-)
    Unary (OP":mul") op -> unary op (*)
    Unary (OP":div") op -> unary op div
    Unary (OP":mod") op -> unary op mod
    Unary (OP":inc") op -> unarySet op (\_t old -> old + 1)
    Unary (OP":dec") op -> unarySet op (\_t old -> old - 1)
    --
    ComplexUnary (OP":complex-create") comp op -> getOperand op >>= complexCreate comp
    ComplexNullary (OP":complex-delete") comp -> complexDelete comp
    ComplexNullary (OP":complex-shrink") comp -> complexShrink comp
    ComplexNullary (OP":complex-push") comp -> use tau >>= complexPush comp
    ComplexNullary (OP":complex-print") comp -> complexPrint comp
    where
        nullary :: (Int64 -> Int64) -> LyapasT IO ()
        nullary op = tau %= op
        unarySet, unary :: Operand -> (Int64 -> Int64 -> Int64) -> LyapasT IO ()
        unarySet (MutableOperand ident) f = do
            t <- use tau
            i <- getIdentifier ident
            setIdentifier ident (f t i)
        unarySet op _ = throwM . TypeError $
            tshow op <> " is not a mutable operand"
        unary op f = do
            t <- use tau
            x <- getOperand op
            tau .= f t x

getOperand :: Operand -> LyapasT IO Int64
getOperand = \case
    MutableOperand ident -> getIdentifier ident
    Constant x -> pure x
    OverflowValue -> use overflow
    TimeValue -> liftIO $ sec <$> getTime Realtime
    UnitVector var -> do
        shift <- getVar var
        pure $ 1 `shiftL` fromIntegral shift

getIdentifier :: Identifier -> LyapasT IO Int64
getIdentifier = \case
    IdentVar v -> getVar v
    IdentComplexSize name -> do
        s <- use shorts
        l <- use longs
        case (lookup name s, lookup name l) of
            (Just (v, _), _) -> pure . olength64 $ v
            (_, Just (v, _)) -> pure . olength64 $ v
            (Nothing, Nothing) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexCap name -> do
        s <- use shorts
        l <- use longs
        case (lookup name s, lookup name l) of
            (Just (_, x), _) -> pure . fromIntegral $ x
            (_, Just (_, x)) -> pure . fromIntegral $ x
            (Nothing, Nothing) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexElement (LongComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (vec, _cap) <- getLongComplex name
        indexGeneric vec ind
    IdentComplexElement (ShortComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (bs, _cap) <- getShortComplex name
        fromIntegral <$> indexGeneric bs ind
    where
        indexGeneric :: IsSequence seq => seq -> Index seq -> LyapasT IO (Element seq)
        indexGeneric v i = case index v i of
            Nothing -> throwM . IndexError $ "Complex index out of range"
            Just x -> pure x

getVar :: VarName -> LyapasT IO Int64
getVar (VarName "X") = liftIO randomIO
getVar name = do
    v <- use vars
    case lookup name v of
        Nothing -> throwM . NotFound $ "Using " <> tshow name <> " before assigning"
        Just x -> pure x

getLongComplex :: ComplexIdentifier -> LyapasT IO (Vector Int64, Int)
getLongComplex name = do
    v <- use longs
    case lookup name v of
        Nothing -> throwM . NotFound $ "Using " <> tshow name <> " before creation"
        Just x -> pure x

getShortComplex :: ComplexIdentifier -> LyapasT IO (ByteString, Int)
getShortComplex name = do
    v <- use shorts
    case lookup name v of
        Nothing -> throwM . NotFound $ "Using " <> tshow name <> " before creation"
        Just x -> pure x

setIdentifier :: Identifier -> Int64 -> LyapasT IO ()
setIdentifier i value = case i of
    IdentVar name -> do
        if name == VarName "X"
            then liftIO . setStdGen . mkStdGen . fromIntegral $ value
            else vars %= insertMap name value
    IdentComplexElement (ShortComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (comp, _) <- getShortComplex name
        when (ind >= olength comp) $
            throwM . IndexError $ "Indexing " <> tshow name <> " beyond size at " <> tshow ind
        shorts % ix name % _1 % ix ind .= fromIntegral value
    IdentComplexElement (LongComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (comp, _) <- getLongComplex name
        when (ind >= olength comp) $
            throwM . IndexError $ "Indexing " <> tshow name <> " beyond size at " <> tshow ind
        longs % ix name % _1 % ix ind .= value
    IdentComplexSize name -> do
        s <- use shorts
        l <- use longs
        case (member name s, member name l) of
            (True, _) ->
                complexSetSize shorts name (fromIntegral value)
            (_, True) ->
                complexSetSize longs name (fromIntegral value)
            (False, False) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexCap name -> do
        let cap = fromIntegral value
        s <- use shorts
        l <- use longs
        case (lookup name s, lookup name l) of
            (Just (_, oldCap), _) -> do
                when (oldCap > cap) $ complexSetSize shorts name cap
                shorts % ix name % _2 .= cap
            (_, Just (_, oldCap)) -> do
                when (oldCap > cap) $ complexSetSize longs name cap
                longs % ix name % _2 .= cap
            (Nothing, Nothing) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"

complexCreate :: ComplexName -> Int64 -> LyapasT IO ()
complexCreate (LongComplex name) cap = do
    exists <- complexExists name
    if exists
     then throwM . NameError $ tshow name <> " already exists"
     else pure ()
    longs %= insertMap name (fromList [], fromIntegral cap)
complexCreate (ShortComplex name) cap = do
    exists <- complexExists name
    if exists
     then throwM . NameError $ tshow name <> " already exists"
     else pure ()
    shorts %= insertMap name (fromList [], fromIntegral cap)

complexDelete :: ComplexName -> LyapasT IO ()
complexDelete n = ensureComplexExists n >> case n of
    LongComplex name  -> longs  %= deleteMap name
    ShortComplex name -> shorts %= deleteMap name

complexSetSize
    :: (IsSequence vec, Num (Element vec), Index vec ~ Int)
    => Lens' FunctionState (HashMap ComplexIdentifier (vec, Int))
    -> ComplexIdentifier -> Index vec -> LyapasT IO ()
complexSetSize complexes name size = do
    (vec, cap) <- preuse (complexes % ix name) >>= \case
        Nothing -> throwM . NameError $ tshow name <> " does not exist"
        Just x -> pure x
    when (size > cap) $ throwM . IndexError $ tshow name <> " doesn't have enough capacity"
    let oldSize = olength vec
    if oldSize <= size
        then complexes % ix name % _1 %= take size
        else complexes % ix name % _1 .= (vec <> fromList (replicate (size - oldSize) 0))

-- | Shrink capacity to size
complexShrink :: ComplexName -> LyapasT IO ()
complexShrink n = ensureComplexExists n >> case n of
    LongComplex  name -> longs  % ix name %= \(v, _) -> (v, olength v)
    ShortComplex name -> shorts % ix name %= \(v, _) -> (v, olength v)

complexPush :: ComplexName -> Int64 -> LyapasT IO ()
complexPush n val = ensureComplexExists n >> case n of
    LongComplex name -> do
        (vec, cap) <- preuse (longs % ix name) -- handle impossible lens failure
            >>= \case {Just x -> pure x; Nothing -> error "unreachable"}
        if olength vec >= cap
            then throwM . IndexError $ "Can't push into " <> tshow name <> ": too small"
            else longs % ix name % _1 %= flip snoc val
    ShortComplex name -> do
        (vec, cap) <- preuse (shorts % ix name) -- handle impossible lens failure
            >>= \case {Just x -> pure x; Nothing -> error "unreachable"}
        if olength vec >= cap
            then throwM . IndexError $ "Can't push into " <> tshow name <> ": too small"
            else shorts % ix name % _1 %= flip snoc (fromIntegral val)

complexPrint :: ComplexName -> LyapasT IO ()
complexPrint (ShortComplex name) = do
    preuse (shorts % ix name) >>= \case
        Nothing -> throwM . NameError $ tshow name <> " does not exist"
        Just (bs, _) -> liftIO . B8.putStr $ bs
complexPrint (LongComplex name) = throwM . TypeError $
    "Can't print long complex " <> tshow name

complexExists :: ComplexIdentifier -> LyapasT IO Bool
complexExists name = do
    s <- use shorts
    if member name s
     then pure True
     else do
        l <- use longs
        pure $ member name l

ensureComplexExists :: ComplexName -> LyapasT IO ()
ensureComplexExists (LongComplex name) = do
    s <- use longs
    if member name s
        then pure ()
        else throwM . NameError $ tshow name <> " does not exist"
ensureComplexExists (ShortComplex name) = do
    s <- use shorts
    if member name s
        then pure ()
        else throwM . NameError $ tshow name <> " does not exist"


tshow :: Show a => a -> Text
tshow = pack . show

unOperatorName :: OperatorName -> Text
unOperatorName (OperatorName n) = n

pattern OP :: String -> OperatorName
pattern OP str <- (unpack . unOperatorName -> str)
    where OP = OperatorName . pack

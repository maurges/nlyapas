{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Language.Lyapas.Interpret
    ( runProgram
    , runFunction
    , runParagraph
    , InterpretError (..), Traceback
    ) where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bits (shiftL, shiftR, (.|.), (.&.), countTrailingZeros, finiteBitSize, complement, popCount, xor, clearBit)
import Data.ByteString (ByteString)
import Data.Containers (mapFromList, lookup, insertMap, member, deleteMap)
import Data.HashMap.Strict (HashMap)
import Data.Int (Int64)
import Data.MonoTraversable (olength, olength64, Element, opoint)
import Data.Sequences (index, fromList, IsSequence, Index, take, snoc, unsnoc, splitAt, uncons)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector.Storable (Vector)
import Optics.At () -- instances for bytestrings and vectors and hashmaps
import Optics.Core (at, ix, (%), (^?), (^.), _1, _2, over) -- orphans for bytestrings and vectors
import Optics.Lens (Lens')
import Optics.State (use, preuse, assign)
import Optics.State.Operators ((%=), (.=))
import Optics.TH (makeLenses)
import System.Clock (Clock (Realtime), getTime, sec)
import System.Random (randomIO, setStdGen, mkStdGen)

import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.IO as TIO

import Language.Lyapas.Syntax
import Prelude hiding (lookup, take, splitAt, drop)


type OpType = Int64

data FunctionState = FunctionState
    { _tau :: OpType
    , _vars :: HashMap VarName OpType
    , _longs :: HashMap ComplexIdentifier ((Vector OpType), Int) -- ^ snd is capacity
    , _shorts :: HashMap ComplexIdentifier (ByteString, Int) -- ^ snd is capacity
    , _overflow :: OpType
    } deriving (Eq, Show)
makeLenses ''FunctionState

emptyState :: FunctionState
emptyState = FunctionState
    { _tau = 0
    , _vars = mapFromList []
    , _longs = mapFromList []
    , _shorts = mapFromList []
    , _overflow = 0
    }

type Traceback = [FunctionName]

data ExecutionState = ExecutionState
    { _programFunctions :: [Function]
    , _traceback :: Traceback
    } deriving (Eq, Show)
makeLenses ''ExecutionState

type LyapasT m =
    StateT FunctionState -- local stackframe
    (ReaderT ExecutionState m)

trace :: Monad m => FunctionName -> LyapasT m a -> LyapasT m a
trace name = Reader.local $ over traceback ((:) name)

throwT :: (MonadThrow m, MonadReader ExecutionState m)
       => (Traceback -> InterpretError) -> m a
throwT err = do
    s <- Reader.ask
    throwM . err $ s ^. traceback

type LyapasThrow m = (MonadThrow m, MonadReader ExecutionState m)

data InterpretError
    = NotFound Text Traceback
    | TypeError Text Traceback
    | IndexError Text Traceback
    | NameError Text Traceback
    | ValueError Text Traceback
    | NotImplemented Text Traceback
    deriving (Eq, Show)
instance Exception InterpretError

getFunction :: (Monad m, MonadReader ExecutionState m)
            => FunctionName -> m (Maybe Function)
getFunction name = do
    s <- Reader.ask
    case filter ((== name) . functionName) $ s ^. programFunctions of
        [] -> pure Nothing
        x:_ -> pure . Just $ x


runProgram :: Program -> IO ()
runProgram (Program funcs) = do
    (func, name) <- case funcs of
        [] -> pure ([], FunctionName "")
        (Function name [] [] body):_ -> pure (body, name)
        (Function (FunctionName name) _ _ _):_ -> throwM . flip TypeError [] $
            "Function " <> name <> " (first function in program) should have no arguments"
    let execState = ExecutionState
            { _traceback = [name]
            , _programFunctions = funcs
            }
    void $ runReaderT (runStateT (runFunction func) emptyState) execState

-- | Prepare new "stack frame" for function, run it, and update outer stack
-- frame on exit
callFunction
    :: FunctionName
    -> [Either ComplexName Operand]
    -> [Either ComplexName Identifier]
    -> LyapasT IO ()
callFunction name rargs wargs = getFunction name >>= \case
    Nothing -> throwT . NameError $ tshow name <> " not found"
    Just (Function _ frargs fwargs body) -> do
        when (olength rargs /= olength frargs || olength wargs /= olength fwargs) $
            throwT . TypeError $ "Incorrect amount of arguments"
        s <- State.get
        State.put emptyState
        mapM_ (uncurry $ copyRArgFrom s) $ zip rargs frargs
        mapM_ (uncurry $ copyWArgFrom s) $ zip wargs fwargs
        trace name $ runFunction body
        finishState <- State.get
        State.put s
        mapM_ (uncurry $ returnWargFrom finishState) $ zip fwargs wargs -- order!
    where
        copyWArgFrom s (Left (LongComplex source)) (ArgComplex (LongComplex dest)) =
            copy s longs source dest
        copyWArgFrom s (Left (ShortComplex source)) (ArgComplex (ShortComplex dest)) =
            copy s shorts source dest
        copyWArgFrom s (Right ident) (ArgVar dest) = do
            source <- getIdentifierIn s ident
            vars % at dest .= Just source
        copyWArgFrom _ source dest = throwT . TypeError $
            "Mismatch between " <> tshow source <> " and " <> tshow dest
        --
        copyRArgFrom s (Left (LongComplex source)) (ArgComplex (LongComplex dest)) =
            copy s longs source dest
        copyRArgFrom s (Left (ShortComplex source)) (ArgComplex (ShortComplex dest)) =
            copy s shorts source dest
        copyRArgFrom s (Right op) (ArgVar dest) = do
            source <- getOperandIn s op
            vars % at dest .= Just source
        copyRArgFrom _ source dest = throwT . TypeError $
            "Mismatch between " <> tshow source <> " and " <> tshow dest
        --
        returnWargFrom s (ArgComplex (LongComplex source)) (Left (LongComplex dest)) =
            copy s longs source dest
        returnWargFrom s (ArgComplex (ShortComplex source)) (Left (ShortComplex dest)) =
            copy s shorts source dest
        returnWargFrom s (ArgVar var) (Right dest) =
            case s ^? vars % ix var of
                Nothing -> throwT . NameError $ "Internal function call error"
                Just source -> setIdentifier dest source
        returnWargFrom _ source dest = throwT . TypeError $
            "Mismatch between " <> tshow source <> " and " <> tshow dest
        --
        copy s lens source dest = case s ^. lens % at source of
            Nothing -> do
                throwT . NameError $ "Bad function call mapping " <> tshow source <> " to " <> tshow dest <> "\nin " <> tshow s
            Just x -> lens % at dest .= Just x


-- | Run function in current "stack frame"
runFunction :: [Paragraph] -> LyapasT IO ()
runFunction [] = pure ()
runFunction allParagraphs = go allParagraphs where
    go [] = pure ()
    go (p:ps) = runParagraph p >>= \case
        Nothing -> go ps
        Just jumpTo ->
            case dropWhile ((/= jumpTo) . paragraphName) allParagraphs of
                [] -> throwT . NotFound $ "Paragraph " <> tshow jumpTo <> " does not exist"
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
        lift $ setIdentifier val $ clearBit v oneIndex
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
    Unary (OP":set-null") op -> create op >> unarySet op (\_t _old -> 0)
    Unary (OP":set-full") op -> create op >> unarySet op (\_t _old -> complement 0)
    Unary (OP":set-assign") op -> create op >> unarySet op (\t _old -> t)
    --
    Unary (OP":disjunction") op -> unary op (.|.)
    Unary (OP":conjunction") op -> unary op (.&.)
    Unary (OP":xor") op -> unary op xor
    Unary (OP":left-shift") op -> unary op (\t x -> t `shiftL` fromIntegral x)
    Unary (OP":right-shift") op -> unary op (\t x -> t `shiftR` fromIntegral x)
    Unary (OP":add") op -> unary op (+)
    Unary (OP":sub") op -> unary op (-)
    Unary (OP":mul") op -> unary op (*)
    Unary (OP":div") op -> unaryDivMod op tau overflow
    Unary (OP":mod") op -> unaryDivMod op overflow tau
    Unary (OP":inc") op -> unarySet op (\_t old -> old + 1)
    Unary (OP":dec") op -> unarySet op (\_t old -> old - 1)
    --
    ComplexUnary (OP":complex-create") comp op -> getOperand op >>= complexCreate comp
    ComplexNullary (OP":complex-delete") comp -> complexDelete comp
    ComplexNullary (OP":complex-shrink") comp -> complexShrink comp
    ComplexNullary (OP":complex-push") comp -> use tau >>= complexPush comp
    ComplexNullary (OP":complex-pop") comp -> complexPop comp >>= assign tau
    ComplexUnary (OP":complex-push") comp op -> do
        t <- use tau
        i <- getOperand op
        complexPushAt i comp t
    ComplexUnary (OP":complex-pop") comp op -> do
        i <- getOperand op
        complexPopAt i comp >>= assign tau
    ComplexNullary (OP":set-null") comp -> complexSetNull comp
    ComplexNullary (OP":complex-print") comp -> complexPrint comp
    ComplexNullary (OP":complex-read") comp -> complexRead comp
    ComplexSymbols (OP":complex-append") comp str -> complexAppend comp str
    --
    FunctionCall name rargs wargs -> callFunction name rargs wargs
    --
    other -> throwT . NotImplemented . tshow $ other
    where
        nullary :: (OpType -> OpType) -> LyapasT IO ()
        nullary op = tau %= op
        --
        create :: Operand -> LyapasT IO ()
        create (MutableOperand ident@(IdentVar _)) = setIdentifier ident 0
        create _ = pure () -- either they implicitly exist, or will throw non-mutable error later
        --
        unarySet, unary :: Operand -> (OpType -> OpType -> OpType) -> LyapasT IO ()
        unarySet (MutableOperand ident) f = do
            t <- use tau
            i <- getIdentifier ident
            setIdentifier ident (f t i)
        unarySet op _ = throwT . TypeError $
            tshow op <> " is not a mutable operand"
        unary op f = do
            t <- use tau
            x <- getOperand op
            tau .= f t x
        unaryDivMod op division modulo = do
            t <- use tau
            x <- getOperand op
            let (d, m) = t `divMod` x
            division .= d
            modulo .= m

getOperandIn
    :: (LyapasThrow m, MonadIO m)
    => FunctionState -> Operand -> m OpType
getOperandIn st = \case
    MutableOperand ident -> getIdentifierIn st ident
    Constant x -> pure x
    OverflowValue -> pure $ st ^. overflow
    TimeValue -> liftIO $ sec <$> getTime Realtime
    UnitVector var -> do
        shift <- getVarIn st var
        pure $ 1 `shiftL` fromIntegral shift

getOperand :: Operand -> LyapasT IO OpType
getOperand op = State.get >>= flip getOperandIn op

getIdentifierIn
    :: forall m. (LyapasThrow m, MonadIO m)
    => FunctionState -> Identifier -> m OpType
getIdentifierIn st = \case
    IdentVar v -> getVarIn st v
    IdentComplexSize name ->
        case (st ^? shorts % ix name, st ^? longs % ix name) of
            (Just (v, _), _) -> pure . olength64 $ v
            (_, Just (v, _)) -> pure . olength64 $ v
            (Nothing, Nothing) ->
                throwT . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexCap name ->
        case (st ^? shorts % ix name, st ^? longs % ix name) of
            (Just (_, x), _) -> pure . fromIntegral $ x
            (_, Just (_, x)) -> pure . fromIntegral $ x
            (Nothing, Nothing) ->
                throwT . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexElement (LongComplex name) op -> do
        ind <- fromIntegral <$> getOperandIn st op
        (vec, _cap) <- getLongComplexIn st name
        indexGeneric vec ind
    IdentComplexElement (ShortComplex name) op -> do
        ind <- fromIntegral <$> getOperandIn st op
        (bs, _cap) <- getShortComplexIn st name
        fromIntegral <$> indexGeneric bs ind
    where
        indexGeneric :: (IsSequence seq) => seq -> Index seq -> m (Element seq)
        indexGeneric v i = case index v i of
            Nothing -> throwT . IndexError $ "Complex index out of range"
            Just x -> pure x

getIdentifier :: Identifier -> LyapasT IO OpType
getIdentifier i = State.get >>= flip getIdentifierIn i

getVarIn :: (LyapasThrow m, MonadIO m) => FunctionState -> VarName -> m OpType
getVarIn _st (VarName "X") = liftIO randomIO
getVarIn st name =
    case st ^? vars % ix name of
        Nothing -> throwT . NotFound $ "Using " <> tshow name <> " before assigning"
        Just x -> pure x

getVar :: VarName -> LyapasT IO OpType
getVar name = State.get >>= flip getVarIn name

getLongComplexIn :: LyapasThrow m => FunctionState -> ComplexIdentifier -> m (Vector OpType, Int)
getLongComplexIn st name =
    case st ^? longs % ix name of
        Nothing -> throwT . NotFound $ "Using " <> tshow name <> " before creation"
        Just x -> pure x

getLongComplex :: ComplexIdentifier -> LyapasT IO (Vector OpType, Int)
getLongComplex name = State.get >>= flip getLongComplexIn name

getShortComplexIn :: LyapasThrow m => FunctionState -> ComplexIdentifier -> m (ByteString, Int)
getShortComplexIn st name =
    case st ^? shorts % ix name of
        Nothing -> throwT . NotFound $ "Using " <> tshow name <> " before creation"
        Just x -> pure x

getShortComplex :: ComplexIdentifier -> LyapasT IO (ByteString, Int)
getShortComplex name = State.get >>= flip getShortComplexIn name

setIdentifier :: Identifier -> OpType -> LyapasT IO ()
setIdentifier i value = case i of
    IdentVar name -> do
        if name == VarName "X"
            then liftIO . setStdGen . mkStdGen . fromIntegral $ value
            else vars %= insertMap name value
    IdentComplexElement (ShortComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (comp, _) <- getShortComplex name
        when (ind >= olength comp) $
            throwT . IndexError $ "Indexing " <> tshow name <> " beyond size at " <> tshow ind
        shorts % ix name % _1 % ix ind .= fromIntegral value
    IdentComplexElement (LongComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (comp, _) <- getLongComplex name
        when (ind >= olength comp) $
            throwT . IndexError $ "Indexing " <> tshow name <> " beyond size at " <> tshow ind
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
                throwT . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexCap name -> do
        let cap = fromIntegral value
        s <- use shorts
        l <- use longs
        case (lookup name s, lookup name l) of
            (Just (v, _), _) -> do
                when (olength v > cap) $ complexSetSize shorts name cap
                shorts % ix name % _2 .= cap
            (_, Just (v, _)) -> do
                when (olength v > cap) $ complexSetSize longs name cap
                longs % ix name % _2 .= cap
            (Nothing, Nothing) ->
                throwT . NotFound $ "Using " <> tshow name <> " before creation"

complexCreate :: ComplexName -> OpType -> LyapasT IO ()
complexCreate (LongComplex name) cap = do
    exists <- complexExists name
    if exists
     then throwT . NameError $ tshow name <> " already exists"
     else pure ()
    longs %= insertMap name (fromList [], fromIntegral cap)
complexCreate (ShortComplex name) cap = do
    exists <- complexExists name
    if exists
     then throwT . NameError $ tshow name <> " already exists"
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
        Nothing -> throwT . NameError $ tshow name <> " does not exist"
        Just x -> pure x
    when (size > cap) $ throwT . IndexError $ tshow name <> " doesn't have enough capacity"
    let oldSize = olength vec
    if oldSize >= size
        then complexes % ix name % _1 %= take size
        else complexes % ix name % _1 .= (vec <> fromList (replicate (size - oldSize) 0))

-- | Shrink capacity to size
complexShrink :: ComplexName -> LyapasT IO ()
complexShrink n = ensureComplexExists n >> case n of
    LongComplex  name -> longs  % ix name %= \(v, _) -> (v, olength v)
    ShortComplex name -> shorts % ix name %= \(v, _) -> (v, olength v)

complexPush :: ComplexName -> OpType -> LyapasT IO ()
complexPush n val = getComplex n >>= \case
    (Left (vec, cap), name) ->
        if olength vec >= cap
            then throwT . ValueError $ "Can't push into " <> tshow name <> ": too small"
            else longs % ix name % _1 %= flip snoc val
    (Right (vec, cap), name) ->
        if olength vec >= cap
            then throwT . ValueError $ "Can't push into " <> tshow name <> ": too small"
            else shorts % ix name % _1 %= flip snoc (fromIntegral val)

complexPop :: ComplexName -> LyapasT IO OpType
complexPop n = getComplex n >>= \case
    (Left (vec, _), name) -> case unsnoc vec of
        Nothing -> throwT . ValueError $ "Can't pop from empty " <> tshow name
        Just (vec', x) -> do
            longs % ix name % _1 .= vec'
            pure x
    (Right (bs, _), name) -> case unsnoc bs of
        Nothing -> throwT . ValueError $ "Can't pop from empty " <> tshow name
        Just (bs', x) -> do
            shorts % ix name % _1 .= bs'
            pure . fromIntegral $ x

complexPushAt :: OpType -> ComplexName -> OpType -> LyapasT IO ()
complexPushAt ind n val = getComplex n >>= \case
    (Left (vec, cap), name) -> do
        checkCap vec cap name
        longs % ix name % _1 .= genericPushAt (fromIntegral ind) vec val
    (Right (bs, cap), name) -> do
        checkCap bs cap name
        shorts % ix name % _1 .= genericPushAt (fromIntegral ind) bs (fromIntegral val)
    where
        checkCap :: IsSequence seq => seq -> Int -> ComplexIdentifier -> LyapasT IO ()
        checkCap v cap name = if olength v >= cap
            then throwT . ValueError $ "Can't push into " <> tshow name <> ": too small"
            else pure ()
        genericPushAt :: IsSequence seq => Index seq -> seq -> Element seq -> seq
        genericPushAt i v x =
            let (begin, end) = splitAt i v
            in begin <> opoint x <> end

complexPopAt :: OpType -> ComplexName -> LyapasT IO OpType
complexPopAt ind n = getComplex n >>= \case
    (Left (vec, _), name) -> do
        (x, vec') <- genericPopAt (fromIntegral ind) vec
        longs % ix name % _1 .= vec'
        pure x
    (Right (bs, _), name) -> do
        (x, bs') <- genericPopAt (fromIntegral ind) bs
        shorts % ix name % _1 .= bs'
        pure . fromIntegral $ x
    where
        genericPopAt :: IsSequence seq => Index seq -> seq -> LyapasT IO (Element seq, seq)
        genericPopAt i v =
            let (begin, end) = splitAt i v
            in case uncons end of
                Just (x, end') -> pure (x, begin <> end')
                Nothing -> throwT . ValueError $
                    "Can't pop from empty " <> tshow n

complexSetNull :: ComplexName -> LyapasT IO ()
complexSetNull n = ensureComplexExists n >> case n of
    LongComplex name ->
        longs % ix name % _1 %= fromList . flip replicate 0 . olength
    ShortComplex name ->
        shorts % ix name % _1 %= fromList . flip replicate 0 . olength

complexPrint :: ComplexName -> LyapasT IO ()
complexPrint (ShortComplex name) = do
    preuse (shorts % ix name) >>= \case
        Nothing -> throwT . NameError $ tshow name <> " does not exist"
        Just (bs, _) -> liftIO . B8.putStr $ bs
complexPrint (LongComplex name) = throwT . TypeError $
    "Can't print long complex " <> tshow name

complexRead :: ComplexName -> LyapasT IO ()
complexRead (ShortComplex name) = preuse (shorts % ix name) >>= \case
    Nothing -> throwT . NameError $ tshow name <> " does not exist"
    Just (old, cap) -> do
        new <- encodeUtf8 <$> liftIO TIO.getLine
        when (olength old + olength new > cap) $
            throwT . IndexError $ tshow name <> " is not big enough for append"
        shorts % ix name % _1 .= old <> new
complexRead (LongComplex name) =
    throwT . TypeError $ tshow name <> " is not a byte complex"

complexAppend :: ComplexName -> StringLiteral -> LyapasT IO ()
complexAppend (ShortComplex name) (StringLiteral text) = preuse (shorts % ix name) >>= \case
    Nothing -> throwT . NameError $ tshow name <> " does not exist"
    Just (old, cap) -> do
        let new = encodeUtf8 text
        when (olength old + olength new > cap) $
            throwT . IndexError $ tshow name <> " is not big enough for append"
        shorts % ix name % _1 .= old <> new
complexAppend (LongComplex name) _ =
    throwT . TypeError $ tshow name <> " is not a byte complex"

complexExists :: ComplexIdentifier -> LyapasT IO Bool
complexExists name = do
    s <- use shorts
    if member name s
     then pure True
     else do
        l <- use longs
        pure $ member name l

ensureComplexExists :: ComplexName -> LyapasT IO ()
ensureComplexExists = void . getComplex

getComplex :: ComplexName -> LyapasT IO (Either (Vector OpType, Int) (ByteString, Int), ComplexIdentifier)
getComplex (LongComplex name) =
    preuse (longs % ix name) >>= \case
        Just x -> pure (Left x, name)
        Nothing -> throwT . NameError $ tshow name <> " does not exist"
getComplex (ShortComplex name) =
    preuse (shorts % ix name) >>= \case
        Just x -> pure (Right x, name)
        Nothing -> throwT . NameError $ tshow name <> " does not exist"


tshow :: Show a => a -> Text
tshow = pack . show

unOperatorName :: OperatorName -> Text
unOperatorName (OperatorName n) = n

functionName :: Function -> FunctionName
functionName (Function x _ _ _) = x

debugState :: LyapasT IO ()
debugState = do
    s <- State.get
    liftIO . putStrLn $ "#debug state:" <> show s

pattern OP :: String -> OperatorName
pattern OP str <- (unpack . unOperatorName -> str)
    where OP = OperatorName . pack

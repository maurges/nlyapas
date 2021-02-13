{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Lyapas.Interpret
    ( runProgram
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
import Data.Bits (shiftL, shiftR, (.|.), (.&.), testBit)
import Data.ByteString (ByteString)
import Data.Containers (mapFromList, lookup, insertMap)
import Data.HashMap.Strict (HashMap)
import Data.Int (Int64)
import Data.MonoTraversable (olength, olength64)
import Data.Sequences (index, fromList)
import Data.Text (Text, pack)
import Data.Vector.Storable (Vector)
import Lens.Micro.Platform (ix, (&), (.~)) -- orphans for bytestrings and vectors
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import System.Clock (Clock (Realtime), getTime, sec)
import System.Random (randomIO, setStdGen, mkStdGen)

import qualified Data.Char as Char

import Language.Lyapas.Syntax
import Prelude hiding (lookup)


data FunctionState = FunctionState
    { _tau :: Int64
    , _vars :: HashMap VarName Int64
    , _longs :: HashMap ComplexIdentifier ((Vector Int64), Int)
    , _shorts :: HashMap ComplexIdentifier (ByteString, Int)
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
    deriving (Eq, Show)
instance Exception InterpretError


runProgram :: Program -> IO ()
runProgram (Program funcs) = do
    func <- case funcs of
        [] -> throwM . NotFound $ "Program has no functions to run"
        f@(Function _ [] [] _):_ -> pure f
        (Function (FunctionName name) _ _ _):_ -> throwM . TypeError $
            "Function " <> name <> " (first function in program) should have no arguments"
    let initialState = FunctionState
            { _tau = 0
            , _vars = mapFromList []
            , _longs = mapFromList []
            , _shorts = mapFromList []
            , _overflow = 0
            }
    void $ runReaderT (runStateT (callFunction func) initialState) funcs

-- | Run function and update outer state on exit
callFunction :: Function -> LyapasT IO ()
callFunction = error "call function"

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
        case findOneIn v of
            Nothing -> do
                lift $ setIdentifier enumerator 0
                goto name
            Just i -> lift $ setIdentifier enumerator i
    where
    goto = ExceptT . pure . Left
    findOneIn :: Int64 -> Maybe Int64
    findOneIn = go 0 where
        go 64 _ = Nothing
        go !n !x = if testBit x n
            then Just . fromIntegral $ n
            else go (n+1) x

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
            (Just (_, x), _) -> pure . fromIntegral $ x
            (_, Just (_, x)) -> pure . fromIntegral $ x
            (Nothing, Nothing) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexCap name -> do
        s <- use shorts
        l <- use longs
        case (lookup name s, lookup name l) of
            (Just (v, _), _) -> pure . olength64 $ v
            (_, Just (v, _)) -> pure . olength64 $ v
            (Nothing, Nothing) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexElement (LongComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (vec, size) <- getLongComplex name
        if ind > size
        then throwM . IndexError $
            "Indexing " <> tshow name
            <> " with size " <> tshow size
            <> " beyond bounds on " <> tshow ind
        else case index vec ind of
            Nothing -> throwM . IndexError $ "Internal error"
            Just x -> pure x
    IdentComplexElement (ShortComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (bs, size) <- getShortComplex name
        if ind > size
        then throwM . IndexError $
            "Indexing " <> tshow name
            <> " with size " <> tshow size
            <> " beyond bounds on " <> tshow ind
        else case index bs ind of
            Nothing -> throwM . IndexError $ "Internal error"
            Just x -> pure . fromIntegral $ x

getVar :: VarName -> LyapasT IO Int64
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
    IdentVar name -> vars %= insertMap name value
    IdentComplexElement (ShortComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (comp, size) <- getShortComplex name
        when (ind >= size) $
            throwM . IndexError $ "Indexing " <> tshow name <> " beyond size at " <> tshow ind
        let comp' = comp & ix ind .~ fromIntegral value
        shorts %= insertMap name (comp', size)
    IdentComplexElement (LongComplex name) op -> do
        ind <- fromIntegral <$> getOperand op
        (comp, size) <- getLongComplex name
        when (ind >= size) $
            throwM . IndexError $ "Indexing " <> tshow name <> " beyond size at " <> tshow ind
        let comp' = comp & ix ind .~ fromIntegral value
        longs %= insertMap name (comp', size)
    IdentComplexSize name -> do
        let size = fromIntegral value
        s <- use shorts
        l <- use longs
        case (lookup name s, lookup name l) of
            (Just (v, _), _) ->
                if size > olength v
                then throwM . IndexError $ "Setting size of " <> tshow name <> " beyond capacity"
                else shorts %= insertMap name (v, size)
            (_, Just (v, _)) ->
                if size > olength v
                then throwM . IndexError $ "Setting size of " <> tshow name <> " beyond capacity"
                else longs %= insertMap name (v, size)
            (Nothing, Nothing) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"
    IdentComplexCap name -> do
        let cap = fromIntegral value
        s <- use shorts
        l <- use longs
        case (lookup name s, lookup name l) of
            (Just (v, size), _) ->
                let v' = v <> fromList (replicate (cap - olength v) 0)
                in shorts %= insertMap name (v', size)
            (_, Just (v, size)) ->
                let v' = v <> fromList (replicate (cap - olength v) 0)
                in longs %= insertMap name (v', size)
            (Nothing, Nothing) ->
                throwM . NotFound $ "Using " <> tshow name <> " before creation"


tshow :: Show a => a -> Text
tshow = pack . show

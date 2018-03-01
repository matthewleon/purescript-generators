module Data.Generator where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.ST (ST, pureST)
import Control.Monad.State (StateT, execStateT, get, put)
import Data.Array (length, replicate, unsafeIndex)
import Data.Array.ST (STArray, modifySTArray, thaw, unsafeFreeze)
import Data.Either (either)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

type GenT e m = ReaderT (e -> m Unit) m

type Producer m e            = GenT e m Unit
type Consumer m e            = e -> m Unit
type Transducer m1 m2 e1 e2  = Producer m1 e1 -> Producer m2 e2

yield :: forall m e. Monad m => e -> Producer m e
yield e = ask >>= \f -> lift $ f e

runGenT :: forall m e. Monad m => Producer m e -> Consumer m e -> m Unit
runGenT = runReaderT

mapG :: forall m e1 e2. Monad m => (e1 -> e2) -> Transducer (GenT e2 m) m e1 e2
mapG f gen = runGenT gen $ yield <<< f

filterG :: forall m e. Monad m => (e -> Boolean) -> Transducer (GenT e m) m e e
filterG f gen = runGenT gen $ \e -> if f e then yield e else pure unit

foldG :: forall m e s. Monad m => (s -> e -> m s) -> s -> Producer (StateT s m) e -> m s
foldG f s0 gen = execStateT (runGenT gen consumer) s0
  where
  consumer x = get >>= (\s -> lift $ f s x) >>= put

forever :: forall m e. MonadRec m => e -> Producer m e
forever = tailRecM \e -> do
  yield e
  pure $ Loop e

orG :: forall m. Monad m => Producer (ExceptT Boolean m) Boolean -> m Boolean
orG gen = either id (const false) <$> runExceptT (runGenT gen orC)
  where
    orC :: forall m'. MonadThrow Boolean m' => Consumer m' Boolean
    orC true  = throwError true
    orC false = pure unit

-- TODO: try to figure out wtf this signature says
anyG :: forall m e. Monad m => (e -> Boolean) -> ReaderT (e -> ReaderT (Boolean -> ExceptT Boolean m Unit) (ExceptT Boolean m) Unit) (ReaderT (Boolean -> ExceptT Boolean m Unit) (ExceptT Boolean m)) Unit -> m Boolean
anyG f = orG <<< mapG f

maybeG :: forall m e. Monad m => Maybe e -> Producer m e
maybeG = maybe (pure unit) yield

arrayG :: forall m e. MonadRec m => Array e -> Producer m e
arrayG xs =
  let len = length xs
  in  flip tailRecM 0 \i ->
    if i >= len
      then pure $ Done unit
      else do
         yield $ unsafePartial unsafeIndex xs i
         pure $ Loop $ i + 1

stArrayC
  :: forall a b h
   . STArray h a -> (a -> b -> a) -> Consumer (Eff (st :: ST h)) (Tuple Int b)
stArrayC starr combine (Tuple i e) = void $ modifySTArray starr i (flip combine e)

accumSTArray
  :: forall a b h
   . STArray h a -> (a -> b -> a) -> (forall h'. Producer (Eff (st :: ST h')) (Tuple Int b)) -> Eff (st :: ST h) Unit
accumSTArray starr combine gen = gen `runGenT` stArrayC starr combine

accumArray
  :: forall a
   . Monoid a
  => Int -> (forall h. Producer (Eff (st :: ST h)) (Tuple Int a)) -> Array a
accumArray = accumArray' mempty append

accumArray'
  :: forall a b
   . a -> (a -> b -> a) -> Int -> (forall h. Producer (Eff (st :: ST h)) (Tuple Int b)) -> Array a
accumArray' initValue combine len gen = pureST do
    as <- thaw (replicate len initValue) -- TODO: unsafeThaw
    accumSTArray as combine gen
    unsafeFreeze as

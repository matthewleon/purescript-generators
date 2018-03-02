module Data.Generator.Eff where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Newtype (class Newtype)

newtype GenEff e eff a = GenEff ((e -> Eff eff Unit) -> Eff eff a)

derive instance newtypeGenEff :: Newtype (GenEff e eff a) _

instance functorGenEff :: Functor (GenEff e eff) where
  map :: forall a b e eff. (a -> b) -> GenEff e eff a -> GenEff e eff b
  map f (GenEff g) = GenEff \e -> f <$> g e

instance applyGenEff :: Apply (GenEff e eff) where
  apply :: forall a b e eff. GenEff e eff (a -> b) -> GenEff e eff a -> GenEff e eff b
  apply (GenEff f) (GenEff g) = GenEff \e -> f e <*> g e

instance applicativeGenEff :: Applicative (GenEff e eff) where
  pure :: forall a e eff. a -> GenEff e eff a
  pure a = GenEff \_ -> pure a

instance bindGenEff :: Bind (GenEff e eff) where
  bind :: forall a b e eff. GenEff e eff a -> (a -> GenEff e eff b) -> GenEff e eff b
  bind (GenEff g) k = GenEff \e -> do
    a <- g e
    case k a of GenEff f -> f e

instance monadGenEff :: Monad (GenEff e eff)

instance monadRecGenEff :: MonadRec (GenEff e eff) where
  tailRecM :: forall a b e eff. (a -> GenEff e eff (Step a b)) -> a -> GenEff e eff b
  tailRecM k a = GenEff \e -> tailRecM (k' e) a
    where
    k' e' a' = case k a' of GenEff f -> pure =<< f e'

type Producer eff e = GenEff e eff Unit
type Consumer eff e = e -> Eff eff Unit
type Transducer eff1 eff2 e1 e2  = Producer eff1 e1 -> Producer eff2 e2

yield :: forall e eff. e -> Producer eff e
yield e = GenEff \f -> f e

runGenEff :: forall eff e. Producer eff e -> Consumer eff e -> Eff eff Unit
runGenEff (GenEff f) = f

forever :: forall eff e. e -> Producer eff e
forever = tailRecM \e -> do
  yield e
  pure $ Loop e

module Data.Generator.Eff where

import Prelude

import Control.Monad.Eff (Eff)

newtype Gen e a = Gen forall eff. ((e -> Eff eff Unit) -> Eff eff a)

type Producer e = Gen e Unit
--type Consumer e = e ->

{-
yield :: forall e. e -> Producer e
yield e = Gen (\f -> f e)
-}

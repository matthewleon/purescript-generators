module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Generator (Producer, accumArray, arrayG, mapG, runGenT, yield)
import Data.String (toUpper)
import Data.Tuple (Tuple(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  runGenT (yield "Hello world.") log
  runGenT (toUpper `mapG` helloWorldArrayProducer) log
  logShow arr
  where
  arr = accumArray 5 do
    yield (Tuple 2 "hey")
    yield (Tuple 0 "hello")
    yield (Tuple 2 "world")
    yield (Tuple 1 "world")

helloWorldArrayProducer :: forall m. MonadRec m => Producer m String
helloWorldArrayProducer = arrayG ["Hello", "everyone", "in", "the", "world"]

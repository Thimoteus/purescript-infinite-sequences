module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Sequence.Infinite (Sequence, (!!))
import Data.Sequence.Infinite as Sequence

even :: Int -> Boolean
even x = (x `mod` 2) == 0

nats :: Sequence Int
nats = Sequence.Sequence id

evens :: Sequence Int
evens = Sequence.filter even nats

nattails :: Sequence (Sequence Int)
nattails = Sequence.duplicate nats

natsagain :: Sequence Int
natsagain = Sequence.choice (Sequence.Sequence \_ -> 0) nattails

fibs :: Sequence Int
fibs = Sequence.Sequence f where
  f 0 = 0
  f 1 = 1
  f n = f (n - 1) + f (n - 2)

main :: Eff (console :: CONSOLE) Unit
main = do
  log "3rd element of 101st element of nattails is:"
  logShow $ nattails !! 101 !! 3
  log "201st even number is:"
  logShow $ evens !! 201
  log "17th fibonacci number is:"
  logShow $ fibs !! 17
  log "Recovering nats from nattails by using choice, should print 10:"
  logShow $ natsagain !! 10

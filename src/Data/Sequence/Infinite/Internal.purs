module Data.Sequence.Infinite.Internal where

import Prelude

mu :: (Int -> Boolean) -> Int
mu = muFrom 0

muFrom :: Int -> (Int -> Boolean) -> Int
muFrom n f = go n where
  go acc
    | f acc = acc
    | otherwise = go (acc + 1)
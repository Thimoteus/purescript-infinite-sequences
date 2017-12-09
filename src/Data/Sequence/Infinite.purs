module Data.Sequence.Infinite where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Ord (abs)
import Data.Sequence.Infinite.Internal (mu, muFrom)

newtype Sequence a = Sequence (Int -> a)

unsequence :: forall a. Sequence a -> Int -> a
unsequence (Sequence s) = s

-- | Note: eq in this instance is at best partially decidable.
instance eqSequence :: Eq a => Eq (Sequence a) where
  eq (Sequence x) (Sequence y) = go 0 where
    go n
      | x n == y n = go (n + 1)
      | otherwise = false

-- | Note: compare in this instance is at best partially decidable.
instance ordSequence :: Ord a => Ord (Sequence a) where
  compare (Sequence x) (Sequence y) = go 0 where
    go n
      | compare (x n) (y n) == EQ = go (n + 1)
      | otherwise = compare (x n) (y n)

instance heytingSequence :: HeytingAlgebra a => HeytingAlgebra (Sequence a) where
  tt = pure tt
  ff = pure ff
  not = map not
  disj = zipWith disj
  conj = zipWith conj
  implies = zipWith implies

instance booleanAlgebraSequence :: BooleanAlgebra a => BooleanAlgebra (Sequence a)

instance semigroupSequence :: Semigroup a => Semigroup (Sequence a) where
  append = zipWith append

instance monoidSequence :: Monoid a => Monoid (Sequence a) where
  mempty = pure mempty

instance semiringSequence :: Semiring a => Semiring (Sequence a) where
  zero = pure zero  
  one = pure one
  add = zipWith add
  mul = zipWith mul

instance ringSequence :: Ring a => Ring (Sequence a) where
  sub = zipWith sub

instance commutativeRingSequence :: CommutativeRing a => CommutativeRing (Sequence a)

instance euclideanRingSequence :: EuclideanRing a => EuclideanRing (Sequence a) where
  degree _ = 0
  div = zipWith div
  mod = zipWith mod

instance divisionRingSequence :: DivisionRing a => DivisionRing (Sequence a) where
  recip = map recip

instance fieldSequence :: Field a => Field (Sequence a)

instance functorSequence :: Functor Sequence where
  map f (Sequence a) = Sequence (f <<< a)

instance applySequence :: Apply Sequence where
  apply (Sequence f) (Sequence a) = Sequence \ i -> f i (a i)

instance applicativeSequence :: Applicative Sequence where
  pure = Sequence <<< const

instance bindSequence :: Bind Sequence where
  bind (Sequence a) k = Sequence \ i -> unsequence (k (a i)) i

instance monadSequence :: Monad Sequence

instance extendSequence :: Extend Sequence where
  extend k = map k <<< duplicate

instance comonadSequence :: Comonad Sequence where
  extract = head

-- | Comonadic `duplicate`, aka `tails`.
duplicate :: forall a. Sequence a -> Sequence (Sequence a)
duplicate = Sequence <<< flip drop

cons :: forall a. a -> Sequence a -> Sequence a
cons x (Sequence xs) = Sequence f where
  f 0 = x
  f n = xs (n - 1)

infixr 6 cons as :

head :: forall a. Sequence a -> a
head = flip index 0

tail :: forall a. Sequence a -> Sequence a
tail (Sequence xs) = Sequence \ i -> xs (i + 1)

index :: forall a. Sequence a -> Int -> a
index (Sequence xs) n = xs (abs n)

infixl 8 index as !!

insertAt :: forall a. Int -> a -> Sequence a -> Sequence a
insertAt idx x (Sequence a) = Sequence f where
  f i
    | i < idx = a i
    | i == idx = x
    | otherwise = a (i - 1)

insertBy :: forall a. (a -> a -> Ordering) -> a -> Sequence a -> Sequence a
insertBy f x s@(Sequence g) = seq where
  idx = mu \ i -> f x (g i) /= GT
  seq = insertAt idx x s

insert :: forall a. Ord a => a -> Sequence a -> Sequence a
insert = insertBy compare

iterate :: forall a. (a -> a) -> a -> Sequence a
iterate f seed = Sequence g where
  g 0 = seed
  g n = f (g (n - 1))

filter :: forall a. (a -> Boolean) -> Sequence a -> Sequence a
filter f (Sequence k) = Sequence s where
  s n = findNth n (mu (f <<< k))
  findNth n idx
    | n == 0 = k idx
    | otherwise = findNth (n - 1) (muFrom (idx + 1) (f <<< k))

updateAt :: forall a. Int -> a -> Sequence a -> Sequence a
updateAt idx x (Sequence a) = Sequence f where
  f i = if i == idx then x else a i

alterAt :: forall a. Int -> (a -> Maybe a) -> Sequence a -> Sequence a
alterAt idx f g = case f (g !! idx) of
  Just x -> updateAt idx x g
  _ -> deleteAt idx g

modifyAt :: forall a. Int -> (a -> a) -> Sequence a -> Sequence a
modifyAt idx f seq = updateAt idx (f (seq !! idx)) seq

merge :: forall a. Sequence a -> Sequence a -> Sequence a
merge (Sequence f) (Sequence h) = Sequence g where
  g i = if i `mod` 2 == 0 then f (i / 2) else h (i / 2)

drop :: forall a. Int -> Sequence a -> Sequence a
drop n (Sequence f) = Sequence h where
  h i = f (i + n)

dropWhile :: forall a. (a -> Boolean) -> Sequence a -> Sequence a
dropWhile f g@(Sequence seq) = drop idx g where
  idx = mu \ i -> not (f (seq i))

deleteBy :: forall a. (a -> a -> Boolean) -> a -> Sequence a -> Sequence a
deleteBy eqrel rep f@(Sequence seq) = deleteAt idx f where
  idx = mu \ i -> seq i `eqrel` rep

delete :: forall a. Eq a => a -> Sequence a -> Sequence a
delete = deleteBy eq

deleteAt :: forall a. Int -> Sequence a -> Sequence a
deleteAt idx (Sequence f) = Sequence g where
  g i = if i < idx then f i else f (i + 1)

elemIndex :: forall a. Eq a => a -> Sequence a -> Int
elemIndex x s = findIndex (_ == x) s

findIndex :: forall a. (a -> Boolean) -> Sequence a -> Int
findIndex f (Sequence a) = mu (f <<< a)

find :: forall a. (a -> Boolean) -> Sequence a -> a
find p s = unsequence s (findIndex p s)

repeat :: forall a. a -> Sequence a
repeat = pure

uncons :: forall a. Sequence a -> {head :: a, tail :: Sequence a}
uncons xs = {head: head xs, tail: tail xs}

zipWith :: forall a b c. (a -> b -> c) -> Sequence a -> Sequence b -> Sequence c
zipWith f (Sequence xs) (Sequence ys) = Sequence \ i -> xs i `f` ys i

mapWithIndex :: forall a b. (Int -> a -> b) -> Sequence a -> Sequence b
mapWithIndex f (Sequence a) = Sequence \ i -> f i (a i)

choice :: forall a. Sequence Int -> Sequence (Sequence a) -> Sequence a
choice cf cs = Sequence \ i -> (cs !! i) !! (cf !! i)

partition :: forall a. (a -> Boolean) -> Sequence a -> {yes :: Sequence a, no :: Sequence a}
partition f s =
  let
    yes = filter f s
    no = filter (not <<< f) s
  in
    {yes, no}
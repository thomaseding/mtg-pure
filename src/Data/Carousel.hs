{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.Carousel (
  Carousel,
  carSingle,
  carLeft,
  carRight,
  carCursor,
  carFromList,
  carToList,
) where

import qualified Data.Array as A

data Carousel a = Carousel (A.Array Int a) Int

carSingle :: a -> Carousel a
carSingle x = Carousel (A.listArray (0, 0) [x]) 0

carFromList :: [a] -> Carousel a
carFromList xs = Carousel (A.listArray (0, length xs - 1) xs) 0

carToList :: Carousel a -> [a]
carToList (Carousel xs i) = bs ++ as
 where
  ys = A.elems xs
  (as, bs) = splitAt i ys

carLeft :: Carousel a -> Carousel a
carLeft (Carousel xs i) = Carousel xs ((i - 1 + s) `mod` s)
 where
  s = A.rangeSize (A.bounds xs)

carRight :: Carousel a -> Carousel a
carRight (Carousel xs i) = Carousel xs ((i + 1) `mod` A.rangeSize (A.bounds xs))

carCursor :: Carousel a -> a
carCursor (Carousel xs i) = xs A.! i

instance Functor Carousel where
  fmap f (Carousel xs i) = Carousel (fmap f xs) i

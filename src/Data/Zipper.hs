{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.Zipper (
  Zipper,
  zipSingle,
  zipLeft,
  zipRight,
  zipCursor,
  zipFromList,
  zipToList,
) where

data Zipper a = Zipper [a] a [a]

zipSingle :: a -> Zipper a
zipSingle a = Zipper [] a []

zipLeft :: Zipper a -> Zipper a
zipLeft (Zipper (l : ls) x rs) = Zipper ls l (x : rs)
zipLeft (Zipper [] x rs) = Zipper [] x rs

zipRight :: Zipper a -> Zipper a
zipRight (Zipper ls x (r : rs)) = Zipper (x : ls) r rs
zipRight (Zipper ls x []) = Zipper ls x []

zipCursor :: Zipper a -> a
zipCursor (Zipper _ x _) = x

zipFromList :: [a] -> Zipper a
zipFromList [] = error "zipFromList: empty list"
zipFromList (x : xs) = Zipper [] x xs

zipToList :: Zipper a -> [a]
zipToList (Zipper ls x rs) = reverse ls ++ [x] ++ rs

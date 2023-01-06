{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.TupleN (
  T0 (..),
  T1 (..),
  T2 (..),
  T3 (..),
  T4 (..),
  T5 (..),
  T6 (..),
  T7 (..),
  T8 (..),
  T9 (..),
  T10 (..),
  T11 (..),
  T12 (..),
  Wrap1 (..),
  Wrap2 (..),
) where

import safe Data.Typeable (Typeable)

data T0 = T0
  deriving (Eq, Ord, Show, Typeable)

newtype T1 a = T1 a
  deriving (Eq, Ord, Show, Typeable)

data T2 a b = T2 a b
  deriving (Eq, Ord, Show, Typeable)

data T3 a b c = T3 a b c
  deriving (Eq, Ord, Show, Typeable)

data T4 a b c d = T4 a b c d
  deriving (Eq, Ord, Show, Typeable)

data T5 a b c d e = T5 a b c d e
  deriving (Eq, Ord, Show, Typeable)

data T6 a b c d e f = T6 a b c d e f
  deriving (Eq, Ord, Show, Typeable)

data T7 a b c d e f g = T7 a b c d e f g
  deriving (Eq, Ord, Show, Typeable)

data T8 a b c d e f g h = T8 a b c d e f g h
  deriving (Eq, Ord, Show, Typeable)

data T9 a b c d e f g h i = T9 a b c d e f g h i
  deriving (Eq, Ord, Show, Typeable)

data T10 a b c d e f g h i j = T10 a b c d e f g h i j
  deriving (Eq, Ord, Show, Typeable)

data T11 a b c d e f g h i j k = T11 a b c d e f g h i j k
  deriving (Eq, Ord, Show, Typeable)

data T12 a b c d e f g h i j k l = T12 a b c d e f g h i j k l
  deriving (Eq, Ord, Show, Typeable)

newtype Wrap1 liftT a = Wrap1 {unWrap1 :: liftT (T1 a)}

newtype Wrap2 liftT a b = Wrap2 {unWrap2 :: liftT (T2 a b)}

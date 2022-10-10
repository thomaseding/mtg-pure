{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.Nat (
  IsNat (..),
  Nat (..),
  NatList (..),
  Fin (..),
  natToInt,
  finToInt,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Eq, Ord, Show, Typeable)

class (Typeable n) => IsNat (n :: Nat) where
  litNat :: Nat

instance IsNat 'Z where
  litNat = Z

instance IsNat n => IsNat ( 'S n) where
  litNat = S (litNat @n)

data NatList (n :: Nat) (a :: Type) where
  LZ :: a -> NatList 'Z a
  LS :: IsNat n => a -> NatList n a -> NatList ( 'S n) a
  deriving (Typeable)

deriving instance Eq a => Eq (NatList n a)

deriving instance Functor (NatList n)

deriving instance Ord a => Ord (NatList n a)

deriving instance Show a => Show (NatList n a)

data Fin (n :: Nat) where
  FZ :: IsNat n => Fin n
  FS :: IsNat n => Fin n -> Fin ( 'S n)
  deriving (Typeable)

natToInt :: Nat -> Int
natToInt = \case
  Z -> 0
  S n -> 1 + natToInt n

finToInt :: Fin n -> Int
finToInt = \case
  FZ -> 0
  FS n -> 1 + finToInt n

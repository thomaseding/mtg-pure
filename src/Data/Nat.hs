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
  ToNat,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe qualified GHC.TypeLits as GHC

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

data NatList (user :: Type) (n :: Nat) (elem :: Type) where
  -- NOTE: `LZ` has `elem` in it to prevent empty list choices
  LZ :: forall user elem. elem -> NatList user 'Z elem
  LS :: (Typeable user, IsNat n) => elem -> NatList user n elem -> NatList user ( 'S n) elem
  deriving (Typeable)

deriving instance Eq a => Eq (NatList user n a)

deriving instance Functor (NatList user n)

deriving instance Ord a => Ord (NatList user n a)

deriving instance Show a => Show (NatList user n a)

data Fin (user :: Type) (n :: Nat) where
  FZ :: (Typeable user, IsNat n) => Fin user n
  FS :: (Typeable user, IsNat n) => Fin user n -> Fin user ( 'S n)
  deriving (Typeable)

natToInt :: Nat -> Int
natToInt = \case
  Z -> 0
  S n -> 1 + natToInt n

finToInt :: Fin user n -> Int
finToInt = \case
  FZ -> 0
  FS n -> 1 + finToInt n

type family ToNat (i :: GHC.Nat) = (n :: Nat) | n -> i

type instance ToNat 0 = 'Z

type instance ToNat 1 = 'S 'Z

type instance ToNat 2 = 'S ( 'S 'Z)

type instance ToNat 3 = 'S ( 'S ( 'S 'Z))

type instance ToNat 4 = 'S ( 'S ( 'S ( 'S 'Z)))

type instance ToNat 5 = 'S ( 'S ( 'S ( 'S ( 'S 'Z))))

type instance ToNat 6 = 'S ( 'S ( 'S ( 'S ( 'S ( 'S 'Z)))))

type instance ToNat 7 = 'S ( 'S ( 'S ( 'S ( 'S ( 'S ( 'S 'Z))))))

type instance ToNat 8 = 'S ( 'S ( 'S ( 'S ( 'S ( 'S ( 'S ( 'S 'Z)))))))

type instance ToNat 9 = 'S ( 'S ( 'S ( 'S ( 'S ( 'S ( 'S ( 'S ( 'S 'Z))))))))

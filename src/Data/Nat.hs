{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module Data.Nat (
  IsNat (..),
  Nat (..),
  NatList (..),
  Fin (..),
  natListElems,
  natListUsers,
  natToInt,
  finToInt,
  intToFin,
  readMaybeFin,
  readsPrecFin,
  showFin,
  ToNat,
) where

import safe Control.Exception (assert)
import safe qualified Data.Char as Char
import safe Data.Inst (Inst2)
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe qualified GHC.TypeLits as GHC

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Eq, Ord, Show, Typeable)

class (Typeable n) => IsNat (n :: Nat) where
  litNat :: Nat
  litFin :: Typeable user => Fin user n

instance IsNat 'Z where
  litNat = Z
  litFin = FZ

instance IsNat n => IsNat ( 'S n) where
  litNat = S (litNat @n)
  litFin = FS (litFin @n)

data NatList (user :: Type) (n :: Nat) (elem :: Type) where
  -- NOTE: `LZ` has `elem` in it to prevent empty list choices. Otherwise would
  -- want to provide a way to encode that `n` is non-zero, since this is the use
  -- case in authoring Magic cards. Seems like an unneeded pain point, so why bother.
  LZ :: forall user elem. (Show user, Typeable user) => user -> elem -> NatList user 'Z elem
  LS :: (Show user, Typeable user, IsNat n) => user -> elem -> NatList user n elem -> NatList user ( 'S n) elem
  deriving (Typeable)

deriving instance Inst2 Eq user elem => Eq (NatList user n elem)

deriving instance Functor (NatList user n)

deriving instance Inst2 Ord user elem => Ord (NatList user n elem)

deriving instance Show elem => Show (NatList user n elem)

data Fin (user :: Type) (n :: Nat) where
  FZ :: (Typeable user, IsNat n) => Fin user n
  FS :: (Typeable user, IsNat n) => Fin user n -> Fin user ( 'S n)
  deriving (Typeable)

deriving instance Show (Fin user n)

natListElems :: NatList user n elem -> [elem]
natListElems = \case
  LZ _ x -> [x]
  LS _ x xs -> x : natListElems xs

natListUsers :: NatList user n elem -> [user]
natListUsers = \case
  LZ u _ -> [u]
  LS u _ xs -> u : natListUsers xs

natToInt :: Nat -> Int
natToInt = \case
  Z -> 0
  S n -> 1 + natToInt n

finToInt :: Fin user n -> Int
finToInt = \case
  FZ -> 0
  FS n -> 1 + finToInt n

-- Sample: `fmap finToInt (intToFin 2 :: Maybe (Fin () (ToNat 6)))`
intToFin :: forall user (n :: Nat). (Typeable user, IsNat n) => Int -> Maybe (Fin user n)
intToFin input
  | 0 <= input && input <= topInt = intToFinRec (topInt - input) topInt topFin
  | otherwise = Nothing
 where
  topFin = litFin @n
  topNat = litNat @n
  topInt = natToInt topNat

intToFinRec :: forall user n. IsNat n => Int -> Int -> Fin user n -> Maybe (Fin user n)
intToFinRec input i curr = case curr of
  FZ -> assert (i == 0) case input == i of
    True -> Just FZ
    False -> Nothing
  FS next -> case input == i of
    True -> Just FZ
    False -> FS <$> intToFinRec input (i - 1) next

readMaybeFin :: (Typeable user, IsNat n) => String -> Maybe (Fin user n)
readMaybeFin s = intToFin =<< read s

readsPrecFin :: (Typeable user, IsNat n) => Int -> String -> [(Fin user n, String)]
readsPrecFin _ s =
  let (digits, rest) = span Char.isDigit s
   in case readMaybeFin digits of
        Nothing -> []
        Just fin -> [(fin, rest)]

showFin :: Fin user n -> String
showFin = show . finToInt

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

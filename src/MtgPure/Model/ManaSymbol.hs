{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ManaSymbol (
  ManaSymbol (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ManaType (ManaType (..))

data ManaSymbol :: ManaType -> Type where
  W :: ManaSymbol 'MTWhite
  U :: ManaSymbol 'MTBlue
  B :: ManaSymbol 'MTBlack
  R :: ManaSymbol 'MTRed
  G :: ManaSymbol 'MTGreen
  C :: ManaSymbol 'MTColorless
  S :: ManaSymbol 'MTSnow
  deriving (Typeable)

deriving instance Eq (ManaSymbol a)

deriving instance Ord (ManaSymbol a)

deriving instance Show (ManaSymbol a)

instance Semigroup (ManaSymbol a) where
  x <> _ = x

instance Monoid (ManaSymbol 'MTWhite) where
  mempty = W

instance Monoid (ManaSymbol 'MTBlue) where
  mempty = U

instance Monoid (ManaSymbol 'MTBlack) where
  mempty = B

instance Monoid (ManaSymbol 'MTRed) where
  mempty = R

instance Monoid (ManaSymbol 'MTGreen) where
  mempty = G

instance Monoid (ManaSymbol 'MTColorless) where
  mempty = C

instance Monoid (ManaSymbol 'MTSnow) where
  mempty = S

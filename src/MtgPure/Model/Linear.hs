{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Linear (
  Linearity (..),
  SLinearity (..),
  IsLinearity (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Linearity
  = Linear
  | NonLinear
  deriving (Eq, Ord, Show, Typeable)

data SLinearity (l :: Linearity) :: Type where
  SLinear :: SLinearity 'Linear
  SNonLinear :: SLinearity 'NonLinear
  deriving (Typeable)

class Typeable l => IsLinearity l where
  singLinearity :: SLinearity l

instance IsLinearity 'Linear where
  singLinearity = SLinear

instance IsLinearity 'NonLinear where
  singLinearity = SNonLinear

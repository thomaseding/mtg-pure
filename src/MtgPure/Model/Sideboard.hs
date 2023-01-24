{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Sideboard (
  Sideboard (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Recursive (AnyCard)

newtype Sideboard :: Type where
  -- NOTE: No need for ZO/ObjectN/Object ID here since cards in this zone are guaranteed to be stateless.
  Sideboard :: {unSideboard :: [AnyCard]} -> Sideboard
  deriving (Typeable)

instance Semigroup Sideboard where
  Sideboard a <> Sideboard b = Sideboard (a <> b)

instance Monoid Sideboard where
  mempty = Sideboard mempty

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Planeswalker (
  Planeswalker (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Loyalty (Loyalty)

data Planeswalker :: Type where
  Planeswalker ::
    { planeswalkerLoyalty :: Loyalty
    } ->
    Planeswalker
  deriving (Typeable)

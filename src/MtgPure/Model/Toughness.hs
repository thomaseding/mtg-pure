{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Toughness (
  Toughness (..),
) where

import safe Data.Typeable (Typeable)

newtype Toughness = Toughness Int
  deriving (Eq, Ord, Show, Typeable)

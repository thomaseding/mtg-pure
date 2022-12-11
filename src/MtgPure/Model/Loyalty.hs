{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Loyalty (
  Loyalty (..),
) where

import safe Data.Typeable (Typeable)

newtype Loyalty = Loyalty Int
  deriving (Eq, Ord, Show, Typeable)

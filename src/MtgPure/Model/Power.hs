{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Power (
  Power (..),
) where

import safe Data.Typeable (Typeable)

newtype Power = Power Int
  deriving (Eq, Ord, Show, Typeable)

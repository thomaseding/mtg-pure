{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Defense (
  Defense (..),
) where

import safe Data.Typeable (Typeable)

newtype Defense = Defense Int
  deriving (Eq, Ord, Show, Typeable)

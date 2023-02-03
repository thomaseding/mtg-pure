{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Life (
  Life (..),
) where

import safe Data.Typeable (Typeable)

newtype Life = Life {unLife :: Int}
  deriving (Eq, Ord, Show, Typeable)

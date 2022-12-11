{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.ConsIndex (
  ConsIndex (..),
) where

-- `Data` not always derivable, hence this.
-- `NumberMonger` VSCode extension nice for populating instances.
class ConsIndex a where
  consIndex :: a -> Int

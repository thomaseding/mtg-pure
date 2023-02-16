{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.ConsIndex (
  ConsIndex (..),
) where

-- `Data` not always derivable, hence this.
-- `NumberMonger` VSCode extension nice for populating instances.
-- `Copilot` handles it well too.
class ConsIndex a where
  consIndex :: a -> Int

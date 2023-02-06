{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Use fromRight" #-}

module MtgPure.Model.Mana.PrintedManaCost (
  PrintedManaCost (..),
  GetPrintedManaCosts (..),
) where

import safe Data.Kind (Type)
import safe MtgPure.Model.Mana.ManaCost (ManaCost)
import safe MtgPure.Model.Variable (Var (..))

--------------------------------------------------------------------------------

data PrintedManaCost :: Type where
  PMC :: ManaCost 'Var -> PrintedManaCost
  PMCAnd :: [PrintedManaCost] -> PrintedManaCost
  PMCOr :: [PrintedManaCost] -> PrintedManaCost

-- XXX: Wait for Tree.hs to exist. Too much pain to do this now.
class GetPrintedManaCosts a where
  getPrintedManaCosts :: a -> Maybe PrintedManaCost

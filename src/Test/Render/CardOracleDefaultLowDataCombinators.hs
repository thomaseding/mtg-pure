{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle pinning what 'defaultShowOptions' renders with (F)
-- 'showOptions_dataCombinators' at 'LowDataCombinators': raw structural
-- constructors, but the compacting leaf helpers (@toColors@,
-- @manaCost@\/@toManaCost@, @toManaPool@, @asFoo@\/@toZO\<n\>@) intact. Like
-- "Test.Render.CardOracleDefault", this tracks the default object (plus the one
-- flipped knob) rather than spelling out fields, so it follows changes to the
-- default. Note 'showOptions_smartAliasing' is forced off by @massageOptions@.
-- See "Test.Render.CardOracleGen".
module Test.Render.CardOracleDefaultLowDataCombinators (
  main,
  mainRenderCardOracleDefaultLowDataCombinators,
  oraclePath,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Model.Recursive.Show (
  DataCombinators (..),
  ShowOptions (..),
  defaultShowOptions,
 )
import safe Test.Render.CardOracleGen (runCardOracle)

oraclePath :: FilePath
oraclePath = "src/Test/Render/card-oracle-default-low-data-combinators.txt"

options :: ShowOptions
options = defaultShowOptions{showOptions_dataCombinators = LowDataCombinators}

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleDefaultLowDataCombinators

mainRenderCardOracleDefaultLowDataCombinators :: (HasCallStack) => IO ()
mainRenderCardOracleDefaultLowDataCombinators = runCardOracle oraclePath options

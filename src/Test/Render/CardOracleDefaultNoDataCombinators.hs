{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle pinning what 'defaultShowOptions' renders with (F)
-- 'showOptions_dataCombinators' at 'NoDataCombinators': the constructors-only
-- rendering with every cleanup option at its default. Like
-- "Test.Render.CardOracleDefault", this tracks the default object (plus the one
-- flipped knob) rather than spelling out fields, so it follows changes to the
-- default. Note 'showOptions_smartAliasing' and
-- 'showOptions_wildcardUnusedVars' are forced off by @massageOptions@.
-- See "Test.Render.CardOracleGen".
module Test.Render.CardOracleDefaultNoDataCombinators (
  main,
  mainRenderCardOracleDefaultNoDataCombinators,
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
oraclePath = "src/Test/Render/card-oracle-default-no-data-combinators.txt"

options :: ShowOptions
options = defaultShowOptions{showOptions_dataCombinators = NoDataCombinators}

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleDefaultNoDataCombinators

mainRenderCardOracleDefaultNoDataCombinators :: (HasCallStack) => IO ()
mainRenderCardOracleDefaultNoDataCombinators = runCardOracle oraclePath options

{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle pinning what 'defaultShowOptions' renders with (F)
-- 'showOptions_dataCombinators' at 'HighDataCombinators': the full authoring
-- vocabulary, as the hand-written cards use it. 'defaultShowOptions' is
-- currently already at 'HighDataCombinators', so this oracle presently matches
-- "Test.Render.CardOracleDefault" byte for byte -- it exists to complete the
-- 'NoDataCombinators'\/'LowDataCombinators'\/'HighDataCombinators' trio and to
-- keep pinning the high rendering even if the default level ever changes.
-- See "Test.Render.CardOracleGen".
module Test.Render.CardOracleDefaultHighDataCombinators (
  main,
  mainRenderCardOracleDefaultHighDataCombinators,
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
oraclePath = "src/Test/Render/card-oracle-default-high-data-combinators.txt"

options :: ShowOptions
options = defaultShowOptions{showOptions_dataCombinators = HighDataCombinators}

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleDefaultHighDataCombinators

mainRenderCardOracleDefaultHighDataCombinators :: (HasCallStack) => IO ()
mainRenderCardOracleDefaultHighDataCombinators = runCardOracle oraclePath options

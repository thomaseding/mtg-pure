{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle with only 'showOptions_smartAliasing' enabled (all other
-- options off). See "Test.Render.CardOracleGen".
module Test.Render.CardOracleSmartAliasing (
  main,
  mainRenderCardOracleSmartAliasing,
  oraclePath,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Model.Recursive.Show (DataCombinators (..), ShowOptions (..))
import safe Test.Render.CardOracleGen (runCardOracle)

oraclePath :: FilePath
oraclePath = "src/Test/Render/card-oracle-smart-aliasing.txt"

-- | Every option spelled out explicitly so this test is immune to changes in
-- 'MtgPure.Model.Recursive.Show.defaultShowOptions'.
options :: ShowOptions
options =
  ShowOptions
    { showOptions_wildcardUnusedVars = False
    , showOptions_blockArguments = False
    , showOptions_multiline = False
    , showOptions_smartAliasing = True
    , showOptions_dataCombinators = HighDataCombinators
    }

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleSmartAliasing

mainRenderCardOracleSmartAliasing :: (HasCallStack) => IO ()
mainRenderCardOracleSmartAliasing = runCardOracle oraclePath options

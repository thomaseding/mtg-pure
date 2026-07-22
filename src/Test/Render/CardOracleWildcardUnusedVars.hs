{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle with only 'showOptions_wildcardUnusedVars' enabled (all
-- other options default). See "Test.Render.CardOracleGen".
module Test.Render.CardOracleWildcardUnusedVars (
  main,
  mainRenderCardOracleWildcardUnusedVars,
  oraclePath,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Model.Recursive.Show (DataCombinators (..), ShowOptions (..))
import safe Test.Render.CardOracleGen (runCardOracle)

oraclePath :: FilePath
oraclePath = "src/Test/Render/card-oracle-wildcard-unused-vars.txt"

-- | Every option spelled out explicitly so this test is immune to changes in
-- 'MtgPure.Model.Recursive.Show.defaultShowOptions'.
options :: ShowOptions
options =
  ShowOptions
    { showOptions_wildcardUnusedVars = True
    , showOptions_blockArguments = False
    , showOptions_multiline = False
    , showOptions_smartAliasing = False
    , showOptions_dataCombinators = HighDataCombinators
    }

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleWildcardUnusedVars

mainRenderCardOracleWildcardUnusedVars :: (HasCallStack) => IO ()
mainRenderCardOracleWildcardUnusedVars = runCardOracle oraclePath options

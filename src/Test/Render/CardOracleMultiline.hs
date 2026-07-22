{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle with only 'showOptions_multiline' enabled (all other
-- options default). See "Test.Render.CardOracleGen".
module Test.Render.CardOracleMultiline (
  main,
  mainRenderCardOracleMultiline,
  oraclePath,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Model.Recursive.Show (DataCombinators (..), ShowOptions (..))
import safe Test.Render.CardOracleGen (runCardOracle)

oraclePath :: FilePath
oraclePath = "src/Test/Render/card-oracle-multiline.txt"

-- | Every option spelled out explicitly so this test is immune to changes in
-- 'MtgPure.Model.Recursive.Show.defaultShowOptions'.
options :: ShowOptions
options =
  ShowOptions
    { showOptions_wildcardUnusedVars = False
    , showOptions_blockArguments = False
    , showOptions_multiline = True
    , showOptions_smartAliasing = False
    , showOptions_dataCombinators = HighDataCombinators
    }

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleMultiline

mainRenderCardOracleMultiline :: (HasCallStack) => IO ()
mainRenderCardOracleMultiline = runCardOracle oraclePath options

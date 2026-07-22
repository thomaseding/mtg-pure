{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle with only 'showOptions_blockArguments' enabled (all
-- other options default). See "Test.Render.CardOracleGen".
module Test.Render.CardOracleBlockArguments (
  main,
  mainRenderCardOracleBlockArguments,
  oraclePath,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Model.Recursive.Show (DataCombinators (..), ShowOptions (..))
import safe Test.Render.CardOracleGen (runCardOracle)

oraclePath :: FilePath
oraclePath = "src/Test/Render/card-oracle-block-arguments.txt"

-- | Every option spelled out explicitly so this test is immune to changes in
-- 'MtgPure.Model.Recursive.Show.defaultShowOptions'.
options :: ShowOptions
options =
  ShowOptions
    { showOptions_wildcardUnusedVars = False
    , showOptions_blockArguments = True
    , showOptions_multiline = False
    , showOptions_smartAliasing = False
    , showOptions_dataCombinators = HighDataCombinators
    }

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleBlockArguments

mainRenderCardOracleBlockArguments :: (HasCallStack) => IO ()
mainRenderCardOracleBlockArguments = runCardOracle oraclePath options

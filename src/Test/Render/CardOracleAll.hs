{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle with every render option enabled. See
-- "Test.Render.CardOracleGen".
module Test.Render.CardOracleAll (
  main,
  mainRenderCardOracleAll,
  oraclePath,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Model.Recursive.Show (DataCombinators (..), ShowOptions (..))
import safe Test.Render.CardOracleGen (runCardOracle)

oraclePath :: FilePath
oraclePath = "src/Test/Render/card-oracle-all.txt"

-- | Every option spelled out explicitly so this test is immune to changes in
-- 'MtgPure.Model.Recursive.Show.defaultShowOptions'.
options :: ShowOptions
options =
  ShowOptions
    { showOptions_wildcardUnusedVars = True
    , showOptions_blockArguments = True
    , showOptions_multiline = True
    , showOptions_smartAliasing = True
    , -- Not a cleanup knob but a rendering mode switch (and lower levels force
      -- smart aliasing off), so the "all cleanups on" oracle keeps it high.
      showOptions_dataCombinators = HighDataCombinators
    }

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleAll

mainRenderCardOracleAll :: (HasCallStack) => IO ()
mainRenderCardOracleAll = runCardOracle oraclePath options

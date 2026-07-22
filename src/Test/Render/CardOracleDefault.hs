{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Card-render oracle pinning whatever 'defaultShowOptions' renders (i.e. what
-- the plain 'Show' instances produce). Unlike the other
-- @Test.Render.CardOracle\<Foo\>@ modules, this one uses the default object
-- directly rather than spelling out fields, so it tracks changes to the
-- default. See "Test.Render.CardOracleGen".
module Test.Render.CardOracleDefault (
  main,
  mainRenderCardOracleDefault,
  oraclePath,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Model.Recursive.Show (defaultShowOptions)
import safe Test.Render.CardOracleGen (runCardOracle)

oraclePath :: FilePath
oraclePath = "src/Test/Render/card-oracle-default.txt"

main :: (HasCallStack) => IO ()
main = mainRenderCardOracleDefault

mainRenderCardOracleDefault :: (HasCallStack) => IO ()
mainRenderCardOracleDefault = runCardOracle oraclePath defaultShowOptions

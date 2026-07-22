{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Template entry point for interactive terminal play.
--
-- This is a scratch launcher for the @Test.Game.Play.*@ games: edit the 'main'
-- binding below to pick which game to launch, then build and run the @play@
-- executable as its own process:
--
-- > cabal run play
--
-- Running it as a compiled executable (rather than from GHCi) is deliberate —
-- interactive play misbehaves under GHCi because GHCi owns stdin. See
-- @README.md@ in this folder for the details and other pitfalls.
--
-- Uncomment the import you want to play with.
module Main (
  main,
) where

-- import safe qualified Test.Game.Play.Hybrid as Play
-- import safe qualified Test.Game.Play.ManaAbility as Play
-- import safe qualified Test.Game.Play.RagingGoblin as Play
-- import safe qualified Test.Game.Play.Shock as Play
import safe qualified Test.Game.Play.StoneRain as Play

main :: IO ()
main = Play.main

{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Evaluate" #-}

module Test.Game.StoneRain (
  main,
  mainStoneRain,
) where

import safe MtgPure.Cards (mountain, stoneRain)
import safe MtgPure.Client.Terminal (TerminalInput (..), fwdImpl, playTerminalGame, runTerminal)
import safe MtgPure.Engine.State (noGameCheats)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainStoneRain

-- NOTE: Still a WIP
mainStoneRain :: IO ()
mainStoneRain = runTerminal input do
  playTerminalGame noGameCheats $ replicate 2 (deck, side)
 where
  input =
    TerminalInput
      { terminalInput_ = ()
      , terminalInput_fwd = fwdImpl
      , terminalInput_replayInputs = replayInputs
      , terminalInput_replayLog = replayLog
      }

deck :: Deck
deck =
  Deck $
    concat $
      replicate
        30
        [ AnyCard1 mountain
        , AnyCard1 stoneRain
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate (if True then 1 else 7) $ AnyCard1 mountain
      , replicate (if True then 1 else 8) $ AnyCard1 stoneRain
      ]

replayLog :: Maybe FilePath
replayLog = Nothing -- Just "replay-StoneRain.log"

replayInputs :: [String]
replayInputs =
  [ "Pass # O=1 UpkeepStep Turn1"
  , "Pass # O=2 UpkeepStep Turn1"
  , "Pass # O=1 DrawStep Turn1"
  , "Pass # O=2 DrawStep Turn1"
  , "PlayLand 124 # O=1 PreCombatMainPhase Turn1"
  , "Pass # O=1 PreCombatMainPhase Turn1"
  , "Pass # O=2 PreCombatMainPhase Turn1"
  , "Pass # O=1 BeginningOfCombatStep Turn1"
  , "Pass # O=2 BeginningOfCombatStep Turn1"
  , "Pass # O=1 DeclareAttackersStep Turn1"
  , "Pass # O=2 DeclareAttackersStep Turn1"
  , "Pass # O=1 EndOfCombatStep Turn1"
  , "Pass # O=2 EndOfCombatStep Turn1"
  , "Pass # O=1 PostCombatMainPhase Turn1"
  , "Pass # O=2 PostCombatMainPhase Turn1"
  , "Pass # O=1 EndStep Turn1"
  , "Pass # O=2 EndStep Turn1"
  , "Pass # O=2 UpkeepStep Turn2"
  , "Pass # O=1 UpkeepStep Turn2"
  , "Pass # O=2 DrawStep Turn2"
  , "Pass # O=1 DrawStep Turn2"
  , "Pass # O=2 PreCombatMainPhase Turn2"
  , "Pass # O=1 PreCombatMainPhase Turn2"
  , "Pass # O=2 BeginningOfCombatStep Turn2"
  , "Pass # O=1 BeginningOfCombatStep Turn2"
  , "Pass # O=2 DeclareAttackersStep Turn2"
  , "Pass # O=1 DeclareAttackersStep Turn2"
  , "Pass # O=2 EndOfCombatStep Turn2"
  , "Pass # O=1 EndOfCombatStep Turn2"
  , "Pass # O=2 PostCombatMainPhase Turn2"
  , "Pass # O=1 PostCombatMainPhase Turn2"
  , "Pass # O=2 EndStep Turn2"
  , "Pass # O=1 EndStep Turn2"
  , "Pass # O=1 UpkeepStep Turn3"
  , "Pass # O=2 UpkeepStep Turn3"
  , "Pass # O=1 DrawStep Turn3"
  , "Pass # O=2 DrawStep Turn3"
  , "PlayLand 126 # O=1 PreCombatMainPhase Turn3"
  , "Pass # O=1 PreCombatMainPhase Turn3"
  , "Pass # O=2 PreCombatMainPhase Turn3"
  , "Pass # O=1 BeginningOfCombatStep Turn3"
  , "Pass # O=2 BeginningOfCombatStep Turn3"
  , "Pass # O=1 DeclareAttackersStep Turn3"
  , "Pass # O=2 DeclareAttackersStep Turn3"
  , "Pass # O=1 EndOfCombatStep Turn3"
  , "Pass # O=2 EndOfCombatStep Turn3"
  , "Pass # O=1 PostCombatMainPhase Turn3"
  , "Pass # O=2 PostCombatMainPhase Turn3"
  , "Pass # O=1 EndStep Turn3"
  , "Pass # O=2 EndStep Turn3"
  , "Pass # O=2 UpkeepStep Turn4"
  , "Pass # O=1 UpkeepStep Turn4"
  , "Pass # O=2 DrawStep Turn4"
  , "Pass # O=1 DrawStep Turn4"
  , "Pass # O=2 PreCombatMainPhase Turn4"
  , "Pass # O=1 PreCombatMainPhase Turn4"
  , "Pass # O=2 BeginningOfCombatStep Turn4"
  , "Pass # O=1 BeginningOfCombatStep Turn4"
  , "Pass # O=2 DeclareAttackersStep Turn4"
  , "Pass # O=1 DeclareAttackersStep Turn4"
  , "Pass # O=2 EndOfCombatStep Turn4"
  , "Pass # O=1 EndOfCombatStep Turn4"
  , "Pass # O=2 PostCombatMainPhase Turn4"
  , "Pass # O=1 PostCombatMainPhase Turn4"
  , "Pass # O=2 EndStep Turn4"
  , "Pass # O=1 EndStep Turn4"
  , "Pass # O=1 UpkeepStep Turn5"
  , "Pass # O=2 UpkeepStep Turn5"
  , "Pass # O=1 DrawStep Turn5"
  , "Pass # O=2 DrawStep Turn5"
  , "PlayLand 128 # O=1 PreCombatMainPhase Turn5"
  , "CastSpell 123 # O=1 PreCombatMainPhase Turn5" -- fail: not enough mana
  , "ActivateAbility 137 R # O=1 PreCombatMainPhase Turn5"
  , "CastSpell 123 # O=1 PreCombatMainPhase Turn5" -- fail: not enough mana
  , "ActivateAbility 140 R # O=1 PreCombatMainPhase Turn5"
  , "CastSpell 123 # O=1 PreCombatMainPhase Turn5" -- fail: not enough mana
  , "ActivateAbility 143 R # O=1 PreCombatMainPhase Turn5"
  , "CastSpell 123 # O=1 PreCombatMainPhase Turn5"
  ]

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

module Test.Game.RagingGoblin (
  main,
  mainRagingGoblin,
) where

import safe MtgPure.Cards (mountain, ragingGoblin)
import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (
  TerminalInput (..),
  runTerminal,
 )
import safe MtgPure.Client.Terminal.PriorityAction (
  playTerminalGame,
 )
import safe MtgPure.Engine.State (noGameCheats)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainRagingGoblin

-- NOTE: Still a WIP
mainRagingGoblin :: IO ()
mainRagingGoblin = runTerminal input do
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
        (if False then 1 else 30)
        [ AnyCard1 mountain
        , AnyCard1 ragingGoblin
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate (if True then 1 else 7) $ AnyCard1 mountain
      , replicate (if True then 1 else 8) $ AnyCard1 ragingGoblin
      ]

replayLog :: Maybe FilePath
replayLog = Nothing -- Just "replay-RagingGoblin.log"

replayInputs :: [String]
replayInputs =
  [ "Pass # O=1 UpkeepStep Turn1"
  , "Pass # O=2 UpkeepStep Turn1"
  , "Pass # O=1 DrawStep Turn1"
  , "Pass # O=2 DrawStep Turn1"
  , "PlayLand 124 # O=1 PreCombatMainPhase Turn1"
  , "ActivateAbility 137 R # O=1 PreCombatMainPhase Turn1"
  , "CastSpell 123 # O=1 PreCombatMainPhase Turn1"
  , "Pass # O=1 PreCombatMainPhase Turn1"
  , "Pass # O=2 PreCombatMainPhase Turn1"
  , "Pass # O=1 PreCombatMainPhase Turn1"
  , "Pass # O=2 PreCombatMainPhase Turn1"
  , "Pass # O=1 BeginningOfCombatStep Turn1"
  , "Pass # O=2 BeginningOfCombatStep Turn1"
  , "140 # Attack ; O=1 DeclareAttackersStep Turn1"
  , "Pass # O=1 DeclareAttackersStep Turn1"
  , "Pass # O=2 DeclareAttackersStep Turn1"
  , "# Block ; O=2 DeclareBlockersStep Turn1"
  , "Pass # O=1 DeclareBlockersStep Turn1"
  , "Pass # O=2 DeclareBlockersStep Turn1"
  , "Pass # O=1 CombatDamageStep Turn1"
  , "Pass # O=2 CombatDamageStep Turn1"
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
  , "PlayLand 131 # O=2 PreCombatMainPhase Turn2"
  , "ActivateAbility 142 R # O=2 PreCombatMainPhase Turn2"
  , "CastSpell 130 # O=2 PreCombatMainPhase Turn2"
  , "Pass # O=2 PreCombatMainPhase Turn2"
  , "Pass # O=1 PreCombatMainPhase Turn2"
  , "Pass # O=2 PreCombatMainPhase Turn2"
  , "Pass # O=1 PreCombatMainPhase Turn2"
  , "Pass # O=2 BeginningOfCombatStep Turn2"
  , "Pass # O=1 BeginningOfCombatStep Turn2"
  , "145 # Attack ; O=2 DeclareAttackersStep Turn2"
  , "Pass # O=2 DeclareAttackersStep Turn2"
  , "Pass # O=1 DeclareAttackersStep Turn2"
  , "# Block ; O=1 DeclareBlockersStep Turn2"
  , "Pass # O=2 DeclareBlockersStep Turn2"
  , "Pass # O=1 DeclareBlockersStep Turn2"
  , "Pass # O=2 CombatDamageStep Turn2"
  , "Pass # O=1 CombatDamageStep Turn2"
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
  , "Pass # O=1 PreCombatMainPhase Turn3"
  , "Pass # O=2 PreCombatMainPhase Turn3"
  , "Pass # O=1 BeginningOfCombatStep Turn3"
  , "Pass # O=2 BeginningOfCombatStep Turn3"
  , "# Attack ; O=1 DeclareAttackersStep Turn3"
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
  , "145 # Attack ; O=2 DeclareAttackersStep Turn4"
  , "Pass # O=2 DeclareAttackersStep Turn4"
  , "Pass # O=1 DeclareAttackersStep Turn4"
  , "145 140 # Block ; O=1 DeclareBlockersStep Turn4"
  , "Pass # O=2 DeclareBlockersStep Turn4"
  , "Pass # O=1 DeclareBlockersStep Turn4"
  ]

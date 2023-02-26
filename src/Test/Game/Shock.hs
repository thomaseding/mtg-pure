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

module Test.Game.Shock (
  main,
  mainShock,
) where

import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (
  TerminalInput (..),
  runTerminal,
 )
import safe MtgPure.Client.Terminal.PriorityAction (
  playTerminalGame,
 )
import safe MtgPure.Engine.State (GameCheats (..), noGameCheats)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainShock

-- NOTE: Still a WIP
mainShock :: IO ()
mainShock = runTerminal input do
  playTerminalGame cheats $ replicate 2 (deck, side)
 where
  cheats = noGameCheats{gameCheats_disableLosing = True}
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
        (if True then 1 else 30)
        [ AnyCard1 mountain
        , AnyCard1 shock
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate (if True then 1 else 7) $ AnyCard1 mountain
      , replicate (if True then 1 else 8) $ AnyCard1 shock
      ]

replayLog :: Maybe FilePath
replayLog = Nothing -- Just "replay-Shock.log"

replayInputs :: [String]
replayInputs =
  [ "Pass # O=1 UpkeepStep Turn1"
  , "Pass # O=2 UpkeepStep Turn1"
  , "Pass # O=1 DrawStep Turn1"
  , "Pass # O=2 DrawStep Turn1"
  , "PlayLand 8 # O=1 PreCombatMainPhase Turn1"
  , "ActivateAbility 12 R # O=1 PreCombatMainPhase Turn1"
  , "CastSpell 7 # O=1 PreCombatMainPhase Turn1"
  , "Pass # O=1 PreCombatMainPhase Turn1"
  , "Pass # O=2 PreCombatMainPhase Turn1"
  ]

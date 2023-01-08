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

module MtgPure.Test.MountainShock (
  main,
  mainMountainShock,
) where

import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Client.Console (ConsoleInput (..), playConsoleGame, runConsole)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainMountainShock

-- NOTE: Still a WIP
mainMountainShock :: IO ()
mainMountainShock = runConsole input do
  playConsoleGame $ replicate 2 (deck, side)
 where
  input =
    ConsoleInput
      { consoleInput_ = ()
      , consoleInput_replayInputs = replayInputs
      , consoleInput_replayLog = replayLog
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
replayLog = Nothing -- Just "replay-Mountain-Shock.log"

replayInputs :: [String]
replayInputs =
  [ "Pass ; O=1 UpkeepStep Turn1"
  , "Pass ; O=2 UpkeepStep Turn1"
  , "Pass ; O=1 DrawStep Turn1"
  , "Pass ; O=2 DrawStep Turn1"
  , "PlayLand 8 ; O=1 PreCombatMainPhase Turn1"
  , "ActivateAbility 11 R ; O=1 PreCombatMainPhase Turn1"
  , "CastSpell 7 ; O=1 PreCombatMainPhase Turn1"
  , "Pass ; O=1 PreCombatMainPhase Turn1"
  , "Pass ; O=2 PreCombatMainPhase Turn1"
  ]

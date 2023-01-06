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

module MtgPure.Test.MountainRagingGoblin (
  main,
  mainMountainRagingGoblin,
) where

import safe MtgPure.Cards (mountain, ragingGoblin)
import safe MtgPure.Client.Console (ConsoleInput (..), playConsoleGame, runConsole)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainMountainRagingGoblin

-- NOTE: Still a WIP
-- FIXME: currently only R gets removed from mana pool instead of RRR
mainMountainRagingGoblin :: IO ()
mainMountainRagingGoblin = runConsole input do
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
replayLog = Nothing -- Just "replay-Mountain-RagingGoblin.log"

replayInputs :: [String]
replayInputs = []

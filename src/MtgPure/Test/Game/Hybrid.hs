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

module MtgPure.Test.Game.ManaAbility (
  main,
  mainHybrid,
) where

import safe MtgPure.Cards (
  bayou,
  blackLotus,
  deathriteShaman,
  moxEmerald,
  moxJet,
  moxPearl,
  moxRuby,
  moxSapphire,
  plains,
  witchEngine,
 )
import safe MtgPure.Client.Console (ConsoleInput (..), playConsoleGame, runConsole)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainHybrid

-- NOTE: Still a WIP
mainHybrid :: IO ()
mainHybrid = runConsole input do
  playConsoleGame [(deck1, side), (deck2, side)]
 where
  input =
    ConsoleInput
      { consoleInput_ = ()
      , consoleInput_replayInputs = replayInputs
      , consoleInput_replayLog = replayLog
      }

deck1 :: Deck
deck1 =
  Deck $
    concat $
      replicate
        4
        [ AnyCard1 deathriteShaman
        , AnyCard1 bayou
        , AnyCard1 moxEmerald
        , AnyCard1 moxJet
        , AnyCard1 moxPearl
        , AnyCard1 moxRuby
        , AnyCard1 moxSapphire
        , AnyCard1 blackLotus
        ]

deck2 :: Deck
deck2 = Deck $ replicate (length $ unDeck deck1) $ AnyCard1 plains

side :: Sideboard
side =
  Sideboard $
    concat
      []

replayLog :: Maybe FilePath
replayLog = Nothing -- Just "replay-Hybrid.log"

replayInputs :: [String]
replayInputs = []

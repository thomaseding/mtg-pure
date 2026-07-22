{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Headless replay of "Test.Game.Play.Hybrid". The original scripted no inputs, so
-- this halts immediately at the first priority prompt — it just exercises game
-- setup end-to-end under the headless backend.
module Test.Game.Headless.Hybrid (
  main,
  mainHeadlessHybrid,
  cheats,
  decks,
  decisions,
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
  thunderingTanadon,
  waspLancer,
 )
import safe MtgPure.Client.Headless.Monad (Decision, runHeadless)
import safe MtgPure.Client.Headless.Replay (playHeadlessGame)
import safe MtgPure.Engine.State (GameCheats, noGameCheats)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe Test.Game.Headless.Util (reportHeadless)

main :: IO ()
main = mainHeadlessHybrid

mainHeadlessHybrid :: IO ()
mainHeadlessHybrid =
  reportHeadless $ runHeadless decisions $ playHeadlessGame cheats decks

cheats :: GameCheats
cheats = noGameCheats

decks :: [(Deck, Sideboard)]
decks = [(deck1, side), (deck2, side)]

decisions :: [Decision]
decisions = []

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
        , AnyCard1 thunderingTanadon
        , AnyCard1 waspLancer
        ]

deck2 :: Deck
deck2 = Deck $ replicate (length $ unDeck deck1) $ AnyCard1 plains

side :: Sideboard
side = Sideboard $ concat []

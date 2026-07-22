{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Headless replay of "Test.Game.Play.ManaAbility".
module Test.Game.Headless.ManaAbility (
  main,
  mainHeadlessManaAbility,
  cheats,
  decks,
  decisions,
) where

import safe MtgPure.Cards (
  bayou,
  blackLotus,
  deathriteShaman,
  forest,
  moxEmerald,
  moxJet,
  plains,
  pollutedDelta,
  swamp,
  witchEngine,
 )
import safe MtgPure.Client.Headless.Monad (Decision (..), runHeadless)
import safe MtgPure.Client.Headless.Replay (playHeadlessGame)
import safe MtgPure.Client.Headless.Script (
  oidAt,
  passUntilMainPhase,
  passUntilStackTopResolves,
 )
import safe MtgPure.Client.Terminal.CommandInput (CIPriorityAction' (..), CommandAbilityIndex (..))
import safe MtgPure.Engine.State (GameCheats, noGameCheats)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe Test.Game.Headless.Util (reportHeadless)

main :: IO ()
main = mainHeadlessManaAbility

mainHeadlessManaAbility :: IO ()
mainHeadlessManaAbility =
  reportHeadless $ runHeadless decisions $ playHeadlessGame cheats decks

cheats :: GameCheats
cheats = noGameCheats

decks :: [(Deck, Sideboard)]
decks = [(deck1, side), (deck2, side)]

decisions :: [Decision]
decisions =
  [ passUntilMainPhase
  , DecidePriority $ CIPlayLand (oidAt ZHand "Polluted Delta") []
  , DecidePriority $ CIActivateAbility (oidAt ZBattlefield "Polluted Delta") (CIAbilityIndex 0) []
  , passUntilStackTopResolves
  , DecidePick $ oidAt ZLibrary "Swamp"
  ]

deck1 :: Deck
deck1 =
  Deck $
    concat $
      replicate
        4
        [ AnyCard1 forest
        , AnyCard1 swamp
        , AnyCard1 bayou
        , AnyCard1 pollutedDelta
        , AnyCard1 deathriteShaman
        , AnyCard1 moxJet
        , AnyCard1 moxEmerald
        , AnyCard1 blackLotus
        , AnyCard1 witchEngine
        ]

deck2 :: Deck
deck2 = Deck $ replicate (length $ unDeck deck1) $ AnyCard1 plains

side :: Sideboard
side = Sideboard $ concat []

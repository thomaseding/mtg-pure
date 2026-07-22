{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Headless replay of the Shock scenario: the same decisions as
-- "Test.Game.Play.Shock", expressed as typed 'Decision's and run with the pure
-- headless backend (no terminal, so it runs anywhere — including
-- non-interactive test harnesses).
module Test.Game.Headless.Shock (
  main,
  mainHeadlessShock,
  cheats,
  decks,
  decisions,
) where

import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Client.Headless.Monad (Decision (..), runHeadless)
import safe MtgPure.Client.Headless.Replay (playHeadlessGame)
import safe MtgPure.Client.Headless.Script (
  oidAt,
  oidPlayer,
  passUntilMainPhase,
  passUntilStackTopResolves,
 )
import safe MtgPure.Client.Terminal.CommandInput (
  CIPriorityAction' (..),
  CommandAbilityIndex (..),
 )
import safe MtgPure.Engine.State (GameCheats (..), noGameCheats)
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe Test.Game.Headless.Util (reportHeadless)

main :: IO ()
main = mainHeadlessShock

mainHeadlessShock :: IO ()
mainHeadlessShock =
  reportHeadless $ runHeadless decisions $ playHeadlessGame cheats decks

cheats :: GameCheats
cheats = noGameCheats{gameCheats_disableLosing = True}

decks :: [(Deck, Sideboard)]
decks = replicate 2 (deck, side)

decisions :: [Decision]
decisions =
  [ passUntilMainPhase -- Turn1
  , DecidePriority $ CIPlayLand (oidAt ZHand "Mountain") []
  , DecidePriority $ CIActivateAbility (oidAt ZBattlefield "Mountain") (CIManaAbility $ Just Mountain) []
  , DecidePriority $ CICastSpell (oidAt ZHand "Shock") []
  , DecidePick $ oidPlayer 2 -- Shock the opponent
  , passUntilStackTopResolves
  ]

deck :: Deck
deck =
  Deck $
    concat $
      replicate
        1
        [ AnyCard1 mountain
        , AnyCard1 shock
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate 1 $ AnyCard1 mountain
      , replicate 1 $ AnyCard1 shock
      ]

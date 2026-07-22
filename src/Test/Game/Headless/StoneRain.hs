{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Headless replay of "Test.Game.Play.StoneRain".
module Test.Game.Headless.StoneRain (
  main,
  mainHeadlessStoneRain,
  cheats,
  decks,
  decisions,
) where

import safe MtgPure.Cards (mountain, stoneRain)
import safe MtgPure.Client.Headless.Monad (
  Decision (..),
  runHeadless,
 )
import safe MtgPure.Client.Headless.Replay (playHeadlessGame)
import safe MtgPure.Client.Headless.Script (
  expectGameplayError,
  oidAt,
  oidAtIx,
  passUntilMainPhase,
  passUntilStackTopResolves,
  passUntilTurn,
 )
import safe MtgPure.Client.Terminal.CommandInput (CIPriorityAction' (..), CommandAbilityIndex (..))
import safe MtgPure.Engine.State (GameCheats, noGameCheats)
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe Test.Game.Headless.Util (checkHeadlessExpectations, reportHeadless)

main :: IO ()
main = mainHeadlessStoneRain

mainHeadlessStoneRain :: IO ()
mainHeadlessStoneRain = do
  let result = runHeadless decisions $ playHeadlessGame cheats decks
  reportHeadless result
  checkHeadlessExpectations result

cheats :: GameCheats
cheats = noGameCheats

decks :: [(Deck, Sideboard)]
decks = replicate 2 (deck, side)

decisions :: [Decision]
decisions =
  [ -- Turn 1: player 1 plays the first Mountain. (The attacker prompts along
    -- the way are auto-declined by the pass-untils.)
    passUntilMainPhase
  , DecidePriority $ CIPlayLand (oidAt ZHand "Mountain") []
  , -- Turn 3: the second Mountain.
    passUntilTurn 3
  , passUntilMainPhase
  , DecidePriority $ CIPlayLand (oidAt ZHand "Mountain") []
  , -- Turn 5: the third Mountain, then cast Stone Rain, retrying after each
    -- underpayment. Each cast targets the first Mountain player 1 played.
    passUntilTurn 5
  , passUntilMainPhase
  , DecidePriority $ CIPlayLand (oidAt ZHand "Mountain") []
  , DecidePriority $ CICastSpell (oidAt ZHand "Stone Rain") [] -- fails: not enough mana
  , DecidePick $ oidAt ZBattlefield "Mountain"
  , expectGameplayError "CastSpell_CantPayCost"
  , DecidePriority $ CIActivateAbility (oidAtIx ZBattlefield "Mountain" 0) (CIManaAbility $ Just Mountain) []
  , DecidePriority $ CICastSpell (oidAt ZHand "Stone Rain") [] -- fails: not enough mana
  , DecidePick $ oidAt ZBattlefield "Mountain"
  , expectGameplayError "CastSpell_CantPayCost"
  , DecidePriority $ CIActivateAbility (oidAtIx ZBattlefield "Mountain" 1) (CIManaAbility $ Just Mountain) []
  , DecidePriority $ CICastSpell (oidAt ZHand "Stone Rain") [] -- fails: not enough mana
  , DecidePick $ oidAt ZBattlefield "Mountain"
  , expectGameplayError "CastSpell_CantPayCost"
  , DecidePriority $ CIActivateAbility (oidAtIx ZBattlefield "Mountain" 2) (CIManaAbility $ Just Mountain) []
  , DecidePriority $ CICastSpell (oidAt ZHand "Stone Rain") []
  , DecidePick $ oidAt ZBattlefield "Mountain"
  , passUntilStackTopResolves -- the targeted Mountain is destroyed
  ]

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
      [ replicate 1 $ AnyCard1 mountain
      , replicate 1 $ AnyCard1 stoneRain
      ]

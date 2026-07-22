{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Headless replay of "Test.Game.Play.RagingGoblin" (exercises attackers + blockers).
module Test.Game.Headless.RagingGoblin (
  main,
  mainHeadlessRagingGoblin,
  cheats,
  decks,
  decisions,
) where

import safe MtgPure.Cards (mountain, ragingGoblin)
import safe MtgPure.Client.Headless.Monad (Decision (..), runHeadless)
import safe MtgPure.Client.Headless.Replay (playHeadlessGame)
import safe MtgPure.Client.Headless.Script (
  oidAt,
  oidOf,
  passUntilDeclareAttackers,
  passUntilDeclareBlockers,
  passUntilEndStep,
  passUntilMainPhase,
  passUntilTurn,
 )
import safe MtgPure.Client.Terminal.CommandInput (CIPriorityAction' (..), CommandAbilityIndex (..))
import safe MtgPure.Engine.State (GameCheats, noGameCheats)
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe Test.Game.Headless.Util (reportHeadless)

main :: IO ()
main = mainHeadlessRagingGoblin

mainHeadlessRagingGoblin :: IO ()
mainHeadlessRagingGoblin =
  reportHeadless $ runHeadless decisions $ playHeadlessGame cheats decks

cheats :: GameCheats
cheats = noGameCheats

decks :: [(Deck, Sideboard)]
decks = replicate 2 (deck, side)

decisions :: [Decision]
decisions =
  [ -- Turn 1: player 1 plays a Mountain and attacks with a hasty goblin.
    passUntilMainPhase
  , DecidePriority $ CIPlayLand (oidAt ZHand "Mountain") []
  , DecidePriority $ CIActivateAbility (oidAt ZBattlefield "Mountain") (CIManaAbility $ Just Mountain) []
  , DecidePriority $ CICastSpell (oidAt ZHand "Raging Goblin") []
  , passUntilDeclareAttackers
  , DecideAttackers [oidAt ZBattlefield "Raging Goblin"]
  , -- Turn 2: player 2 does the same. (Turn 1's blocker prompt is auto-declined
    -- by the pass-until.)
    passUntilTurn 2
  , passUntilMainPhase
  , DecidePriority $ CIPlayLand (oidOf 2 ZHand "Mountain") []
  , DecidePriority $ CIActivateAbility (oidOf 2 ZBattlefield "Mountain") (CIManaAbility $ Just Mountain) []
  , DecidePriority $ CICastSpell (oidOf 2 ZHand "Raging Goblin") []
  , passUntilDeclareAttackers
  , DecideAttackers [oidOf 2 ZBattlefield "Raging Goblin"]
  , -- Turn 4: player 2 attacks again and this time player 1 blocks; the goblins
    -- trade. (Turn 2's blocker prompt and turn 3's attacker prompt are
    -- auto-declined by the pass-until.)
    passUntilTurn 4
  , passUntilDeclareAttackers
  , DecideAttackers [oidOf 2 ZBattlefield "Raging Goblin"]
  , passUntilDeclareBlockers
  , DecideBlockers [(oidOf 2 ZBattlefield "Raging Goblin", oidOf 1 ZBattlefield "Raging Goblin")]
  , passUntilEndStep -- let combat damage resolve
  ]

deck :: Deck
deck =
  Deck $
    concat $
      replicate
        30
        [ AnyCard1 mountain
        , AnyCard1 ragingGoblin
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate 1 $ AnyCard1 mountain
      , replicate 1 $ AnyCard1 ragingGoblin
      ]

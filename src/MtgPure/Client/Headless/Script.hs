{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Sugar for authoring headless replay scripts (see @Test.Game.Headless.*@).
--
-- Instead of hard-coding raw 'ObjectId's and counting priority passes, a script
-- can name objects by zone and card name ('oidAt') and skip ahead with the
-- @passUntil*@ decisions, which pass priority until a game-state condition
-- holds. The raw-id path remains available via 'oid'.
module MtgPure.Client.Headless.Script (
  oid,
  oidAt,
  oidAtIx,
  oidOf,
  oidOfIx,
  oidPlayer,
  expectGameplayError,
  pass,
  passUntilDeclareAttackers,
  passUntilDeclareBlockers,
  passUntilDrawStep,
  passUntilEndStep,
  passUntilMainPhase,
  passUntilPostCombatMain,
  passUntilPreCombatMain,
  passUntilStackEmpty,
  passUntilStackTopResolves,
  passUntilTurn,
  passUntilUpkeep,
) where

import safe MtgPure.Client.Headless.Monad (
  Decision (..),
  ObjectRef (..),
  PassUntil (..),
 )
import safe MtgPure.Client.Terminal.CommandInput (CIPriorityAction' (..))
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.Object.ObjectId (ObjectId (..))
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Zone (Zone)

-- | Name an object by raw id.
oid :: Int -> ObjectRef
oid = ORId . ObjectId

-- | Name an object by zone and card name. When several objects match, this
-- picks the one with the lowest id; use 'oidAtIx' to pick another. Name lookups
-- cannot see the stack or exile (an engine query limitation); use 'oid' for
-- objects there.
oidAt :: Zone -> CardName -> ObjectRef
oidAt zone name = ORNameZone zone name 0

-- | Like 'oidAt', but picks the match at the given index, where matches are
-- sorted ascending by id.
oidAtIx :: Zone -> CardName -> Int -> ObjectRef
oidAtIx = ORNameZone

-- | Like 'oidAt', restricted to objects the given player controls. The player
-- number is 1-based (player 1 is the starting player), matching the @O=N@
-- annotations in the replay scripts.
oidOf :: Int -> Zone -> CardName -> ObjectRef
oidOf playerNumber zone name = ORControllerNameZone (playerNumber - 1) zone name 0

-- | Like 'oidOf', but picks the match at the given (0-based) index.
oidOfIx :: Int -> Zone -> CardName -> Int -> ObjectRef
oidOfIx playerNumber = ORControllerNameZone (playerNumber - 1)

-- | Name a player. The player number is 1-based (player 1 is the starting
-- player), matching the @O=N@ annotations in the replay scripts.
oidPlayer :: Int -> ObjectRef
oidPlayer playerNumber = ORPlayer (playerNumber - 1)

-- | Assert that a rejected gameplay action is pending at this point in the
-- script, matched by message prefix and drained in order. Place one right after
-- the decision that provokes the error (the failing cast's target pick, say).
expectGameplayError :: String -> Decision
expectGameplayError = ExpectGameplayError

-- | Pass priority once.
pass :: Decision
pass = DecidePriority CIPass

-- | Pass priority until the upkeep step.
passUntilUpkeep :: Decision
passUntilUpkeep = DecidePassUntil $ PassUntilPhaseStep $ PSBeginningPhase UpkeepStep

-- | Pass priority until the draw step.
passUntilDrawStep :: Decision
passUntilDrawStep = DecidePassUntil $ PassUntilPhaseStep $ PSBeginningPhase DrawStep

-- | Pass priority until either main phase. If the game is already in a main
-- phase, this is satisfied immediately; combine with 'passUntilTurn' to reach a
-- later turn's main phase.
passUntilMainPhase :: Decision
passUntilMainPhase = DecidePassUntil PassUntilMainPhase

-- | Pass priority until the pre-combat main phase.
passUntilPreCombatMain :: Decision
passUntilPreCombatMain = DecidePassUntil $ PassUntilPhaseStep PSPreCombatMainPhase

-- | Pass priority until the post-combat main phase.
passUntilPostCombatMain :: Decision
passUntilPostCombatMain = DecidePassUntil $ PassUntilPhaseStep PSPostCombatMainPhase

-- | Pass priority until the declare attackers step, so the next decision can
-- answer the attacker prompt. (A pass-until that is still waiting when an
-- attacker prompt arrives declares no attackers instead.)
passUntilDeclareAttackers :: Decision
passUntilDeclareAttackers = DecidePassUntil $ PassUntilPhaseStep $ PSCombatPhase DeclareAttackersStep

-- | Pass priority until the declare blockers step, so the next decision can
-- answer the blocker prompt. (A pass-until that is still waiting when a
-- blocker prompt arrives declares no blockers instead.)
passUntilDeclareBlockers :: Decision
passUntilDeclareBlockers = DecidePassUntil $ PassUntilPhaseStep $ PSCombatPhase DeclareBlockersStep

-- | Pass priority until the end step.
passUntilEndStep :: Decision
passUntilEndStep = DecidePassUntil $ PassUntilPhaseStep $ PSEndingPhase EndStep

-- | Pass priority until the stack is empty.
passUntilStackEmpty :: Decision
passUntilStackEmpty = DecidePassUntil PassUntilStackEmpty

-- | Pass priority until the current top of the stack resolves (i.e. until the
-- stack shrinks below its size when this decision is first consulted). Halts
-- the replay if the stack is already empty.
passUntilStackTopResolves :: Decision
passUntilStackTopResolves = DecidePassUntil $ PassUntilStackShrinks Nothing

-- | Pass priority until the given turn number (satisfied when the current turn
-- is @>=@ it).
passUntilTurn :: Int -> Decision
passUntilTurn = DecidePassUntil . PassUntilTurn

{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Regression tests for the resume / replay machinery that the interactive
-- @Test.Game.Play.*@ games depend on but cannot exercise non-interactively.
--
-- Each test runs entirely in the pure headless backend: a decision script is
-- fast-forwarded to a hand-off ('ResumePoint'), and the game is then resumed —
-- under an auto-piloting dumb prompt via 'replayThenPrompt' — and required to run
-- to a natural end. This covers the three shapes of hand-off:
--
--   * a plain priority boundary (empty replay),
--   * a combat declaration priority round (restored from persisted combat state),
--   * a mid-operation suspension (checkpoint + non-empty replay).
--
-- The scripts are sliced from the existing @Test.Game.Headless.*@ scenarios; the
-- phase assertions guard against those scripts drifting out from under the slice.
module Test.Game.Resume.Replay (
  main,
  mainResumePriorityBoundary,
  mainResumeCombat,
  mainResumeMidOperation,
) where

import safe Control.Monad (unless, when)
import safe MtgPure.Client.Headless.Monad (
  Decision,
  Headless,
  HeadlessResult (..),
  ResumePoint (..),
  popDecision,
  runHeadless,
 )
import safe MtgPure.Client.Headless.Replay (playHeadlessGame, replayThenPrompt)
import safe MtgPure.Client.Headless.Script (passUntilMainPhase)
import safe MtgPure.Client.Query (getCurrentTurn, getPhaseStep, getStackSize)
import safe MtgPure.Engine.PlayGame (resumeGame)
import safe MtgPure.Engine.State (
  GameCheats,
  GameResult (..),
  GameState (..),
  OpaqueGameState,
  dumbHeadlessPrompt,
  mkOpaqueGameState,
  unOpaqueGameState,
 )
import safe MtgPure.Model.Deck (Deck)
import safe MtgPure.Model.PhaseStep (PhaseStep (..), prettyPhaseStep)
import safe MtgPure.Model.Sideboard (Sideboard)
import safe MtgPure.Model.Step (Step (..))
import safe qualified Test.Game.Headless.RagingGoblin as RG
import safe qualified Test.Game.Headless.StoneRain as SR

main :: IO ()
main = do
  mainResumePriorityBoundary
  mainResumeCombat
  mainResumeMidOperation

--------------------------------------------------------------------------------
-- Helpers

-- | The resume point a decision script hands off at (errors if there is none).
captureResume :: String -> GameCheats -> [(Deck, Sideboard)] -> [Decision] -> ResumePoint
captureResume label cheats decks decisions =
  case headlessResumeState (runHeadless decisions (playHeadlessGame cheats decks)) of
    Just rp -> rp
    Nothing -> error $ label ++ ": expected a resume point but got none"

-- | Resume a hand-off under the dumb auto-pilot: replay its recorded suffix (via
-- 'replayThenPrompt', popping the answers out of the headless decision list),
-- then let the dumb prompt drive the rest. The game must run to a natural end.
resumeToGameEnd :: String -> ResumePoint -> GameResult Headless
resumeToGameEnd label rp =
  let cp = unOpaqueGameState (resumePoint_checkpoint rp)
      cp' = mkOpaqueGameState cp{magicPrompt = replayThenPrompt popDecision dumbHeadlessPrompt}
   in case headlessOutcome (runHeadless (resumePoint_replay rp) (resumeGame cp')) of
        Right (Just gr) -> gr
        Right Nothing -> error $ label ++ ": resume produced no game result"
        Left reason -> error $ label ++ ": resume halted: " ++ reason

expectPhase :: String -> PhaseStep -> OpaqueGameState Headless -> IO ()
expectPhase label expected o =
  unless (prettyPhaseStep actual == prettyPhaseStep expected) $
    error $
      label
        ++ ": expected checkpoint at "
        ++ prettyPhaseStep expected
        ++ " but got "
        ++ prettyPhaseStep actual
 where
  actual = getPhaseStep o

-- | The observable shape of a state, for equivalence checks.
proj :: OpaqueGameState Headless -> (String, Int, Int)
proj o = (prettyPhaseStep (getPhaseStep o), getCurrentTurn o, getStackSize o)

reportEnd :: String -> GameResult Headless -> IO ()
reportEnd label gr =
  putStrLn $
    label
      ++ ": ran to game end; winners="
      ++ show (gameWinners gr)
      ++ " losers="
      ++ show (gameLosers gr)

--------------------------------------------------------------------------------
-- Tests

-- | A hand-off at a plain priority boundary: empty replay, resumes directly.
mainResumePriorityBoundary :: IO ()
mainResumePriorityBoundary = do
  let label = "resume/priority-boundary"
      rp = captureResume label RG.cheats RG.decks [passUntilMainPhase]
  expectPhase label PSPreCombatMainPhase (resumePoint_checkpoint rp)
  unless (null (resumePoint_replay rp)) $
    error $
      label ++ ": a priority-boundary hand-off should have an empty replay"
  reportEnd label (resumeToGameEnd label rp)

-- | Hand-offs at the declare-attackers and declare-blockers priority rounds,
-- restored from the combat state persisted in the game state.
mainResumeCombat :: IO ()
mainResumeCombat = do
  -- After the turn-1 attacker is declared (the 6th scripted decision).
  let atkLabel = "resume/declare-attackers"
      rpAtk = captureResume atkLabel RG.cheats RG.decks (take 6 RG.decisions)
  expectPhase atkLabel (PSCombatPhase DeclareAttackersStep) (resumePoint_checkpoint rpAtk)
  reportEnd atkLabel (resumeToGameEnd atkLabel rpAtk)

  -- After the turn-4 blockers are declared (everything but the trailing pass).
  let blkLabel = "resume/declare-blockers"
      rpBlk = captureResume blkLabel RG.cheats RG.decks (init RG.decisions)
  expectPhase blkLabel (PSCombatPhase DeclareBlockersStep) (resumePoint_checkpoint rpBlk)
  reportEnd blkLabel (resumeToGameEnd blkLabel rpBlk)

-- | A mid-operation hand-off (suspended at Stone Rain's target pick): the
-- checkpoint is the cast's priority boundary and the replay is non-empty.
-- Restoring the checkpoint and replaying must reproduce the uninterrupted run.
mainResumeMidOperation :: IO ()
mainResumeMidOperation = do
  let label = "resume/mid-cast"
      (partial, remaining) = splitAt (length SR.decisions - 2) SR.decisions
      rpFull = captureResume "resume/full" SR.cheats SR.decks SR.decisions
      rpPartial = captureResume label SR.cheats SR.decks partial

  expectPhase label PSPreCombatMainPhase (resumePoint_checkpoint rpPartial)
  when (null (resumePoint_replay rpPartial)) $
    error $
      label ++ ": a mid-operation hand-off should have a non-empty replay"

  -- Equivalence: checkpoint + recorded replay + the two dropped decisions must
  -- land on the same final boundary as playing straight through.
  let splitRun =
        runHeadless
          (resumePoint_replay rpPartial ++ remaining)
          (resumeGame (resumePoint_checkpoint rpPartial))
  rpSplit <- case headlessResumeState splitRun of
    Just rp -> pure rp
    Nothing -> error $ label ++ ": split resume produced no resume point"
  let pFull = proj (resumePoint_checkpoint rpFull)
      pSplit = proj (resumePoint_checkpoint rpSplit)
  unless (pFull == pSplit) $
    error $
      label ++ ": checkpoint+replay diverged from the straight run: " ++ show pFull ++ " /= " ++ show pSplit
  putStrLn $ label ++ ": checkpoint+replay reproduced the straight run at " ++ show pSplit

  reportEnd label (resumeToGameEnd label rpPartial)

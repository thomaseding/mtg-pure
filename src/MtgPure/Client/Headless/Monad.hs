{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- | A pure, non-interactive backend for driving the engine in tests.
--
-- Unlike the terminal client, this touches no console: it consumes a
-- pre-scripted list of typed 'Decision's and accumulates a game-state snapshot
-- on every engine event (via 'pushState', wired to @promptLogCallPush@). When
-- the script is exhausted (or a decision doesn't fit the prompt asking for it)
-- the run short-circuits via 'stopHeadless' instead of blocking on input or
-- looping forever — the accumulated states survive the short-circuit and are
-- returned by 'runHeadless'.
--
-- Decisions name objects through 'ObjectRef' (raw id or name+zone lookup), and
-- 'DecidePassUntil' passes priority until a 'PassUntil' condition holds — both
-- are resolved against the live game state by the replay prompt.
module MtgPure.Client.Headless.Monad (
  Decision (..),
  ObjectRef (..),
  PassUntil (..),
  GameplayError (..),
  ResumePoint (..),
  HeadlessState (..),
  Headless (..),
  HeadlessResult (..),
  runHeadless,
  stopHeadless,
  popDecision,
  peekDecision,
  setHeadDecision,
  pushState,
  setCheckpoint,
  recordAnswer,
  captureResumePoint,
  pushError,
  peekPendingError,
  dropPendingError,
  pendingErrors,
  clearPendingErrors,
  recordExpectationFailure,
  currentDecisionIndex,
) where

import safe Control.Monad.State.Strict (State, runState)
import safe qualified Control.Monad.State.Strict as State
import safe Control.Monad.Trans.Class (lift)
import safe Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import safe Data.Kind (Type)
import safe MtgPure.Client.Terminal.CommandInput (CIPriorityAction' (..))
import safe MtgPure.Engine.State (OpaqueGameState)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.Object.ObjectId (ObjectId)
import safe MtgPure.Model.PhaseStep (PhaseStep, prettyPhaseStep)
import safe MtgPure.Model.Zone (Zone)

-- | How a scripted decision names an object. All indices are 0-based and index
-- into candidates sorted ascending by id (index 0 = lowest id); player indices
-- likewise index into the players sorted ascending by id.
--
-- Name lookups cannot see the stack or exile (an engine query limitation); use
-- 'ORId' for objects there.
data ObjectRef :: Type where
  -- | By raw id.
  ORId :: ObjectId -> ObjectRef
  -- | By zone + card name + index among the matches.
  ORNameZone :: Zone -> CardName -> Int -> ObjectRef
  -- | A player, by index.
  ORPlayer :: Int -> ObjectRef
  -- | Like 'ORNameZone', restricted to objects the indexed player controls.
  ORControllerNameZone :: Int -> Zone -> CardName -> Int -> ObjectRef
  deriving (Eq, Ord, Show)

deriving instance Show (CIPriorityAction' ObjectRef)

-- | A game-state predicate for 'DecidePassUntil', re-evaluated at every
-- scripted prompt.
data PassUntil :: Type where
  PassUntilPhaseStep :: PhaseStep -> PassUntil
  PassUntilMainPhase :: PassUntil
  PassUntilStackEmpty :: PassUntil
  -- | @Nothing@ until the replay records the baseline stack size at first
  -- evaluation; satisfied once the stack is smaller than the baseline.
  PassUntilStackShrinks :: Maybe Int -> PassUntil
  PassUntilTurn :: Int -> PassUntil
  deriving (Eq, Ord)

instance Show PassUntil where
  show :: PassUntil -> String
  show = \case
    PassUntilPhaseStep ps -> "PassUntilPhaseStep " ++ prettyPhaseStep ps
    PassUntilMainPhase -> "PassUntilMainPhase"
    PassUntilStackEmpty -> "PassUntilStackEmpty"
    PassUntilStackShrinks mBaseline -> "PassUntilStackShrinks " ++ show mBaseline
    PassUntilTurn n -> "PassUntilTurn " ++ show n

-- | A single scripted answer. The engine asks its questions in a fixed order
-- for a deterministic game, so a flat in-order list mirrors the old
-- string-replay scripts, but expressed as typed ADTs (no parsing).
data Decision :: Type where
  DecidePriority :: CIPriorityAction' ObjectRef -> Decision
  DecidePick :: ObjectRef -> Decision
  DecideChooseOption :: Int -> Decision
  DecideAttackers :: [ObjectRef] -> Decision
  DecideBlockers :: [(ObjectRef, ObjectRef)] -> Decision
  DecidePassUntil :: PassUntil -> Decision
  -- | Not a prompt answer: a scripted assertion that a rejected gameplay action
  -- is already queued at this point. Placed after the decision that provokes the
  -- error has run (so the error has been produced), it pops the oldest pending
  -- error and matches it by message prefix. See 'nextScriptStep' in the Headless
  -- client.
  ExpectGameplayError :: String -> Decision
  deriving (Show)

-- | A gameplay user error the engine rejected (e.g. an illegal cast), as
-- observed by the replay prompt. Built from the exception callback's
-- arguments plus replay-side bookkeeping ('currentDecisionIndex').
data GameplayError = GameplayError
  { gameplayError_message :: String
  , gameplayError_player :: ObjectId
  -- ^ the player with priority when the error occurred
  , gameplayError_turn :: Int
  , gameplayError_decisionIndex :: Int
  -- ^ index (0-based, in script order) of the most recently popped 'Decision'
  }
  deriving (Eq, Ord, Show)

-- | A hand-off point for continuing a game under another backend: the game state
-- at the most recent priority boundary, plus the prompt answers made since it.
--
-- Resuming from a priority boundary alone ('resumePoint_checkpoint') covers the
-- common case, but a game can be suspended at any prompt — including ones with no
-- at-rest state of their own (mid-resolution picks, combat declaration). For
-- those, restore the checkpoint and deterministically replay 'resumePoint_replay'
-- back to the exact suspension point (see @replayThenPrompt@); the replay
-- reconstructs the on-stack continuation that a bare game state cannot capture.
-- Every recorded answer uses raw-id 'ORId' references, since the replayed game is
-- identical to the recorded one.
data ResumePoint = ResumePoint
  { resumePoint_checkpoint :: OpaqueGameState Headless
  -- ^ game state at the most recent priority boundary (directly resumable)
  , resumePoint_replay :: [Decision]
  -- ^ answers given since that boundary, in order, leading up to (but not
  -- including) the suspension point; empty when the boundary *is* the suspension
  }

data HeadlessState = HeadlessState
  { hsDecisions :: [Decision]
  , hsDecisionIndex :: Int
  -- ^ index the *next* popped decision will receive
  , hsStates :: [OpaqueGameState Headless]
  -- ^ ordered from most recent state to oldest (i.e. start) state
  , hsCheckpoint :: Maybe (OpaqueGameState Headless)
  -- ^ the game state at the most recent priority prompt, reset there
  , hsReplay :: [Decision]
  -- ^ answers recorded since the checkpoint, newest first (reversed at capture)
  , hsResumeState :: Maybe ResumePoint
  -- ^ the hand-off point captured when the script ran out (see 'ResumePoint');
  -- @Nothing@ until then
  , hsErrors :: [GameplayError]
  -- ^ ordered from most recent error to oldest
  , hsPendingErrors :: [GameplayError]
  -- ^ errors produced but not yet matched by an 'ExpectGameplayError', FIFO
  , hsExpectationFailures :: [String]
  -- ^ recorded expectation violations, in occurrence order
  }

newtype Headless a = Headless
  { unHeadless :: ExceptT String (State HeadlessState) a
  }
  deriving (Functor)

instance Applicative Headless where
  pure :: a -> Headless a
  pure = Headless . pure

  (<*>) :: Headless (a -> b) -> Headless a -> Headless b
  Headless f <*> Headless a = Headless (f <*> a)

instance Monad Headless where
  (>>=) :: Headless a -> (a -> Headless b) -> Headless b
  Headless a >>= f = Headless (a >>= unHeadless . f)

instance State.MonadState HeadlessState Headless where
  get :: Headless HeadlessState
  get = Headless (lift State.get)

  put :: HeadlessState -> Headless ()
  put = Headless . lift . State.put

-- | Halt the replay early with a reason. The engine loop unwinds through this
-- (the game never reaches a natural end when losing is disabled), and the
-- accumulated states are preserved.
stopHeadless :: String -> Headless a
stopHeadless reason = Headless (throwE reason)

-- | Pop the next scripted decision, if any remain.
popDecision :: Headless (Maybe Decision)
popDecision =
  State.gets hsDecisions >>= \case
    [] -> pure Nothing
    d : ds -> do
      State.modify' \st -> st{hsDecisions = ds, hsDecisionIndex = hsDecisionIndex st + 1}
      pure (Just d)

-- | Look at the next scripted decision without consuming it.
peekDecision :: Headless (Maybe Decision)
peekDecision =
  State.gets hsDecisions >>= \case
    [] -> pure Nothing
    d : _ -> pure (Just d)

-- | Overwrite the head decision (used by the replay to record a
-- 'PassUntilStackShrinks' baseline in place). No-op if the script is empty.
setHeadDecision :: Decision -> Headless ()
setHeadDecision d =
  State.gets hsDecisions >>= \case
    [] -> pure ()
    _ : ds -> State.modify' \st -> st{hsDecisions = d : ds}

-- | Record a game-state snapshot. Wired to @promptLogCallPush@ so one is
-- captured on every engine event.
pushState :: OpaqueGameState Headless -> Headless ()
pushState opaque = State.modify' \st -> st{hsStates = opaque : hsStates st}

-- | Mark a new priority boundary: record the checkpoint state and clear the
-- since-boundary answer log. Called at every priority prompt.
setCheckpoint :: OpaqueGameState Headless -> Headless ()
setCheckpoint opaque = State.modify' \st -> st{hsCheckpoint = Just opaque, hsReplay = []}

-- | Append a prompt answer to the since-boundary log (newest first). Answers use
-- raw-id 'ORId' references, so they replay verbatim against the identical game.
recordAnswer :: Decision -> Headless ()
recordAnswer d = State.modify' \st -> st{hsReplay = d : hsReplay st}

-- | Capture the current @(checkpoint, answers-since)@ as the resume point. Called
-- at whichever prompt the script runs out on; no-op before the first boundary.
captureResumePoint :: Headless ()
captureResumePoint = State.modify' \st -> case hsCheckpoint st of
  Nothing -> st
  Just checkpoint ->
    st
      { hsResumeState =
          Just
            ResumePoint
              { resumePoint_checkpoint = checkpoint
              , resumePoint_replay = reverse $ hsReplay st
              }
      }

-- | Record a rejected gameplay action. Wired to the engine's @exception*@
-- prompt callbacks (see @replayHeadlessPrompt@ in the Headless client). Adds to
-- both the full latest-first log ('hsErrors') and the FIFO match queue
-- ('hsPendingErrors').
pushError :: GameplayError -> Headless ()
pushError err =
  State.modify' \st ->
    st
      { hsErrors = err : hsErrors st
      , hsPendingErrors = hsPendingErrors st ++ [err]
      }

-- | The oldest error not yet matched to an 'ExpectGameplayError', if any.
peekPendingError :: Headless (Maybe GameplayError)
peekPendingError =
  State.gets hsPendingErrors >>= \case
    [] -> pure Nothing
    e : _ -> pure (Just e)

-- | Drop the oldest pending error (the one 'peekPendingError' returned). No-op
-- if the queue is empty.
dropPendingError :: Headless ()
dropPendingError =
  State.gets hsPendingErrors >>= \case
    [] -> pure ()
    _ : es -> State.modify' \st -> st{hsPendingErrors = es}

-- | All errors not yet matched by an 'ExpectGameplayError', FIFO (oldest first).
pendingErrors :: Headless [GameplayError]
pendingErrors = State.gets hsPendingErrors

-- | Empty the pending-error queue. Used when an expectation has already failed
-- on these errors, so they are not double-reported by the end-of-run check.
clearPendingErrors :: Headless ()
clearPendingErrors = State.modify' \st -> st{hsPendingErrors = []}

-- | Record an expectation violation (see @drainExpectedError@ /
-- @guardNoPendingErrors@ in the Headless client).
recordExpectationFailure :: String -> Headless ()
recordExpectationFailure msg =
  State.modify' \st -> st{hsExpectationFailures = hsExpectationFailures st ++ [msg]}

-- | The index (0-based, in script order) of the most recently popped
-- 'Decision'. Used to stamp 'GameplayError's with the decision that caused
-- them.
currentDecisionIndex :: Headless Int
currentDecisionIndex = State.gets \st -> hsDecisionIndex st - 1

data HeadlessResult a = HeadlessResult
  { headlessOutcome :: Either String a
  -- ^ @Left reason@ if the replay halted early (e.g. script exhausted);
  -- @Right value@ if the computation ran to completion.
  , headlessStates :: [OpaqueGameState Headless]
  -- ^ every game state observed, latest first
  , headlessResumeState :: Maybe ResumePoint
  -- ^ the hand-off point to resume interactive play from, if the script ran out;
  -- @Nothing@ otherwise (see 'ResumePoint' / 'hsResumeState')
  , headlessErrors :: [GameplayError]
  -- ^ every rejected gameplay action observed, latest first
  , headlessExpectationFailures :: [String]
  -- ^ expectation violations recorded during the replay, in occurrence order
  }

runHeadless :: [Decision] -> Headless a -> HeadlessResult a
runHeadless decisions action =
  let st0 =
        HeadlessState
          { hsDecisions = decisions
          , hsDecisionIndex = 0
          , hsStates = []
          , hsCheckpoint = Nothing
          , hsReplay = []
          , hsResumeState = Nothing
          , hsErrors = []
          , hsPendingErrors = []
          , hsExpectationFailures = []
          }
      (outcome, st1) = runState (runExceptT (unHeadless action)) st0
   in HeadlessResult
        { headlessOutcome = outcome
        , headlessStates = hsStates st1
        , headlessResumeState = hsResumeState st1
        , headlessErrors = hsErrors st1
        , headlessExpectationFailures = hsExpectationFailures st1
        }

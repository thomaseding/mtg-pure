{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- | The scripted replay prompt for the headless backend.
--
-- Built by overriding the input-driven callbacks of 'dumbHeadlessPrompt' with
-- ones that pop the next 'Decision' and translate it — via the same engine
-- action-builders the terminal client uses — into a concrete engine action.
-- Non-decision callbacks (shuffle, starting player, ...) keep the dumb
-- defaults.
module MtgPure.Client.Headless.Replay (
  replayHeadlessPrompt,
  replayThenPrompt,
  headlessGameInput,
  playHeadlessGame,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Util (Attempt)
import safe Data.Functor ((<&>))
import safe Data.Functor.Identity (Identity (..))
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes)
import safe Data.Monoid (First (..))
import safe qualified Data.Traversable as T
import safe MtgPure.Client.Headless.Monad (
  Decision (..),
  GameplayError (..),
  Headless,
  ObjectRef (..),
  PassUntil (..),
  captureResumePoint,
  clearPendingErrors,
  currentDecisionIndex,
  dropPendingError,
  peekDecision,
  peekPendingError,
  pendingErrors,
  popDecision,
  pushError,
  pushState,
  recordAnswer,
  recordExpectationFailure,
  setCheckpoint,
  setHeadDecision,
  stopHeadless,
 )
import safe MtgPure.Client.Query (
  getByControllerNameZone,
  getByNameZone,
  getCurrentTurn,
  getPhaseStep,
  getPlayerWithPriority,
  getPlayers,
  getStackSize,
 )
import safe MtgPure.Client.Terminal.CommandInput (
  CIPriorityAction,
  CIPriorityAction' (..),
  CommandAbilityIndex (..),
  traverseCIPriorityAction,
 )
import safe MtgPure.Engine.Fwd.Api (
  getIntrinsicManaAbilities,
  getTrivialManaAbilities,
  indexToActivated,
  toZO,
 )
import safe MtgPure.Engine.Monad (internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex (..),
  AttackingPlayer (..),
  DeclaredAttacker (..),
  DeclaredBlocker (..),
  DefendingPlayer (..),
  PickVariety (..),
  PriorityAction (..),
  Prompt' (..),
  RelativeAbilityIndex (..),
  SomeActivatedAbility (..),
  SpecialAction (..),
 )
import safe MtgPure.Engine.State (
  GameCheats,
  GameFormat (..),
  GameInput (..),
  GameResult,
  Magic,
  OpaqueGameState,
  Prompt,
  dumbHeadlessPrompt,
  queryMagic,
 )
import safe MtgPure.Model.Deck (Deck)
import safe MtgPure.Model.Mana.IsManaAbility (isTrivialManaAbility)
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTNAliases (OTNAny, OTNPermanent)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (GetObjectId, ObjectId, getObjectId)
import safe MtgPure.Model.PhaseStep (isMainPhase, prettyPhaseStep)
import safe MtgPure.Model.Sideboard (Sideboard)
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (oToZO1, toZO0, toZO1, toZO2, zo0ToSpell)

--------------------------------------------------------------------------------
-- The prompt

replayHeadlessPrompt :: Prompt Headless
replayHeadlessPrompt =
  dumbHeadlessPrompt
    { promptPriorityAction = replayPriorityAction
    , promptPick = replayPick
    , promptChooseAttackers = replayChooseAttackers
    , promptChooseBlockers = replayChooseBlockers
    , promptLogCallPush = \opaque _frame -> pushState opaque
    , exceptionInvalidCastSpell = \opaque _oPlayer err -> logGameplayError opaque (show err)
    , exceptionInvalidPlayLand = \opaque _oPlayer err -> logGameplayError opaque (show err)
    }

-- | Wrap a prompt so it first replays a recorded answer suffix (popped one at a
-- time via @popNext@), then delegates to the inner prompt once the suffix is
-- exhausted. This is how a 'MtgPure.Client.Headless.Monad.ResumePoint' is
-- continued: restore its checkpoint, feed its @resumePoint_replay@ through
-- @popNext@, and the engine deterministically walks back to the exact suspension
-- point before handing control to @inner@.
--
-- The recorded answers use raw-id 'ORId' references (the replayed game is
-- identical to the recorded one), so resolution here is a plain id lookup rather
-- than the name/zone resolution 'replayHeadlessPrompt' does. Only the answer
-- kinds that can appear in a suffix are intercepted (priority actions and picks);
-- everything else — and every prompt once the suffix is spent — falls through to
-- @inner@.
replayThenPrompt :: (Monad m) => m (Maybe Decision) -> Prompt m -> Prompt m
replayThenPrompt popNext inner =
  inner
    { promptPriorityAction = \attempt opaque oPlayer ->
        popNext >>= \case
          Nothing -> promptPriorityAction inner attempt opaque oPlayer
          Just (DecidePriority ciRef) -> queryMagic opaque $ buildPriorityAction $ concreteCI ciRef
          Just other -> error $ "replayThenPrompt: expected a priority answer, got " ++ show other
    , promptPick = \attempt opaque oPlayer variety xs -> case xs of
        x :| [] -> pure x
        _ -> case variety of
          PickZO ->
            popNext >>= \case
              Nothing -> promptPick inner attempt opaque oPlayer variety xs
              Just (DecidePick ref) -> pure $ findById xs ref
              Just other -> error $ "replayThenPrompt: expected a pick answer, got " ++ show other
    }
 where
  concreteId :: ObjectRef -> ObjectId
  concreteId = \case
    ORId objId -> objId
    ref -> error $ "replayThenPrompt: recorded answers must use ORId, got " ++ show ref
  concreteCI :: CIPriorityAction' ObjectRef -> CIPriorityAction
  concreteCI = runIdentity . traverseCIPriorityAction (Identity . concreteId)
  findById :: (GetObjectId a) => NonEmpty a -> ObjectRef -> a
  findById xs ref =
    let objId = concreteId ref
     in case List.find ((== objId) . getObjectId) (NonEmpty.toList xs) of
          Just x -> x
          Nothing -> error $ "replayThenPrompt: no pick candidate with id " ++ show objId

-- | Record a rejected gameplay action against the player who currently has
-- priority and the script position that provoked it.
logGameplayError :: OpaqueGameState Headless -> String -> Headless ()
logGameplayError opaque message = do
  decisionIndex <- currentDecisionIndex
  pushError
    GameplayError
      { gameplayError_message = message
      , gameplayError_player = getObjectId $ getPlayerWithPriority opaque
      , gameplayError_turn = getCurrentTurn opaque
      , gameplayError_decisionIndex = decisionIndex
      }

replayPriorityAction ::
  Attempt ->
  OpaqueGameState Headless ->
  Object 'OTPlayer ->
  Headless (PriorityAction ())
replayPriorityAction _attempt opaque _oPlayer = do
  -- A priority prompt is an at-rest boundary: (re)set the checkpoint here and
  -- record the answer, so a later suspension can be replayed back from here.
  setCheckpoint opaque
  nextScriptStep opaque >>= \case
    ScriptWaiting _cond -> do
      recordAnswer $ DecidePriority CIPass
      pure PassPriority
    ScriptDecision (DecidePriority ciRef) -> do
      ci <- traverseCIPriorityAction (resolveRef opaque) ciRef
      recordAnswer $ DecidePriority $ runIdentity $ traverseCIPriorityAction (Identity . ORId) ci
      queryMagic opaque (buildPriorityAction ci)
    ScriptDecision other -> stopHeadless ("promptPriorityAction: expected priority, got " ++ show other)
    ScriptExhausted -> do
      captureResumePoint
      stopHeadless "promptPriorityAction: out of decisions"

replayPick ::
  Attempt ->
  OpaqueGameState Headless ->
  Object 'OTPlayer ->
  PickVariety a ->
  NonEmpty a ->
  Headless a
replayPick _attempt opaque _oPlayer variety xs =
  case xs of
    x :| [] -> pure x
    _ -> case variety of
      PickZO ->
        nextScriptStep opaque >>= \case
          ScriptWaiting cond ->
            stopHeadless $
              "promptPick: a pick cannot pass; still waiting on "
                ++ show cond
                ++ " during "
                ++ prettyPhaseStep (getPhaseStep opaque)
          ScriptDecision (DecidePick ref) -> do
            x <- resolvePickRef opaque xs ref
            recordAnswer $ DecidePick $ ORId $ getObjectId x
            pure x
          ScriptDecision other -> stopHeadless ("promptPick: expected pick, got " ++ show other)
          ScriptExhausted -> captureResumePoint >> stopHeadless "promptPick: out of decisions"

replayChooseAttackers ::
  Attempt ->
  OpaqueGameState Headless ->
  AttackingPlayer ->
  DefendingPlayer ->
  Headless [DeclaredAttacker]
replayChooseAttackers _attempt opaque _attacking (DefendingPlayer oDefender) =
  nextScriptStep opaque >>= \case
    ScriptWaiting _cond -> pure []
    ScriptDecision (DecideAttackers refs) -> do
      oAttackers <- T.for refs (resolveRef opaque)
      queryMagic opaque (buildAttackers oAttackers oDefender)
    ScriptDecision other -> stopHeadless ("promptChooseAttackers: expected attackers, got " ++ show other)
    ScriptExhausted -> captureResumePoint >> stopHeadless "promptChooseAttackers: out of decisions"

replayChooseBlockers ::
  Attempt ->
  OpaqueGameState Headless ->
  AttackingPlayer ->
  DefendingPlayer ->
  NonEmpty DeclaredAttacker ->
  Headless [DeclaredBlocker]
replayChooseBlockers _attempt opaque _attacking _defending _attackers =
  nextScriptStep opaque >>= \case
    ScriptWaiting _cond -> pure []
    ScriptDecision (DecideBlockers refPairs) -> do
      pairs <- T.for refPairs \(refAttacker, refBlocker) -> do
        oAttacker <- resolveRef opaque refAttacker
        oBlocker <- resolveRef opaque refBlocker
        pure (oAttacker, oBlocker)
      queryMagic opaque (buildBlockers pairs)
    ScriptDecision other -> stopHeadless ("promptChooseBlockers: expected blockers, got " ++ show other)
    ScriptExhausted -> captureResumePoint >> stopHeadless "promptChooseBlockers: out of decisions"

--------------------------------------------------------------------------------
-- Script interpretation

-- | What the script wants at the current prompt, after 'nextScriptStep' drains
-- any satisfied pass-until conditions off the head.
data ScriptStep :: Type where
  -- | The head is an unsatisfied 'DecidePassUntil', left in place; the prompt
  -- should answer with its pass-like default.
  ScriptWaiting :: PassUntil -> ScriptStep
  ScriptDecision :: Decision -> ScriptStep
  ScriptExhausted :: ScriptStep

nextScriptStep :: OpaqueGameState Headless -> Headless ScriptStep
nextScriptStep opaque =
  peekDecision >>= \case
    Just (ExpectGameplayError matcher) -> do
      drainExpectedError matcher
      popDecision
      nextScriptStep opaque
    -- Anything else hands control back to the engine; every error produced since
    -- the last prompt must have been matched by an 'ExpectGameplayError' first.
    Nothing -> guardNoPendingErrors "gameplay error(s) left unmatched at end of script" >> pure ScriptExhausted
    Just (DecidePassUntil cond) -> do
      guardNoPendingErrors beforeNextDecision
      evalPassUntil opaque cond >>= \case
        Nothing -> popDecision >> nextScriptStep opaque
        Just cond' -> do
          setHeadDecision $ DecidePassUntil cond'
          pure $ ScriptWaiting cond'
    Just decision -> do
      guardNoPendingErrors beforeNextDecision
      popDecision
      pure (ScriptDecision decision)
 where
  beforeNextDecision = "unexpected gameplay error(s) with no matching ExpectGameplayError"

-- | Pop and match the oldest pending error against an 'ExpectGameplayError'
-- matcher (a message prefix). The error must already be queued -- an empty queue
-- means the provoking decision did not produce the expected rejection.
drainExpectedError :: String -> Headless ()
drainExpectedError matcher =
  peekPendingError >>= \case
    Nothing ->
      failExpectation $
        "ExpectGameplayError: expected an error matching "
          ++ show matcher
          ++ " but the error queue is empty"
    Just e
      | matcher `List.isPrefixOf` gameplayError_message e -> dropPendingError
      | otherwise ->
          failExpectation $
            "ExpectGameplayError: expected an error matching "
              ++ show matcher
              ++ " but got "
              ++ show (gameplayError_message e)

-- | Fail (with the given context) if any rejected gameplay action is still
-- unmatched by an 'ExpectGameplayError'. Called before the script hands control
-- back to the engine and once more when the script is exhausted.
guardNoPendingErrors :: String -> Headless ()
guardNoPendingErrors context =
  pendingErrors >>= \case
    [] -> pure ()
    es -> failExpectation $ context ++ ": " ++ show (map gameplayError_message es)

-- | Record an expectation violation and halt the replay. The recorded message
-- is what makes the test throw (see 'checkHeadlessExpectations'); the halt also
-- surfaces via 'reportHeadless'. The pending queue is cleared so these errors
-- are not double-reported by later checks.
failExpectation :: String -> Headless a
failExpectation msg = recordExpectationFailure msg >> clearPendingErrors >> stopHeadless msg

-- | @Nothing@ when the condition is satisfied; otherwise the condition to keep
-- waiting on (with a freshly recorded stack baseline, if applicable).
evalPassUntil :: OpaqueGameState Headless -> PassUntil -> Headless (Maybe PassUntil)
evalPassUntil opaque cond = case cond of
  PassUntilPhaseStep ps -> pure $ satisfiedWhen $ getPhaseStep opaque == ps
  PassUntilMainPhase -> pure $ satisfiedWhen $ isMainPhase $ getPhaseStep opaque
  PassUntilStackEmpty -> pure $ satisfiedWhen $ getStackSize opaque == 0
  PassUntilStackShrinks Nothing -> case getStackSize opaque of
    0 -> stopHeadless "DecidePassUntil: stack is empty; nothing to resolve"
    n -> pure $ Just $ PassUntilStackShrinks $ Just n
  PassUntilStackShrinks (Just baseline) -> pure $ satisfiedWhen $ getStackSize opaque < baseline
  PassUntilTurn n -> pure $ satisfiedWhen $ getCurrentTurn opaque >= n
 where
  satisfiedWhen = \case
    True -> Nothing
    False -> Just cond

-- | The ids a reference selects among (ascending) and the index it selects.
refCandidates :: OpaqueGameState Headless -> ObjectRef -> Headless ([ObjectId], Int)
refCandidates opaque = \case
  ORId objId -> pure ([objId], 0)
  ORNameZone zone name index -> pure (getByNameZone opaque name zone, index)
  ORPlayer index -> pure (map getObjectId $ getPlayers opaque, index)
  ORControllerNameZone playerIndex zone name index -> do
    player <- resolvePlayer opaque playerIndex
    pure (getByControllerNameZone opaque player name zone, index)

resolvePlayer :: OpaqueGameState Headless -> Int -> Headless (Object 'OTPlayer)
resolvePlayer opaque index = do
  let players = getPlayers opaque
  case 0 <= index && index < length players of
    True -> pure $ players !! index
    False ->
      stopHeadless $
        "resolvePlayer: no player at index "
          ++ show index
          ++ "; player ids are "
          ++ show (map getObjectId players)

-- | Resolve a scripted object reference to a concrete id.
resolveRef :: OpaqueGameState Headless -> ObjectRef -> Headless ObjectId
resolveRef opaque ref = do
  (candidates, index) <- refCandidates opaque ref
  case 0 <= index && index < length candidates of
    True -> pure $ candidates !! index
    False ->
      stopHeadless $
        "resolveRef: no candidate at index for "
          ++ show ref
          ++ "; candidate ids are "
          ++ show candidates

-- | Resolve a scripted object reference against a pick prompt's candidates. A
-- name-based ref indexes into the prompt's candidates matching the name (and
-- zone/controller), sorted ascending by id; an id-based ref must name one of
-- the candidates directly.
resolvePickRef ::
  (GetObjectId a) =>
  OpaqueGameState Headless ->
  NonEmpty a ->
  ObjectRef ->
  Headless a
resolvePickRef opaque xs ref = case ref of
  ORId{} -> byId
  ORPlayer{} -> byId
  ORNameZone{} -> byName
  ORControllerNameZone{} -> byName
 where
  findCandidate objId = List.find (\y -> objId == getObjectId y) (NonEmpty.toList xs)
  byId = do
    objId <- resolveRef opaque ref
    case findCandidate objId of
      Just y -> pure y
      Nothing -> stopHeadless ("promptPick: no candidate with id " ++ show objId)
  byName = do
    (candidateIds, index) <- refCandidates opaque ref
    let matches = [y | objId <- candidateIds, Just y <- [findCandidate objId]]
    case 0 <= index && index < length matches of
      True -> pure $ matches !! index
      False ->
        stopHeadless $
          "promptPick: no candidate at index for "
            ++ show ref
            ++ "; matching candidate ids are "
            ++ show (map getObjectId matches)

--------------------------------------------------------------------------------
-- Engine action-builders (monad-generic; mirror the terminal client)

buildPriorityAction :: (Monad m) => CIPriorityAction -> Magic 'Public 'RO m (PriorityAction ())
buildPriorityAction = \case
  CIPass -> pure PassPriority
  CIConcede -> pure Concede
  CIAskAgain -> pure (AskPriorityActionAgain Nothing)
  CIPlayLand landId _extras -> buildPlayLand landId
  CICastSpell spellId _extras -> buildCastSpell spellId
  CIActivateAbility objId abilityIndex _extras -> buildActivateAbility objId abilityIndex
  -- Interactive-only commands; a headless script never issues these.
  CIHelp _ -> pure (AskPriorityActionAgain Nothing)
  CIExamineObject _ -> pure (AskPriorityActionAgain Nothing)
  CIExamineAbility _ _ -> pure (AskPriorityActionAgain Nothing)
  CIQuit -> pure (AskPriorityActionAgain Nothing)

buildPlayLand :: (Monad m) => ObjectId -> Magic 'Public 'RO m (PriorityAction ())
buildPlayLand landId = do
  let zo0 = toZO0 @'ZHand landId
      zo = toZO1 zo0
  pure $ PriorityAction $ SpecialAction $ PlayLand zo

buildCastSpell :: (Monad m) => ObjectId -> Magic 'Public 'RO m (PriorityAction ())
buildCastSpell spellId = do
  let zo0 = toZO0 @'ZHand spellId
      zo = zo0ToSpell zo0
  pure $ PriorityAction $ CastSpell zo

buildActivateAbility ::
  forall m.
  (Monad m) =>
  ObjectId ->
  CommandAbilityIndex ->
  Magic 'Public 'RO m (PriorityAction ())
buildActivateAbility objId abilityIndex = do
  mZoAny <- internalFromPrivate $ toZO @'ZBattlefield @OTNAny objId
  mZoPerm <- internalFromPrivate $ toZO @'ZBattlefield @OTNPermanent objId
  case abilityIndex of
    CIAbilityIndex relIndex -> case mZoAny of
      Nothing -> tryAgain
      Just _zoAny -> do
        let index = AbsoluteActivatedAbilityIndex objId $ RelativeAbilityIndex relIndex
        mAction <- mconcat . map First <$> sequence [goIndex @'ZBattlefield index]
        case getFirst mAction of
          Just action -> pure action
          Nothing -> tryAgain
    CIManaAbility ty -> case mZoPerm of
      Nothing -> tryAgain
      Just zoPerm -> do
        abilities <- internalFromPrivate $ getIntrinsicManaAbilities zoPerm
        let cond (SomeActivatedAbility _zo ability) = isTrivialManaAbility ability == Just ty
        case List.find cond abilities of
          Just ability -> pure $ PriorityAction $ ActivateAbility ability
          Nothing -> tryAgain
    CIInferManaAbility -> case mZoPerm of
      Nothing -> tryAgain
      Just zoPerm -> do
        abilities <- internalFromPrivate $ getTrivialManaAbilities zoPerm
        case abilities of
          [ability] -> pure $ PriorityAction $ ActivateAbility ability
          _ -> tryAgain
 where
  tryAgain :: Magic 'Public 'RO m (PriorityAction ())
  tryAgain = pure $ AskPriorityActionAgain Nothing

  goIndex ::
    forall zone.
    (IsZone zone) =>
    AbsoluteActivatedAbilityIndex ->
    Magic 'Public 'RO m (Maybe (PriorityAction ()))
  goIndex index = do
    mAbility <- internalFromPrivate $ indexToActivated index
    case mAbility of
      Just ability -> pure $ Just $ PriorityAction $ ActivateAbility (ability :: SomeActivatedAbility zone OTNAny)
      Nothing -> pure Nothing

buildAttackers ::
  (Monad m) =>
  [ObjectId] ->
  Object 'OTPlayer ->
  Magic 'Public 'RO m [DeclaredAttacker]
buildAttackers oAttackers oDefender = do
  zoAttackers <- fmap catMaybes $ internalFromPrivate $ T.for oAttackers toZO
  let zoVictim = toZO2 $ oToZO1 oDefender
  pure $ case length zoAttackers == length oAttackers of
    False -> []
    True ->
      zoAttackers <&> \zoAttacker ->
        DeclaredAttacker
          { declaredAttacker_attacker = zoAttacker
          , declaredAttacker_victim = zoVictim
          }

buildBlockers ::
  (Monad m) =>
  [(ObjectId, ObjectId)] ->
  Magic 'Public 'RO m [DeclaredBlocker]
buildBlockers pairs = do
  let oAttackers = map fst pairs
      oBlockers = map snd pairs
  zoAttackers <- fmap catMaybes $ internalFromPrivate $ T.for oAttackers toZO
  zoBlockers <- fmap catMaybes $ internalFromPrivate $ T.for oBlockers toZO
  case length zoAttackers == length pairs && length zoBlockers == length pairs of
    False -> pure []
    True -> do
      let combined = zip zoAttackers zoBlockers
          insert m (a, b) = Map.insertWith (<>) b (a :| []) m
          blockerToAttackers = foldl' insert Map.empty combined
      pure $
        Map.toList blockerToAttackers <&> \(zoBlocker, zoAtts) ->
          DeclaredBlocker
            { declaredBlocker_blocker = zoBlocker
            , declaredBlocker_attackers = zoAtts
            }

--------------------------------------------------------------------------------
-- Game entry point

headlessGameInput :: GameCheats -> [(Deck, Sideboard)] -> GameInput Headless
headlessGameInput cheats decks =
  GameInput
    { gameInput_ = ()
    , gameInput_decks = decks
    , gameInput_gameCheats = cheats
    , gameInput_gameFormat = Vintage
    , gameInput_mulligan = DisableMulligan
    , gameInput_prompt = replayHeadlessPrompt
    }

playHeadlessGame :: GameCheats -> [(Deck, Sideboard)] -> Headless (Maybe (GameResult Headless))
playHeadlessGame cheats decks = playGame $ headlessGameInput cheats decks

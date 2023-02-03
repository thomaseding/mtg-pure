{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant if" #-}

module MtgPure.Engine.Turn (
  startGame,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (untilJust)
import safe Data.List.NonEmpty (NonEmpty ((:|)))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe qualified Data.Traversable as T
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd.Api (
  allControlledPermanentsOf,
  eachLogged_,
  enact,
  gainPriority,
  getActivePlayer,
  getAlivePlayerCount,
  getAlivePlayers,
  getPermanent,
  getPlayer,
  setPermanent,
  setPlayer,
 )
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromPublicRO,
  fromRO,
  get,
  gets,
  liftCont,
  modify,
  put,
 )
import safe MtgPure.Engine.Prompt (
  DeclaredAttacker (..),
  DeclaredBlocker (..),
  InternalLogicError (..),
  PlayerCount (..),
  PlayerIndex (..),
  Prompt' (..),
 )
import safe MtgPure.Engine.State (GameState (..), Magic, MagicCont, logCall, mkOpaqueGameState, runMagicCont)
import safe MtgPure.Model.Object.OTNAliases (OTNCreature, OTNPermanent)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Permanent (Permanent (..), Phased (..))
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Effect (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (oToZO1)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

-- (103)
startGame :: Monad m => Magic 'Private 'RW m Void
startGame = logCall 'startGame do
  determineStartingPlayer -- (103.1)
  ps <- fromPublicRO getAlivePlayers
  eachLogged_ ps $ M.void . enact Nothing . ShuffleLibrary . oToZO1 -- (103.2)
  pure () -- (103.3) See `mkPlayer`
  drawStartingHands -- (103.4)
  pure () -- (103.5) TODO: leylines and such
  pure () -- (103.6) TODO: planechase stuff

  -- NOTE: MagicCont is appropriate in order to nicely support cards like [Stasis] and [Time Stop]
  either id id <$> runMagicCont untapStep

-- (103.1)
determineStartingPlayer :: Monad m => Magic 'Private 'RW m ()
determineStartingPlayer = logCall 'determineStartingPlayer do
  st <- fromRO get
  let prompt = magicPrompt st
      playerCount = Map.size $ magicPlayers st
  M.when (playerCount == 0) do
    undefined -- TODO: complain and abort
  logicalStartingIndex <- fromPublic $ fromRO do
    untilJust \attempt -> do
      PlayerIndex playerIndex <- promptGetStartingPlayer prompt attempt $ PlayerCount playerCount
      case playerIndex < playerCount of
        True -> pure $ Just playerIndex
        False -> M.lift do
          exceptionInvalidStartingPlayer prompt (PlayerCount playerCount) $ PlayerIndex playerIndex
          pure Nothing
  let startingIndex = logicalStartingIndex + playerCount - 1 -- This compensates for initial `advanceTurnState`
  let ps = Stream.drop startingIndex $ magicPlayerOrderTurn st
  put
    st
      { magicPlayerOrderAPNAP = ps
      , magicPlayerOrderPriority = []
      , magicPlayerOrderTurn = ps
      }

-- (103.4)
drawStartingHands :: Monad m => Magic 'Private 'RW m ()
drawStartingHands = logCall 'drawStartingHands do
  ps <- fromPublic $ fromRO getAlivePlayers
  eachLogged_ ps drawStartingHand

drawStartingHand :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
drawStartingHand oPlayer = logCall 'drawStartingHand do
  player <- fromRO $ getPlayer oPlayer
  M.void $ enact Nothing $ DrawCards (oToZO1 oPlayer) $ playerStartingHandSize player

setPhaseStep :: PhaseStep -> Monad m => Magic 'Private 'RW m ()
setPhaseStep phaseStep = logCall 'setPhaseStep do
  modify \st -> st{magicPhaseStep = phaseStep}

-- NOTE: This hangs if there are not enough unique items.
takeUnique :: Eq a => Int -> Stream.Stream a -> [a]
takeUnique n s = case n <= 0 of
  True -> []
  False ->
    let x = Stream.head s
        s' = Stream.tail $ Stream.filter (/= x) s
     in x : takeUnique (n - 1) s'

mkAPNAP :: PlayerCount -> Stream.Stream (Object 'OTPlayer) -> Stream.Stream (Object 'OTPlayer)
mkAPNAP (PlayerCount n) = Stream.cycle . takeUnique n

advanceStateForNewTurn :: Monad m => Magic 'Private 'RW m ()
advanceStateForNewTurn = logCall 'advanceStateForNewTurn do
  n <- fromPublicRO getAlivePlayerCount
  ps <- fromRO $ gets $ Stream.tail . magicPlayerOrderTurn
  modify \st ->
    assert
      (null $ magicPlayerOrderPriority st)
      st
        { magicCurrentTurn = magicCurrentTurn st + 1
        , magicPlayerOrderAPNAP = mkAPNAP n ps
        , magicPlayerOrderTurn = ps
        }

-- (502)
untapStep :: Monad m => MagicCont 'Private 'RW Void m Void
untapStep = M.join $ logCall 'untapStep do
  liftCont do
    setPhaseStep $ PSBeginningPhase UntapStep
    advanceStateForNewTurn
    oPlayer <- fromPublicRO getActivePlayer
    do
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer player{playerLandsPlayedThisTurn = 0}
    do
      zos <- fromPublicRO $ allControlledPermanentsOf oPlayer
      eachLogged_ zos togglePermanentPhase -- (502.1)
    pure () -- (502.2) TODO: day/night
    do
      zos <- fromPublicRO $ allControlledPermanentsOf oPlayer
      eachLogged_ zos $ M.void . enact Nothing . Untap -- (502.3) TODO: fine-grained untapping
    pure () -- (502.4) Rule states that players can't get priority, so nothing to do here.
  pure upkeepStep

-- TODO: Make this an Effect constructor
togglePermanentPhase :: Monad m => ZO 'ZBattlefield OTNPermanent -> Magic 'Private 'RW m ()
togglePermanentPhase oPerm = do
  perm <- fromRO $ getPermanent oPerm
  setPermanent
    oPerm
    $ Just
      perm
        { permanentPhased = case permanentPhased perm of
            PhasedIn -> PhasedOut
            PhasedOut -> PhasedIn
        }

upkeepStep :: Monad m => MagicCont 'Private 'RW Void m Void
upkeepStep = M.join $ logCall 'upkeepStep do
  liftCont do
    setPhaseStep $ PSBeginningPhase UpkeepStep
  oActive <- liftCont $ fromPublicRO getActivePlayer
  gainPriority oActive
  pure drawStep

drawStep :: Monad m => MagicCont 'Private 'RW Void m Void
drawStep = M.join $ logCall 'drawStep do
  oActive <- liftCont $ fromPublicRO getActivePlayer
  liftCont do
    setPhaseStep $ PSBeginningPhase DrawStep
    st <- fromRO get
    case magicCurrentTurn st of
      1 -> pure () -- (103.7.*) TODO: this needs to account for game format
      _ -> M.void $ enact Nothing $ DrawCards (oToZO1 oActive) 1
  gainPriority oActive
  pure precombatMainPhase

precombatMainPhase :: Monad m => MagicCont 'Private 'RW Void m Void
precombatMainPhase = M.join $ logCall 'precombatMainPhase do
  liftCont do
    setPhaseStep PSPreCombatMainPhase
  mainPhaseCommon
  pure beginningOfCombatStep

postcombatMainPhase :: Monad m => MagicCont 'Private 'RW Void m Void
postcombatMainPhase = M.join $ logCall 'postcombatMainPhase do
  liftCont do
    setPhaseStep PSPostCombatMainPhase
  mainPhaseCommon
  pure endStep

-- (505)
mainPhaseCommon :: Monad m => MagicCont 'Private 'RW Void m ()
mainPhaseCommon = logCall 'mainPhaseCommon do
  pure () -- (505.1) Rule just states nomenclature. Nothing special to do
  pure () -- (505.2) Rule just states this phase has no steps
  pure () -- (505.3) TODO: Archenemy
  pure () -- (505.4) TODO: Sage lore counters
  oActive <- liftCont $ fromPublicRO getActivePlayer
  gainPriority oActive

beginningOfCombatStep :: Monad m => MagicCont 'Private 'RW Void m Void
beginningOfCombatStep = M.join $ logCall 'beginningOfCombatStep do
  liftCont $ setPhaseStep $ PSCombatPhase BeginningOfCombatStep
  pure () -- TODO: in multiplayer, choose defending player
  oActive <- liftCont $ fromPublicRO getActivePlayer
  gainPriority oActive
  pure declareAttackersStep

declareAttackersStep :: Monad m => MagicCont 'Private 'RW Void m Void
declareAttackersStep = M.join $ logCall 'declareAttackersStep do
  oActive <- liftCont $ fromPublic $ fromRO getActivePlayer
  attackers <- liftCont $ do
    setPhaseStep $ PSCombatPhase DeclareAttackersStep
    prompt <- fromRO $ gets magicPrompt
    opaque <- fromRO $ gets mkOpaqueGameState
    fromPublic $ fromRO do
      untilJust \attempt -> do
        attackers <- promptChooseAttackers prompt attempt opaque oActive
        pure () -- TODO: validate attackers
        pure () -- TODO: store attackers in game state for Elects and Effects
        pure $ Just attackers
  gainPriority oActive
  pure case NonEmpty.nonEmpty attackers of
    Just attackers' -> declareBlockersStep attackers'
    Nothing -> endOfCombatStep

getDefendingPlayer :: Monad m => Magic 'Private 'RO m (Object 'OTPlayer)
getDefendingPlayer = do
  let isOneVersusOne = True -- TODO: multiplayer
  case isOneVersusOne of
    False -> undefined -- TODO: prompt the active player to choose a defending player
    True -> do
      active <- fromPublicRO getActivePlayer
      alive <- fromPublicRO getAlivePlayers
      case filter (/= active) alive of
        defender : _ -> pure defender
        [] -> error $ show GameShouldHaveEndedBecauseThereIsOnlyOnePlayerLeft

type AssignedCombatOrdering = Map.Map (ZO 'ZBattlefield OTNCreature) [ZO 'ZBattlefield OTNCreature]

assignCombatDamageOrder ::
  Monad m =>
  NonEmpty DeclaredAttacker ->
  [DeclaredBlocker] ->
  Magic 'Private 'RW m AssignedCombatOrdering
assignCombatDamageOrder attackers blockers = do
  let attackerToBlockers = mkAttackerToBlockers attackers blockers
  attackerOrderings <- T.for (NonEmpty.toList attackers) \attacker -> do
    let attacker' = declaredAttacker_attacker attacker
    (,) attacker' <$> case attackerToBlockers attacker' of
      [] -> pure []
      [blocker] -> pure [blocker]
      _ -> undefined -- TODO: prompt how to assign combat damage to multiple blockers
  blockerOrderings <- T.for blockers \blocker -> do
    let blocker' = declaredBlocker_blocker blocker
    (,) blocker' <$> case declaredBlocker_attackers blocker of
      attacker :| [] -> pure [attacker]
      _ -> undefined -- TODO: prompt how to assign combat damage to multiple attackers
  pure $ Map.fromList $ attackerOrderings <> blockerOrderings

type AttackerToBlockers = ZO 'ZBattlefield OTNCreature -> [ZO 'ZBattlefield OTNCreature]

mkAttackerToBlockers :: NonEmpty DeclaredAttacker -> [DeclaredBlocker] -> AttackerToBlockers
mkAttackerToBlockers attackers blockers =
  let mapping = attackerToBlockers' blockers
      attackers' = map declaredAttacker_attacker $ NonEmpty.toList attackers
   in \attacker -> case Map.lookup attacker mapping of
        Just blockers' -> blockers'
        Nothing -> case attacker `elem` attackers' of
          True -> []
          False -> error $ show (undefined :: InternalLogicError)

attackerToBlockers' :: [DeclaredBlocker] -> Map.Map (ZO 'ZBattlefield OTNCreature) [ZO 'ZBattlefield OTNCreature]
attackerToBlockers' blockers = Map.fromListWith (++) $ do
  blocker <- blockers
  attacker <- NonEmpty.toList $ declaredBlocker_attackers blocker
  pure (attacker, [declaredBlocker_blocker blocker])

declareBlockersStep :: Monad m => NonEmpty DeclaredAttacker -> MagicCont 'Private 'RW Void m Void
declareBlockersStep attackers = M.join $ logCall 'declareBlockersStep do
  oActive <- liftCont $ fromPublicRO getActivePlayer
  ordering <- liftCont do
    setPhaseStep $ PSCombatPhase DeclareBlockersStep
    prompt <- fromRO $ gets magicPrompt
    opaque <- fromRO $ gets mkOpaqueGameState
    oDefender <- fromRO getDefendingPlayer
    blockers <- fromPublic $ fromRO do
      untilJust \attempt -> do
        blockers <- promptChooseBlockers prompt attempt opaque oDefender attackers
        pure () -- TODO: validate blockers
        pure () -- TODO: store blockers in game state for Elects and Effects
        pure $ Just blockers
    assignCombatDamageOrder attackers blockers
  gainPriority oActive
  pure $ combatDamageStep ordering

combatDamageStep :: Monad m => AssignedCombatOrdering -> MagicCont 'Private 'RW Void m Void
combatDamageStep ordering = M.join $ logCall 'combatDamageStep do
  liftCont $ setPhaseStep $ PSCombatPhase CombatDamageStep
  _ <- undefined ordering -- TODO
  oActive <- liftCont $ fromPublicRO getActivePlayer
  gainPriority oActive
  pure endOfCombatStep

endOfCombatStep :: Monad m => MagicCont 'Private 'RW Void m Void
endOfCombatStep = M.join $ logCall 'endOfCombatStep do
  liftCont do
    setPhaseStep $ PSCombatPhase EndOfCombatStep
  oActive <- liftCont $ fromPublicRO getActivePlayer
  gainPriority oActive
  pure postcombatMainPhase

endStep :: Monad m => MagicCont 'Private 'RW Void m Void
endStep = M.join $ logCall 'endStep do
  liftCont do
    setPhaseStep $ PSEndingPhase EndStep
  oActive <- liftCont $ fromPublicRO getActivePlayer
  gainPriority oActive
  pure cleanupStep

cleanupStep :: Monad m => MagicCont 'Private 'RW Void m Void
cleanupStep = M.join $ logCall 'cleanupStep do
  liftCont $ setPhaseStep $ PSEndingPhase CleanupStep
  --_ <- undefined
  pure untapStep

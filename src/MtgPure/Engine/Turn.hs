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
  eachLogged_ ps $ M.void . enact . ShuffleLibrary . oToZO1 -- (103.2)
  pure () -- (103.3) See `mkPlayer`
  drawStartingHands -- (103.4)
  pure () -- (103.5) TODO: leylines and such
  pure () -- (103.6) TODO: planechase stuff

  -- NOTE: MagicCont is appropriate in order to nicely support cards like [Stasis] and [Time Stop]
  runMagicCont (either id id) untapStep

-- (103.1)
determineStartingPlayer :: Monad m => Magic 'Private 'RW m ()
determineStartingPlayer = logCall 'determineStartingPlayer do
  st <- fromRO get
  let prompt = magicPrompt st
      playerCount = Map.size $ magicPlayers st
  M.when (playerCount == 0) do
    undefined -- TODO: complain and abort
  logicalStartingIndex <- M.lift $
    untilJust \attempt -> do
      PlayerIndex playerIndex <- promptGetStartingPlayer prompt attempt $ PlayerCount playerCount
      case playerIndex < playerCount of
        True -> pure $ Just playerIndex
        False -> do
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
  M.void $ enact $ DrawCards (oToZO1 oPlayer) $ playerStartingHandSize player

setPhaseStep :: PhaseStep -> Monad m => MagicCont 'Private 'RW m Void ()
setPhaseStep phaseStep = logCall 'setPhaseStep do
  liftCont $ modify \st -> st{magicPhaseStep = phaseStep}

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

advanceTurnState :: Monad m => Magic 'Private 'RW m ()
advanceTurnState = logCall 'advanceTurnState do
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
untapStep :: Monad m => MagicCont 'Private 'RW m Void Void
untapStep = do
  logCall 'untapStep do
    setPhaseStep $ PSBeginningPhase UntapStep
    liftCont do
      advanceTurnState
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
        eachLogged_ zos $ M.void . enact . Untap -- (502.3) TODO: fine-grained untapping
      pure () -- (502.4) Rule states that players can't get priority, so nothing to do here.
  upkeepStep

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

upkeepStep :: Monad m => MagicCont 'Private 'RW m Void Void
upkeepStep = do
  logCall 'upkeepStep do
    setPhaseStep $ PSBeginningPhase UpkeepStep
    liftCont do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  drawStep

drawStep :: Monad m => MagicCont 'Private 'RW m Void Void
drawStep = do
  logCall 'drawStep do
    setPhaseStep $ PSBeginningPhase DrawStep
    liftCont do
      st <- fromRO get
      oActive <- fromPublicRO getActivePlayer
      case magicCurrentTurn st of
        1 -> pure () -- (103.7.*) TODO: this needs to account for game format
        _ -> M.void $ enact $ DrawCards (oToZO1 oActive) 1
      gainPriority oActive
  precombatMainPhase

precombatMainPhase :: Monad m => MagicCont 'Private 'RW m Void Void
precombatMainPhase = do
  logCall 'precombatMainPhase do
    setPhaseStep PSPreCombatMainPhase
    liftCont mainPhaseCommon
  beginningOfCombatStep

postcombatMainPhase :: Monad m => MagicCont 'Private 'RW m Void Void
postcombatMainPhase = do
  logCall 'postcombatMainPhase do
    setPhaseStep PSPostCombatMainPhase
    liftCont mainPhaseCommon
  endStep

-- (505)
mainPhaseCommon :: Monad m => Magic 'Private 'RW m ()
mainPhaseCommon = logCall 'mainPhaseCommon do
  pure () -- (505.1) Rule just states nomenclature. Nothing special to do
  pure () -- (505.2) Rule just states this phase has no steps
  pure () -- (505.3) TODO: Archenemy
  pure () -- (505.4) TOOD: Sage lore counters
  oActive <- fromPublicRO getActivePlayer
  gainPriority oActive

beginningOfCombatStep :: Monad m => MagicCont 'Private 'RW m Void Void
beginningOfCombatStep = do
  logCall 'beginningOfCombatStep do
    setPhaseStep $ PSCombatPhase BeginningOfCombatStep
    pure () -- TODO: in multiplayer, choose defending player
    liftCont do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  declareAttackersStep

declareAttackersStep :: Monad m => MagicCont 'Private 'RW m Void Void
declareAttackersStep = do
  attackers <- logCall 'declareAttackersStep do
    setPhaseStep $ PSCombatPhase DeclareAttackersStep
    pure () -- TODO
    liftCont do
      prompt <- fromRO $ gets magicPrompt
      opaque <- fromRO $ gets mkOpaqueGameState
      oActive <- fromPublicRO getActivePlayer
      attackers <- fromRO $ untilJust \attempt -> do
        attackers <- M.lift $ promptChooseAttackers prompt attempt opaque oActive
        pure $ Just attackers -- TODO: validate attackers
      gainPriority oActive
      pure attackers
  case NonEmpty.nonEmpty attackers of
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

assignCombatDamageOrder :: Monad m => NonEmpty DeclaredAttacker -> [DeclaredBlocker] -> Magic 'Private 'RW m AssignedCombatOrdering
assignCombatDamageOrder attackers blockers = do
  attackerOrderings <- T.for (NonEmpty.toList attackers) \attacker -> do
    let attacker' = declaredAttacker_attacker attacker
    (,) attacker' . map declaredBlocker_blocker <$> case Map.findWithDefault [] attacker' attackerToBlockers of
      [] -> pure []
      [blocker] -> pure [blocker]
      _ -> undefined -- TODO: prompt how to assign combat damage to multiple blockers
  blockerOrderings <- T.for blockers \blocker -> do
    let blocker' = declaredBlocker_blocker blocker
    (,) blocker' <$> case declaredBlocker_attackers blocker of
      attacker :| [] -> pure [attacker]
      _ -> undefined -- TODO: prompt how to assign combat damage to multiple attackers
  pure $ Map.fromList $ attackerOrderings <> blockerOrderings
 where
  attackerToBlockers :: Map.Map (ZO 'ZBattlefield OTNCreature) [DeclaredBlocker]
  attackerToBlockers = Map.fromListWith (++) $ do
    blocker <- blockers
    attacker <- NonEmpty.toList $ declaredBlocker_attackers blocker
    pure (attacker, [blocker])

declareBlockersStep :: Monad m => NonEmpty DeclaredAttacker -> MagicCont 'Private 'RW m Void Void
declareBlockersStep attackers = do
  ordering <- logCall 'declareBlockersStep do
    setPhaseStep $ PSCombatPhase DeclareBlockersStep
    liftCont do
      prompt <- fromRO $ gets magicPrompt
      opaque <- fromRO $ gets mkOpaqueGameState
      oDefender <- fromRO getDefendingPlayer
      blockers <- fromRO $ untilJust \attempt -> do
        blockers <- M.lift $ promptChooseBlockers prompt attempt opaque oDefender attackers
        pure $ Just blockers -- TODO: validate blockers
      oActive <- fromPublicRO getActivePlayer
      ordering <- assignCombatDamageOrder attackers blockers
      gainPriority oActive
      pure ordering
  combatDamageStep ordering

combatDamageStep :: Monad m => AssignedCombatOrdering -> MagicCont 'Private 'RW m Void Void
combatDamageStep ordering = do
  logCall 'combatDamageStep do
    setPhaseStep $ PSCombatPhase CombatDamageStep
    _ <- undefined ordering -- TODO
    liftCont do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  endOfCombatStep

endOfCombatStep :: Monad m => MagicCont 'Private 'RW m Void Void
endOfCombatStep = do
  logCall 'endOfCombatStep do
    setPhaseStep $ PSCombatPhase EndOfCombatStep
    liftCont do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  postcombatMainPhase

endStep :: Monad m => MagicCont 'Private 'RW m Void Void
endStep = do
  logCall 'endStep do
    setPhaseStep $ PSEndingPhase EndStep
    liftCont do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  cleanupStep

cleanupStep :: Monad m => MagicCont 'Private 'RW m Void Void
cleanupStep = do
  logCall 'cleanupStep do
    setPhaseStep $ PSEndingPhase CleanupStep
  --_ <- undefined
  untapStep

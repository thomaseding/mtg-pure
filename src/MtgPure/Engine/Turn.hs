{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Turn (
  startGameImpl,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (untilJust)
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd.Wrap (
  enact,
  gainPriority,
  getActivePlayer,
  getAlivePlayerCount,
  getPermanent,
  getPlayer,
  logCall,
  runMagicCont,
  setPermanent,
  setPlayer,
  withEachControlledPermanent_,
  withEachPlayer_,
 )
import safe MtgPure.Engine.Monad (
  fromPublicRO,
  fromRO,
  get,
  gets,
  modify,
  put,
 )
import safe MtgPure.Engine.Prompt (PlayerCount (..), PlayerIndex (..), Prompt' (..))
import safe MtgPure.Engine.State (GameState (..), Magic, MagicCont)
import safe MtgPure.Model.Object (Object, ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Permanent (Permanent (..), Phased (..))
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Effect (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (ZO)
import safe MtgPure.Model.ZoneObject.Convert (oToZO1)

-- (103)
startGameImpl :: Monad m => Magic 'Private 'RW m Void
startGameImpl = logCall 'startGameImpl $ do
  determineStartingPlayer -- (103.1)
  withEachPlayer_ $ enact . ShuffleLibrary . oToZO1 -- (103.2)
  pure () -- (103.3) See `mkPlayer`
  drawStartingHands -- (103.4)
  pure () -- (103.5) TODO: leylines and such
  pure () -- (103.6) TODO: planechase stuff

  -- NOTE: MagicCont is appropriate in order to nicely support cards like [Stasis] and [Time Stop]
  runMagicCont (either id id) untapStep

-- (103.1)
determineStartingPlayer :: Monad m => Magic 'Private 'RW m ()
determineStartingPlayer = logCall 'determineStartingPlayer $ do
  st <- fromRO get
  let prompt = magicPrompt st
      playerCount = Map.size $ magicPlayers st
  startingIndex <- lift $
    untilJust $ do
      PlayerIndex playerIndex <- promptGetStartingPlayer prompt $ PlayerCount playerCount
      case playerIndex < playerCount of
        True -> pure $ Just playerIndex
        False -> do
          exceptionInvalidStartingPlayer prompt (PlayerCount playerCount) $ PlayerIndex playerIndex
          pure Nothing
  let ps = Stream.drop startingIndex $ magicPlayerOrderTurn st
  put $
    st
      { magicPlayerOrderAPNAP = ps
      , magicPlayerOrderPriority = []
      , magicPlayerOrderTurn = ps
      }

-- (103.4)
drawStartingHands :: Monad m => Magic 'Private 'RW m ()
drawStartingHands = logCall 'drawStartingHands $ do
  withEachPlayer_ drawStartingHand

drawStartingHand :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
drawStartingHand oPlayer = logCall 'drawStartingHand $ do
  player <- fromRO $ getPlayer oPlayer
  enact $ DrawCards (oToZO1 oPlayer) $ playerStartingHandSize player

setPhaseStep :: PhaseStep -> Monad m => MagicCont 'Private 'RW m Void ()
setPhaseStep phaseStep = logCall 'setPhaseStep $ do
  lift $ modify $ \st -> st{magicPhaseStep = phaseStep}

-- NB: This hangs if there are not enough unique items.
takeUnique :: Eq a => Int -> Stream.Stream a -> [a]
takeUnique n s = case n <= 0 of
  True -> []
  False ->
    let x = Stream.head s
        s' = Stream.tail $ Stream.filter (/= x) s
     in x : takeUnique (n - 1) s'

mkAPNAP :: PlayerCount -> Stream.Stream (Object 'OTPlayer) -> Stream.Stream (Object 'OTPlayer)
mkAPNAP (PlayerCount n) = Stream.cycle . takeUnique n

-- (502)
untapStep :: Monad m => MagicCont 'Private 'RW m Void Void
untapStep = do
  logCall 'untapStep $ do
    setPhaseStep $ PSBeginningPhase UntapStep
    lift $ do
      do
        n <- fromPublicRO getAlivePlayerCount
        ps <- fromRO $ gets magicPlayerOrderTurn
        modify $ \st ->
          st
            { magicCurrentTurn = magicCurrentTurn st + 1
            , magicPlayerOrderAPNAP = mkAPNAP n ps
            }
      oPlayer <- fromPublicRO getActivePlayer
      do
        player <- fromRO $ getPlayer oPlayer
        setPlayer oPlayer player{playerLandsPlayedThisTurn = 0}
      withEachControlledPermanent_ oPlayer togglePermanentPhase -- (502.1)
      pure () -- (502.2) TODO: day/night
      withEachControlledPermanent_ oPlayer (enact . Untap) -- (502.3) TODO: fine-grained untapping
      pure () -- (502.4) Rule states that players can't get priority, so nothing to do here.
  upkeepStep

-- TODO: Make this an Effect constructor
togglePermanentPhase :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RW m ()
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
  logCall 'upkeepStep $ do
    setPhaseStep $ PSBeginningPhase UpkeepStep
    lift $ do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  drawStep

drawStep :: Monad m => MagicCont 'Private 'RW m Void Void
drawStep = do
  logCall 'drawStep $ do
    setPhaseStep $ PSBeginningPhase DrawStep
    lift $ do
      st <- fromRO get
      oActive <- fromPublicRO getActivePlayer
      case magicCurrentTurn st of
        1 -> pure () -- (103.7.*) TODO: this needs to account for game format
        _ -> enact $ DrawCards (oToZO1 oActive) 1
      gainPriority oActive
  precombatMainPhase

precombatMainPhase :: Monad m => MagicCont 'Private 'RW m Void Void
precombatMainPhase = do
  logCall 'precombatMainPhase $ do
    setPhaseStep PSPreCombatMainPhase
    lift mainPhaseCommon
  beginningOfCombatStep

postcombatMainPhase :: Monad m => MagicCont 'Private 'RW m Void Void
postcombatMainPhase = do
  logCall 'postcombatMainPhase $ do
    setPhaseStep PSPreCombatMainPhase
    lift mainPhaseCommon
  endStep

-- (505)
mainPhaseCommon :: Monad m => Magic 'Private 'RW m ()
mainPhaseCommon = logCall 'mainPhaseCommon $ do
  pure () -- (505.1) Rule just states nomenclature. Nothing special to do
  pure () -- (505.2) Rule just states this phase has no steps
  pure () -- (505.3) TODO: Archenemy
  pure () -- (505.4) TOOD: Sage lore counters
  oActive <- fromPublicRO getActivePlayer
  gainPriority oActive

beginningOfCombatStep :: Monad m => MagicCont 'Private 'RW m Void Void
beginningOfCombatStep = do
  logCall 'beginningOfCombatStep $ do
    setPhaseStep $ PSCombatPhase BeginningOfCombatStep
    --_ <- undefined
    lift $ do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  declareAttackersStep

declareAttackersStep :: Monad m => MagicCont 'Private 'RW m Void Void
declareAttackersStep = do
  logCall 'declareAttackersStep $ do
    setPhaseStep $ PSCombatPhase DeclareAttackersStep
    --_ <- undefined
    lift $ do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  declareBlockersStep

declareBlockersStep :: Monad m => MagicCont 'Private 'RW m Void Void
declareBlockersStep = do
  logCall 'declareBlockersStep $ do
    setPhaseStep $ PSCombatPhase DeclareBlockersStep
    --_ <- undefined
    lift $ do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  combatDamageStep

combatDamageStep :: Monad m => MagicCont 'Private 'RW m Void Void
combatDamageStep = do
  logCall 'combatDamageStep $ do
    setPhaseStep $ PSCombatPhase CombatDamageStep
    --_ <- undefined
    lift $ do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  endOfCombatStep

endOfCombatStep :: Monad m => MagicCont 'Private 'RW m Void Void
endOfCombatStep = do
  logCall 'endOfCombatStep $ do
    setPhaseStep $ PSCombatPhase EndOfCombatStep
    lift $ do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  postcombatMainPhase

endStep :: Monad m => MagicCont 'Private 'RW m Void Void
endStep = do
  logCall 'endStep $ do
    setPhaseStep $ PSEndingPhase EndStep
    lift $ do
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive
  cleanupStep

cleanupStep :: Monad m => MagicCont 'Private 'RW m Void Void
cleanupStep = do
  logCall 'cleanupStep $ do
    setPhaseStep $ PSEndingPhase CleanupStep
  --_ <- undefined
  untapStep
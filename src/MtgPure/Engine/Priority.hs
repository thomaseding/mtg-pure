{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Priority (
  gainPriority,
  getPlayerWithPriority,
  getHasPriority,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Functor ((<&>))
import safe qualified Data.Stream as Stream
import safe Data.Void (Void, absurd)
import safe MtgPure.Engine.Fwd.Api (
  askActivateAbility,
  askCastSpell,
  askPlayLand,
  getAPNAP,
  getAlivePlayerCount,
  performStateBasedActions,
  resolveTopOfStack,
 )
import safe MtgPure.Engine.Monad (
  fromPublicRO,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  liftCont,
  magicCont,
  modify,
 )
import safe MtgPure.Engine.Prompt (PlayerCount (..))
import safe MtgPure.Engine.State (GameState (..), Magic, MagicCont, logCall, runMagicCont)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.PhaseStep (isMainPhase)
import safe MtgPure.Model.Stack (Stack (..))

gainPriority :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
gainPriority oPlayer = do
  logCall 'gainPriority do
    PlayerCount n <- fromPublicRO getAlivePlayerCount
    ps <- fromRO $ Stream.take n . Stream.dropWhile (/= oPlayer) <$> getAPNAP
    modify \st -> st{magicPlayerOrderPriority = ps}
  runMagicCont (either id absurd) runPriorityQueue

runPriorityQueue :: Monad m => MagicCont 'Private 'RW m () Void
runPriorityQueue = do
  logCall 'runPriorityQueue do
    liftCont (fromRO $ gets magicPlayerOrderPriority) >>= \case
      [] -> magicCont resolveTopOfStack -- (117.4)
      oPlayer : oPlayers -> do
        liftCont performStateBasedActions -- (117.5)
        askCastSpell oPlayer -- (117.1a)
        askActivateAbility oPlayer -- (117.1b) (117.1d)
        askSpecialAction oPlayer -- (117.1c)
        liftCont $ modify \st -> st{magicPlayerOrderPriority = oPlayers} -- (117.3d)
  runPriorityQueue

getPlayerWithPriority :: Monad m => Magic 'Public 'RO m (Maybe (Object 'OTPlayer))
getPlayerWithPriority = logCall 'getPlayerWithPriority do
  oPlayers <- internalFromPrivate $ gets magicPlayerOrderPriority
  pure case oPlayers of
    oPlayer : _ -> Just oPlayer
    [] -> Nothing

getHasPriority :: Monad m => Object 'OTPlayer -> Magic 'Public 'RO m Bool
getHasPriority oPlayer = logCall 'getHasPriority do
  getPlayerWithPriority <&> \case
    Nothing -> False
    Just p -> oPlayer == p

-- (117.1c)
askSpecialAction :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askSpecialAction oPlayer = logCall 'askSpecialAction do
  st <- liftCont $ fromRO get
  case unStack $ magicStack st of
    [] -> case isMainPhase $ magicPhaseStep st of
      True -> do
        askPlayLand oPlayer
      False -> pure ()
    _ -> pure ()

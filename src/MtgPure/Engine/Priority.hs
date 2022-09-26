{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Priority (
  performSpecialAction,
  --
  gainPriorityImpl,
  getPlayerWithPriorityImpl,
  getHasPriorityImpl,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Trans.Except (throwE)
import safe Data.Functor ((<&>))
import safe qualified Data.Stream as Stream
import safe Data.Void (Void, absurd)
import safe MtgPure.Engine.Fwd.Wrap (
  askPlayLand,
  gainPriority,
  getAPNAP,
  getAlivePlayerCount,
  getPlayerWithPriority,
  playLand,
  resolveTopOfStack,
 )
import safe MtgPure.Engine.Legality (Legality)
import safe MtgPure.Engine.Monad (fromPublicRO, fromRO, gets, internalFromPrivate, modify, runMagicCont)
import safe MtgPure.Engine.Prompt (PlayLand (..), PlayerCount (..), SpecialAction (..))
import safe MtgPure.Engine.State (GameState (..), Magic, MagicCont)
import safe MtgPure.Model.Object (Object, ObjectType (..))

gainPriorityImpl :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
gainPriorityImpl oPlayer = do
  pure () -- (117.5) TODO: state-based actions
  PlayerCount n <- fromPublicRO getAlivePlayerCount
  ps <- fromRO $ Stream.take n . Stream.dropWhile (/= oPlayer) <$> getAPNAP
  modify $ \st -> st{magicPlayerOrderPriority = ps}
  runMagicCont (either id absurd) runPriorityQueue

runPriorityQueue :: Monad m => MagicCont 'Private 'RW m () Void
runPriorityQueue = do
  lift (fromRO $ gets magicPlayerOrderPriority) >>= \case
    [] -> throwE resolveTopOfStack -- (117.4)
    oPlayer : oPlayers -> do
      askCastSpell oPlayer -- (117.1a)
      askActivateAbility oPlayer -- (117.1b) (117.1d)
      askSpecialAction oPlayer -- (117.1c)
      lift $ modify $ \st -> st{magicPlayerOrderPriority = oPlayers} -- (117.3d)
      runPriorityQueue

getPlayerWithPriorityImpl :: Monad m => Magic 'Public 'RO m (Maybe (Object 'OTPlayer))
getPlayerWithPriorityImpl = do
  oPlayers <- internalFromPrivate $ gets magicPlayerOrderPriority
  pure $ case oPlayers of
    oPlayer : _ -> Just oPlayer
    [] -> Nothing

getHasPriorityImpl :: Monad m => Object 'OTPlayer -> Magic 'Public 'RO m Bool
getHasPriorityImpl oPlayer =
  getPlayerWithPriority <&> \case
    Nothing -> False
    Just p -> oPlayer == p

-- (117.1a)
askCastSpell :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askCastSpell oPlayer = do
  pure () -- TODO
  let spellIsCast = False
  pure () -- (305.9) TODO: dont forget this rule: lands + other types can never be cast
  case spellIsCast of
    True -> throwE $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1b) (117.1d)
askActivateAbility :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askActivateAbility oPlayer = do
  pure () -- TODO
  let abilityIsActivated = False
  case abilityIsActivated of
    True -> throwE $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1c)
askSpecialAction :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askSpecialAction oPlayer = do
  askPlayLand oPlayer

performSpecialAction :: Monad m => Object 'OTPlayer -> SpecialAction -> Magic 'Private 'RW m Legality
performSpecialAction oPlayer = \case
  SA_PlayLand (PlayLand oLand) -> playLand oPlayer $ PlayLand oLand

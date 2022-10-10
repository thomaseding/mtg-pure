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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Fwd.Wrap (
  allZOs,
  activateAbility,
  askPlayLand,
  caseOf,
  castSpell,
  enact,
  findPermanent,
  findPlayer,
  gainPriority,
  getActivePlayer,
  getAlivePlayerCount,
  getAPNAP,
  getHasPriority,
  getPermanent,
  getPermanents,
  getPlayer,
  getPlayers,
  getPlayerWithPriority,
  newObjectId,
  pay,
  performElections,
  playLand,
  resolveTopOfStack,
  rewindIllegal,
  satisfies,
  setPermanent,
  setPlayer,
  startGame,
  withEachControlledPermanent_,
  withEachPermanent,
  withEachPermanent_,
  withEachPlayer_,
  zosSatisfying,
) where

import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (AndLike)
import safe qualified Data.Stream as Stream
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd (Fwd' (..))
import safe MtgPure.Engine.Legality (Legality)
import safe MtgPure.Engine.Monad (fromRO, gets, internalFromPrivate)
import safe MtgPure.Engine.Prompt (
  ActivateAbility,
  CastSpell,
  PlayLand,
  PlayerCount (..),
 )
import safe MtgPure.Engine.State (Fwd, GameState (..), Magic, MagicCont)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object (OT0, Object, ObjectType (..))
import safe MtgPure.Model.ObjectId (ObjectId)
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Permanent (Permanent)
import safe MtgPure.Model.Player (Player)
import safe MtgPure.Model.Recursive (
  Case,
  Cost,
  Effect,
  Elect,
  Requirement,
 )
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO)

getFwd :: (Monad m, IsReadWrite rw) => Magic v rw m (Fwd m)
getFwd = internalFromPrivate $ fromRO $ gets magicFwd

fwd0 :: (IsReadWrite rw, Monad m) => (Fwd m -> Magic v rw m z) -> Magic v rw m z
fwd0 go = do
  fwd <- getFwd
  go fwd

fwd1 :: (IsReadWrite rw, Monad m) => (Fwd m -> (a -> Magic v rw m z)) -> a -> Magic v rw m z
fwd1 go a = do
  fwd <- getFwd
  go fwd a

fwd2 :: (IsReadWrite rw, Monad m) => (Fwd m -> (a -> b -> Magic v rw m z)) -> a -> b -> Magic v rw m z
fwd2 go a b = do
  fwd <- getFwd
  go fwd a b

fwd4 :: (IsReadWrite rw, Monad m) => (Fwd m -> (a -> b -> c -> d -> Magic v rw m z)) -> a -> b -> c -> d -> Magic v rw m z
fwd4 go a b c d = do
  fwd <- getFwd
  go fwd a b c d

activateAbility :: forall m. Monad m => Object 'OTPlayer -> ActivateAbility -> Magic 'Private 'RW m Legality
activateAbility = fwd2 fwd_activateAbility

askPlayLand :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askPlayLand p = do
  fwd <- lift getFwd
  fwd_askPlayLand fwd p

allZOs :: (Monad m, IsZO zone ot) => Magic 'Private 'RO m [ZO zone ot]
allZOs = fwd0 fwd_allZOs

caseOf :: Monad m => (x -> Magic 'Private 'RW m a) -> Case x -> Magic 'Private 'RW m a
caseOf = fwd2 fwd_caseOf

castSpell :: forall m. Monad m => Object 'OTPlayer -> CastSpell -> Magic 'Private 'RW m Legality
castSpell = fwd2 fwd_castSpell

enact :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m ()
enact = fwd1 fwd_enact

findPermanent :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m (Maybe Permanent)
findPermanent = fwd1 fwd_findPermanent

findPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m (Maybe Player)
findPlayer = fwd1 fwd_findPlayer

gainPriority :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
gainPriority = fwd1 fwd_gainPriority

getActivePlayer :: Monad m => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayer = fwd0 fwd_getActivePlayer

getAlivePlayerCount :: Monad m => Magic 'Public 'RO m PlayerCount
getAlivePlayerCount = fwd0 fwd_getAlivePlayerCount

getAPNAP :: Monad m => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAP = fwd0 fwd_getAPNAP

getHasPriority :: Monad m => Object 'OTPlayer -> Magic 'Public 'RO m Bool
getHasPriority = fwd1 fwd_getHasPriority

getPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m Player
getPlayer = fwd1 fwd_getPlayer

getPlayers :: Monad m => Magic 'Public 'RO m [Object 'OTPlayer]
getPlayers = fwd0 fwd_getPlayers

getPlayerWithPriority :: Monad m => Magic 'Public 'RO m (Maybe (Object 'OTPlayer))
getPlayerWithPriority = fwd0 fwd_getPlayerWithPriority

newObjectId :: Monad m => Magic 'Private 'RW m ObjectId
newObjectId = fwd0 fwd_newObjectId

pay :: Monad m => Object 'OTPlayer -> Cost ot -> Magic 'Private 'RW m Legality
pay = fwd2 fwd_pay

performElections ::
  forall ot m p el x.
  (Monad m, AndLike (Maybe x)) =>
  ([Magic 'Private 'RW m (Maybe x)] -> Magic 'Private 'RW m (Maybe x)) ->
  ZO 'ZStack OT0 ->
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect p el ot ->
  Magic 'Private 'RW m (Maybe x)
performElections = fwd4 fwd_performElections

playLand :: Monad m => Object 'OTPlayer -> PlayLand -> Magic 'Private 'RW m Legality
playLand = fwd2 fwd_playLand

resolveTopOfStack :: Monad m => Magic 'Private 'RW m ()
resolveTopOfStack = fwd0 fwd_resolveTopOfStack

rewindIllegal :: Monad m => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegal = fwd1 fwd_rewindIllegal

satisfies :: (Monad m, IsZO zone ot) => ZO zone ot -> Requirement zone ot -> Magic 'Private 'RO m Bool
satisfies = fwd2 fwd_satisfies

getPermanent :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m Permanent
getPermanent = fwd1 fwd_getPermanent

getPermanents :: Monad m => Magic 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
getPermanents = fwd0 fwd_getPermanents

setPermanent :: Monad m => ZO 'ZBattlefield OTPermanent -> Permanent -> Magic 'Private 'RW m ()
setPermanent = fwd2 fwd_setPermanent

setPlayer :: Monad m => Object 'OTPlayer -> Player -> Magic 'Private 'RW m ()
setPlayer = fwd2 fwd_setPlayer

startGame :: Monad m => Magic 'Private 'RW m Void
startGame = fwd0 fwd_startGame

withEachControlledPermanent_ ::
  (IsReadWrite rw, Monad m) =>
  Object 'OTPlayer ->
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachControlledPermanent_ = fwd2 fwd_withEachControlledPermanent_

withEachPermanent ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m a) ->
  Magic v rw m [a]
withEachPermanent = fwd1 fwd_withEachPermanent

withEachPermanent_ ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachPermanent_ = fwd1 fwd_withEachPermanent_

withEachPlayer_ ::
  (IsReadWrite rw, Monad m) =>
  (Object 'OTPlayer -> Magic v rw m ()) ->
  Magic v rw m ()
withEachPlayer_ = fwd1 fwd_withEachPlayer_

zosSatisfying :: (Monad m, IsZO zone ot) => Requirement zone ot -> Magic 'Private 'RO m [ZO zone ot]
zosSatisfying = fwd1 fwd_zosSatisfying

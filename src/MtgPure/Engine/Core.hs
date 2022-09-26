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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Core (
  allZOsImpl,
  findPermanentImpl,
  findPlayerImpl,
  getActivePlayerImpl,
  getAlivePlayerCountImpl,
  getAPNAPImpl,
  getPermanentImpl,
  getPermanentsImpl,
  getPlayerImpl,
  getPlayersImpl,
  newObjectIdImpl,
  rewindIllegalImpl,
  setPermanentImpl,
  setPlayerImpl,
  withEachControlledPermanentImpl_,
  withEachPermanentImpl_,
  withEachPermanentImpl,
  withEachPlayerImpl_,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe qualified Data.DList as DList
import safe Data.Functor ((<&>))
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes, mapMaybe)
import safe qualified Data.Stream as Stream
import safe MtgPure.Engine.Fwd.Wrap (
  findPlayer,
  getAPNAP,
  getPermanent,
  getPermanents,
  getPlayers,
  withEachPermanent,
  withEachPermanent_,
 )
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromPublicRO,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  magicCatch,
  magicThrow,
  modify,
  put,
 )
import safe MtgPure.Engine.Prompt (
  InternalLogicError (..),
  PlayerCount,
 )
import safe MtgPure.Engine.State (
  GameResult (..),
  GameState (..),
  Magic,
 )
import safe MtgPure.Model.Object (IsObjectType (..), Object, ObjectType (..))
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (..))
import safe MtgPure.Model.ObjectType.Index (IndexOT (..))
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO)
import safe MtgPure.Model.ZoneObject.Convert (castOToZO, toZO0, zo0ToPermanent)

getAlivePlayerCountImpl :: Monad m => Magic 'Public 'RO m PlayerCount
getAlivePlayerCountImpl = undefined

getAPNAPImpl :: Monad m => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAPImpl = internalFromPrivate $ gets magicPlayerOrderAPNAP

getActivePlayerImpl :: Monad m => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayerImpl = Stream.head <$> getAPNAP

getPlayersImpl :: Monad m => Magic 'Public 'RO m [Object 'OTPlayer]
getPlayersImpl = do
  st <- internalFromPrivate get
  let ps = Map.assocs $ magicPlayers st
  pure $ map fst $ filter (not . playerLost . snd) ps

withEachPlayerImpl_ :: (IsReadWrite rw, Monad m) => (Object 'OTPlayer -> Magic v rw m ()) -> Magic v rw m ()
withEachPlayerImpl_ f = fromPublicRO getPlayers >>= mapM_ f

getPermanentsImpl :: Monad m => Magic 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
getPermanentsImpl = map zo0ToPermanent <$> perms0
 where
  perms0 = internalFromPrivate $ gets $ Map.keys . magicPermanents

withEachPermanentImpl ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m a) ->
  Magic v rw m [a]
withEachPermanentImpl f = fromPublicRO getPermanents >>= mapM f

withEachPermanentImpl_ ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachPermanentImpl_ f = fromPublicRO getPermanents >>= mapM_ f

withEachControlledPermanentImpl_ ::
  (IsReadWrite rw, Monad m) =>
  Object 'OTPlayer ->
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachControlledPermanentImpl_ oPlayer f = withEachPermanent_ $ \oPerm -> do
  perm <- internalFromPrivate $ fromRO $ getPermanent oPerm
  M.when (permanentController perm == oPlayer) $ f oPerm

rewindIllegalImpl :: Monad m => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegalImpl m = do
  -- (104.1) (727.1) XXX: Is it possible for GameResult to be thrown during an illegal action?
  -- If so, is should it sometimes/always/never be rewound?
  let m' = magicCatch m $ \case
        GameResult{gameWinners = []} -> pure Illegal
        ex -> magicThrow ex
  st <- fromRO get
  m' >>= \case
    Legal -> pure True
    Illegal -> put st >> pure False

allZOsImpl :: forall m zone ot. (Monad m, IsZO zone ot) => Magic 'Private 'RO m [ZO zone ot]
allZOsImpl = case singZone @zone of
  SZBattlefield -> case indexOT @ot of
    objectTypes ->
      let goPerms :: ObjectType -> Magic 'Private 'RO m [ZO zone ot]
          goPerms ot = fmap catMaybes $
            withEachPermanent $ \oPerm -> do
              perm <- getPermanent oPerm
              let goPerm ::
                    forall a x.
                    IsObjectType a =>
                    (Permanent -> Maybe x) ->
                    Maybe (ZO 'ZBattlefield ot)
                  goPerm viewPerm = case viewPerm perm of
                    Nothing -> Nothing
                    Just{} -> castOToZO $ idToObject @a $ getObjectId oPerm
              pure $ case ot of
                OTArtifact -> goPerm @ 'OTArtifact permanentArtifact
                OTCreature -> goPerm @ 'OTCreature permanentCreature
                OTEnchantment -> goPerm @ 'OTEnchantment undefined
                OTLand -> goPerm @ 'OTLand permanentLand
                OTPlaneswalker -> goPerm @ 'OTPlaneswalker undefined
                _ -> Nothing
          goPlayers :: ObjectType -> Magic 'Private 'RO m [ZO zone ot]
          goPlayers = \case
            OTPlayer -> mapMaybe castOToZO <$> fromPublic getPlayers
            _ -> pure []
          goRec :: [ObjectType] -> Magic 'Private 'RO m (DList.DList (ZO zone ot))
          goRec = \case
            [] -> pure DList.empty
            ot : ots -> do
              oPerms <- goPerms ot
              oPlayers <- goPlayers ot
              oRecs <- goRec ots
              pure $ DList.fromList oPerms <> DList.fromList oPlayers <> oRecs
       in DList.toList <$> goRec objectTypes
  _ -> undefined

newObjectIdImpl :: Monad m => Magic 'Private 'RW m ObjectId
newObjectIdImpl = do
  ObjectId i <- fromRO $ gets magicNextObjectId
  modify $ \st -> st{magicNextObjectId = ObjectId $ i + 1}
  pure $ ObjectId i

findPermanentImpl :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m (Maybe Permanent)
findPermanentImpl oPerm = gets $ Map.lookup (toZO0 oPerm) . magicPermanents

getPermanentImpl :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m Permanent
getPermanentImpl oPerm =
  findPermanentImpl oPerm <&> \case
    Nothing -> error $ show $ InvalidPermanent oPerm
    Just perm -> perm

setPermanentImpl :: Monad m => ZO 'ZBattlefield OTPermanent -> Permanent -> Magic 'Private 'RW m ()
setPermanentImpl oPerm perm = modify $ \st ->
  let permMap = magicPermanents st
      permMap' = Map.insert (toZO0 oPerm) perm permMap
   in st{magicPermanents = permMap'}

findPlayerImpl :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m (Maybe Player)
findPlayerImpl oPlayer = gets $ Map.lookup oPlayer . magicPlayers

getPlayerImpl :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m Player
getPlayerImpl oPlayer =
  findPlayer oPlayer <&> \case
    Nothing -> error $ show $ InvalidPlayer oPlayer
    Just player -> player

setPlayerImpl :: Monad m => Object 'OTPlayer -> Player -> Magic 'Private 'RW m ()
setPlayerImpl oPlayer player = modify $ \st ->
  let playerMap = magicPlayers st
      playerMap' = Map.insertWith (\_ _ -> player) oPlayer fatal playerMap
      fatal = error $ show $ InvalidPlayer oPlayer
   in st{magicPlayers = playerMap'}

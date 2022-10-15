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
{-# LANGUAGE TemplateHaskellQuotes #-}
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
  findHandCardImpl,
  findLibraryCardImpl,
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
  pushHandCardImpl,
  pushLibraryCardImpl,
  removeHandCardImpl,
  removeLibraryCardImpl,
  rewindIllegalImpl,
  setPermanentImpl,
  setPlayerImpl,
  withEachControlledPermanentImpl_,
  withEachPermanentImpl_,
  withEachPermanentImpl,
  withEachPlayerImpl_,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe qualified Data.DList as DList
import safe Data.Functor ((<&>))
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes, isJust, mapMaybe)
import safe qualified Data.Stream as Stream
import safe MtgPure.Engine.Fwd.Wrap (
  findPlayer,
  getAPNAP,
  getPermanent,
  getPermanents,
  getPlayer,
  getPlayers,
  logCall,
  newObjectId,
  setPlayer,
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
  PlayerCount (..),
  ShowZO (..),
 )
import safe MtgPure.Engine.State (
  GameResult (..),
  GameState (..),
  Magic,
 )
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.IsCardList (pushCard)
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Object (IsObjectType (..), Object, ObjectType (..))
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (..))
import safe MtgPure.Model.ObjectType.Index (IndexOT (..))
import safe MtgPure.Model.ObjectType.Kind (OTCard, OTPermanent)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO)
import safe MtgPure.Model.ZoneObject.Convert (castOToZO, toZO0, zo0ToCard, zo0ToPermanent)

getAlivePlayerCountImpl :: Monad m => Magic 'Public 'RO m PlayerCount
getAlivePlayerCountImpl = logCall 'getAlivePlayerCountImpl $ PlayerCount . length <$> getPlayersImpl

getAPNAPImpl :: Monad m => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAPImpl = logCall 'getAPNAPImpl $ internalFromPrivate $ gets magicPlayerOrderAPNAP

getActivePlayerImpl :: Monad m => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayerImpl = logCall 'getActivePlayerImpl $ Stream.head <$> getAPNAP

getPlayersImpl :: Monad m => Magic 'Public 'RO m [Object 'OTPlayer]
getPlayersImpl = logCall 'getPlayersImpl $ do
  st <- internalFromPrivate get
  let ps = Map.assocs $ magicPlayers st
  pure $ map fst $ filter (not . playerLost . snd) ps

withEachPlayerImpl_ :: (IsReadWrite rw, Monad m) => (Object 'OTPlayer -> Magic v rw m ()) -> Magic v rw m ()
withEachPlayerImpl_ f = logCall 'withEachPlayerImpl_ $ fromPublicRO getPlayers >>= mapM_ f

getPermanentsImpl :: Monad m => Magic 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
getPermanentsImpl = logCall 'getPermanentsImpl $ map zo0ToPermanent <$> perms0
 where
  perms0 = internalFromPrivate $ gets $ Map.keys . magicPermanents

withEachPermanentImpl ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m a) ->
  Magic v rw m [a]
withEachPermanentImpl f = logCall 'withEachPermanentImpl $ fromPublicRO getPermanents >>= mapM f

withEachPermanentImpl_ ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachPermanentImpl_ f = logCall 'withEachPermanentImpl_ $ fromPublicRO getPermanents >>= mapM_ f

withEachControlledPermanentImpl_ ::
  (IsReadWrite rw, Monad m) =>
  Object 'OTPlayer ->
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachControlledPermanentImpl_ oPlayer f = logCall 'withEachControlledPermanentImpl_ $
  withEachPermanent_ $ \oPerm -> do
    perm <- internalFromPrivate $ fromRO $ getPermanent oPerm
    M.when (permanentController perm == oPlayer) $ f oPerm

rewindIllegalImpl :: Monad m => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegalImpl m = logCall 'rewindIllegalImpl $ do
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
allZOsImpl = logCall 'allZOsImpl $ case singZone @zone of
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
newObjectIdImpl = logCall 'newObjectIdImpl $ do
  ObjectId i <- fromRO $ gets magicNextObjectId
  modify $ \st -> st{magicNextObjectId = ObjectId $ i + 1}
  pure $ ObjectId i

pushLibraryCardImpl :: Monad m => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZLibrary OTCard)
pushLibraryCardImpl oPlayer card = logCall 'pushLibraryCardImpl $ do
  player <- fromRO $ getPlayer oPlayer
  i <- newObjectId
  let zo0 = toZO0 i
      zoCard = zo0ToCard zo0
  modify $ \st -> st{magicLibraryCards = Map.insert zo0 card $ magicLibraryCards st}
  setPlayer oPlayer player{playerLibrary = pushCard zoCard $ playerLibrary player}
  pure zoCard

pushHandCardImpl :: Monad m => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZHand OTCard)
pushHandCardImpl oPlayer card = logCall 'pushHandCardImpl $ do
  player <- fromRO $ getPlayer oPlayer
  i <- newObjectId
  let zo0 = toZO0 i
      zoCard = zo0ToCard zo0
  modify $ \st -> st{magicHandCards = Map.insert zo0 card $ magicHandCards st}
  setPlayer oPlayer player{playerHand = pushCard zoCard $ playerHand player}
  pure zoCard

findHandCardImpl :: Monad m => Object 'OTPlayer -> ZO 'ZHand OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
findHandCardImpl oPlayer zoCard = logCall 'findHandCardImpl $ do
  fromRO (gets (Map.lookup (toZO0 zoCard) . magicHandCards)) >>= \case
    Nothing -> pure Nothing
    Just card -> do
      player <- fromRO $ getPlayer oPlayer
      pure $ case zoCard `elem` unHand (playerHand player) of
        False -> Nothing
        True -> Just card

findLibraryCardImpl :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
findLibraryCardImpl oPlayer zoCard = logCall 'findLibraryCardImpl $ do
  fromRO (gets (Map.lookup (toZO0 zoCard) . magicLibraryCards)) >>= \case
    Nothing -> pure Nothing
    Just card -> do
      player <- fromRO $ getPlayer oPlayer
      pure $ case zoCard `elem` unLibrary (playerLibrary player) of
        False -> Nothing
        True -> Just card

removeHandCardImpl :: Monad m => Object 'OTPlayer -> ZO 'ZHand OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeHandCardImpl oPlayer zoCard = logCall 'removeHandCardImpl $ do
  findHandCardImpl oPlayer zoCard >>= \case
    Nothing -> pure Nothing
    Just{} -> do
      mCard <- fromRO $ gets $ Map.lookup (toZO0 zoCard) . magicHandCards
      modify $ \st -> st{magicHandCards = Map.delete (toZO0 zoCard) $ magicHandCards st}
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer player{playerHand = Hand $ List.delete zoCard $ unHand (playerHand player)}
      pure $ assert (isJust mCard) mCard

removeLibraryCardImpl :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeLibraryCardImpl oPlayer zoCard = logCall 'removeLibraryCardImpl $ do
  findLibraryCardImpl oPlayer zoCard >>= \case
    Nothing -> pure Nothing
    Just{} -> do
      mCard <- fromRO $ gets $ Map.lookup (toZO0 zoCard) . magicLibraryCards
      modify $ \st -> st{magicLibraryCards = Map.delete (toZO0 zoCard) $ magicLibraryCards st}
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer player{playerLibrary = Library $ List.delete zoCard $ unLibrary (playerLibrary player)}
      pure $ assert (isJust mCard) mCard

findPermanentImpl :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m (Maybe Permanent)
findPermanentImpl oPerm = logCall 'findPermanentImpl $ gets $ Map.lookup (toZO0 oPerm) . magicPermanents

getPermanentImpl :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m Permanent
getPermanentImpl oPerm = logCall 'getPermanentImpl $ do
  findPermanentImpl oPerm <&> \case
    Nothing -> error $ show $ InvalidPermanent $ ShowZO oPerm
    Just perm -> perm

setPermanentImpl :: Monad m => ZO 'ZBattlefield OTPermanent -> Maybe Permanent -> Magic 'Private 'RW m ()
setPermanentImpl oPerm mPerm = logCall 'setPermanentImpl $
  modify $ \st ->
    let permMap = magicPermanents st
        permMap' = case mPerm of
          Just perm -> Map.insert (toZO0 oPerm) perm permMap
          Nothing -> Map.delete (toZO0 oPerm) permMap
     in st{magicPermanents = permMap'}

findPlayerImpl :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m (Maybe Player)
findPlayerImpl oPlayer = logCall 'findPlayerImpl $ gets $ Map.lookup oPlayer . magicPlayers

getPlayerImpl :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m Player
getPlayerImpl oPlayer = logCall 'getPlayerImpl $ do
  findPlayer oPlayer <&> \case
    Nothing -> error $ show $ InvalidPlayer oPlayer
    Just player -> player

setPlayerImpl :: Monad m => Object 'OTPlayer -> Player -> Magic 'Private 'RW m ()
setPlayerImpl oPlayer player = logCall 'setPlayerImpl $
  modify $ \st ->
    let playerMap = magicPlayers st
        playerMap' = Map.insertWith (\_ _ -> player) oPlayer fatal playerMap
        fatal = error $ show $ InvalidPlayer oPlayer
     in st{magicPlayers = playerMap'}

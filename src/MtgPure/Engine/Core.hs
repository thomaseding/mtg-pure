{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
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
  allZOs,
  findHandCard,
  findLibraryCard,
  findPermanent,
  findPlayer,
  getActivatedAbilities,
  getActivePlayer,
  getAlivePlayerCount,
  getAllActivatedAbilities,
  getAPNAP,
  getPermanent,
  getPermanents,
  getPlayer,
  getPlayers,
  newObjectId,
  pushHandCard,
  pushLibraryCard,
  removeHandCard,
  removeLibraryCard,
  rewindIllegal,
  setPermanent,
  setPlayer,
  withEachControlledPermanent_,
  withEachPermanent_,
  withEachPermanent,
  withEachPlayer_,
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
import safe qualified Data.Traversable as T
import safe Data.Typeable (cast)
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
  SomeActivatedAbility (..),
 )
import safe MtgPure.Engine.State (
  GameResult (..),
  GameState (..),
  Magic,
  logCall,
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
import safe MtgPure.Model.Recursive (
  Ability (..),
  AnyCard (..),
  WithThisActivated,
  fromSome,
 )
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO)
import safe MtgPure.Model.ZoneObject.Convert (
  castOToZO,
  toZO0,
  zo0ToCard,
  zo0ToPermanent,
 )

getAlivePlayerCount :: Monad m => Magic 'Public 'RO m PlayerCount
getAlivePlayerCount = logCall 'getAlivePlayerCount $ PlayerCount . length <$> getPlayers

getAPNAP :: Monad m => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAP = logCall 'getAPNAP $ internalFromPrivate $ gets magicPlayerOrderAPNAP

getActivePlayer :: Monad m => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayer = logCall 'getActivePlayer $ Stream.head <$> getAPNAP

getPlayers :: Monad m => Magic 'Public 'RO m [Object 'OTPlayer]
getPlayers = logCall 'getPlayers do
  st <- internalFromPrivate get
  let ps = Map.assocs $ magicPlayers st
  pure $ map fst $ filter (not . playerLost . snd) ps

withEachPlayer_ :: (IsReadWrite rw, Monad m) => (Object 'OTPlayer -> Magic v rw m ()) -> Magic v rw m ()
withEachPlayer_ f = logCall 'withEachPlayer_ $ fromPublicRO getPlayers >>= mapM_ f

getPermanents :: Monad m => Magic 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
getPermanents = logCall 'getPermanents $ map zo0ToPermanent <$> perms0
 where
  perms0 = internalFromPrivate $ gets $ Map.keys . magicPermanents

withEachPermanent ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m a) ->
  Magic v rw m [a]
withEachPermanent f = logCall 'withEachPermanent $ fromPublicRO getPermanents >>= mapM f

withEachPermanent_ ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachPermanent_ f = logCall 'withEachPermanent_ $ fromPublicRO getPermanents >>= mapM_ f

withEachControlledPermanent_ ::
  (IsReadWrite rw, Monad m) =>
  Object 'OTPlayer ->
  (ZO 'ZBattlefield OTPermanent -> Magic v rw m ()) ->
  Magic v rw m ()
withEachControlledPermanent_ oPlayer f = logCall 'withEachControlledPermanent_ $
  withEachPermanent_ $ \oPerm -> do
    perm <- internalFromPrivate $ fromRO $ getPermanent oPerm
    M.when (permanentController perm == oPlayer) $ f oPerm

rewindIllegal :: Monad m => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegal m = logCall 'rewindIllegal $ do
  -- (104.1) (727.1) XXX: Is it possible for GameResult to be thrown during an illegal action?
  -- If so, is should it sometimes/always/never be rewound?
  let m' = magicCatch m \case
        GameResult{gameWinners = []} -> pure Illegal
        ex -> magicThrow ex
  st <- fromRO get
  m' >>= \case
    Legal -> pure True
    Illegal -> put st >> pure False

allZOs :: forall zone ot m. (Monad m, IsZO zone ot) => Magic 'Private 'RO m [ZO zone ot]
allZOs = logCall 'allZOs case singZone @zone of
  SZBattlefield -> case indexOT @ot of
    objectTypes ->
      let goPerms :: ObjectType -> Magic 'Private 'RO m [ZO zone ot]
          goPerms ot =
            catMaybes
              <$> withEachPermanent \oPerm -> do
                perm <- getPermanent oPerm
                let goPerm ::
                      forall a x.
                      IsObjectType a =>
                      (Permanent -> Maybe x) ->
                      Maybe (ZO 'ZBattlefield ot)
                    goPerm viewPerm = case viewPerm perm of
                      Nothing -> Nothing
                      Just{} -> castOToZO $ idToObject @a $ getObjectId oPerm
                pure case ot of
                  OTArtifact -> goPerm @ 'OTArtifact permanentArtifact
                  OTCreature -> goPerm @ 'OTCreature permanentCreature
                  --OTEnchantment -> goPerm @ 'OTEnchantment undefined
                  OTLand -> goPerm @ 'OTLand permanentLand
                  --OTPlaneswalker -> goPerm @ 'OTPlaneswalker undefined
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

getAllActivatedAbilities ::
  forall zone ot m.
  (IsZO zone ot, Monad m) =>
  Magic 'Private 'RO m [SomeActivatedAbility zone ot]
getAllActivatedAbilities = logCall 'getAllActivatedAbilities do
  zos <- allZOs @zone @ot
  concat <$> T.for zos getActivatedAbilities

getActivatedAbilities ::
  forall zone ot m.
  (IsZO zone ot, Monad m) =>
  ZO zone ot ->
  Magic 'Private 'RO m [SomeActivatedAbility zone ot]
getActivatedAbilities zo = logCall 'getActivatedAbilities do
  -- XXX: Being lazy at the moment and assuming it's a permanent
  case singZone @zone of
    SZBattlefield -> do
      let zoPerm = zo0ToPermanent $ toZO0 zo
      findPermanent zoPerm <&> \case
        Nothing -> []
        Just perm -> catMaybes $ flip map (permanentAbilities perm) \ability ->
          fromSome ability \case
            Activated withThis ->
              let go ::
                    forall zone' ot'.
                    IsZO zone' ot' =>
                    WithThisActivated zone' ot' ->
                    Maybe (SomeActivatedAbility zone ot)
                  go withThis' = case cast withThis' of
                    Nothing -> Nothing
                    Just (withThis'' :: WithThisActivated zone ot') ->
                      Just
                        SomeActivatedAbility
                          { someActivatedZO = zo
                          , someActivatedAbility = withThis''
                          }
               in go withThis
            _ -> Nothing
    _ -> undefined

newObjectId :: Monad m => Magic 'Private 'RW m ObjectId
newObjectId = logCall 'newObjectId do
  ObjectId i <- fromRO $ gets magicNextObjectId
  modify \st -> st{magicNextObjectId = ObjectId $ i + 1}
  pure $ ObjectId i

pushLibraryCard :: Monad m => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZLibrary OTCard)
pushLibraryCard oPlayer card = logCall 'pushLibraryCard do
  player <- fromRO $ getPlayer oPlayer
  i <- newObjectId
  let zo0 = toZO0 i
      zoCard = zo0ToCard zo0
  modify \st -> st{magicLibraryCards = Map.insert zo0 card $ magicLibraryCards st}
  setPlayer oPlayer player{playerLibrary = pushCard zoCard $ playerLibrary player}
  pure zoCard

pushHandCard :: Monad m => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZHand OTCard)
pushHandCard oPlayer card = logCall 'pushHandCard do
  player <- fromRO $ getPlayer oPlayer
  i <- newObjectId
  let zo0 = toZO0 i
      zoCard = zo0ToCard zo0
  modify \st -> st{magicHandCards = Map.insert zo0 card $ magicHandCards st}
  setPlayer oPlayer player{playerHand = pushCard zoCard $ playerHand player}
  pure zoCard

findHandCard :: Monad m => Object 'OTPlayer -> ZO 'ZHand OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
findHandCard oPlayer zoCard = logCall 'findHandCard do
  fromRO (gets $ Map.lookup (toZO0 zoCard) . magicHandCards) >>= \case
    Nothing -> pure Nothing
    Just card -> do
      player <- fromRO $ getPlayer oPlayer
      pure case zoCard `elem` unHand (playerHand player) of
        False -> Nothing
        True -> Just card

findLibraryCard :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
findLibraryCard oPlayer zoCard = logCall 'findLibraryCard do
  fromRO (gets $ Map.lookup (toZO0 zoCard) . magicLibraryCards) >>= \case
    Nothing -> pure Nothing
    Just card -> do
      player <- fromRO $ getPlayer oPlayer
      pure case zoCard `elem` unLibrary (playerLibrary player) of
        False -> Nothing
        True -> Just card

removeHandCard :: Monad m => Object 'OTPlayer -> ZO 'ZHand OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeHandCard oPlayer zoCard = logCall 'removeHandCard do
  findHandCard oPlayer zoCard >>= \case
    Nothing -> pure Nothing
    Just{} -> do
      mCard <- fromRO $ gets $ Map.lookup (toZO0 zoCard) . magicHandCards
      modify \st -> st{magicHandCards = Map.delete (toZO0 zoCard) $ magicHandCards st}
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer player{playerHand = Hand $ List.delete zoCard $ unHand $ playerHand player}
      pure $ assert (isJust mCard) mCard

removeLibraryCard :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeLibraryCard oPlayer zoCard = logCall 'removeLibraryCard do
  findLibraryCard oPlayer zoCard >>= \case
    Nothing -> pure Nothing
    Just{} -> do
      mCard <- fromRO $ gets $ Map.lookup (toZO0 zoCard) . magicLibraryCards
      modify \st -> st{magicLibraryCards = Map.delete (toZO0 zoCard) $ magicLibraryCards st}
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer player{playerLibrary = Library $ List.delete zoCard $ unLibrary (playerLibrary player)}
      pure $ assert (isJust mCard) mCard

findPermanent :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m (Maybe Permanent)
findPermanent zoPerm = logCall 'findPermanent $ gets $ Map.lookup (toZO0 zoPerm) . magicPermanents

getPermanent :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RO m Permanent
getPermanent zoPerm = logCall 'getPermanent $ do
  findPermanent zoPerm <&> \case
    Nothing -> error $ show $ InvalidPermanent $ ShowZO zoPerm
    Just perm -> perm

setPermanent :: Monad m => ZO 'ZBattlefield OTPermanent -> Maybe Permanent -> Magic 'Private 'RW m ()
setPermanent zoPerm mPerm = logCall 'setPermanent do
  modify \st ->
    let permMap = magicPermanents st
        permMap' = case mPerm of
          Just perm -> Map.insert (toZO0 zoPerm) perm permMap
          Nothing -> Map.delete (toZO0 zoPerm) permMap
     in st{magicPermanents = permMap'}

findPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m (Maybe Player)
findPlayer oPlayer = logCall 'findPlayer $ gets $ Map.lookup oPlayer . magicPlayers

getPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m Player
getPlayer oPlayer = logCall 'getPlayer do
  findPlayer oPlayer <&> \case
    Nothing -> error $ show $ InvalidPlayer oPlayer
    Just player -> player

setPlayer :: Monad m => Object 'OTPlayer -> Player -> Magic 'Private 'RW m ()
setPlayer oPlayer player = logCall 'setPlayer do
  modify \st ->
    let playerMap = magicPlayers st
        playerMap' = Map.insertWith (\_ _ -> player) oPlayer fatal playerMap
        fatal = error $ show $ InvalidPlayer oPlayer
     in st{magicPlayers = playerMap'}

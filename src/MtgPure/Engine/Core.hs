{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Core (
  activatedToIndex,
  allControlledPermanentsOf,
  allPermanents,
  allZOActivatedAbilities,
  allZOs,
  doesObjectIdExist,
  doesObjectNExist,
  doesZoneObjectExist,
  findHandCard,
  findLibraryCard,
  findPermanent,
  findPlayer,
  getActivatedAbilitiesOf,
  getActivePlayer,
  getAlivePlayers,
  getAlivePlayerCount,
  getAPNAP,
  getBasicLandTypes,
  getPermanent,
  getPlayer,
  indexToActivated,
  newObjectId,
  pushHandCard,
  pushLibraryCard,
  removeHandCard,
  removeLibraryCard,
  rewindIllegal,
  setPermanent,
  setPlayer,
  toZO,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Data.DList as DList
import safe Data.Functor ((<&>))
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes, isJust, mapMaybe)
import safe qualified Data.Stream as Stream
import safe qualified Data.Traversable as T
import safe Data.Typeable (cast)
import safe MtgPure.Engine.Fwd.Api (eachLogged)
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (
  fromPublic,
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
  AbsoluteActivatedAbilityIndex (AbsoluteActivatedAbilityIndex),
  InternalLogicError (..),
  PlayerCount (..),
  RelativeAbilityIndex (RelativeAbilityIndex),
  SomeActivatedAbility (..),
 )
import safe MtgPure.Engine.State (
  GameResult (..),
  GameState (..),
  Magic,
  logCall,
 )
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.IsCardList (pushCard)
import safe MtgPure.Model.Land (Land (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Object.IndexOT (IndexOT (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.OTNAliases (OTCard, OTPermanent)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (..),
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (ObjectN (O0))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN (toObject1, toObjectNAny)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  AnyCard (..),
  Card (..),
  CardFacet (..),
  WithThisActivated,
  YourCardFacet (..),
  fromSome,
 )
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (
  castOToON,
  oToZO1,
  toZO0,
  zo0ToCard,
  zo0ToPermanent,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOT, IsZO, ZO, ZoneObject (ZO), toZone)

getAlivePlayerCount :: Monad m => Magic 'Public 'RO m PlayerCount
getAlivePlayerCount = logCall 'getAlivePlayerCount $ PlayerCount . length <$> getAlivePlayers

getAPNAP :: Monad m => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAP = logCall 'getAPNAP $ internalFromPrivate $ gets magicPlayerOrderAPNAP

getActivePlayer :: Monad m => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayer = logCall 'getActivePlayer $ Stream.head <$> getAPNAP

getAlivePlayers :: Monad m => Magic 'Public 'RO m [Object 'OTPlayer]
getAlivePlayers = logCall 'getAlivePlayers do
  st <- internalFromPrivate get
  let ps = Map.assocs $ magicPlayers st
  pure $ map fst $ filter (not . playerLost . snd) ps

allPermanents :: Monad m => Magic 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
allPermanents = logCall 'allPermanents $ map zo0ToPermanent <$> perms0
 where
  perms0 = internalFromPrivate $ gets $ Map.keys . magicPermanents

allControlledPermanentsOf ::
  Monad m =>
  Object 'OTPlayer ->
  Magic 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
allControlledPermanentsOf oPlayer =
  logCall 'allControlledPermanentsOf $
    allPermanents >>= M.filterM \zoPerm -> do
      perm <- internalFromPrivate $ fromRO $ getPermanent zoPerm
      pure $ permanentController perm == oPlayer

rewindIllegal :: Monad m => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegal m = logCall 'rewindIllegal do
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
          goPerms ot = do
            perms <- fromPublic allPermanents
            catMaybes <$> eachLogged perms \oPerm -> do
              perm <- getPermanent oPerm
              let goPerm ::
                    forall a x.
                    IsObjectType a =>
                    (Permanent -> Maybe x) ->
                    Maybe (ZO 'ZBattlefield ot)
                  goPerm viewPerm = case viewPerm perm of
                    Nothing -> Nothing
                    Just{} -> castOToZO $ idToObject @a $ UntypedObject DefaultObjectDiscriminant $ getObjectId oPerm
              pure case ot of
                OTArtifact -> goPerm @ 'OTArtifact permanentArtifact
                OTCreature -> goPerm @ 'OTCreature permanentCreature
                --OTEnchantment -> goPerm @ 'OTEnchantment undefined
                OTLand -> goPerm @ 'OTLand permanentLand
                --OTPlaneswalker -> goPerm @ 'OTPlaneswalker undefined
                _ -> Nothing
          goPlayers :: ObjectType -> Magic 'Private 'RO m [ZO zone ot]
          goPlayers = \case
            OTPlayer -> mapMaybe castOToZO <$> fromPublic getAlivePlayers
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
 where
  castOToZO :: IsObjectType z => Object z -> Maybe (ZO zone ot)
  castOToZO = fmap toZone . castOToON

toZO :: (IsZO zone ot, Monad m) => ObjectId -> Magic 'Private 'RO m (Maybe (ZO zone ot))
toZO i = logCall 'toZO do
  -- TODO: Eventually optimize to avoid this slow implementation.
  -- Don't want to build up allZOs and don't want linear scan.
  -- Probably will end up implementing `allZOs` in terms of this.
  allZOs <&> List.find \zo -> i == getObjectId zo

allZOActivatedAbilities ::
  forall zone ot m.
  (IsZO zone ot, Monad m) =>
  Magic 'Private 'RO m [SomeActivatedAbility zone ot]
allZOActivatedAbilities = logCall 'allZOActivatedAbilities do
  zos <- allZOs @zone @ot
  concat <$> T.for zos getActivatedAbilitiesOf

doesZoneObjectExist :: forall zone ot m. (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m Bool
doesZoneObjectExist zo = logCall 'doesZoneObjectExist do
  toZO @zone @ot (getObjectId zo) <&> \case
    Nothing -> False
    Just{} -> True

doesObjectNExist :: forall ot m. (IsOT ot, Monad m) => ObjectN ot -> Magic 'Private 'RO m Bool
doesObjectNExist o = logCall 'doesObjectNExist do
  or -- XXX: Use monadic `orM`
    <$> sequence
      [ doesZoneObjectExist $ ZO SZBattlefield o
      , doesZoneObjectExist $ ZO SZExile o
      , doesZoneObjectExist $ ZO SZGraveyard o
      , doesZoneObjectExist $ ZO SZHand o
      , doesZoneObjectExist $ ZO SZLibrary o
      , doesZoneObjectExist $ ZO SZStack o
      ]

type OTArbitrary = 'OTLand

doesObjectIdExist :: Monad m => ObjectId -> Magic 'Private 'RO m Bool
doesObjectIdExist i = logCall 'doesObjectIdExist do
  doesObjectNExist $ toObjectNAny o1
 where
  o0 = O0 $ UntypedObject DefaultObjectDiscriminant i
  o1 = toObject1 @OT0 @OTArbitrary o0

getActivatedAbilitiesOf ::
  forall zone ot m.
  (IsZO zone ot, Monad m) =>
  ZO zone ot ->
  Magic 'Private 'RO m [SomeActivatedAbility zone ot]
getActivatedAbilitiesOf zo = logCall 'getActivatedAbilitiesOf do
  -- XXX: Being lazy at the moment and assuming it's a permanent
  case singZone @zone of
    SZBattlefield -> do
      let zoPerm = zo0ToPermanent $ toZO0 zo
      findPermanent zoPerm <&> \case
        Nothing -> []
        Just perm -> catMaybes $ flip map (permanentAbilities perm) \ability ->
          fromSome ability \case
            Activated withThis -> go withThis
            _ -> Nothing
    _ -> undefined
 where
  go ::
    forall zone' ot'.
    IsZO zone' ot' =>
    WithThisActivated zone' ot' ->
    Maybe (SomeActivatedAbility zone ot)
  go withThis = case cast withThis of
    Nothing -> Nothing
    Just (withThis' :: WithThisActivated zone ot') ->
      Just
        SomeActivatedAbility
          { someActivatedZO = zo
          , someActivatedAbility = withThis'
          }

activatedToIndex ::
  (IsZO zone ot, Monad m) =>
  SomeActivatedAbility zone ot ->
  Magic 'Private 'RO m AbsoluteActivatedAbilityIndex
activatedToIndex ability = logCall 'activatedToIndex do
  let zo = someActivatedZO ability
      i = getObjectId zo
  abilities <- getActivatedAbilitiesOf zo
  pure case List.elemIndex ability abilities of
    Nothing -> error $ show $ ObjectDoesNotHaveAbility ability
    Just index -> AbsoluteActivatedAbilityIndex i $ RelativeAbilityIndex index

indexToActivated ::
  (IsZO zone ot, Monad m) =>
  AbsoluteActivatedAbilityIndex ->
  Magic 'Private 'RO m (Maybe (SomeActivatedAbility zone ot))
indexToActivated absIndex = logCall 'indexToActivated do
  let AbsoluteActivatedAbilityIndex i relIndex = absIndex
      RelativeAbilityIndex index = relIndex
  mZo <- toZO i
  case mZo of
    Nothing -> pure Nothing
    Just zo -> do
      abilities <- getActivatedAbilitiesOf zo
      let len = length abilities
      pure case 0 <= index && index < len of
        False -> Nothing
        True -> Just $ abilities !! index

getBasicLandTypes :: forall zone ot m. (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m [BasicLandType]
getBasicLandTypes zo = logCall 'getBasicLandTypes do
  case singZone @zone of
    SZBattlefield -> do
      let zoPerm = zo0ToPermanent $ toZO0 zo
      findPermanent zoPerm <&> \case
        Nothing -> []
        Just perm -> case permanentLand perm of
          Nothing -> []
          Just land -> fromLandTypes $ landTypes land
    SZHand -> do
      let zoHand = zo0ToCard $ toZO0 zo
      oPlayers <- fromPublic getAlivePlayers
      concat <$> T.for oPlayers \oPlayer -> do
        let zoPlayer = oToZO1 @ 'ZBattlefield oPlayer
        findHandCard oPlayer zoHand <&> \case
          Nothing -> []
          Just handCard -> case handCard of -- TODO: make a type class to fetch facets
            AnyCard anyCard -> case anyCard of
              Card _ card -> case card of
                YourArtifactLand playerToFacet -> case playerToFacet zoPlayer of
                  ArtifactLandFacet{artifactLand_landTypes = tys} -> fromLandTypes tys
                YourLand playerToFacet -> case playerToFacet zoPlayer of
                  LandFacet{land_landTypes = tys} -> fromLandTypes tys
                _ -> undefined
    _ -> undefined
 where
  fromLandTypes :: [LandType] -> [BasicLandType]
  fromLandTypes tys =
    tys >>= \case
      BasicLand ty -> [ty]
      _ -> []

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

findHandCard :: Monad m => Object 'OTPlayer -> ZO 'ZHand OTCard -> Magic 'Private 'RO m (Maybe AnyCard)
findHandCard oPlayer zoCard = logCall 'findHandCard do
  fromRO (gets $ Map.lookup (toZO0 zoCard) . magicHandCards) >>= \case
    Nothing -> pure Nothing
    Just card -> do
      player <- fromRO $ getPlayer oPlayer
      pure case zoCard `elem` unHand (playerHand player) of
        False -> Nothing
        True -> Just card

findLibraryCard :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic 'Private 'RO m (Maybe AnyCard)
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
  fromRO (findHandCard oPlayer zoCard) >>= \case
    Nothing -> pure Nothing
    Just{} -> do
      mCard <- fromRO $ gets $ Map.lookup (toZO0 zoCard) . magicHandCards
      modify \st -> st{magicHandCards = Map.delete (toZO0 zoCard) $ magicHandCards st}
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer player{playerHand = Hand $ List.delete zoCard $ unHand $ playerHand player}
      pure $ assert (isJust mCard) mCard

removeLibraryCard :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeLibraryCard oPlayer zoCard = logCall 'removeLibraryCard do
  fromRO (findLibraryCard oPlayer zoCard) >>= \case
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
getPermanent zoPerm = logCall 'getPermanent do
  findPermanent zoPerm <&> \case
    Nothing -> error $ show $ InvalidPermanent zoPerm
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

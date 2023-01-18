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
  findGraveyardCard,
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
  pickOneZO,
  pushGraveyardCard,
  pushHandCard,
  pushLibraryCard,
  removeGraveyardCard,
  removeHandCard,
  removeLibraryCard,
  rewindIllegal,
  rewindIllegalActivation,
  rewindNothing,
  setPermanent,
  setPlayer,
  toZO,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (untilJust)
import safe qualified Data.DList as DList
import safe Data.Functor ((<&>))
import safe qualified Data.List as List
import safe Data.List.NonEmpty (NonEmpty (..))
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
  AbsoluteActivatedAbilityIndex (..),
  ActivateResult (..),
  InternalLogicError (..),
  PlayerCount (..),
  Prompt' (..),
  RelativeAbilityIndex (..),
  SomeActivatedAbility (..),
 )
import safe MtgPure.Engine.State (
  GameResult (..),
  GameState (..),
  Magic,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.IsCardList (pushCard)
import safe MtgPure.Model.Land (Land (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Object.IndexOT (IndexOT (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.OTNAliases (OTNCard, OTNPermanent)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (..),
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (ObjectN)
import safe MtgPure.Model.Object.ObjectN_ (ObjectN' (O0))
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
  ToZO0,
  castOToON,
  oToZO1,
  toZO0,
  zo0ToCard,
  zo0ToPermanent,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZoneObject (ZO), toZone)

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

allPermanents :: Monad m => Magic 'Public 'RO m [ZO 'ZBattlefield OTNPermanent]
allPermanents = logCall 'allPermanents $ map zo0ToPermanent <$> perms0
 where
  perms0 = internalFromPrivate $ gets $ Map.keys . magicPermanents

allControlledPermanentsOf ::
  Monad m =>
  Object 'OTPlayer ->
  Magic 'Public 'RO m [ZO 'ZBattlefield OTNPermanent]
allControlledPermanentsOf oPlayer =
  logCall 'allControlledPermanentsOf $
    allPermanents >>= M.filterM \zoPerm -> do
      perm <- internalFromPrivate $ fromRO $ getPermanent zoPerm
      pure $ permanentController perm == oPlayer

rewindLeft :: Monad m => ex -> Magic 'Private 'RW m (Either ex a) -> Magic 'Private 'RW m (Either ex a)
rewindLeft ex m = logCall 'rewindLeft do
  -- (104.1) (727.1) XXX: Is it possible for GameResult to be thrown during an illegal action?
  -- If so, is should it sometimes/always/never be rewound?
  let m' = magicCatch m \case
        GameResult{gameWinners = []} -> pure $ Left ex
        gameResult -> magicThrow gameResult
  st <- fromRO get
  result <- m'
  case result of
    Right{} -> pure result
    Left{} -> put st >> pure result

rewindNothing :: Monad m => Magic 'Private 'RW m (Maybe a) -> Magic 'Private 'RW m (Maybe a)
rewindNothing m = logCall 'rewindNothing do
  result <-
    rewindLeft () $
      m <&> \case
        Nothing -> Left ()
        Just x -> Right x
  pure case result of
    Left () -> Nothing
    Right x -> Just x

rewindIllegal :: Monad m => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegal m = logCall 'rewindIllegal do
  result <-
    rewindLeft () $
      m <&> \case
        Legal -> Right ()
        Illegal -> Left ()
  pure case result of
    Left () -> False
    Right () -> True

rewindIllegalActivation :: Monad m => Magic 'Private 'RW m ActivateResult -> Magic 'Private 'RW m ActivateResult
rewindIllegalActivation m = logCall 'rewindIllegalActivation do
  result <-
    rewindLeft IllegalActivation $
      m <&> \x -> case x of
        IllegalActivation{} -> Left x
        ActivatedManaAbility{} -> Right x
        ActivatedNonManaAbility{} -> Right x
  pure $ either id id result

pickOneZO ::
  (IsZO zone ot, Monad m) =>
  Object 'OTPlayer ->
  [ZO zone ot] ->
  Magic 'Private 'RO m (Maybe (ZO zone ot))
pickOneZO oPlayer = \case
  [] -> pure Nothing
  zos@(zosHead : zosTail) -> do
    prompt <- gets magicPrompt
    opaque <- gets mkOpaqueGameState
    Just <$> untilJust \attempt -> do
      zo <- M.lift $ promptPickZO prompt attempt opaque oPlayer $ zosHead :| zosTail
      pure case zo `elem` zos of
        False -> Nothing
        True -> Just zo

allZOs :: forall zone ot m. (Monad m, IsZO zone ot) => Magic 'Private 'RO m [ZO zone ot]
allZOs = logCall 'allZOs case singZone @zone of
  SZBattlefield ->
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
     in DList.toList <$> case indexOT @ot of
          [ots] -> goRec ots
          otts -> goRec $ List.nub $ concat otts
  _ -> undefined -- XXX: sung zone
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

doesObjectNExist :: forall ot m. (IsOTN ot, Monad m) => ObjectN ot -> Magic 'Private 'RO m Bool
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
    _ -> undefined -- XXX: sung zone
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
    SZHand -> goFindCard findHandCard
    SZLibrary -> goFindCard findLibraryCard
    SZGraveyard -> goFindCard findGraveyardCard
    _ -> undefined -- XXX: sung zone
 where
  goFindCard ::
    (Object 'OTPlayer -> ZO zone OTNCard -> Magic 'Private 'RO m (Maybe AnyCard)) ->
    Magic 'Private 'RO m [BasicLandType]
  goFindCard findCard = do
    let zoCard = zo0ToCard $ toZO0 zo
    oPlayers <- fromPublic getAlivePlayers
    uniquify . concat <$> T.for oPlayers \oPlayer -> do
      let zoPlayer = oToZO1 @ 'ZBattlefield oPlayer
          goRec :: AnyCard -> Magic 'Private 'RO m [BasicLandType]
          goRec = \case
            AnyCard1 (Card _ card) -> pure case card of
              YourArtifactLand playerToFacet -> case playerToFacet zoPlayer of
                ArtifactLandFacet{artifactLand_landTypes = tys} -> fromLandTypes tys
              YourLand playerToFacet -> case playerToFacet zoPlayer of
                LandFacet{land_landTypes = tys} -> fromLandTypes tys
              _ -> []
            AnyCard2 (DoubleSidedCard c1 c2) -> do
              tys1 <- goRec $ AnyCard1 c1
              tys2 <- goRec $ AnyCard1 c2
              pure $ tys1 ++ tys2
            AnyCard2 SplitCard{splitCard_card1 = c1, splitCard_card2 = c2} -> do
              tys1 <- goRec $ AnyCard1 c1
              tys2 <- goRec $ AnyCard1 c2
              pure $ tys1 ++ tys2
      findCard oPlayer zoCard >>= \case
        Nothing -> pure []
        Just card -> goRec card
  fromLandTypes :: [LandType] -> [BasicLandType]
  fromLandTypes tys =
    tys >>= \case
      BasicLand ty -> [ty]
      _ -> []
  uniquify = List.nub . List.sort

newObjectId :: Monad m => Magic 'Private 'RW m ObjectId
newObjectId = logCall 'newObjectId do
  ObjectId i <- fromRO $ gets magicNextObjectId
  modify \st -> st{magicNextObjectId = ObjectId $ i + 1}
  pure $ ObjectId i

data PlayerCardsInfo zone m = PlayerCardsInfo
  { pci_ :: ()
  , pci_magicZoneCards :: GameState m -> Map.Map (ZO zone OT0) AnyCard
  , pci_setMagicZoneCards :: GameState m -> Map.Map (ZO zone OT0) AnyCard -> GameState m
  , pci_playerZoneCards :: Player -> [ZO zone OTNCard]
  , pci_setPlayerZoneCards :: Player -> [ZO zone OTNCard] -> Player
  , pci_pushZoneCard :: ZO zone OTNCard -> [ZO zone OTNCard] -> [ZO zone OTNCard]
  }

pciGraveyard :: PlayerCardsInfo 'ZGraveyard m
pciGraveyard =
  PlayerCardsInfo
    { pci_ = ()
    , pci_magicZoneCards = magicGraveyardCards
    , pci_setMagicZoneCards = \st cards -> st{magicGraveyardCards = cards}
    , pci_playerZoneCards = unGraveyard . playerGraveyard
    , pci_setPlayerZoneCards = \player cards -> player{playerGraveyard = Graveyard cards}
    , pci_pushZoneCard = \zo -> unGraveyard . pushCard zo . Graveyard
    }

pciHand :: PlayerCardsInfo 'ZHand m
pciHand =
  PlayerCardsInfo
    { pci_ = ()
    , pci_magicZoneCards = magicHandCards
    , pci_setMagicZoneCards = \st cards -> st{magicHandCards = cards}
    , pci_playerZoneCards = unHand . playerHand
    , pci_setPlayerZoneCards = \player cards -> player{playerHand = Hand cards}
    , pci_pushZoneCard = \zo -> unHand . pushCard zo . Hand
    }

pciLibrary :: PlayerCardsInfo 'ZLibrary m
pciLibrary =
  PlayerCardsInfo
    { pci_ = ()
    , pci_magicZoneCards = magicLibraryCards
    , pci_setMagicZoneCards = \st cards -> st{magicLibraryCards = cards}
    , pci_playerZoneCards = unLibrary . playerLibrary
    , pci_setPlayerZoneCards = \player cards -> player{playerLibrary = Library cards}
    , pci_pushZoneCard = \zo -> unLibrary . pushCard zo . Library
    }

pushZoneCard ::
  ( IsZO zone OTNCard
  , Monad m
  ) =>
  PlayerCardsInfo zone m ->
  Object 'OTPlayer ->
  AnyCard ->
  Magic 'Private 'RW m (ZO zone OTNCard)
pushZoneCard info oPlayer card = logCall 'pushZoneCard do
  player <- fromRO $ getPlayer oPlayer
  i <- newObjectId
  let zo0 = toZO0 i
      zoCard = zo0ToCard zo0
  modify \st -> pci_setMagicZoneCards info st $ Map.insert zo0 card $ pci_magicZoneCards info st
  setPlayer oPlayer $
    pci_setPlayerZoneCards info player $
      pci_pushZoneCard info zoCard $
        pci_playerZoneCards info player
  pure zoCard

pushGraveyardCard :: Monad m => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZGraveyard OTNCard)
pushGraveyardCard = logCall 'pushGraveyardCard $ pushZoneCard pciGraveyard

pushLibraryCard :: Monad m => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZLibrary OTNCard)
pushLibraryCard = logCall 'pushLibraryCard $ pushZoneCard pciLibrary

pushHandCard :: Monad m => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZHand OTNCard)
pushHandCard = logCall 'pushHandCard $ pushZoneCard pciHand

findZoneCard ::
  (IsZO zone OTNCard, Monad m, ToZO0 zone (ZO zone OTNCard)) =>
  PlayerCardsInfo zone m ->
  Object 'OTPlayer ->
  ZO zone OTNCard ->
  Magic 'Private 'RO m (Maybe AnyCard)
findZoneCard info oPlayer zoCard = logCall 'findZoneCard do
  fromRO (gets $ Map.lookup (toZO0 zoCard) . pci_magicZoneCards info) >>= \case
    Nothing -> pure Nothing
    Just card -> do
      player <- fromRO $ getPlayer oPlayer
      pure case zoCard `elem` pci_playerZoneCards info player of
        False -> Nothing
        True -> Just card

findGraveyardCard :: Monad m => Object 'OTPlayer -> ZO 'ZGraveyard OTNCard -> Magic 'Private 'RO m (Maybe AnyCard)
findGraveyardCard = logCall 'findGraveyardCard $ findZoneCard pciGraveyard

findHandCard :: Monad m => Object 'OTPlayer -> ZO 'ZHand OTNCard -> Magic 'Private 'RO m (Maybe AnyCard)
findHandCard = logCall 'findHandCard $ findZoneCard pciHand

findLibraryCard :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Magic 'Private 'RO m (Maybe AnyCard)
findLibraryCard = logCall 'findLibraryCard $ findZoneCard pciLibrary

removeZoneCard ::
  (IsZO zone OTNCard, Monad m, ToZO0 zone (ZO zone OTNCard)) =>
  PlayerCardsInfo zone m ->
  Object 'OTPlayer ->
  ZO zone OTNCard ->
  Magic 'Private 'RW m (Maybe AnyCard)
removeZoneCard info oPlayer zoCard = logCall 'removeZoneCard do
  fromRO (findZoneCard info oPlayer zoCard) >>= \case
    Nothing -> pure Nothing
    Just{} -> do
      mCard <- fromRO $ gets $ Map.lookup (toZO0 zoCard) . pci_magicZoneCards info
      modify \st -> pci_setMagicZoneCards info st $ Map.delete (toZO0 zoCard) $ pci_magicZoneCards info st
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer $
        pci_setPlayerZoneCards info player $
          List.delete zoCard $
            pci_playerZoneCards info player
      pure $ assert (isJust mCard) mCard

removeGraveyardCard :: Monad m => Object 'OTPlayer -> ZO 'ZGraveyard OTNCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeGraveyardCard = logCall 'removeGraveyardCard $ removeZoneCard pciGraveyard

removeHandCard :: Monad m => Object 'OTPlayer -> ZO 'ZHand OTNCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeHandCard = logCall 'removeHandCard $ removeZoneCard pciHand

removeLibraryCard :: Monad m => Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeLibraryCard = logCall 'removeLibraryCard $ removeZoneCard pciLibrary

findPermanent :: Monad m => ZO 'ZBattlefield OTNPermanent -> Magic 'Private 'RO m (Maybe Permanent)
findPermanent zoPerm = logCall 'findPermanent $ gets $ Map.lookup (toZO0 zoPerm) . magicPermanents

getPermanent :: Monad m => ZO 'ZBattlefield OTNPermanent -> Magic 'Private 'RO m Permanent
getPermanent zoPerm = logCall 'getPermanent do
  findPermanent zoPerm <&> \case
    Nothing -> error $ show $ InvalidPermanent zoPerm
    Just perm -> perm

setPermanent :: Monad m => ZO 'ZBattlefield OTNPermanent -> Maybe Permanent -> Magic 'Private 'RW m ()
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

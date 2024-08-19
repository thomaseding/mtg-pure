{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Satisfies (
  isSatisfied,
  satisfies,
  zosSatisfying,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Engine.Fwd.Api (
  allZOs,
  controllerOf,
  getPermanent,
  ownerOf,
  performIntrinsicElections,
 )
import safe MtgPure.Engine.Monad (gets)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (ElectionInput (..))
import safe MtgPure.Engine.State (GameState (..), Magic, logCall)
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.Land (Land (landTypes))
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.OTNAliases (OTNLand, OTNPlayer)
import safe MtgPure.Model.Object.ObjectId (getObjectId)
import safe MtgPure.Model.Permanent (Permanent (..), Tapped (..))
import safe MtgPure.Model.Recursive (
  AnyCard (..),
  Card (..),
  CardCharacteristic (..),
  Condition (..),
  Elect,
  Requirement (..),
 )
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Zone (IsZone (..), SingZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZOPlayer)

zosSatisfying :: (Monad m, IsZO zone ot) => Requirement zone ot -> Magic 'Private 'RO m [ZO zone ot]
zosSatisfying req = allZOs >>= M.filterM (`satisfies` req)

isSatisfied :: (Monad m) => Condition -> Magic 'Private 'RO m Bool
isSatisfied = \case
  CAnd cs -> and <$> mapM isSatisfied cs
  CNot c -> not <$> isSatisfied c
  COr cs -> or <$> mapM isSatisfied cs
  Satisfies zo reqs -> satisfies zo $ RAnd reqs

satisfies ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
satisfies zo = logCall 'satisfies \case
  ControlledBy zoPlayer -> controlledBy' zo zoPlayer
  ControlsA{} -> undefined
  HasAbility{} -> undefined
  HasLandType landType -> hasLandType' zo landType
  Is zo' -> is' zo zo'
  IsOpponentOf zoPlayer -> isOpponentOf' zo zoPlayer
  IsTapped -> isTapped' zo
  Not req -> not' zo req
  OfColors{} -> undefined
  OwnedBy{} -> undefined
  RAnd reqs -> rAnd' zo reqs
  ROr reqs -> rOr' zo reqs
  Req2{} -> undefined
  Req3{} -> undefined
  Req4{} -> undefined
  Req5{} -> undefined

hasLandType' ::
  forall zone m.
  (Monad m, IsZone zone) =>
  ZO zone OTNLand ->
  LandType ->
  Magic 'Private 'RO m Bool
hasLandType' zo landType = logCall 'hasLandType' case singZone @zone of
  SingZBattlefield -> do
    perm <- getPermanent $ zo0ToPermanent $ toZO0 zo
    pure case permanentLand perm of
      Nothing -> False
      Just land -> landType `elem` landTypes land
  SingZGraveyard -> do
    zoToAnyCard <- gets magicGraveyardCards
    goMap zoToAnyCard
  SingZLibrary -> do
    zoToAnyCard <- gets magicLibraryCards
    goMap zoToAnyCard
  _ -> undefined -- XXX: sung zone
 where
  goMap :: Map.Map (ZO zone OT0) AnyCard -> Magic 'Private 'RO m Bool
  goMap zoToAnyCard = case Map.lookup (toZO0 zo) zoToAnyCard of
    Nothing -> pure False
    Just anyCard -> goAnyCard anyCard

  goAnyCard :: AnyCard -> Magic 'Private 'RO m Bool
  goAnyCard = \case
    AnyCard1 card -> goCard card
    AnyCard2 card -> goCard card

  goCard :: Card ot -> Magic 'Private 'RO m Bool
  goCard = \case
    Card _name electCharacteristic -> goElectCharacteristic electCharacteristic
    _ -> undefined

  goElectCharacteristic :: (IsZO zone ot) => Elect 'IntrinsicStage (CardCharacteristic ot) ot -> Magic 'Private 'RO m Bool
  goElectCharacteristic elect = do
    owner <- ownerOf zo
    character <- performIntrinsicElections (IntrinsicInput owner) pure elect
    goCardCharacteristic character

  goCardCharacteristic :: CardCharacteristic ot -> Magic 'Private 'RO m Bool
  goCardCharacteristic c = case c of
    ArtifactLandCharacteristic{} -> goLandTypes $ artifactLand_landTypes c
    LandCharacteristic{} -> goLandTypes $ land_landTypes c
    --
    ArtifactCharacteristic{} -> pure False
    ArtifactCreatureCharacteristic{} -> pure False
    BattleCharacteristic{} -> pure False
    CreatureCharacteristic{} -> pure False
    EnchantmentCharacteristic{} -> pure False
    EnchantmentCreatureCharacteristic{} -> pure False
    InstantCharacteristic{} -> pure False
    PlaneswalkerCharacteristic{} -> pure False
    SorceryCharacteristic{} -> pure False

  goLandTypes :: [LandType] -> Magic 'Private 'RO m Bool
  goLandTypes tys = pure $ landType `elem` tys

isTapped' :: (IsOTN ot, Monad m) => ZO 'ZBattlefield ot -> Magic 'Private 'RO m Bool
isTapped' zo = logCall 'isTapped' do
  perm <- getPermanent $ zo0ToPermanent $ toZO0 zo
  pure $ permanentTapped perm == Tapped

controlledBy' ::
  forall m zone ot.
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  ZOPlayer ->
  Magic 'Private 'RO m Bool
controlledBy' zo zoPlayer = logCall 'controlledBy' do
  controller <- controllerOf zo
  pure $ getObjectId controller == getObjectId zoPlayer

is' :: (Monad m, IsZO zone ot) => ZO zone ot -> ZO zone ot -> Magic 'Private 'RO m Bool
is' zo zo' = logCall 'is' do
  pure $ getObjectId zo == getObjectId zo' -- XXX: Also check for ObjectId liveliness?

not' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
not' zo = logCall 'not' $ fmap not . satisfies zo

isOpponentOf' ::
  forall m zone.
  (Monad m, IsZO zone OTNPlayer) =>
  ZO zone OTNPlayer ->
  ZOPlayer ->
  Magic 'Private 'RO m Bool
isOpponentOf' candidatePlayer referencePlayer = logCall 'isOpponentOf' do
  -- TODO: multiplayer
  let candidateId = getObjectId candidatePlayer
      referenceId = getObjectId referencePlayer
  pure $ candidateId /= referenceId

rAnd' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  [Requirement zone ot] ->
  Magic 'Private 'RO m Bool
rAnd' zo = logCall 'rAnd' \case
  [] -> pure True
  req : reqs ->
    satisfies zo req >>= \case
      False -> pure False
      True -> rAnd' zo reqs

rOr' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  [Requirement zone ot] ->
  Magic 'Private 'RO m Bool
rOr' zo = logCall 'rOr' \case
  [] -> pure False
  req : reqs ->
    satisfies zo req >>= \case
      True -> pure True
      False -> rOr' zo reqs

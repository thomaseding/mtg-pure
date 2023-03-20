{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PutOntoBattlefield (
  putOntoBattlefield,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Engine.Fwd.Api (newObjectId, ownerOf, performIntrinsicElections, removeHandCard, removeLibraryCard, setPermanent)
import safe MtgPure.Engine.Monad (fromRO, modify)
import safe MtgPure.Engine.Prompt (ElectionInput (..))
import safe MtgPure.Engine.State (GameState (..), Magic, logCall)
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.Object.IndexOT (areObjectTypesSatisfied)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTNAliases (OTNPermanent)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.Recursive (AnyCard (..), Card (..), CardCharacteristic (..), CardSpec, Elect)
import safe MtgPure.Model.Zone (IsZone (..), SingZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (ToZO0 (..), zo0ToCard, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)

putOntoBattlefield ::
  forall zone ot m.
  (IsZO zone ot, CoPermanent ot, Monad m) =>
  Object 'OTPlayer ->
  ZO zone ot ->
  Magic 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
putOntoBattlefield oPlayer zo = logCall 'putOntoBattlefield do
  owner <- fromRO $ ownerOf zo
  mAnyCard <- case singZone @zone of
    SingZBattlefield -> pure Nothing -- Do something else
    SingZHand -> removeHandCard owner $ zo0ToCard $ toZO0 zo
    SingZLibrary -> removeLibraryCard owner $ zo0ToCard $ toZO0 zo
    _ -> undefined -- TODO: other zones
  case mAnyCard of
    Nothing -> pure Nothing
    Just anyCard -> goAnyCard anyCard
 where
  logCall' s = logCall ('putOntoBattlefield, s :: String)

  goAnyCard :: AnyCard -> Magic 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
  goAnyCard anyCard = logCall' "goAnyCard" case anyCard of
    AnyCard1 card -> goCard anyCard card
    AnyCard2 card -> goCard anyCard card

  goCard :: forall ot'. AnyCard -> Card ot' -> Magic 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
  goCard anyCard card = logCall' "goCard" case card of
    Card _name elect -> case areObjectTypesSatisfied @ot @ot' of
      True -> goElectIntrinsic anyCard elect
      False -> pure Nothing
    _ -> do
      pure () -- XXX: dual/split/double-sided/etc card needs to have a matching ot... also might require a prompt
      undefined

  goElectIntrinsic ::
    AnyCard ->
    Elect 'IntrinsicStage (CardCharacteristic ot') ot' ->
    Magic 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
  goElectIntrinsic anyCard electIntrinsic = logCall' "goElectIntrinsic" do
    character <- fromRO $ performIntrinsicElections (IntrinsicInput oPlayer) pure electIntrinsic
    goCharacteristic anyCard character

  goCharacteristic :: AnyCard -> CardCharacteristic ot' -> Magic 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
  goCharacteristic anyCard character = logCall' "goCharacteristic" case character of
    ArtifactCharacteristic{} -> go $ artifact_spec character
    ArtifactCreatureCharacteristic{} -> go $ artifactCreature_spec character
    ArtifactLandCharacteristic{} -> go $ artifactLand_spec character
    CreatureCharacteristic{} -> go $ creature_spec character
    EnchantmentCharacteristic{} -> go $ enchantment_spec character
    EnchantmentCreatureCharacteristic{} -> go $ enchantmentCreature_spec character
    LandCharacteristic{} -> go $ land_spec character
    PlaneswalkerCharacteristic{} -> go $ planeswalker_spec character
    InstantCharacteristic{} -> pure Nothing
    SorceryCharacteristic{} -> pure Nothing
   where
    go = goSpec anyCard character

  goSpec :: AnyCard -> CardCharacteristic ot' -> CardSpec ot' -> Magic 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
  goSpec anyCard character spec = logCall' "goSpec" do
    pure () -- TODO: Implement this in terms of `resolvePermanent` or somehow share code with it
    i <- newObjectId
    let zoPerm = zo0ToPermanent $ toZO0 i
    case cardToPermanent anyCard character spec of
      Nothing -> pure Nothing
      Just perm -> do
        modify \st' ->
          st'
            { magicControllerMap = Map.insert i oPlayer $ magicControllerMap st'
            , magicOwnerMap = Map.insert i oPlayer $ magicOwnerMap st'
            }
        setPermanent zoPerm $ Just perm
        pure $ Just zoPerm

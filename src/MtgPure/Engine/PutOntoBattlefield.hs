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
import safe MtgPure.Engine.Fwd.Api (newObjectId, ownerOf, performElections, removeHandCard, setPermanent)
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (fromRO, modify)
import safe MtgPure.Engine.State (GameState (..), Magic, logCall)
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.Object.IndexOT (IndexOT (..))
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.Recursive (AnyCard (..), Card (..), CardCharacteristic (..), CardSpec, Elect)
import safe MtgPure.Model.Zone (IsZone (..), SZone (..))
import safe MtgPure.Model.ZoneObject.Convert (ToZO0 (..), zo0ToCard, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)

putOntoBattlefield ::
  forall zone ot m.
  (IsZO zone ot, CoPermanent ot, Monad m) =>
  Object 'OTPlayer ->
  ZO zone ot ->
  Magic 'Private 'RW m Legality
putOntoBattlefield oPlayer zo = logCall 'putOntoBattlefield do
  owner <- fromRO $ ownerOf zo
  mAnyCard <- case singZone @zone of
    SZBattlefield -> pure Nothing -- Do something else
    SZHand -> removeHandCard owner $ zo0ToCard $ toZO0 zo
    _ -> undefined -- TODO: other zones
  case mAnyCard of
    Nothing -> pure Illegal
    Just anyCard -> goAnyCard anyCard
 where
  logCall' s = logCall ('putOntoBattlefield, s :: String)

  goAnyCard :: AnyCard -> Magic 'Private 'RW m Legality
  goAnyCard anyCard = logCall' "goAnyCard" case anyCard of
    AnyCard1 card -> goCard anyCard card
    AnyCard2 card -> goCard anyCard card

  goCard :: forall ot'. AnyCard -> Card ot' -> Magic 'Private 'RW m Legality
  goCard anyCard card = logCall' "goCard" case card of
    Card _name elect -> case areObjectTypesSatisfied @ot @ot' of
      True -> goElectIntrinsic anyCard elect
      False -> pure Illegal
    _ -> do
      pure () -- XXX: dual/split/double-sided/etc card needs to have a matching ot... also might require a prompt
      undefined

  goElectIntrinsic :: AnyCard -> Elect 'IntrinsicStage (CardCharacteristic ot') ot' -> Magic 'Private 'RW m Legality
  goElectIntrinsic anyCard electIntrinsic = logCall' "goElectIntrinsic" do
    i <- newObjectId
    -- Yes, lands don't use the stack, but we make a faux stack object anyway
    -- so elections like `Your` work for free. If this ends up being too hokey
    -- the type of `performElections` would need to be changed. Prolly would
    -- change the `ZO 'ZStack OT0` to be the result of a type family for the
    -- non-intrinsic `ElectStage` cases and `ZOPlayer` for `IntrinsicStage`.
    let zoStack0 = toZO0 i
    modify \st' ->
      st'
        { magicControllerMap = Map.insert i oPlayer $ magicControllerMap st'
        , magicOwnerMap = Map.insert i oPlayer $ magicOwnerMap st'
        }
    mCharacteristic <- fromRO $ performElections zoStack0 (pure . Just) electIntrinsic
    case mCharacteristic of
      Nothing -> pure Illegal
      Just character -> do
        result <- goCharacteristic anyCard character
        modify \st' ->
          st'
            { magicControllerMap = Map.delete i $ magicControllerMap st'
            , magicOwnerMap = Map.delete i $ magicOwnerMap st'
            }
        pure result

  goCharacteristic :: AnyCard -> CardCharacteristic ot' -> Magic 'Private 'RW m Legality
  goCharacteristic anyCard character = logCall' "goCharacteristic" case character of
    ArtifactCharacteristic{} -> go $ artifact_spec character
    ArtifactCreatureCharacteristic{} -> go $ artifactCreature_spec character
    ArtifactLandCharacteristic{} -> go $ artifactLand_spec character
    CreatureCharacteristic{} -> go $ creature_spec character
    EnchantmentCharacteristic{} -> go $ enchantment_spec character
    EnchantmentCreatureCharacteristic{} -> go $ enchantmentCreature_spec character
    LandCharacteristic{} -> go $ land_spec character
    PlaneswalkerCharacteristic{} -> go $ planeswalker_spec character
    InstantCharacteristic{} -> pure Illegal
    SorceryCharacteristic{} -> pure Illegal
   where
    go = goSpec anyCard character

  goSpec :: AnyCard -> CardCharacteristic ot' -> CardSpec ot' -> Magic 'Private 'RW m Legality
  goSpec anyCard character spec = logCall' "goSpec" do
    pure () -- TODO: Implement this in terms of `resolvePermanent` or somehow share code with it
    i <- newObjectId
    let zoPerm = zo0ToPermanent $ toZO0 i
    case cardToPermanent anyCard character spec of
      Nothing -> pure Illegal
      Just perm -> do
        modify \st' ->
          st'
            { magicControllerMap = Map.insert i oPlayer $ magicControllerMap st'
            , magicOwnerMap = Map.insert i oPlayer $ magicOwnerMap st'
            }
        setPermanent zoPerm $ Just perm
        pure Legal

-- | Examples:
--  * `areObjectTypesSatisfied \@OTNArtifact \@OTNArtifact => True`
--  * `areObjectTypesSatisfied \@OTNArtifact \@OTNCreature => False`
--  * `areObjectTypesSatisfied \@OTNArtifact \@OTNArtifactCreature => True`
--  * `areObjectTypesSatisfied \@OTNArtifactCreature \@OTNArtifact => False`
--  * `areObjectTypesSatisfied \@OTNArtifactCreature \@OTNArtifactCreature => True`
areObjectTypesSatisfied :: forall ot ot'. (IndexOT ot, IndexOT ot') => Bool
areObjectTypesSatisfied = gos (indexOT @ot) (indexOT @ot')
 where
  gos :: [[ObjectType]] -> [[ObjectType]] -> Bool
  gos [] [] = True
  gos [] _ = False
  gos _ [] = False
  gos (ot : ots) (ot' : ots') = case go ot ot' of
    True -> gos ots ots'
    False -> False

  go :: [ObjectType] -> [ObjectType] -> Bool
  go [] _ = True
  go _ [] = False
  go (ot : ots) (ot' : ots') = case ot == ot' of
    True -> go ots ots'
    False -> go (ot : ots) ots'
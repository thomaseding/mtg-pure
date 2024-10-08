{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}

module MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  ZoneObject (..),
  ZOAbility,
  ZOActivatedAbility,
  ZOActivatedOrTriggeredAbility,
  ZOAny,
  ZOArtifact,
  ZOArtifactCreature,
  ZOBattle,
  ZOCard,
  ZOCreature,
  ZOCreaturePlaneswalker,
  ZOCreaturePlayer,
  ZOCreaturePlayerPlaneswalker,
  ZOEmblem,
  ZOEnchantment,
  ZOInstant,
  ZOLand,
  ZONonCreature,
  ZOPermanent,
  ZOPlaneswalker,
  ZOPlayer,
  ZOPlayerPlaneswalker,
  ZOSorcery,
  ZOSpell,
  ZOStaticAbility,
  ZOTriggeredAbility,
  zoToObjectN,
  toZone,
) where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.IndexOT (IndexOT (..))
import safe MtgPure.Model.Object.LitOTN (LitOTN)
import safe MtgPure.Model.Object.OTNAliases (
  OTNAbility,
  OTNActivatedAbility,
  OTNActivatedOrTriggeredAbility,
  OTNAny,
  OTNArtifact,
  OTNArtifactCreature,
  OTNBattle,
  OTNCard,
  OTNCreature,
  OTNCreaturePlaneswalker,
  OTNCreaturePlayer,
  OTNCreaturePlayerPlaneswalker,
  OTNEmblem,
  OTNEnchantment,
  OTNInstant,
  OTNLand,
  OTNNonCreature,
  OTNPermanent,
  OTNPlaneswalker,
  OTNPlayer,
  OTNPlayerPlaneswalker,
  OTNSorcery,
  OTNSpell,
  OTNStaticAbility,
  OTNTriggeredAbility,
 )
import safe MtgPure.Model.Object.ObjectId (GetObjectId (..), UntypedObject)
import safe MtgPure.Model.Object.ObjectN (ObjectN)
import safe MtgPure.Model.Object.PromoteIdToObjectN (PromoteIdToObjectN)
import safe MtgPure.Model.Object.VisitObjectN ()
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Zone (IsZone (..), SingZone (..), Zone (..))

type ZO = ZoneObject

data ZoneObject (zone :: Zone) (ot :: Type) :: Type where
  ZO :: SingZone zone -> ObjectN ot -> ZoneObject zone ot
  deriving (Typeable)

zoToObjectN :: ZO zone ot -> ObjectN ot
zoToObjectN = \case
  ZO _ o -> o

instance ConsIndex (ZO zone ot) where
  consIndex :: ZO zone ot -> Int
  consIndex = \case
    ZO{} -> 1

instance (IsZone zone, PrettyType ot) => PrettyType (ZO zone ot) where
  prettyType :: (IsZone zone, PrettyType ot) => String
  prettyType = "ZO '" ++ sZone ++ " " ++ open ++ sOT ++ close
   where
    sZone = show (litZone @zone)
    sOT = prettyType @ot
    (open, close) = case ' ' `elem` sOT of
      True -> ("(", ")")
      False -> ("", "")

instance GetObjectId (ZO zone ot) where
  getUntypedObject :: ZO zone ot -> UntypedObject
  getUntypedObject = getUntypedObject . zoToObjectN

type IsOTN (ot :: Type) =
  ( IndexOT ot
  , PromoteIdToObjectN ot
  , PrettyType ot
  , LitOTN ot
  )

type IsZO (zone :: Zone) (ot :: Type) =
  ( IsOTN ot
  , IsZone zone
  -- XXX: Prolly don't want to put these here so they don't leak to Authoring
  -- , Eq (ZO zone ot)
  -- , Ord (ZO zone ot)
  -- , Show (ZO zone ot)
  )

type ZOAny = ZO 'ZBattlefield OTNAny

type ZOAbility = ZO 'ZBattlefield OTNAbility

type ZOActivatedAbility = ZO 'ZBattlefield OTNActivatedAbility

type ZOActivatedOrTriggeredAbility =
  ZO 'ZBattlefield OTNActivatedOrTriggeredAbility

type ZOArtifact = ZO 'ZBattlefield OTNArtifact

type ZOArtifactCreature = ZO 'ZBattlefield OTNArtifactCreature

type ZOBattle = ZO 'ZBattlefield OTNBattle

type ZOCard = ZO 'ZBattlefield OTNCard

type ZOCreature = ZO 'ZBattlefield OTNCreature

type ZOCreaturePlaneswalker = ZO 'ZBattlefield OTNCreaturePlaneswalker

type ZOCreaturePlayer = ZO 'ZBattlefield OTNCreaturePlayer

type ZOCreaturePlayerPlaneswalker =
  ZO 'ZBattlefield OTNCreaturePlayerPlaneswalker

type ZOEmblem = ZO 'ZBattlefield OTNEmblem

type ZOEnchantment = ZO 'ZBattlefield OTNEnchantment

type ZOInstant = ZO 'ZBattlefield OTNInstant

type ZOLand = ZO 'ZBattlefield OTNLand

type ZONonCreature = ZO 'ZBattlefield OTNNonCreature

type ZOPermanent = ZO 'ZBattlefield OTNPermanent

type ZOPlaneswalker = ZO 'ZBattlefield OTNPlaneswalker

type ZOPlayer = ZO 'ZBattlefield OTNPlayer

type ZOPlayerPlaneswalker = ZO 'ZBattlefield OTNPlayerPlaneswalker

type ZOSorcery = ZO 'ZBattlefield OTNSorcery

type ZOSpell = ZO 'ZBattlefield OTNSpell

type ZOStaticAbility = ZO 'ZBattlefield OTNStaticAbility

type ZOTriggeredAbility = ZO 'ZBattlefield OTNTriggeredAbility

toZone :: forall zone ot. (IsZone zone) => ObjectN ot -> ZO zone ot
toZone = ZO (singZone @zone)

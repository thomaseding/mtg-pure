{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}

module MtgPure.Model.ZoneObject.ZoneObject (
  IsOT,
  IsZO,
  ZO,
  ZoneObject (..),
  ZOAbility,
  ZOActivatedAbility,
  ZOActivatedOrTriggeredAbility,
  ZOAny,
  ZOArtifact,
  ZOArtifactCreature,
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
import safe MtgPure.Model.Object.LitOT (LitOT)
import safe MtgPure.Model.Object.OTKind (
  OTAbility,
  OTActivatedAbility,
  OTActivatedOrTriggeredAbility,
  OTAny,
  OTArtifact,
  OTArtifactCreature,
  OTCard,
  OTCreature,
  OTCreaturePlaneswalker,
  OTCreaturePlayer,
  OTCreaturePlayerPlaneswalker,
  OTEmblem,
  OTEnchantment,
  OTInstant,
  OTLand,
  OTNonCreature,
  OTPermanent,
  OTPlaneswalker,
  OTPlayer,
  OTPlayerPlaneswalker,
  OTSorcery,
  OTSpell,
  OTStaticAbility,
  OTTriggeredAbility,
 )
import safe MtgPure.Model.Object.ObjectId (GetObjectId (..))
import safe MtgPure.Model.Object.ObjectN (ObjectN (..))
import safe MtgPure.Model.Object.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))

type ZO = ZoneObject

data ZoneObject (zone :: Zone) (ot :: Type) :: Type where
  ZO :: SZone zone -> ObjectN ot -> ZoneObject zone ot
  deriving (Typeable)

zoToObjectN :: ZO zone ot -> ObjectN ot
zoToObjectN = \case
  ZO _ o -> o

instance ConsIndex (ZO zone ot) where
  consIndex = \case
    ZO{} -> 1

instance (IsZone zone, PrettyType ot) => PrettyType (ZO zone ot) where
  prettyType = "ZO '" ++ sZone ++ " " ++ open ++ sOT ++ close
   where
    sZone = show (litZone @zone)
    sOT = prettyType @ot
    (open, close) = case ' ' `elem` sOT of
      True -> ("(", ")")
      False -> ("", "")

instance GetObjectId (ObjectN ot) => GetObjectId (ZO zone ot) where
  getUntypedObject = getUntypedObject . zoToObjectN

type IsOT (ot :: Type) =
  ( IndexOT ot
  , VisitObjectN ot
  , PrettyType ot
  , LitOT ot
  , GetObjectId (ObjectN ot)
  )

type IsZO (zone :: Zone) (ot :: Type) =
  ( IsOT ot
  , IsZone zone
  , PrettyType (ZO zone ot)
  -- XXX: Prolly dont want to put these here so they dont leak to Authoring
  --, Eq (ZO zone ot)
  --, Ord (ZO zone ot)
  --, Show (ZO zone ot)
  )

type ZOAny = ZO 'ZBattlefield OTAny

type ZOAbility = ZO 'ZBattlefield OTAbility

type ZOActivatedAbility = ZO 'ZBattlefield OTActivatedAbility

type ZOActivatedOrTriggeredAbility =
  ZO 'ZBattlefield OTActivatedOrTriggeredAbility

type ZOArtifact = ZO 'ZBattlefield OTArtifact

type ZOArtifactCreature = ZO 'ZBattlefield OTArtifactCreature

type ZOCard = ZO 'ZBattlefield OTCard

type ZOCreature = ZO 'ZBattlefield OTCreature

type ZOCreaturePlaneswalker = ZO 'ZBattlefield OTCreaturePlaneswalker

type ZOCreaturePlayer = ZO 'ZBattlefield OTCreaturePlayer

type ZOCreaturePlayerPlaneswalker =
  ZO 'ZBattlefield OTCreaturePlayerPlaneswalker

type ZOEmblem = ZO 'ZBattlefield OTEmblem

type ZOEnchantment = ZO 'ZBattlefield OTEnchantment

type ZOInstant = ZO 'ZBattlefield OTInstant

type ZOLand = ZO 'ZBattlefield OTLand

type ZONonCreature = ZO 'ZBattlefield OTNonCreature

type ZOPermanent = ZO 'ZBattlefield OTPermanent

type ZOPlaneswalker = ZO 'ZBattlefield OTPlaneswalker

type ZOPlayer = ZO 'ZBattlefield OTPlayer

type ZOPlayerPlaneswalker = ZO 'ZBattlefield OTPlayerPlaneswalker

type ZOSorcery = ZO 'ZBattlefield OTSorcery

type ZOSpell = ZO 'ZBattlefield OTSpell

type ZOStaticAbility = ZO 'ZBattlefield OTStaticAbility

type ZOTriggeredAbility = ZO 'ZBattlefield OTTriggeredAbility

toZone :: forall zone ot. IsZone zone => ObjectN ot -> ZO zone ot
toZone = ZO (singZone @zone)

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}

module MtgPure.Model.ZoneObject (
  IsOT,
  IsZO,
  ZO,
  ZoneObject (..),
  OAbility,
  OActivatedAbility,
  OActivatedOrTriggeredAbility,
  OAny,
  OArtifact,
  OArtifactCreature,
  OCard,
  OCreature,
  OCreaturePlaneswalker,
  OCreaturePlayer,
  OCreaturePlayerPlaneswalker,
  ODamageSource,
  OEmblem,
  OEnchantment,
  OInstant,
  OLand,
  ONonCreature,
  OPermanent,
  OPlaneswalker,
  OPlayer,
  OPlayerPlaneswalker,
  OSorcery,
  OSpell,
  OStaticAbility,
  OTriggeredAbility,
  zoToObjectN,
) where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object (LitOT)
import safe MtgPure.Model.ObjectId (GetObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectType.Index (IndexOT (..))
import safe MtgPure.Model.ObjectType.Kind (
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
  OTDamageSource,
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
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))
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
  getObjectId = getObjectId . zoToObjectN

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

type OAny = ZO 'ZBattlefield OTAny

type OAbility = ZO 'ZBattlefield OTAbility

type OActivatedAbility = ZO 'ZBattlefield OTActivatedAbility

type OActivatedOrTriggeredAbility =
  ZO 'ZBattlefield OTActivatedOrTriggeredAbility

type OArtifact = ZO 'ZBattlefield OTArtifact

type OArtifactCreature = ZO 'ZBattlefield OTArtifactCreature

type OCard = ZO 'ZBattlefield OTCard

type OCreature = ZO 'ZBattlefield OTCreature

type OCreaturePlaneswalker = ZO 'ZBattlefield OTCreaturePlaneswalker

type OCreaturePlayer = ZO 'ZBattlefield OTCreaturePlayer

type OCreaturePlayerPlaneswalker =
  ZO 'ZBattlefield OTCreaturePlayerPlaneswalker

type ODamageSource = ZO 'ZBattlefield OTDamageSource

type OEmblem = ZO 'ZBattlefield OTEmblem

type OEnchantment = ZO 'ZBattlefield OTEnchantment

type OInstant = ZO 'ZBattlefield OTInstant

type OLand = ZO 'ZBattlefield OTLand

type ONonCreature = ZO 'ZBattlefield OTNonCreature

type OPermanent = ZO 'ZBattlefield OTPermanent

type OPlaneswalker = ZO 'ZBattlefield OTPlaneswalker

type OPlayer = ZO 'ZBattlefield OTPlayer

type OPlayerPlaneswalker = ZO 'ZBattlefield OTPlayerPlaneswalker

type OSorcery = ZO 'ZBattlefield OTSorcery

type OSpell = ZO 'ZBattlefield OTSpell

type OStaticAbility = ZO 'ZBattlefield OTStaticAbility

type OTriggeredAbility = ZO 'ZBattlefield OTTriggeredAbility

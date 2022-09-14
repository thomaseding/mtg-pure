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

module MtgPure.Model.ZoneObject (
  ZO,
  ZoneObject (..),
  toZO1,
  toZO2,
  toZO3,
  toZO4,
  toZO5,
  toZO6,
  toZO7,
  toZO8,
  toZO9,
  toZO10,
  toZO11,
  toZO12,
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
) where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectType (
  OT1,
  OT10,
  OT11,
  OT12,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OT8,
  OT9,
 )
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
import MtgPure.Model.ToObjectN.Classes (
  ToObject1 (..),
  ToObject10 (..),
  ToObject11 (..),
  ToObject12 (..),
  ToObject2 (..),
  ToObject3 (..),
  ToObject4 (..),
  ToObject5 (..),
  ToObject6 (..),
  ToObject7 (..),
  ToObject8 (..),
  ToObject9 (..),
 )
import safe MtgPure.Model.Zone (IsZone (litZone), SZone (..), Zone (..))

type ZO = ZoneObject

data ZoneObject :: Zone -> Type -> Type where
  ZOBattlefield :: SZone 'Battlefield -> ObjectN ot -> ZoneObject 'Battlefield ot
  ZOLibrary :: SZone 'Library -> ObjectN ot -> ZoneObject 'Library ot
  ZOStack :: SZone 'Stack -> ObjectN ot -> ZoneObject 'Stack ot
  deriving (Typeable)

instance ConsIndex (ZO zone ot) where
  consIndex = \case
    ZOBattlefield{} -> 1
    ZOLibrary{} -> 2
    ZOStack{} -> 3

instance (IsZone zone, PrettyType ot) => PrettyType (ZO zone ot) where
  prettyType _ = "ZO '" ++ sZone ++ " " ++ open ++ sOT ++ close
   where
    sZone = show $ litZone (Proxy @zone)
    sOT = prettyType (Proxy @ot)
    (open, close) = case ' ' `elem` sOT of
      True -> ("(", ")")
      False -> ("", "")

toZO1 :: ToObject1 ot a => ZO zone ot -> ZO zone (OT1 a)
toZO1 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject1 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject1 o
  ZOStack SStack o -> ZOStack SStack $ toObject1 o

toZO2 :: ToObject2 ot a b => ZO zone ot -> ZO zone (OT2 a b)
toZO2 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject2 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject2 o
  ZOStack SStack o -> ZOStack SStack $ toObject2 o

toZO3 :: ToObject3 ot a b c => ZO zone ot -> ZO zone (OT3 a b c)
toZO3 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject3 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject3 o
  ZOStack SStack o -> ZOStack SStack $ toObject3 o

toZO4 :: ToObject4 ot a b c d => ZO zone ot -> ZO zone (OT4 a b c d)
toZO4 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject4 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject4 o
  ZOStack SStack o -> ZOStack SStack $ toObject4 o

toZO5 :: ToObject5 ot a b c d e => ZO zone ot -> ZO zone (OT5 a b c d e)
toZO5 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject5 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject5 o
  ZOStack SStack o -> ZOStack SStack $ toObject5 o

toZO6 :: ToObject6 ot a b c d e f => ZO zone ot -> ZO zone (OT6 a b c d e f)
toZO6 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject6 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject6 o
  ZOStack SStack o -> ZOStack SStack $ toObject6 o

toZO7 ::
  ToObject7 ot a b c d e f g => ZO zone ot -> ZO zone (OT7 a b c d e f g)
toZO7 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject7 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject7 o
  ZOStack SStack o -> ZOStack SStack $ toObject7 o

toZO8 ::
  ToObject8 ot a b c d e f g h => ZO zone ot -> ZO zone (OT8 a b c d e f g h)
toZO8 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject8 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject8 o
  ZOStack SStack o -> ZOStack SStack $ toObject8 o

toZO9 ::
  ToObject9 ot a b c d e f g h i =>
  ZO zone ot ->
  ZO zone (OT9 a b c d e f g h i)
toZO9 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject9 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject9 o
  ZOStack SStack o -> ZOStack SStack $ toObject9 o

toZO10 ::
  ToObject10 ot a b c d e f g h i j =>
  ZO zone ot ->
  ZO zone (OT10 a b c d e f g h i j)
toZO10 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject10 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject10 o
  ZOStack SStack o -> ZOStack SStack $ toObject10 o

toZO11 ::
  ToObject11 ot a b c d e f g h i j k =>
  ZO zone ot ->
  ZO zone (OT11 a b c d e f g h i j k)
toZO11 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject11 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject11 o
  ZOStack SStack o -> ZOStack SStack $ toObject11 o

toZO12 ::
  ToObject12 ot a b c d e f g h i j k l =>
  ZO zone ot ->
  ZO zone (OT12 a b c d e f g h i j k l)
toZO12 = \case
  ZOBattlefield SBattlefield o -> ZOBattlefield SBattlefield $ toObject12 o
  ZOLibrary SLibrary o -> ZOLibrary SLibrary $ toObject12 o
  ZOStack SStack o -> ZOStack SStack $ toObject12 o

type OAny = ZO 'Battlefield OTAny

type OAbility = ZO 'Battlefield OTAbility

type OActivatedAbility = ZO 'Battlefield OTActivatedAbility

type OActivatedOrTriggeredAbility =
  ZO 'Battlefield OTActivatedOrTriggeredAbility

type OArtifact = ZO 'Battlefield OTArtifact

type OArtifactCreature = ZO 'Battlefield OTArtifactCreature

type OCard = ZO 'Battlefield OTCard

type OCreature = ZO 'Battlefield OTCreature

type OCreaturePlaneswalker = ZO 'Battlefield OTCreaturePlaneswalker

type OCreaturePlayer = ZO 'Battlefield OTCreaturePlayer

type OCreaturePlayerPlaneswalker =
  ZO 'Battlefield OTCreaturePlayerPlaneswalker

type ODamageSource = ZO 'Battlefield OTDamageSource

type OEmblem = ZO 'Battlefield OTEmblem

type OEnchantment = ZO 'Battlefield OTEnchantment

type OInstant = ZO 'Battlefield OTInstant

type OLand = ZO 'Battlefield OTLand

type ONonCreature = ZO 'Battlefield OTNonCreature

type OPermanent = ZO 'Battlefield OTPermanent

type OPlaneswalker = ZO 'Battlefield OTPlaneswalker

type OPlayer = ZO 'Battlefield OTPlayer

type OPlayerPlaneswalker = ZO 'Battlefield OTPlayerPlaneswalker

type OSorcery = ZO 'Battlefield OTSorcery

type OSpell = ZO 'Battlefield OTSpell

type OStaticAbility = ZO 'Battlefield OTStaticAbility

type OTriggeredAbility = ZO 'Battlefield OTTriggeredAbility

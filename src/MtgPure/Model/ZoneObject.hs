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
import safe MtgPure.Model.ToObjectN.Classes (
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

-- TODO: Place zone-specific record data into these, such as Controller and Owner information.
data ZoneObject :: Zone -> Type -> Type where
  ZOBattlefield :: SZone 'ZBattlefield -> ObjectN ot -> ZoneObject 'ZBattlefield ot
  ZOExile :: SZone 'ZExile -> ObjectN ot -> ZoneObject 'ZExile ot
  ZOGraveyard :: SZone 'ZGraveyard -> ObjectN ot -> ZoneObject 'ZGraveyard ot
  ZOHand :: SZone 'ZHand -> ObjectN ot -> ZoneObject 'ZHand ot
  ZOLibrary :: SZone 'ZLibrary -> ObjectN ot -> ZoneObject 'ZLibrary ot
  ZOStack :: SZone 'ZStack -> ObjectN ot -> ZoneObject 'ZStack ot
  deriving (Typeable)

instance ConsIndex (ZO zone ot) where
  consIndex = \case
    ZOBattlefield{} -> 1
    ZOExile{} -> 2
    ZOGraveyard{} -> 3
    ZOHand{} -> 4
    ZOLibrary{} -> 5
    ZOStack{} -> 6

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
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject1 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject1 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject1 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject1 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject1 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject1 o

toZO2 :: ToObject2 ot a b => ZO zone ot -> ZO zone (OT2 a b)
toZO2 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject2 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject2 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject2 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject2 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject2 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject2 o

toZO3 :: ToObject3 ot a b c => ZO zone ot -> ZO zone (OT3 a b c)
toZO3 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject3 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject3 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject3 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject3 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject3 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject3 o

toZO4 :: ToObject4 ot a b c d => ZO zone ot -> ZO zone (OT4 a b c d)
toZO4 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject4 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject4 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject4 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject4 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject4 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject4 o

toZO5 :: ToObject5 ot a b c d e => ZO zone ot -> ZO zone (OT5 a b c d e)
toZO5 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject5 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject5 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject5 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject5 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject5 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject5 o

toZO6 :: ToObject6 ot a b c d e f => ZO zone ot -> ZO zone (OT6 a b c d e f)
toZO6 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject6 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject6 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject6 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject6 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject6 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject6 o

toZO7 ::
  ToObject7 ot a b c d e f g => ZO zone ot -> ZO zone (OT7 a b c d e f g)
toZO7 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject7 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject7 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject7 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject7 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject7 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject7 o

toZO8 ::
  ToObject8 ot a b c d e f g h => ZO zone ot -> ZO zone (OT8 a b c d e f g h)
toZO8 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject8 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject8 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject8 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject8 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject8 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject8 o

toZO9 ::
  ToObject9 ot a b c d e f g h i =>
  ZO zone ot ->
  ZO zone (OT9 a b c d e f g h i)
toZO9 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject9 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject9 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject9 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject9 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject9 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject9 o

toZO10 ::
  ToObject10 ot a b c d e f g h i j =>
  ZO zone ot ->
  ZO zone (OT10 a b c d e f g h i j)
toZO10 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject10 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject10 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject10 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject10 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject10 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject10 o

toZO11 ::
  ToObject11 ot a b c d e f g h i j k =>
  ZO zone ot ->
  ZO zone (OT11 a b c d e f g h i j k)
toZO11 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject11 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject11 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject11 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject11 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject11 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject11 o

toZO12 ::
  ToObject12 ot a b c d e f g h i j k l =>
  ZO zone ot ->
  ZO zone (OT12 a b c d e f g h i j k l)
toZO12 = \case
  ZOBattlefield SZBattlefield o -> ZOBattlefield SZBattlefield $ toObject12 o
  ZOExile SZExile o -> ZOExile SZExile $ toObject12 o
  ZOGraveyard SZGraveyard o -> ZOGraveyard SZGraveyard $ toObject12 o
  ZOHand SZHand o -> ZOHand SZHand $ toObject12 o
  ZOLibrary SZLibrary o -> ZOLibrary SZLibrary $ toObject12 o
  ZOStack SZStack o -> ZOStack SZStack $ toObject12 o

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

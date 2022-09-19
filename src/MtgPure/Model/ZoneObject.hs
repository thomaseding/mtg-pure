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
  IsOT,
  IsZO,
  ZO,
  ZoneObject (..),
  zoToObjectN,
  toZO0,
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
  objectToZO,
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
import safe Data.Inst (
  Inst1,
 )
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Model.IsObjectType (HasSingOT (..), IsObjectType, SingOT (..))
import safe MtgPure.Model.Object (Object)
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectType (
  OT0,
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
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))

type ZO = ZoneObject

data ZoneObject (zone :: Zone) (ot :: Type) :: Type where
  ZOBattlefield :: ObjectN ot -> ZoneObject 'ZBattlefield ot
  ZOExile :: ObjectN ot -> ZoneObject 'ZExile ot
  ZOGraveyard :: ObjectN ot -> ZoneObject 'ZGraveyard ot
  ZOHand :: ObjectN ot -> ZoneObject 'ZHand ot
  ZOLibrary :: ObjectN ot -> ZoneObject 'ZLibrary ot
  ZOStack :: ObjectN ot -> ZoneObject 'ZStack ot
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
  prettyType = "ZO '" ++ sZone ++ " " ++ open ++ sOT ++ close
   where
    sZone = show (litZone @zone)
    sOT = prettyType @ot
    (open, close) = case ' ' `elem` sOT of
      True -> ("(", ")")
      False -> ("", "")

instance GetObjectId (ObjectN ot) => GetObjectId (ZO zone ot) where
  getObjectId = getObjectId . zoToObjectN

zoToObjectN :: ZO zone ot -> ObjectN ot
zoToObjectN = \case
  ZOBattlefield o -> o
  ZOExile o -> o
  ZOGraveyard o -> o
  ZOHand o -> o
  ZOLibrary o -> o
  ZOStack o -> o

class ToZO0 (zone :: Zone) (object :: Type) where
  toZO0 :: object -> ZO zone OT0

instance IsZone zone => ToZO0 zone ObjectId where
  toZO0 o = case singZone @zone of
    SZBattlefield -> ZOBattlefield $ O0 o
    SZExile -> ZOExile $ O0 o
    SZGraveyard -> ZOGraveyard $ O0 o
    SZHand -> ZOHand $ O0 o
    SZLibrary -> ZOLibrary $ O0 o
    SZStack -> ZOStack $ O0 o

instance IsZone zone => ToZO0 zone (Object ot) where
  toZO0 = toZO0 . getObjectId

instance (VisitObjectN ot, IsZone zone) => ToZO0 zone (ObjectN ot) where
  toZO0 = toZO0 . visitObjectN' getObjectId

instance (VisitObjectN ot, IsZone zone) => ToZO0 zone (ZO zone ot) where
  toZO0 = toZO0 . zoToObjectN

toZO1 :: ToObject1 ot a => ZO zone ot -> ZO zone (OT1 a)
toZO1 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject1 o
  ZOExile o -> ZOExile $ toObject1 o
  ZOGraveyard o -> ZOGraveyard $ toObject1 o
  ZOHand o -> ZOHand $ toObject1 o
  ZOLibrary o -> ZOLibrary $ toObject1 o
  ZOStack o -> ZOStack $ toObject1 o

toZO2 :: ToObject2 ot a b => ZO zone ot -> ZO zone (OT2 a b)
toZO2 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject2 o
  ZOExile o -> ZOExile $ toObject2 o
  ZOGraveyard o -> ZOGraveyard $ toObject2 o
  ZOHand o -> ZOHand $ toObject2 o
  ZOLibrary o -> ZOLibrary $ toObject2 o
  ZOStack o -> ZOStack $ toObject2 o

toZO3 :: ToObject3 ot a b c => ZO zone ot -> ZO zone (OT3 a b c)
toZO3 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject3 o
  ZOExile o -> ZOExile $ toObject3 o
  ZOGraveyard o -> ZOGraveyard $ toObject3 o
  ZOHand o -> ZOHand $ toObject3 o
  ZOLibrary o -> ZOLibrary $ toObject3 o
  ZOStack o -> ZOStack $ toObject3 o

toZO4 :: ToObject4 ot a b c d => ZO zone ot -> ZO zone (OT4 a b c d)
toZO4 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject4 o
  ZOExile o -> ZOExile $ toObject4 o
  ZOGraveyard o -> ZOGraveyard $ toObject4 o
  ZOHand o -> ZOHand $ toObject4 o
  ZOLibrary o -> ZOLibrary $ toObject4 o
  ZOStack o -> ZOStack $ toObject4 o

toZO5 :: ToObject5 ot a b c d e => ZO zone ot -> ZO zone (OT5 a b c d e)
toZO5 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject5 o
  ZOExile o -> ZOExile $ toObject5 o
  ZOGraveyard o -> ZOGraveyard $ toObject5 o
  ZOHand o -> ZOHand $ toObject5 o
  ZOLibrary o -> ZOLibrary $ toObject5 o
  ZOStack o -> ZOStack $ toObject5 o

toZO6 :: ToObject6 ot a b c d e f => ZO zone ot -> ZO zone (OT6 a b c d e f)
toZO6 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject6 o
  ZOExile o -> ZOExile $ toObject6 o
  ZOGraveyard o -> ZOGraveyard $ toObject6 o
  ZOHand o -> ZOHand $ toObject6 o
  ZOLibrary o -> ZOLibrary $ toObject6 o
  ZOStack o -> ZOStack $ toObject6 o

toZO7 ::
  ToObject7 ot a b c d e f g => ZO zone ot -> ZO zone (OT7 a b c d e f g)
toZO7 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject7 o
  ZOExile o -> ZOExile $ toObject7 o
  ZOGraveyard o -> ZOGraveyard $ toObject7 o
  ZOHand o -> ZOHand $ toObject7 o
  ZOLibrary o -> ZOLibrary $ toObject7 o
  ZOStack o -> ZOStack $ toObject7 o

toZO8 ::
  ToObject8 ot a b c d e f g h => ZO zone ot -> ZO zone (OT8 a b c d e f g h)
toZO8 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject8 o
  ZOExile o -> ZOExile $ toObject8 o
  ZOGraveyard o -> ZOGraveyard $ toObject8 o
  ZOHand o -> ZOHand $ toObject8 o
  ZOLibrary o -> ZOLibrary $ toObject8 o
  ZOStack o -> ZOStack $ toObject8 o

toZO9 ::
  ToObject9 ot a b c d e f g h i =>
  ZO zone ot ->
  ZO zone (OT9 a b c d e f g h i)
toZO9 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject9 o
  ZOExile o -> ZOExile $ toObject9 o
  ZOGraveyard o -> ZOGraveyard $ toObject9 o
  ZOHand o -> ZOHand $ toObject9 o
  ZOLibrary o -> ZOLibrary $ toObject9 o
  ZOStack o -> ZOStack $ toObject9 o

toZO10 ::
  ToObject10 ot a b c d e f g h i j =>
  ZO zone ot ->
  ZO zone (OT10 a b c d e f g h i j)
toZO10 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject10 o
  ZOExile o -> ZOExile $ toObject10 o
  ZOGraveyard o -> ZOGraveyard $ toObject10 o
  ZOHand o -> ZOHand $ toObject10 o
  ZOLibrary o -> ZOLibrary $ toObject10 o
  ZOStack o -> ZOStack $ toObject10 o

toZO11 ::
  ToObject11 ot a b c d e f g h i j k =>
  ZO zone ot ->
  ZO zone (OT11 a b c d e f g h i j k)
toZO11 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject11 o
  ZOExile o -> ZOExile $ toObject11 o
  ZOGraveyard o -> ZOGraveyard $ toObject11 o
  ZOHand o -> ZOHand $ toObject11 o
  ZOLibrary o -> ZOLibrary $ toObject11 o
  ZOStack o -> ZOStack $ toObject11 o

toZO12 ::
  ToObject12 ot a b c d e f g h i j k l =>
  ZO zone ot ->
  ZO zone (OT12 a b c d e f g h i j k l)
toZO12 = \case
  ZOBattlefield o -> ZOBattlefield $ toObject12 o
  ZOExile o -> ZOExile $ toObject12 o
  ZOGraveyard o -> ZOGraveyard $ toObject12 o
  ZOHand o -> ZOHand $ toObject12 o
  ZOLibrary o -> ZOLibrary $ toObject12 o
  ZOStack o -> ZOStack $ toObject12 o

type IsOT (ot :: Type) =
  ( IndexOT ot
  , VisitObjectN ot
  , PrettyType ot
  , HasSingOT ot
  , GetObjectId (ObjectN ot)
  )

type IsZO (zone :: Zone) (ot :: Type) =
  ( IsOT ot
  , IsZone zone
  , PrettyType (ZO zone ot)
  )

objectToZO :: forall z zone ot. (IsObjectType z, IsZO zone ot) => Object z -> Maybe (ZO zone ot)
objectToZO o = case singOT @ot of
  ot@OT1 ->
    let go :: forall a. Inst1 IsObjectType a => SingOT (OT1 a) -> Maybe (ZO zone (OT1 a))
        go _ = case cast o of
          Nothing -> Nothing
          Just o' ->
            let obj1 = O1 @a o'
                zo1 = case singZone @zone of
                  SZBattlefield -> ZOBattlefield obj1
                  SZExile -> undefined
                  SZGraveyard -> undefined
                  SZHand -> undefined
                  SZLibrary -> undefined
                  SZStack -> undefined
             in Just zo1
     in go ot
  ot@OT2 -> undefined ot
  ot@OT3 -> undefined ot
  ot@OT4 -> undefined ot
  ot@OT5 -> undefined ot
  ot@OT6 -> undefined ot
  ot@OT7 -> undefined ot
  ot@OT8 -> undefined ot
  ot@OT9 -> undefined ot
  ot@OT10 -> undefined ot
  ot@OT11 -> undefined ot
  ot@OT12 -> undefined ot

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

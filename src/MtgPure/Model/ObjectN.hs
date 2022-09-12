{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.ObjectN
  ( ObjectN (..),
    OAbility,
    OActivatedAbility,
    OActivatedOrTriggeredAbility,
    OAny,
    OArtifact,
    OArtifactCreature,
    OCreature,
    OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OEmblem,
    OEnchantment,
    OInstant,
    OLand,
    OPermanent,
    OPlaneswalker,
    OPlayer,
    OPlayerPlaneswalker,
    OSorcery,
    OSpell,
    OStaticAbility,
    OTriggeredAbility,
  )
where

import Data.Inst
  ( Inst1,
    Inst10,
    Inst11,
    Inst12,
    Inst2,
    Inst3,
    Inst4,
    Inst5,
    Inst6,
    Inst7,
    Inst8,
    Inst9,
  )
import Data.Kind (Type)
import Data.Typeable (Typeable)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.Object (Object)
import MtgPure.Model.ObjectType
  ( OTAbility,
    OTActivatedAbility,
    OTActivatedOrTriggeredAbility,
    OTAny,
    OTArtifact,
    OTArtifactCreature,
    OTCreature,
    OTCreaturePlaneswalker,
    OTCreaturePlayer,
    OTCreaturePlayerPlaneswalker,
    OTDamageSource,
    OTEmblem,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPermanent,
    OTPlaneswalker,
    OTPlayer,
    OTPlayerPlaneswalker,
    OTSorcery,
    OTSpell,
    OTStaticAbility,
    OTTriggeredAbility,
    ObjectType,
  )

type OAny = ObjectN OTAny

type OAbility = ObjectN OTAbility

type OActivatedAbility = ObjectN OTActivatedAbility

type OActivatedOrTriggeredAbility = ObjectN OTActivatedOrTriggeredAbility

type OArtifact = ObjectN OTArtifact

type OArtifactCreature = ObjectN OTArtifactCreature

type OCreature = ObjectN OTCreature

type OCreaturePlaneswalker = ObjectN OTCreaturePlaneswalker

type OCreaturePlayer = ObjectN OTCreaturePlayer

type OCreaturePlayerPlaneswalker = ObjectN OTCreaturePlayerPlaneswalker

type ODamageSource = ObjectN OTDamageSource

type OEmblem = ObjectN OTEmblem

type OEnchantment = ObjectN OTEnchantment

type OInstant = ObjectN OTInstant

type OLand = ObjectN OTLand

type OPermanent = ObjectN OTPermanent

type OPlaneswalker = ObjectN OTPlaneswalker

type OPlayer = ObjectN OTPlayer

type OPlayerPlaneswalker = ObjectN OTPlayerPlaneswalker

type OSorcery = ObjectN OTSorcery

type OSpell = ObjectN OTSpell

type OStaticAbility = ObjectN OTStaticAbility

type OTriggeredAbility = ObjectN OTTriggeredAbility

data family ObjectN (a :: k)

-- TODO:
-- The constructors should be private to disallow pattern matching during authoring
-- Supply factory constructors for authoring

data instance ObjectN :: ObjectType -> Type where
  O :: Inst1 IsObjectType a => Object a -> ObjectN (a :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType) -> Type where
  O2a :: Inst2 IsObjectType a b => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType)
  O2b :: Inst2 IsObjectType a b => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType)
  ON2a :: Inst2 IsObjectType a b => ObjectN b -> ObjectN '(a :: ObjectType, b :: ObjectType)
  ON2b :: Inst2 IsObjectType a b => ObjectN a -> ObjectN '(a :: ObjectType, b :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType) -> Type where
  O3b :: Inst3 IsObjectType a b c => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType)
  O3a :: Inst3 IsObjectType a b c => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType)
  O3c :: Inst3 IsObjectType a b c => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType)
  ON3a :: Inst3 IsObjectType a b c => ObjectN '(b, c) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType)
  ON3b :: Inst3 IsObjectType a b c => ObjectN '(a, c) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType)
  ON3c :: Inst3 IsObjectType a b c => ObjectN '(a, b) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O4a :: Inst4 IsObjectType a b c d => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  O4b :: Inst4 IsObjectType a b c d => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  O4c :: Inst4 IsObjectType a b c d => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  O4d :: Inst4 IsObjectType a b c d => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4a :: Inst4 IsObjectType a b c d => ObjectN '(b, c, d) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4b :: Inst4 IsObjectType a b c d => ObjectN '(a, c, d) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4c :: Inst4 IsObjectType a b c d => ObjectN '(a, b, d) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4d :: Inst4 IsObjectType a b c d => ObjectN '(a, b, c) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O5b :: Inst5 IsObjectType a b c d e => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5a :: Inst5 IsObjectType a b c d e => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5c :: Inst5 IsObjectType a b c d e => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5d :: Inst5 IsObjectType a b c d e => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5e :: Inst5 IsObjectType a b c d e => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5a :: Inst5 IsObjectType a b c d e => ObjectN '(b, c, d, e) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5b :: Inst5 IsObjectType a b c d e => ObjectN '(a, c, d, e) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5c :: Inst5 IsObjectType a b c d e => ObjectN '(a, b, d, e) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5d :: Inst5 IsObjectType a b c d e => ObjectN '(a, b, c, e) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5e :: Inst5 IsObjectType a b c d e => ObjectN '(a, b, c, d) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O6a :: Inst6 IsObjectType a b c d e f => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6b :: Inst6 IsObjectType a b c d e f => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6c :: Inst6 IsObjectType a b c d e f => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6d :: Inst6 IsObjectType a b c d e f => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6e :: Inst6 IsObjectType a b c d e f => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6f :: Inst6 IsObjectType a b c d e f => Object f -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6a :: Inst6 IsObjectType a b c d e f => ObjectN '(b, c, d, e, f) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6b :: Inst6 IsObjectType a b c d e f => ObjectN '(a, c, d, e, f) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6c :: Inst6 IsObjectType a b c d e f => ObjectN '(a, b, d, e, f) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6d :: Inst6 IsObjectType a b c d e f => ObjectN '(a, b, c, e, f) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6e :: Inst6 IsObjectType a b c d e f => ObjectN '(a, b, c, d, f) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6f :: Inst6 IsObjectType a b c d e f => ObjectN '(a, b, c, d, e) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O7a :: Inst7 IsObjectType a b c d e f g => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7b :: Inst7 IsObjectType a b c d e f g => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7c :: Inst7 IsObjectType a b c d e f g => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7d :: Inst7 IsObjectType a b c d e f g => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7e :: Inst7 IsObjectType a b c d e f g => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7f :: Inst7 IsObjectType a b c d e f g => Object f -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7g :: Inst7 IsObjectType a b c d e f g => Object g -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7a :: Inst7 IsObjectType a b c d e f g => ObjectN '(b, c, d, e, f, g) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7b :: Inst7 IsObjectType a b c d e f g => ObjectN '(a, c, d, e, f, g) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7c :: Inst7 IsObjectType a b c d e f g => ObjectN '(a, b, d, e, f, g) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7d :: Inst7 IsObjectType a b c d e f g => ObjectN '(a, b, c, e, f, g) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7e :: Inst7 IsObjectType a b c d e f g => ObjectN '(a, b, c, d, f, g) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7f :: Inst7 IsObjectType a b c d e f g => ObjectN '(a, b, c, d, e, g) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7g :: Inst7 IsObjectType a b c d e f g => ObjectN '(a, b, c, d, e, f) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O8a :: Inst8 IsObjectType a b c d e f g h => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8b :: Inst8 IsObjectType a b c d e f g h => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8c :: Inst8 IsObjectType a b c d e f g h => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8d :: Inst8 IsObjectType a b c d e f g h => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8e :: Inst8 IsObjectType a b c d e f g h => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8f :: Inst8 IsObjectType a b c d e f g h => Object f -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8g :: Inst8 IsObjectType a b c d e f g h => Object g -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8h :: Inst8 IsObjectType a b c d e f g h => Object h -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8a :: Inst8 IsObjectType a b c d e f g h => ObjectN '(b, c, d, e, f, g, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8b :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, c, d, e, f, g, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8c :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, b, d, e, f, g, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8d :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, b, c, e, f, g, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8e :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, b, c, d, f, g, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8f :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, b, c, d, e, g, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8g :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, b, c, d, e, f, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8h :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, b, c, d, e, f, g) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O9a :: Inst9 IsObjectType a b c d e f g h i => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9b :: Inst9 IsObjectType a b c d e f g h i => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9c :: Inst9 IsObjectType a b c d e f g h i => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9d :: Inst9 IsObjectType a b c d e f g h i => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9e :: Inst9 IsObjectType a b c d e f g h i => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9f :: Inst9 IsObjectType a b c d e f g h i => Object f -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9g :: Inst9 IsObjectType a b c d e f g h i => Object g -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9h :: Inst9 IsObjectType a b c d e f g h i => Object h -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9i :: Inst9 IsObjectType a b c d e f g h i => Object i -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9a :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(b, c, d, e, f, g, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9b :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, c, d, e, f, g, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9c :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, b, d, e, f, g, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9d :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, b, c, e, f, g, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9e :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, b, c, d, f, g, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9f :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, b, c, d, e, g, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9g :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, b, c, d, e, f, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9h :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, b, c, d, e, f, g, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9i :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(a, b, c, d, e, f, g, h) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O10b :: Inst10 IsObjectType a b c d e f g h i j => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10a :: Inst10 IsObjectType a b c d e f g h i j => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10c :: Inst10 IsObjectType a b c d e f g h i j => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10d :: Inst10 IsObjectType a b c d e f g h i j => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10e :: Inst10 IsObjectType a b c d e f g h i j => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10f :: Inst10 IsObjectType a b c d e f g h i j => Object f -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10g :: Inst10 IsObjectType a b c d e f g h i j => Object g -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10h :: Inst10 IsObjectType a b c d e f g h i j => Object h -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10i :: Inst10 IsObjectType a b c d e f g h i j => Object i -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10j :: Inst10 IsObjectType a b c d e f g h i j => Object j -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10a :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(b, c, d, e, f, g, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10b :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, c, d, e, f, g, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10c :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, d, e, f, g, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10d :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, c, e, f, g, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10e :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, c, d, f, g, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10f :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, c, d, e, g, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10g :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, c, d, e, f, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10h :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, c, d, e, f, g, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10i :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, c, d, e, f, g, h, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10j :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(a, b, c, d, e, f, g, h, i) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O11b :: Inst11 IsObjectType a b c d e f g h i j k => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11a :: Inst11 IsObjectType a b c d e f g h i j k => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11c :: Inst11 IsObjectType a b c d e f g h i j k => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11d :: Inst11 IsObjectType a b c d e f g h i j k => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11e :: Inst11 IsObjectType a b c d e f g h i j k => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11f :: Inst11 IsObjectType a b c d e f g h i j k => Object f -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11g :: Inst11 IsObjectType a b c d e f g h i j k => Object g -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11h :: Inst11 IsObjectType a b c d e f g h i j k => Object h -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11i :: Inst11 IsObjectType a b c d e f g h i j k => Object i -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11j :: Inst11 IsObjectType a b c d e f g h i j k => Object j -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11k :: Inst11 IsObjectType a b c d e f g h i j k => Object k -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11a :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(b, c, d, e, f, g, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11b :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, c, d, e, f, g, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11c :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, d, e, f, g, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11d :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, e, f, g, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11e :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, d, f, g, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11f :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, d, e, g, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11g :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, d, e, f, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11h :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, d, e, f, g, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11i :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, d, e, f, g, h, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11j :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, d, e, f, g, h, i, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11k :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(a, b, c, d, e, f, g, h, i, j) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: (ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType) -> Type where
  O12b :: Inst12 IsObjectType a b c d e f g h i j k l => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12a :: Inst12 IsObjectType a b c d e f g h i j k l => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12c :: Inst12 IsObjectType a b c d e f g h i j k l => Object c -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12d :: Inst12 IsObjectType a b c d e f g h i j k l => Object d -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12e :: Inst12 IsObjectType a b c d e f g h i j k l => Object e -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12f :: Inst12 IsObjectType a b c d e f g h i j k l => Object f -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12g :: Inst12 IsObjectType a b c d e f g h i j k l => Object g -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12h :: Inst12 IsObjectType a b c d e f g h i j k l => Object h -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12i :: Inst12 IsObjectType a b c d e f g h i j k l => Object i -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12j :: Inst12 IsObjectType a b c d e f g h i j k l => Object j -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12k :: Inst12 IsObjectType a b c d e f g h i j k l => Object k -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12l :: Inst12 IsObjectType a b c d e f g h i j k l => Object l -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12a :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(b, c, d, e, f, g, h, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12b :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, c, d, e, f, g, h, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12c :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, d, e, f, g, h, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12d :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, e, f, g, h, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12e :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, f, g, h, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12f :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, e, g, h, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12g :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, e, f, h, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12h :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, e, f, g, i, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12i :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, e, f, g, h, j, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12j :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, e, f, g, h, i, k, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12k :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, e, f, g, h, i, j, l) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12l :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(a, b, c, d, e, f, g, h, i, j, k) -> ObjectN '(a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  deriving (Typeable)

deriving instance Inst1 IsObjectType a => Show (ObjectN a)

deriving instance Inst2 IsObjectType a b => Show (ObjectN '(a, b))

deriving instance Inst3 IsObjectType a b c => Show (ObjectN '(a, b, c))

deriving instance Inst4 IsObjectType a b c d => Show (ObjectN '(a, b, c, d))

deriving instance Inst5 IsObjectType a b c d e => Show (ObjectN '(a, b, c, d, e))

deriving instance Inst6 IsObjectType a b c d e f => Show (ObjectN '(a, b, c, d, e, f))

deriving instance Inst7 IsObjectType a b c d e f g => Show (ObjectN '(a, b, c, d, e, f, g))

deriving instance Inst8 IsObjectType a b c d e f g h => Show (ObjectN '(a, b, c, d, e, f, g, h))

deriving instance Inst9 IsObjectType a b c d e f g h i => Show (ObjectN '(a, b, c, d, e, f, g, h, i))

deriving instance Inst10 IsObjectType a b c d e f g h i j => Show (ObjectN '(a, b, c, d, e, f, g, h, i, j))

deriving instance Inst11 IsObjectType a b c d e f g h i j k => Show (ObjectN '(a, b, c, d, e, f, g, h, i, j, k))

deriving instance Inst12 IsObjectType a b c d e f g h i j k l => Show (ObjectN '(a, b, c, d, e, f, g, h, i, j, k, l))

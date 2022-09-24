{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object (
  ObjectType (..),
  OT (..),
  OT0,
  OT1,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OT8,
  OT9,
  OT10,
  OT11,
  OT12,
  OTK0,
  OTK1,
  OTK2,
  OTK3,
  OTK4,
  OTK5,
  OTK6,
  OTK7,
  OTK8,
  OTK9,
  OTK10,
  OTK11,
  OTK12,
  ObjectType0,
  ObjectType1,
  ObjectType2,
  ObjectType3,
  ObjectType4,
  ObjectType5,
  ObjectType6,
  ObjectType7,
  ObjectType8,
  ObjectType9,
  ObjectType10,
  ObjectType11,
  ObjectType12,
  SObjectType (..),
  IsObjectType (..),
  ObjectVisitor (..),
  visitObject',
  LitOT (..),
  ObjectDiscriminant' (..),
  ObjectDiscriminant,
  pattern DefaultObjectDiscriminant,
  Object (..),
) where

import safe Data.Inst (
  Inst1,
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
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId)

-- This generalizes the notion of "object" in MTG (e.g. contains "player")
--
-- Don't bother encoding OTManaAbility; since mana abilities are activated/triggered abilities and cannot be interacted with (605.3b)
--
-- If combinatorial explosion starts to be an issue, just stop the ToObjectN limit to that of 12 or so and also special case those
-- types to directly jump to ToObjectAny. Then it's the client's responsibility to provide any obtuse instances for ToObjectN.
data ObjectType
  = OTActivatedAbility
  | OTArtifact
  | OTCreature
  | OTEmblem
  | OTEnchantment
  | OTInstant
  | OTLand
  | OTPlaneswalker
  | OTPlayer
  | OTSorcery
  | OTStaticAbility
  | OTTriggeredAbility
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

data SObjectType :: ObjectType -> Type where
  SActivatedAbility :: SObjectType 'OTActivatedAbility
  SArtifact :: SObjectType 'OTArtifact
  SCreature :: SObjectType 'OTCreature
  SEmblem :: SObjectType 'OTEmblem
  SEnchantment :: SObjectType 'OTEnchantment
  SInstant :: SObjectType 'OTInstant
  SLand :: SObjectType 'OTLand
  SPlaneswalker :: SObjectType 'OTPlaneswalker
  SPlayer :: SObjectType 'OTPlayer
  SSorcery :: SObjectType 'OTSorcery
  SStaticAbility :: SObjectType 'OTStaticAbility
  STriggeredAbility :: SObjectType 'OTTriggeredAbility
  deriving (Typeable)

deriving instance Eq (SObjectType a)

deriving instance Ord (SObjectType a)

deriving instance Show (SObjectType a)

type ObjectType0 = ()

type ObjectType1 = ((), ObjectType)

type ObjectType2 = ((), ObjectType, ObjectType)

type ObjectType3 = ((), ObjectType, ObjectType, ObjectType)

type ObjectType4 = ((), ObjectType, ObjectType, ObjectType, ObjectType)

type ObjectType5 =
  ((), ObjectType, ObjectType, ObjectType, ObjectType, ObjectType)

type ObjectType6 =
  ((), ObjectType, ObjectType, ObjectType, ObjectType, ObjectType, ObjectType)

type ObjectType7 =
  ( ()
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  )

type ObjectType8 =
  ( ()
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  )

type ObjectType9 =
  ( ()
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  )

type ObjectType10 =
  ( ()
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  )

type ObjectType11 =
  ( ()
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  )

type ObjectType12 =
  ( ()
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  , ObjectType
  )

type OTK0 = '()

type OTK1 a = '( '(), a :: ObjectType)

type OTK2 a b = '( '(), a :: ObjectType, b :: ObjectType)

type OTK3 a b c = '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType)

type OTK4 a b c d =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)

type OTK5 a b c d e =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)

type OTK6 a b c d e f =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)

type OTK7 a b c d e f g =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)

type OTK8 a b c d e f g h =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)

type OTK9 a b c d e f g h i =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)

type OTK10 a b c d e f g h i j =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)

type OTK11 a b c d e f g h i j k =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)

type OTK12 a b c d e f g h i j k l =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)

data OT (otk :: k) :: Type where
  OT0 :: OT0
  OT1 :: Inst1 IsObjectType a => OT1 a
  OT2 :: Inst2 IsObjectType a b => OT2 a b
  OT3 :: Inst3 IsObjectType a b c => OT3 a b c
  OT4 :: Inst4 IsObjectType a b c d => OT4 a b c d
  OT5 :: Inst5 IsObjectType a b c d e => OT5 a b c d e
  OT6 :: Inst6 IsObjectType a b c d e f => OT6 a b c d e f
  OT7 :: Inst7 IsObjectType a b c d e f g => OT7 a b c d e f g
  OT8 :: Inst8 IsObjectType a b c d e f g h => OT8 a b c d e f g h
  OT9 :: Inst9 IsObjectType a b c d e f g h i => OT9 a b c d e f g h i
  OT10 :: Inst10 IsObjectType a b c d e f g h i j => OT10 a b c d e f g h i j
  OT11 :: Inst11 IsObjectType a b c d e f g h i j k => OT11 a b c d e f g h i j k
  OT12 :: Inst12 IsObjectType a b c d e f g h i j k l => OT12 a b c d e f g h i j k l

type OT0 = OT OTK0

type OT1 a = OT (OTK1 a)

type OT2 a b = OT (OTK2 a b)

type OT3 a b c = OT (OTK3 a b c)

type OT4 a b c d = OT (OTK4 a b c d)

type OT5 a b c d e = OT (OTK5 a b c d e)

type OT6 a b c d e f = OT (OTK6 a b c d e f)

type OT7 a b c d e f g = OT (OTK7 a b c d e f g)

type OT8 a b c d e f g h = OT (OTK8 a b c d e f g h)

type OT9 a b c d e f g h i = OT (OTK9 a b c d e f g h i)

type OT10 a b c d e f g h i j = OT (OTK10 a b c d e f g h i j)

type OT11 a b c d e f g h i j k = OT (OTK11 a b c d e f g h i j k)

type OT12 a b c d e f g h i j k l = OT (OTK12 a b c d e f g h i j k l)

data ObjectVisitor a = ObjectVisitor
  { visitOActivatedAbility :: Object 'OTActivatedAbility -> a
  , visitOArtifact :: Object 'OTArtifact -> a
  , visitOCreature :: Object 'OTCreature -> a
  , visitOEmblem :: Object 'OTEmblem -> a
  , visitOEnchantment :: Object 'OTEnchantment -> a
  , visitOInstant :: Object 'OTInstant -> a
  , visitOLand :: Object 'OTLand -> a
  , visitOPlaneswalker :: Object 'OTPlaneswalker -> a
  , visitOPlayer :: Object 'OTPlayer -> a
  , visitOSorcery :: Object 'OTSorcery -> a
  , visitOStaticAbility :: Object 'OTStaticAbility -> a
  , visitOTriggeredAbility :: Object 'OTTriggeredAbility -> a
  }
  deriving (Typeable)

class Typeable a => IsObjectType (a :: ObjectType) where
  idToObject :: ObjectId -> Object a
  objectToId :: Object a -> ObjectId
  singObjectType :: SObjectType a
  litObjectType :: ObjectType
  visitObject :: ObjectVisitor b -> Object a -> b

visitObject' ::
  IsObjectType a =>
  (forall b. IsObjectType b => Object b -> x) ->
  Object a ->
  x
visitObject' f = visitObject $ ObjectVisitor f f f f f f f f f f f f

instance IsObjectType 'OTActivatedAbility where
  idToObject = Object SActivatedAbility DefaultObjectDiscriminant
  objectToId (Object SActivatedAbility _ i) = i
  singObjectType = SActivatedAbility
  litObjectType = OTActivatedAbility
  visitObject = visitOActivatedAbility

instance IsObjectType 'OTArtifact where
  idToObject = Object SArtifact DefaultObjectDiscriminant
  objectToId (Object SArtifact _ i) = i
  singObjectType = SArtifact
  litObjectType = OTArtifact
  visitObject = visitOArtifact

instance IsObjectType 'OTCreature where
  idToObject = Object SCreature DefaultObjectDiscriminant
  objectToId (Object SCreature _ i) = i
  singObjectType = SCreature
  litObjectType = OTCreature
  visitObject = visitOCreature

instance IsObjectType 'OTEmblem where
  idToObject = Object SEmblem DefaultObjectDiscriminant
  objectToId (Object SEmblem _ i) = i
  singObjectType = SEmblem
  litObjectType = OTEmblem
  visitObject = visitOEmblem

instance IsObjectType 'OTEnchantment where
  idToObject = Object SEnchantment DefaultObjectDiscriminant
  objectToId (Object SEnchantment _ i) = i
  singObjectType = SEnchantment
  litObjectType = OTEnchantment
  visitObject = visitOEnchantment

instance IsObjectType 'OTInstant where
  idToObject = Object SInstant DefaultObjectDiscriminant
  objectToId (Object SInstant _ i) = i
  singObjectType = SInstant
  litObjectType = OTInstant
  visitObject = visitOInstant

instance IsObjectType 'OTLand where
  idToObject = Object SLand DefaultObjectDiscriminant
  objectToId (Object SLand _ i) = i
  singObjectType = SLand
  litObjectType = OTLand
  visitObject = visitOLand

instance IsObjectType 'OTPlaneswalker where
  idToObject = Object SPlaneswalker DefaultObjectDiscriminant
  objectToId (Object SPlaneswalker _ i) = i
  singObjectType = SPlaneswalker
  litObjectType = OTPlaneswalker
  visitObject = visitOPlaneswalker

instance IsObjectType 'OTPlayer where
  idToObject = Object SPlayer DefaultObjectDiscriminant
  objectToId (Object SPlayer _ i) = i
  singObjectType = SPlayer
  litObjectType = OTPlayer
  visitObject = visitOPlayer

instance IsObjectType 'OTSorcery where
  idToObject = Object SSorcery DefaultObjectDiscriminant
  objectToId (Object SSorcery _ i) = i
  singObjectType = SSorcery
  litObjectType = OTSorcery
  visitObject = visitOSorcery

instance IsObjectType 'OTStaticAbility where
  idToObject = Object SStaticAbility DefaultObjectDiscriminant
  objectToId (Object SStaticAbility _ i) = i
  singObjectType = SStaticAbility
  litObjectType = OTStaticAbility
  visitObject = visitOStaticAbility

instance IsObjectType 'OTTriggeredAbility where
  idToObject = Object STriggeredAbility DefaultObjectDiscriminant
  objectToId (Object STriggeredAbility _ i) = i
  singObjectType = STriggeredAbility
  litObjectType = OTTriggeredAbility
  visitObject = visitOTriggeredAbility

class LitOT (ot :: Type) where
  litOT :: ot
  mapOT :: (forall k (otk :: k). OT otk -> liftOT (OT otk)) -> liftOT ot

instance LitOT OT0 where
  litOT = OT0
  mapOT f = f litOT

instance Inst1 IsObjectType a => LitOT (OT1 a) where
  litOT = OT1
  mapOT f = f litOT

instance Inst2 IsObjectType a b => LitOT (OT2 a b) where
  litOT = OT2
  mapOT f = f litOT

instance Inst3 IsObjectType a b c => LitOT (OT3 a b c) where
  litOT = OT3
  mapOT f = f litOT

instance Inst4 IsObjectType a b c d => LitOT (OT4 a b c d) where
  litOT = OT4
  mapOT f = f litOT

instance Inst5 IsObjectType a b c d e => LitOT (OT5 a b c d e) where
  litOT = OT5
  mapOT f = f litOT

instance Inst6 IsObjectType a b c d e f => LitOT (OT6 a b c d e f) where
  litOT = OT6
  mapOT f = f litOT

instance Inst7 IsObjectType a b c d e f g => LitOT (OT7 a b c d e f g) where
  litOT = OT7
  mapOT f = f litOT

instance Inst8 IsObjectType a b c d e f g h => LitOT (OT8 a b c d e f g h) where
  litOT = OT8
  mapOT f = f litOT

instance Inst9 IsObjectType a b c d e f g h i => LitOT (OT9 a b c d e f g h i) where
  litOT = OT9
  mapOT f = f litOT

instance Inst10 IsObjectType a b c d e f g h i j => LitOT (OT10 a b c d e f g h i j) where
  litOT = OT10
  mapOT f = f litOT

instance Inst11 IsObjectType a b c d e f g h i j k => LitOT (OT11 a b c d e f g h i j k) where
  litOT = OT11
  mapOT f = f litOT

instance Inst12 IsObjectType a b c d e f g h i j k l => LitOT (OT12 a b c d e f g h i j k l) where
  litOT = OT12
  mapOT f = f litOT

newtype ObjectDiscriminant' a = ObjectDiscriminant a
  deriving (Eq, Functor, Ord, Show, Typeable)

type ObjectDiscriminant = ObjectDiscriminant' Int

pattern DefaultObjectDiscriminant :: ObjectDiscriminant
pattern DefaultObjectDiscriminant = ObjectDiscriminant 0

data Object :: ObjectType -> Type where
  Object :: SObjectType a -> ObjectDiscriminant -> ObjectId -> Object a
  deriving (Show, Typeable)

instance Eq (Object ot) where
  Object _ _ i == Object _ _ j = i == j

instance Ord (Object ot) where
  compare (Object _ _ i) (Object _ _ j) = compare i j

instance GetObjectId (Object ot) where
  getObjectId (Object _ _ i) = i

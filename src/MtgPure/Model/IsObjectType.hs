{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.IsObjectType (
  IsObjectType (..),
  ObjectVisitor (..),
  visitObject',
  SingOT (..),
  HasSingOT (..),
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
import safe MtgPure.Model.Object (Object (Object))
import safe MtgPure.Model.ObjectId (ObjectId)
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
  ObjectType (..),
  SObjectType (..),
 )

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
  idToObject = Object SActivatedAbility
  objectToId (Object SActivatedAbility i) = i
  singObjectType = SActivatedAbility
  litObjectType = OTActivatedAbility
  visitObject = visitOActivatedAbility

instance IsObjectType 'OTArtifact where
  idToObject = Object SArtifact
  objectToId (Object SArtifact i) = i
  singObjectType = SArtifact
  litObjectType = OTArtifact
  visitObject = visitOArtifact

instance IsObjectType 'OTCreature where
  idToObject = Object SCreature
  objectToId (Object SCreature i) = i
  singObjectType = SCreature
  litObjectType = OTCreature
  visitObject = visitOCreature

instance IsObjectType 'OTEmblem where
  idToObject = Object SEmblem
  objectToId (Object SEmblem i) = i
  singObjectType = SEmblem
  litObjectType = OTEmblem
  visitObject = visitOEmblem

instance IsObjectType 'OTEnchantment where
  idToObject = Object SEnchantment
  objectToId (Object SEnchantment i) = i
  singObjectType = SEnchantment
  litObjectType = OTEnchantment
  visitObject = visitOEnchantment

instance IsObjectType 'OTInstant where
  idToObject = Object SInstant
  objectToId (Object SInstant i) = i
  singObjectType = SInstant
  litObjectType = OTInstant
  visitObject = visitOInstant

instance IsObjectType 'OTLand where
  idToObject = Object SLand
  objectToId (Object SLand i) = i
  singObjectType = SLand
  litObjectType = OTLand
  visitObject = visitOLand

instance IsObjectType 'OTPlaneswalker where
  idToObject = Object SPlaneswalker
  objectToId (Object SPlaneswalker i) = i
  singObjectType = SPlaneswalker
  litObjectType = OTPlaneswalker
  visitObject = visitOPlaneswalker

instance IsObjectType 'OTPlayer where
  idToObject = Object SPlayer
  objectToId (Object SPlayer i) = i
  singObjectType = SPlayer
  litObjectType = OTPlayer
  visitObject = visitOPlayer

instance IsObjectType 'OTSorcery where
  idToObject = Object SSorcery
  objectToId (Object SSorcery i) = i
  singObjectType = SSorcery
  litObjectType = OTSorcery
  visitObject = visitOSorcery

instance IsObjectType 'OTStaticAbility where
  idToObject = Object SStaticAbility
  objectToId (Object SStaticAbility i) = i
  singObjectType = SStaticAbility
  litObjectType = OTStaticAbility
  visitObject = visitOStaticAbility

instance IsObjectType 'OTTriggeredAbility where
  idToObject = Object STriggeredAbility
  objectToId (Object STriggeredAbility i) = i
  singObjectType = STriggeredAbility
  litObjectType = OTTriggeredAbility
  visitObject = visitOTriggeredAbility

data SingOT (ot :: Type) :: Type where
  -- Prolly dont want to include OT0 in this to avoid some ambiguous (Object z) seeding point
  OT1 :: Inst1 IsObjectType a => SingOT (OT1 a)
  OT2 :: Inst2 IsObjectType a b => SingOT (OT2 a b)
  OT3 :: Inst3 IsObjectType a b c => SingOT (OT3 a b c)
  OT4 :: Inst4 IsObjectType a b c d => SingOT (OT4 a b c d)
  OT5 :: Inst5 IsObjectType a b c d e => SingOT (OT5 a b c d e)
  OT6 :: Inst6 IsObjectType a b c d e f => SingOT (OT6 a b c d e f)
  OT7 :: Inst7 IsObjectType a b c d e f g => SingOT (OT7 a b c d e f g)
  OT8 :: Inst8 IsObjectType a b c d e f g h => SingOT (OT8 a b c d e f g h)
  OT9 :: Inst9 IsObjectType a b c d e f g h i => SingOT (OT9 a b c d e f g h i)
  OT10 :: Inst10 IsObjectType a b c d e f g h i j => SingOT (OT10 a b c d e f g h i j)
  OT11 :: Inst11 IsObjectType a b c d e f g h i j k => SingOT (OT11 a b c d e f g h i j k)
  OT12 :: Inst12 IsObjectType a b c d e f g h i j k l => SingOT (OT12 a b c d e f g h i j k l)

class HasSingOT (ot :: Type) where
  singOT :: SingOT ot

instance Inst1 IsObjectType a => HasSingOT (OT1 a) where
  singOT = OT1

instance Inst2 IsObjectType a b => HasSingOT (OT2 a b) where
  singOT = OT2

instance Inst3 IsObjectType a b c => HasSingOT (OT3 a b c) where
  singOT = OT3

instance Inst4 IsObjectType a b c d => HasSingOT (OT4 a b c d) where
  singOT = OT4

instance Inst5 IsObjectType a b c d e => HasSingOT (OT5 a b c d e) where
  singOT = OT5

instance Inst6 IsObjectType a b c d e f => HasSingOT (OT6 a b c d e f) where
  singOT = OT6

instance Inst7 IsObjectType a b c d e f g => HasSingOT (OT7 a b c d e f g) where
  singOT = OT7

instance Inst8 IsObjectType a b c d e f g h => HasSingOT (OT8 a b c d e f g h) where
  singOT = OT8

instance Inst9 IsObjectType a b c d e f g h i => HasSingOT (OT9 a b c d e f g h i) where
  singOT = OT9

instance Inst10 IsObjectType a b c d e f g h i j => HasSingOT (OT10 a b c d e f g h i j) where
  singOT = OT10

instance Inst11 IsObjectType a b c d e f g h i j k => HasSingOT (OT11 a b c d e f g h i j k) where
  singOT = OT11

instance Inst12 IsObjectType a b c d e f g h i j k l => HasSingOT (OT12 a b c d e f g h i j k l) where
  singOT = OT12

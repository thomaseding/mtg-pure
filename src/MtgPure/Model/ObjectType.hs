{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType (
  ObjectType (..),
  OT,
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
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

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

data family OT (ot :: k) :: Type

data instance OT :: ObjectType0 -> Type

data instance OT :: ObjectType1 -> Type

data instance OT :: ObjectType2 -> Type

data instance OT :: ObjectType3 -> Type

data instance OT :: ObjectType4 -> Type

data instance OT :: ObjectType5 -> Type

data instance OT :: ObjectType6 -> Type

data instance OT :: ObjectType7 -> Type

data instance OT :: ObjectType8 -> Type

data instance OT :: ObjectType9 -> Type

data instance OT :: ObjectType10 -> Type

data instance OT :: ObjectType11 -> Type

data instance OT :: ObjectType12 -> Type

type OT0 = OT '()

type OT1 a = OT '( '(), a :: ObjectType)

type OT2 a b = OT '( '(), a :: ObjectType, b :: ObjectType)

type OT3 a b c = OT '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType)

type OT4 a b c d =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)

type OT5 a b c d e =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)

type OT6 a b c d e f =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)

type OT7 a b c d e f g =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)

type OT8 a b c d e f g h =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)

type OT9 a b c d e f g h i =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)

type OT10 a b c d e f g h i j =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)

type OT11 a b c d e f g h i j k =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)

type OT12 a b c d e f g h i j k l =
  OT
    '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)

-- XXX: Data.Sing
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

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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.ObjectN
  ( ObjectN (..),
    OAny,
    OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    OPermanent,
    OPlayerPlaneswalker,
  )
where

import Data.Inst
  ( Inst2,
    Inst3,
    Inst4,
    Inst5,
    Inst6,
    Inst7,
    Inst8,
  )
import Data.Kind (Type)
import Data.Typeable (Typeable)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.Object (Object)
import MtgPure.Model.ObjectType
  ( OTAny,
    OTCreaturePlaneswalker,
    OTCreaturePlayer,
    OTCreaturePlayerPlaneswalker,
    OTPermanent,
    OTPlayerPlaneswalker,
    ObjectType,
  )

type OAny = ObjectN OTAny

type OCreaturePlayer = ObjectN OTCreaturePlayer

type OCreaturePlaneswalker = ObjectN OTCreaturePlaneswalker

type OCreaturePlayerPlaneswalker = ObjectN OTCreaturePlayerPlaneswalker

type OPermanent = ObjectN OTPermanent

type OPlayerPlaneswalker = ObjectN OTPlayerPlaneswalker

data family ObjectN (a :: k)

-- TODO:
-- The constructors should be private to disallow pattern matching during authoring
-- Supply factory constructors for authoring

data instance ObjectN :: (ObjectType, ObjectType) -> Type where
  O2a :: Inst2 IsObjectType a b => Object a -> ObjectN '(a :: ObjectType, b :: ObjectType)
  O2b :: Inst2 IsObjectType a b => Object b -> ObjectN '(a :: ObjectType, b :: ObjectType)
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

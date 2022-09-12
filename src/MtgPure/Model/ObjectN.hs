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
  )
where

import safe Data.Inst
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
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object (Object)
import safe MtgPure.Model.ObjectType
  ( OT,
    ObjectType,
    ObjectType1,
    ObjectType10,
    ObjectType11,
    ObjectType12,
    ObjectType2,
    ObjectType3,
    ObjectType4,
    ObjectType5,
    ObjectType6,
    ObjectType7,
    ObjectType8,
    ObjectType9,
  )

data family ObjectN (a :: k)

-- TODO:
-- The constructors should be private to disallow pattern matching during authoring
-- Supply factory constructors for authoring

data instance ObjectN :: ObjectType1 -> Type where
  O :: Inst1 IsObjectType a => Object a -> ObjectN '(OT, a :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType2 -> Type where
  O2a :: Inst2 IsObjectType a b => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType)
  O2b :: Inst2 IsObjectType a b => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType)
  ON2b :: Inst2 IsObjectType a b => ObjectN '(OT, a) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType)
  ON2a :: Inst2 IsObjectType a b => ObjectN '(OT, b) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType3 -> Type where
  O3b :: Inst3 IsObjectType a b c => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType)
  O3a :: Inst3 IsObjectType a b c => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType)
  O3c :: Inst3 IsObjectType a b c => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType)
  ON3a :: Inst3 IsObjectType a b c => ObjectN '(OT, b, c) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType)
  ON3b :: Inst3 IsObjectType a b c => ObjectN '(OT, a, c) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType)
  ON3c :: Inst3 IsObjectType a b c => ObjectN '(OT, a, b) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType4 -> Type where
  O4a :: Inst4 IsObjectType a b c d => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  O4b :: Inst4 IsObjectType a b c d => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  O4c :: Inst4 IsObjectType a b c d => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  O4d :: Inst4 IsObjectType a b c d => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4a :: Inst4 IsObjectType a b c d => ObjectN '(OT, b, c, d) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4b :: Inst4 IsObjectType a b c d => ObjectN '(OT, a, c, d) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4c :: Inst4 IsObjectType a b c d => ObjectN '(OT, a, b, d) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  ON4d :: Inst4 IsObjectType a b c d => ObjectN '(OT, a, b, c) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType5 -> Type where
  O5b :: Inst5 IsObjectType a b c d e => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5a :: Inst5 IsObjectType a b c d e => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5c :: Inst5 IsObjectType a b c d e => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5d :: Inst5 IsObjectType a b c d e => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  O5e :: Inst5 IsObjectType a b c d e => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5a :: Inst5 IsObjectType a b c d e => ObjectN '(OT, b, c, d, e) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5b :: Inst5 IsObjectType a b c d e => ObjectN '(OT, a, c, d, e) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5c :: Inst5 IsObjectType a b c d e => ObjectN '(OT, a, b, d, e) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5d :: Inst5 IsObjectType a b c d e => ObjectN '(OT, a, b, c, e) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  ON5e :: Inst5 IsObjectType a b c d e => ObjectN '(OT, a, b, c, d) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType6 -> Type where
  O6a :: Inst6 IsObjectType a b c d e f => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6b :: Inst6 IsObjectType a b c d e f => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6c :: Inst6 IsObjectType a b c d e f => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6d :: Inst6 IsObjectType a b c d e f => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6e :: Inst6 IsObjectType a b c d e f => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  O6f :: Inst6 IsObjectType a b c d e f => Object f -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6a :: Inst6 IsObjectType a b c d e f => ObjectN '(OT, b, c, d, e, f) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6b :: Inst6 IsObjectType a b c d e f => ObjectN '(OT, a, c, d, e, f) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6c :: Inst6 IsObjectType a b c d e f => ObjectN '(OT, a, b, d, e, f) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6d :: Inst6 IsObjectType a b c d e f => ObjectN '(OT, a, b, c, e, f) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6e :: Inst6 IsObjectType a b c d e f => ObjectN '(OT, a, b, c, d, f) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  ON6f :: Inst6 IsObjectType a b c d e f => ObjectN '(OT, a, b, c, d, e) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType7 -> Type where
  O7a :: Inst7 IsObjectType a b c d e f g => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7b :: Inst7 IsObjectType a b c d e f g => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7c :: Inst7 IsObjectType a b c d e f g => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7d :: Inst7 IsObjectType a b c d e f g => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7e :: Inst7 IsObjectType a b c d e f g => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7f :: Inst7 IsObjectType a b c d e f g => Object f -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  O7g :: Inst7 IsObjectType a b c d e f g => Object g -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7a :: Inst7 IsObjectType a b c d e f g => ObjectN '(OT, b, c, d, e, f, g) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7b :: Inst7 IsObjectType a b c d e f g => ObjectN '(OT, a, c, d, e, f, g) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7c :: Inst7 IsObjectType a b c d e f g => ObjectN '(OT, a, b, d, e, f, g) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7d :: Inst7 IsObjectType a b c d e f g => ObjectN '(OT, a, b, c, e, f, g) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7e :: Inst7 IsObjectType a b c d e f g => ObjectN '(OT, a, b, c, d, f, g) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7f :: Inst7 IsObjectType a b c d e f g => ObjectN '(OT, a, b, c, d, e, g) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  ON7g :: Inst7 IsObjectType a b c d e f g => ObjectN '(OT, a, b, c, d, e, f) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType8 -> Type where
  O8a :: Inst8 IsObjectType a b c d e f g h => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8b :: Inst8 IsObjectType a b c d e f g h => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8c :: Inst8 IsObjectType a b c d e f g h => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8d :: Inst8 IsObjectType a b c d e f g h => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8e :: Inst8 IsObjectType a b c d e f g h => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8f :: Inst8 IsObjectType a b c d e f g h => Object f -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8g :: Inst8 IsObjectType a b c d e f g h => Object g -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  O8h :: Inst8 IsObjectType a b c d e f g h => Object h -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8a :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, b, c, d, e, f, g, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8b :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, c, d, e, f, g, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8c :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, b, d, e, f, g, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8d :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, b, c, e, f, g, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8e :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, b, c, d, f, g, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8f :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, b, c, d, e, g, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8g :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, b, c, d, e, f, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  ON8h :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, b, c, d, e, f, g) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType9 -> Type where
  O9a :: Inst9 IsObjectType a b c d e f g h i => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9b :: Inst9 IsObjectType a b c d e f g h i => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9c :: Inst9 IsObjectType a b c d e f g h i => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9d :: Inst9 IsObjectType a b c d e f g h i => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9e :: Inst9 IsObjectType a b c d e f g h i => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9f :: Inst9 IsObjectType a b c d e f g h i => Object f -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9g :: Inst9 IsObjectType a b c d e f g h i => Object g -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9h :: Inst9 IsObjectType a b c d e f g h i => Object h -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  O9i :: Inst9 IsObjectType a b c d e f g h i => Object i -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9a :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, b, c, d, e, f, g, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9b :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, c, d, e, f, g, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9c :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, b, d, e, f, g, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9d :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, b, c, e, f, g, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9e :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, b, c, d, f, g, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9f :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, b, c, d, e, g, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9g :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, b, c, d, e, f, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9h :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, b, c, d, e, f, g, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  ON9i :: Inst9 IsObjectType a b c d e f g h i => ObjectN '(OT, a, b, c, d, e, f, g, h) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType10 -> Type where
  O10b :: Inst10 IsObjectType a b c d e f g h i j => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10a :: Inst10 IsObjectType a b c d e f g h i j => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10c :: Inst10 IsObjectType a b c d e f g h i j => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10d :: Inst10 IsObjectType a b c d e f g h i j => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10e :: Inst10 IsObjectType a b c d e f g h i j => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10f :: Inst10 IsObjectType a b c d e f g h i j => Object f -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10g :: Inst10 IsObjectType a b c d e f g h i j => Object g -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10h :: Inst10 IsObjectType a b c d e f g h i j => Object h -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10i :: Inst10 IsObjectType a b c d e f g h i j => Object i -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  O10j :: Inst10 IsObjectType a b c d e f g h i j => Object j -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10a :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, b, c, d, e, f, g, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10b :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, c, d, e, f, g, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10c :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, d, e, f, g, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10d :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, c, e, f, g, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10e :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, c, d, f, g, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10f :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, c, d, e, g, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10g :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, c, d, e, f, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10h :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, c, d, e, f, g, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10i :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, c, d, e, f, g, h, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  ON10j :: Inst10 IsObjectType a b c d e f g h i j => ObjectN '(OT, a, b, c, d, e, f, g, h, i) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType11 -> Type where
  O11b :: Inst11 IsObjectType a b c d e f g h i j k => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11a :: Inst11 IsObjectType a b c d e f g h i j k => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11c :: Inst11 IsObjectType a b c d e f g h i j k => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11d :: Inst11 IsObjectType a b c d e f g h i j k => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11e :: Inst11 IsObjectType a b c d e f g h i j k => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11f :: Inst11 IsObjectType a b c d e f g h i j k => Object f -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11g :: Inst11 IsObjectType a b c d e f g h i j k => Object g -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11h :: Inst11 IsObjectType a b c d e f g h i j k => Object h -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11i :: Inst11 IsObjectType a b c d e f g h i j k => Object i -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11j :: Inst11 IsObjectType a b c d e f g h i j k => Object j -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  O11k :: Inst11 IsObjectType a b c d e f g h i j k => Object k -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11a :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, b, c, d, e, f, g, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11b :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, c, d, e, f, g, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11c :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, d, e, f, g, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11d :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, e, f, g, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11e :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, d, f, g, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11f :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, d, e, g, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11g :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, d, e, f, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11h :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, d, e, f, g, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11i :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, d, e, f, g, h, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11j :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, d, e, f, g, h, i, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  ON11k :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN '(OT, a, b, c, d, e, f, g, h, i, j) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)
  deriving (Typeable)

data instance ObjectN :: ObjectType12 -> Type where
  O12b :: Inst12 IsObjectType a b c d e f g h i j k l => Object b -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12a :: Inst12 IsObjectType a b c d e f g h i j k l => Object a -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12c :: Inst12 IsObjectType a b c d e f g h i j k l => Object c -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12d :: Inst12 IsObjectType a b c d e f g h i j k l => Object d -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12e :: Inst12 IsObjectType a b c d e f g h i j k l => Object e -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12f :: Inst12 IsObjectType a b c d e f g h i j k l => Object f -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12g :: Inst12 IsObjectType a b c d e f g h i j k l => Object g -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12h :: Inst12 IsObjectType a b c d e f g h i j k l => Object h -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12i :: Inst12 IsObjectType a b c d e f g h i j k l => Object i -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12j :: Inst12 IsObjectType a b c d e f g h i j k l => Object j -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12k :: Inst12 IsObjectType a b c d e f g h i j k l => Object k -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  O12l :: Inst12 IsObjectType a b c d e f g h i j k l => Object l -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12a :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, b, c, d, e, f, g, h, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12b :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, c, d, e, f, g, h, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12c :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, d, e, f, g, h, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12d :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, e, f, g, h, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12e :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, f, g, h, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12f :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, g, h, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12g :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, f, h, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12h :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, f, g, i, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12i :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, f, g, h, j, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12j :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, f, g, h, i, k, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12k :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, f, g, h, i, j, l) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  ON12l :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, f, g, h, i, j, k) -> ObjectN '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)
  deriving (Typeable)

deriving instance Inst1 IsObjectType a => Show (ObjectN '(OT, a))

deriving instance Inst2 IsObjectType a b => Show (ObjectN '(OT, a, b))

deriving instance Inst3 IsObjectType a b c => Show (ObjectN '(OT, a, b, c))

deriving instance Inst4 IsObjectType a b c d => Show (ObjectN '(OT, a, b, c, d))

deriving instance Inst5 IsObjectType a b c d e => Show (ObjectN '(OT, a, b, c, d, e))

deriving instance Inst6 IsObjectType a b c d e f => Show (ObjectN '(OT, a, b, c, d, e, f))

deriving instance Inst7 IsObjectType a b c d e f g => Show (ObjectN '(OT, a, b, c, d, e, f, g))

deriving instance Inst8 IsObjectType a b c d e f g h => Show (ObjectN '(OT, a, b, c, d, e, f, g, h))

deriving instance Inst9 IsObjectType a b c d e f g h i => Show (ObjectN '(OT, a, b, c, d, e, f, g, h, i))

deriving instance Inst10 IsObjectType a b c d e f g h i j => Show (ObjectN '(OT, a, b, c, d, e, f, g, h, i, j))

deriving instance Inst11 IsObjectType a b c d e f g h i j k => Show (ObjectN '(OT, a, b, c, d, e, f, g, h, i, j, k))

deriving instance Inst12 IsObjectType a b c d e f g h i j k l => Show (ObjectN '(OT, a, b, c, d, e, f, g, h, i, j, k, l))

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.ObjectN (
  ObjectN (..),
  ON0,
  ON1,
  ON2,
  ON3,
  ON4,
  ON5,
  ON6,
  ON7,
  ON8,
  ON9,
  ON10,
  ON11,
  ON12,
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
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.OTN (
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
import safe MtgPure.Model.Object (
  Object,
 )
import safe MtgPure.Model.ObjectId (UntypedObject)

type ON0 = ObjectN OT0

type ON1 a = ObjectN (OT1 a)

type ON2 a b = ObjectN (OT2 a b)

type ON3 a b c = ObjectN (OT3 a b c)

type ON4 a b c d = ObjectN (OT4 a b c d)

type ON5 a b c d e = ObjectN (OT5 a b c d e)

type ON6 a b c d e f = ObjectN (OT6 a b c d e f)

type ON7 a b c d e f g = ObjectN (OT7 a b c d e f g)

type ON8 a b c d e f g h = ObjectN (OT8 a b c d e f g h)

type ON9 a b c d e f g h i = ObjectN (OT9 a b c d e f g h i)

type ON10 a b c d e f g h i j = ObjectN (OT10 a b c d e f g h i j)

type ON11 a b c d e f g h i j k = ObjectN (OT11 a b c d e f g h i j k)

type ON12 a b c d e f g h i j k l = ObjectN (OT12 a b c d e f g h i j k l)

-- TODO:
-- The constructors should be private to disallow pattern matching during authoring
-- Supply factory constructors for authoring

data ObjectN (ot :: Type) :: Type where
  O0 :: UntypedObject -> ObjectN OT0
  --
  O1 :: Inst1 IsObjectType a => Object a -> ObjectN (OT1 a)
  --
  O2a :: Inst2 IsObjectType a b => Object a -> ObjectN (OT2 a b)
  O2b :: Inst2 IsObjectType a b => Object b -> ObjectN (OT2 a b)
  ON2b :: Inst2 IsObjectType a b => ObjectN (OT1 a) -> ObjectN (OT2 a b)
  ON2a :: Inst2 IsObjectType a b => ObjectN (OT1 b) -> ObjectN (OT2 a b)
  --
  O3b :: Inst3 IsObjectType a b c => Object b -> ObjectN (OT3 a b c)
  O3a :: Inst3 IsObjectType a b c => Object a -> ObjectN (OT3 a b c)
  O3c :: Inst3 IsObjectType a b c => Object c -> ObjectN (OT3 a b c)
  ON3a :: Inst3 IsObjectType a b c => ObjectN (OT2 b c) -> ObjectN (OT3 a b c)
  ON3b :: Inst3 IsObjectType a b c => ObjectN (OT2 a c) -> ObjectN (OT3 a b c)
  ON3c :: Inst3 IsObjectType a b c => ObjectN (OT2 a b) -> ObjectN (OT3 a b c)
  --
  O4a :: Inst4 IsObjectType a b c d => Object a -> ObjectN (OT4 a b c d)
  O4b :: Inst4 IsObjectType a b c d => Object b -> ObjectN (OT4 a b c d)
  O4c :: Inst4 IsObjectType a b c d => Object c -> ObjectN (OT4 a b c d)
  O4d :: Inst4 IsObjectType a b c d => Object d -> ObjectN (OT4 a b c d)
  ON4a :: Inst4 IsObjectType a b c d => ObjectN (OT3 b c d) -> ObjectN (OT4 a b c d)
  ON4b :: Inst4 IsObjectType a b c d => ObjectN (OT3 a c d) -> ObjectN (OT4 a b c d)
  ON4c :: Inst4 IsObjectType a b c d => ObjectN (OT3 a b d) -> ObjectN (OT4 a b c d)
  ON4d :: Inst4 IsObjectType a b c d => ObjectN (OT3 a b c) -> ObjectN (OT4 a b c d)
  --
  O5b :: Inst5 IsObjectType a b c d e => Object b -> ObjectN (OT5 a b c d e)
  O5a :: Inst5 IsObjectType a b c d e => Object a -> ObjectN (OT5 a b c d e)
  O5c :: Inst5 IsObjectType a b c d e => Object c -> ObjectN (OT5 a b c d e)
  O5d :: Inst5 IsObjectType a b c d e => Object d -> ObjectN (OT5 a b c d e)
  O5e :: Inst5 IsObjectType a b c d e => Object e -> ObjectN (OT5 a b c d e)
  ON5a :: Inst5 IsObjectType a b c d e => ObjectN (OT4 b c d e) -> ObjectN (OT5 a b c d e)
  ON5b :: Inst5 IsObjectType a b c d e => ObjectN (OT4 a c d e) -> ObjectN (OT5 a b c d e)
  ON5c :: Inst5 IsObjectType a b c d e => ObjectN (OT4 a b d e) -> ObjectN (OT5 a b c d e)
  ON5d :: Inst5 IsObjectType a b c d e => ObjectN (OT4 a b c e) -> ObjectN (OT5 a b c d e)
  ON5e :: Inst5 IsObjectType a b c d e => ObjectN (OT4 a b c d) -> ObjectN (OT5 a b c d e)
  --
  O6a :: Inst6 IsObjectType a b c d e f => Object a -> ObjectN (OT6 a b c d e f)
  O6b :: Inst6 IsObjectType a b c d e f => Object b -> ObjectN (OT6 a b c d e f)
  O6c :: Inst6 IsObjectType a b c d e f => Object c -> ObjectN (OT6 a b c d e f)
  O6d :: Inst6 IsObjectType a b c d e f => Object d -> ObjectN (OT6 a b c d e f)
  O6e :: Inst6 IsObjectType a b c d e f => Object e -> ObjectN (OT6 a b c d e f)
  O6f :: Inst6 IsObjectType a b c d e f => Object f -> ObjectN (OT6 a b c d e f)
  ON6a :: Inst6 IsObjectType a b c d e f => ObjectN (OT5 b c d e f) -> ObjectN (OT6 a b c d e f)
  ON6b :: Inst6 IsObjectType a b c d e f => ObjectN (OT5 a c d e f) -> ObjectN (OT6 a b c d e f)
  ON6c :: Inst6 IsObjectType a b c d e f => ObjectN (OT5 a b d e f) -> ObjectN (OT6 a b c d e f)
  ON6d :: Inst6 IsObjectType a b c d e f => ObjectN (OT5 a b c e f) -> ObjectN (OT6 a b c d e f)
  ON6e :: Inst6 IsObjectType a b c d e f => ObjectN (OT5 a b c d f) -> ObjectN (OT6 a b c d e f)
  ON6f :: Inst6 IsObjectType a b c d e f => ObjectN (OT5 a b c d e) -> ObjectN (OT6 a b c d e f)
  --
  O7a :: Inst7 IsObjectType a b c d e f g => Object a -> ObjectN (OT7 a b c d e f g)
  O7b :: Inst7 IsObjectType a b c d e f g => Object b -> ObjectN (OT7 a b c d e f g)
  O7c :: Inst7 IsObjectType a b c d e f g => Object c -> ObjectN (OT7 a b c d e f g)
  O7d :: Inst7 IsObjectType a b c d e f g => Object d -> ObjectN (OT7 a b c d e f g)
  O7e :: Inst7 IsObjectType a b c d e f g => Object e -> ObjectN (OT7 a b c d e f g)
  O7f :: Inst7 IsObjectType a b c d e f g => Object f -> ObjectN (OT7 a b c d e f g)
  O7g :: Inst7 IsObjectType a b c d e f g => Object g -> ObjectN (OT7 a b c d e f g)
  ON7a :: Inst7 IsObjectType a b c d e f g => ObjectN (OT6 b c d e f g) -> ObjectN (OT7 a b c d e f g)
  ON7b :: Inst7 IsObjectType a b c d e f g => ObjectN (OT6 a c d e f g) -> ObjectN (OT7 a b c d e f g)
  ON7c :: Inst7 IsObjectType a b c d e f g => ObjectN (OT6 a b d e f g) -> ObjectN (OT7 a b c d e f g)
  ON7d :: Inst7 IsObjectType a b c d e f g => ObjectN (OT6 a b c e f g) -> ObjectN (OT7 a b c d e f g)
  ON7e :: Inst7 IsObjectType a b c d e f g => ObjectN (OT6 a b c d f g) -> ObjectN (OT7 a b c d e f g)
  ON7f :: Inst7 IsObjectType a b c d e f g => ObjectN (OT6 a b c d e g) -> ObjectN (OT7 a b c d e f g)
  ON7g :: Inst7 IsObjectType a b c d e f g => ObjectN (OT6 a b c d e f) -> ObjectN (OT7 a b c d e f g)
  --
  O8a :: Inst8 IsObjectType a b c d e f g h => Object a -> ObjectN (OT8 a b c d e f g h)
  O8b :: Inst8 IsObjectType a b c d e f g h => Object b -> ObjectN (OT8 a b c d e f g h)
  O8c :: Inst8 IsObjectType a b c d e f g h => Object c -> ObjectN (OT8 a b c d e f g h)
  O8d :: Inst8 IsObjectType a b c d e f g h => Object d -> ObjectN (OT8 a b c d e f g h)
  O8e :: Inst8 IsObjectType a b c d e f g h => Object e -> ObjectN (OT8 a b c d e f g h)
  O8f :: Inst8 IsObjectType a b c d e f g h => Object f -> ObjectN (OT8 a b c d e f g h)
  O8g :: Inst8 IsObjectType a b c d e f g h => Object g -> ObjectN (OT8 a b c d e f g h)
  O8h :: Inst8 IsObjectType a b c d e f g h => Object h -> ObjectN (OT8 a b c d e f g h)
  ON8a :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 b c d e f g h) -> ObjectN (OT8 a b c d e f g h)
  ON8b :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 a c d e f g h) -> ObjectN (OT8 a b c d e f g h)
  ON8c :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 a b d e f g h) -> ObjectN (OT8 a b c d e f g h)
  ON8d :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 a b c e f g h) -> ObjectN (OT8 a b c d e f g h)
  ON8e :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 a b c d f g h) -> ObjectN (OT8 a b c d e f g h)
  ON8f :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 a b c d e g h) -> ObjectN (OT8 a b c d e f g h)
  ON8g :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 a b c d e f h) -> ObjectN (OT8 a b c d e f g h)
  ON8h :: Inst8 IsObjectType a b c d e f g h => ObjectN (OT7 a b c d e f g) -> ObjectN (OT8 a b c d e f g h)
  --
  O9a :: Inst9 IsObjectType a b c d e f g h i => Object a -> ObjectN (OT9 a b c d e f g h i)
  O9b :: Inst9 IsObjectType a b c d e f g h i => Object b -> ObjectN (OT9 a b c d e f g h i)
  O9c :: Inst9 IsObjectType a b c d e f g h i => Object c -> ObjectN (OT9 a b c d e f g h i)
  O9d :: Inst9 IsObjectType a b c d e f g h i => Object d -> ObjectN (OT9 a b c d e f g h i)
  O9e :: Inst9 IsObjectType a b c d e f g h i => Object e -> ObjectN (OT9 a b c d e f g h i)
  O9f :: Inst9 IsObjectType a b c d e f g h i => Object f -> ObjectN (OT9 a b c d e f g h i)
  O9g :: Inst9 IsObjectType a b c d e f g h i => Object g -> ObjectN (OT9 a b c d e f g h i)
  O9h :: Inst9 IsObjectType a b c d e f g h i => Object h -> ObjectN (OT9 a b c d e f g h i)
  O9i :: Inst9 IsObjectType a b c d e f g h i => Object i -> ObjectN (OT9 a b c d e f g h i)
  ON9a :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 b c d e f g h i) -> ObjectN (OT9 a b c d e f g h i)
  ON9b :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a c d e f g h i) -> ObjectN (OT9 a b c d e f g h i)
  ON9c :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a b d e f g h i) -> ObjectN (OT9 a b c d e f g h i)
  ON9d :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a b c e f g h i) -> ObjectN (OT9 a b c d e f g h i)
  ON9e :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a b c d f g h i) -> ObjectN (OT9 a b c d e f g h i)
  ON9f :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a b c d e g h i) -> ObjectN (OT9 a b c d e f g h i)
  ON9g :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a b c d e f h i) -> ObjectN (OT9 a b c d e f g h i)
  ON9h :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a b c d e f g i) -> ObjectN (OT9 a b c d e f g h i)
  ON9i :: Inst9 IsObjectType a b c d e f g h i => ObjectN (OT8 a b c d e f g h) -> ObjectN (OT9 a b c d e f g h i)
  --
  O10b :: Inst10 IsObjectType a b c d e f g h i j => Object b -> ObjectN (OT10 a b c d e f g h i j)
  O10a :: Inst10 IsObjectType a b c d e f g h i j => Object a -> ObjectN (OT10 a b c d e f g h i j)
  O10c :: Inst10 IsObjectType a b c d e f g h i j => Object c -> ObjectN (OT10 a b c d e f g h i j)
  O10d :: Inst10 IsObjectType a b c d e f g h i j => Object d -> ObjectN (OT10 a b c d e f g h i j)
  O10e :: Inst10 IsObjectType a b c d e f g h i j => Object e -> ObjectN (OT10 a b c d e f g h i j)
  O10f :: Inst10 IsObjectType a b c d e f g h i j => Object f -> ObjectN (OT10 a b c d e f g h i j)
  O10g :: Inst10 IsObjectType a b c d e f g h i j => Object g -> ObjectN (OT10 a b c d e f g h i j)
  O10h :: Inst10 IsObjectType a b c d e f g h i j => Object h -> ObjectN (OT10 a b c d e f g h i j)
  O10i :: Inst10 IsObjectType a b c d e f g h i j => Object i -> ObjectN (OT10 a b c d e f g h i j)
  O10j :: Inst10 IsObjectType a b c d e f g h i j => Object j -> ObjectN (OT10 a b c d e f g h i j)
  ON10a :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 b c d e f g h i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10b :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a c d e f g h i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10c :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b d e f g h i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10d :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b c e f g h i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10e :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b c d f g h i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10f :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b c d e g h i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10g :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b c d e f h i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10h :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b c d e f g i j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10i :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b c d e f g h j) -> ObjectN (OT10 a b c d e f g h i j)
  ON10j :: Inst10 IsObjectType a b c d e f g h i j => ObjectN (OT9 a b c d e f g h i) -> ObjectN (OT10 a b c d e f g h i j)
  --
  O11b :: Inst11 IsObjectType a b c d e f g h i j k => Object b -> ObjectN (OT11 a b c d e f g h i j k)
  O11a :: Inst11 IsObjectType a b c d e f g h i j k => Object a -> ObjectN (OT11 a b c d e f g h i j k)
  O11c :: Inst11 IsObjectType a b c d e f g h i j k => Object c -> ObjectN (OT11 a b c d e f g h i j k)
  O11d :: Inst11 IsObjectType a b c d e f g h i j k => Object d -> ObjectN (OT11 a b c d e f g h i j k)
  O11e :: Inst11 IsObjectType a b c d e f g h i j k => Object e -> ObjectN (OT11 a b c d e f g h i j k)
  O11f :: Inst11 IsObjectType a b c d e f g h i j k => Object f -> ObjectN (OT11 a b c d e f g h i j k)
  O11g :: Inst11 IsObjectType a b c d e f g h i j k => Object g -> ObjectN (OT11 a b c d e f g h i j k)
  O11h :: Inst11 IsObjectType a b c d e f g h i j k => Object h -> ObjectN (OT11 a b c d e f g h i j k)
  O11i :: Inst11 IsObjectType a b c d e f g h i j k => Object i -> ObjectN (OT11 a b c d e f g h i j k)
  O11j :: Inst11 IsObjectType a b c d e f g h i j k => Object j -> ObjectN (OT11 a b c d e f g h i j k)
  O11k :: Inst11 IsObjectType a b c d e f g h i j k => Object k -> ObjectN (OT11 a b c d e f g h i j k)
  ON11a :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 b c d e f g h i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11b :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a c d e f g h i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11c :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b d e f g h i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11d :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c e f g h i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11e :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c d f g h i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11f :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c d e g h i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11g :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c d e f h i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11h :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c d e f g i j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11i :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c d e f g h j k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11j :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c d e f g h i k) -> ObjectN (OT11 a b c d e f g h i j k)
  ON11k :: Inst11 IsObjectType a b c d e f g h i j k => ObjectN (OT10 a b c d e f g h i j) -> ObjectN (OT11 a b c d e f g h i j k)
  --
  O12b :: Inst12 IsObjectType a b c d e f g h i j k l => Object b -> ObjectN (OT12 a b c d e f g h i j k l)
  O12a :: Inst12 IsObjectType a b c d e f g h i j k l => Object a -> ObjectN (OT12 a b c d e f g h i j k l)
  O12c :: Inst12 IsObjectType a b c d e f g h i j k l => Object c -> ObjectN (OT12 a b c d e f g h i j k l)
  O12d :: Inst12 IsObjectType a b c d e f g h i j k l => Object d -> ObjectN (OT12 a b c d e f g h i j k l)
  O12e :: Inst12 IsObjectType a b c d e f g h i j k l => Object e -> ObjectN (OT12 a b c d e f g h i j k l)
  O12f :: Inst12 IsObjectType a b c d e f g h i j k l => Object f -> ObjectN (OT12 a b c d e f g h i j k l)
  O12g :: Inst12 IsObjectType a b c d e f g h i j k l => Object g -> ObjectN (OT12 a b c d e f g h i j k l)
  O12h :: Inst12 IsObjectType a b c d e f g h i j k l => Object h -> ObjectN (OT12 a b c d e f g h i j k l)
  O12i :: Inst12 IsObjectType a b c d e f g h i j k l => Object i -> ObjectN (OT12 a b c d e f g h i j k l)
  O12j :: Inst12 IsObjectType a b c d e f g h i j k l => Object j -> ObjectN (OT12 a b c d e f g h i j k l)
  O12k :: Inst12 IsObjectType a b c d e f g h i j k l => Object k -> ObjectN (OT12 a b c d e f g h i j k l)
  O12l :: Inst12 IsObjectType a b c d e f g h i j k l => Object l -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12a :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 b c d e f g h i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12b :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a c d e f g h i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12c :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b d e f g h i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12d :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c e f g h i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12e :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d f g h i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12f :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d e g h i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12g :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d e f h i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12h :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d e f g i j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12i :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d e f g h j k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12j :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d e f g h i k l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12k :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d e f g h i j l) -> ObjectN (OT12 a b c d e f g h i j k l)
  ON12l :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN (OT11 a b c d e f g h i j k) -> ObjectN (OT12 a b c d e f g h i j k l)
  deriving (Typeable)

deriving instance Inst1 IsObjectType a => Show (ObjectN (OT1 a))

deriving instance Inst2 IsObjectType a b => Show (ObjectN (OT2 a b))

deriving instance Inst3 IsObjectType a b c => Show (ObjectN (OT3 a b c))

deriving instance Inst4 IsObjectType a b c d => Show (ObjectN (OT4 a b c d))

deriving instance Inst5 IsObjectType a b c d e => Show (ObjectN (OT5 a b c d e))

deriving instance Inst6 IsObjectType a b c d e f => Show (ObjectN (OT6 a b c d e f))

deriving instance Inst7 IsObjectType a b c d e f g => Show (ObjectN (OT7 a b c d e f g))

deriving instance Inst8 IsObjectType a b c d e f g h => Show (ObjectN (OT8 a b c d e f g h))

deriving instance Inst9 IsObjectType a b c d e f g h i => Show (ObjectN (OT9 a b c d e f g h i))

deriving instance Inst10 IsObjectType a b c d e f g h i j => Show (ObjectN (OT10 a b c d e f g h i j))

deriving instance Inst11 IsObjectType a b c d e f g h i j k => Show (ObjectN (OT11 a b c d e f g h i j k))

deriving instance Inst12 IsObjectType a b c d e f g h i j k l => Show (ObjectN (OT12 a b c d e f g h i j k l))

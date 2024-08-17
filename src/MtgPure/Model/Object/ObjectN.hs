{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.Object.ObjectN (
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
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OTN (
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
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (UntypedObject)

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

-- NOTE: There is no need to use something like `(otk :: OTK)`.
-- That just makes the types harder to pass around. Use `ViewObjectN` to
-- get easy access to the `ObjectN` type.
data ObjectN (otn :: Type) :: Type where
  O0 :: UntypedObject -> ON0
  --
  O1 :: (Inst1 IsObjectType a) => Object a -> ON1 a
  --
  O2a :: (Inst2 IsObjectType a b) => Object a -> ON2 a b
  O2b :: (Inst2 IsObjectType a b) => Object b -> ON2 a b
  ON2a :: (Inst2 IsObjectType a b) => ON1 b -> ON2 a b
  ON2b :: (Inst2 IsObjectType a b) => ON1 a -> ON2 a b
  --
  O3a :: (Inst3 IsObjectType a b c) => Object a -> ON3 a b c
  O3b :: (Inst3 IsObjectType a b c) => Object b -> ON3 a b c
  O3c :: (Inst3 IsObjectType a b c) => Object c -> ON3 a b c
  ON3a :: (Inst3 IsObjectType a b c) => ON2 b c -> ON3 a b c
  ON3b :: (Inst3 IsObjectType a b c) => ON2 a c -> ON3 a b c
  ON3c :: (Inst3 IsObjectType a b c) => ON2 a b -> ON3 a b c
  --
  O4a :: (Inst4 IsObjectType a b c d) => Object a -> ON4 a b c d
  O4b :: (Inst4 IsObjectType a b c d) => Object b -> ON4 a b c d
  O4c :: (Inst4 IsObjectType a b c d) => Object c -> ON4 a b c d
  O4d :: (Inst4 IsObjectType a b c d) => Object d -> ON4 a b c d
  ON4a :: (Inst4 IsObjectType a b c d) => ON3 b c d -> ON4 a b c d
  ON4b :: (Inst4 IsObjectType a b c d) => ON3 a c d -> ON4 a b c d
  ON4c :: (Inst4 IsObjectType a b c d) => ON3 a b d -> ON4 a b c d
  ON4d :: (Inst4 IsObjectType a b c d) => ON3 a b c -> ON4 a b c d
  --
  O5a :: (Inst5 IsObjectType a b c d e) => Object a -> ON5 a b c d e
  O5b :: (Inst5 IsObjectType a b c d e) => Object b -> ON5 a b c d e
  O5c :: (Inst5 IsObjectType a b c d e) => Object c -> ON5 a b c d e
  O5d :: (Inst5 IsObjectType a b c d e) => Object d -> ON5 a b c d e
  O5e :: (Inst5 IsObjectType a b c d e) => Object e -> ON5 a b c d e
  ON5a :: (Inst5 IsObjectType a b c d e) => ON4 b c d e -> ON5 a b c d e
  ON5b :: (Inst5 IsObjectType a b c d e) => ON4 a c d e -> ON5 a b c d e
  ON5c :: (Inst5 IsObjectType a b c d e) => ON4 a b d e -> ON5 a b c d e
  ON5d :: (Inst5 IsObjectType a b c d e) => ON4 a b c e -> ON5 a b c d e
  ON5e :: (Inst5 IsObjectType a b c d e) => ON4 a b c d -> ON5 a b c d e
  --
  O6a :: (Inst6 IsObjectType a b c d e f) => Object a -> ON6 a b c d e f
  O6b :: (Inst6 IsObjectType a b c d e f) => Object b -> ON6 a b c d e f
  O6c :: (Inst6 IsObjectType a b c d e f) => Object c -> ON6 a b c d e f
  O6d :: (Inst6 IsObjectType a b c d e f) => Object d -> ON6 a b c d e f
  O6e :: (Inst6 IsObjectType a b c d e f) => Object e -> ON6 a b c d e f
  O6f :: (Inst6 IsObjectType a b c d e f) => Object f -> ON6 a b c d e f
  ON6a :: (Inst6 IsObjectType a b c d e f) => ON5 b c d e f -> ON6 a b c d e f
  ON6b :: (Inst6 IsObjectType a b c d e f) => ON5 a c d e f -> ON6 a b c d e f
  ON6c :: (Inst6 IsObjectType a b c d e f) => ON5 a b d e f -> ON6 a b c d e f
  ON6d :: (Inst6 IsObjectType a b c d e f) => ON5 a b c e f -> ON6 a b c d e f
  ON6e :: (Inst6 IsObjectType a b c d e f) => ON5 a b c d f -> ON6 a b c d e f
  ON6f :: (Inst6 IsObjectType a b c d e f) => ON5 a b c d e -> ON6 a b c d e f
  --
  O7a :: (Inst7 IsObjectType a b c d e f g) => Object a -> ON7 a b c d e f g
  O7b :: (Inst7 IsObjectType a b c d e f g) => Object b -> ON7 a b c d e f g
  O7c :: (Inst7 IsObjectType a b c d e f g) => Object c -> ON7 a b c d e f g
  O7d :: (Inst7 IsObjectType a b c d e f g) => Object d -> ON7 a b c d e f g
  O7e :: (Inst7 IsObjectType a b c d e f g) => Object e -> ON7 a b c d e f g
  O7f :: (Inst7 IsObjectType a b c d e f g) => Object f -> ON7 a b c d e f g
  O7g :: (Inst7 IsObjectType a b c d e f g) => Object g -> ON7 a b c d e f g
  ON7a :: (Inst7 IsObjectType a b c d e f g) => ON6 b c d e f g -> ON7 a b c d e f g
  ON7b :: (Inst7 IsObjectType a b c d e f g) => ON6 a c d e f g -> ON7 a b c d e f g
  ON7c :: (Inst7 IsObjectType a b c d e f g) => ON6 a b d e f g -> ON7 a b c d e f g
  ON7d :: (Inst7 IsObjectType a b c d e f g) => ON6 a b c e f g -> ON7 a b c d e f g
  ON7e :: (Inst7 IsObjectType a b c d e f g) => ON6 a b c d f g -> ON7 a b c d e f g
  ON7f :: (Inst7 IsObjectType a b c d e f g) => ON6 a b c d e g -> ON7 a b c d e f g
  ON7g :: (Inst7 IsObjectType a b c d e f g) => ON6 a b c d e f -> ON7 a b c d e f g
  --
  O8a :: (Inst8 IsObjectType a b c d e f g h) => Object a -> ON8 a b c d e f g h
  O8b :: (Inst8 IsObjectType a b c d e f g h) => Object b -> ON8 a b c d e f g h
  O8c :: (Inst8 IsObjectType a b c d e f g h) => Object c -> ON8 a b c d e f g h
  O8d :: (Inst8 IsObjectType a b c d e f g h) => Object d -> ON8 a b c d e f g h
  O8e :: (Inst8 IsObjectType a b c d e f g h) => Object e -> ON8 a b c d e f g h
  O8f :: (Inst8 IsObjectType a b c d e f g h) => Object f -> ON8 a b c d e f g h
  O8g :: (Inst8 IsObjectType a b c d e f g h) => Object g -> ON8 a b c d e f g h
  O8h :: (Inst8 IsObjectType a b c d e f g h) => Object h -> ON8 a b c d e f g h
  ON8a :: (Inst8 IsObjectType a b c d e f g h) => ON7 b c d e f g h -> ON8 a b c d e f g h
  ON8b :: (Inst8 IsObjectType a b c d e f g h) => ON7 a c d e f g h -> ON8 a b c d e f g h
  ON8c :: (Inst8 IsObjectType a b c d e f g h) => ON7 a b d e f g h -> ON8 a b c d e f g h
  ON8d :: (Inst8 IsObjectType a b c d e f g h) => ON7 a b c e f g h -> ON8 a b c d e f g h
  ON8e :: (Inst8 IsObjectType a b c d e f g h) => ON7 a b c d f g h -> ON8 a b c d e f g h
  ON8f :: (Inst8 IsObjectType a b c d e f g h) => ON7 a b c d e g h -> ON8 a b c d e f g h
  ON8g :: (Inst8 IsObjectType a b c d e f g h) => ON7 a b c d e f h -> ON8 a b c d e f g h
  ON8h :: (Inst8 IsObjectType a b c d e f g h) => ON7 a b c d e f g -> ON8 a b c d e f g h
  --
  O9a :: (Inst9 IsObjectType a b c d e f g h i) => Object a -> ON9 a b c d e f g h i
  O9b :: (Inst9 IsObjectType a b c d e f g h i) => Object b -> ON9 a b c d e f g h i
  O9c :: (Inst9 IsObjectType a b c d e f g h i) => Object c -> ON9 a b c d e f g h i
  O9d :: (Inst9 IsObjectType a b c d e f g h i) => Object d -> ON9 a b c d e f g h i
  O9e :: (Inst9 IsObjectType a b c d e f g h i) => Object e -> ON9 a b c d e f g h i
  O9f :: (Inst9 IsObjectType a b c d e f g h i) => Object f -> ON9 a b c d e f g h i
  O9g :: (Inst9 IsObjectType a b c d e f g h i) => Object g -> ON9 a b c d e f g h i
  O9h :: (Inst9 IsObjectType a b c d e f g h i) => Object h -> ON9 a b c d e f g h i
  O9i :: (Inst9 IsObjectType a b c d e f g h i) => Object i -> ON9 a b c d e f g h i
  ON9a :: (Inst9 IsObjectType a b c d e f g h i) => ON8 b c d e f g h i -> ON9 a b c d e f g h i
  ON9b :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a c d e f g h i -> ON9 a b c d e f g h i
  ON9c :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a b d e f g h i -> ON9 a b c d e f g h i
  ON9d :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a b c e f g h i -> ON9 a b c d e f g h i
  ON9e :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a b c d f g h i -> ON9 a b c d e f g h i
  ON9f :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a b c d e g h i -> ON9 a b c d e f g h i
  ON9g :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a b c d e f h i -> ON9 a b c d e f g h i
  ON9h :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a b c d e f g i -> ON9 a b c d e f g h i
  ON9i :: (Inst9 IsObjectType a b c d e f g h i) => ON8 a b c d e f g h -> ON9 a b c d e f g h i
  --
  O10a :: (Inst10 IsObjectType a b c d e f g h i j) => Object a -> ON10 a b c d e f g h i j
  O10b :: (Inst10 IsObjectType a b c d e f g h i j) => Object b -> ON10 a b c d e f g h i j
  O10c :: (Inst10 IsObjectType a b c d e f g h i j) => Object c -> ON10 a b c d e f g h i j
  O10d :: (Inst10 IsObjectType a b c d e f g h i j) => Object d -> ON10 a b c d e f g h i j
  O10e :: (Inst10 IsObjectType a b c d e f g h i j) => Object e -> ON10 a b c d e f g h i j
  O10f :: (Inst10 IsObjectType a b c d e f g h i j) => Object f -> ON10 a b c d e f g h i j
  O10g :: (Inst10 IsObjectType a b c d e f g h i j) => Object g -> ON10 a b c d e f g h i j
  O10h :: (Inst10 IsObjectType a b c d e f g h i j) => Object h -> ON10 a b c d e f g h i j
  O10i :: (Inst10 IsObjectType a b c d e f g h i j) => Object i -> ON10 a b c d e f g h i j
  O10j :: (Inst10 IsObjectType a b c d e f g h i j) => Object j -> ON10 a b c d e f g h i j
  ON10a :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 b c d e f g h i j -> ON10 a b c d e f g h i j
  ON10b :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a c d e f g h i j -> ON10 a b c d e f g h i j
  ON10c :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b d e f g h i j -> ON10 a b c d e f g h i j
  ON10d :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b c e f g h i j -> ON10 a b c d e f g h i j
  ON10e :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b c d f g h i j -> ON10 a b c d e f g h i j
  ON10f :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b c d e g h i j -> ON10 a b c d e f g h i j
  ON10g :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b c d e f h i j -> ON10 a b c d e f g h i j
  ON10h :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b c d e f g i j -> ON10 a b c d e f g h i j
  ON10i :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b c d e f g h j -> ON10 a b c d e f g h i j
  ON10j :: (Inst10 IsObjectType a b c d e f g h i j) => ON9 a b c d e f g h i -> ON10 a b c d e f g h i j
  --
  O11a :: (Inst11 IsObjectType a b c d e f g h i j k) => Object a -> ON11 a b c d e f g h i j k
  O11b :: (Inst11 IsObjectType a b c d e f g h i j k) => Object b -> ON11 a b c d e f g h i j k
  O11c :: (Inst11 IsObjectType a b c d e f g h i j k) => Object c -> ON11 a b c d e f g h i j k
  O11d :: (Inst11 IsObjectType a b c d e f g h i j k) => Object d -> ON11 a b c d e f g h i j k
  O11e :: (Inst11 IsObjectType a b c d e f g h i j k) => Object e -> ON11 a b c d e f g h i j k
  O11f :: (Inst11 IsObjectType a b c d e f g h i j k) => Object f -> ON11 a b c d e f g h i j k
  O11g :: (Inst11 IsObjectType a b c d e f g h i j k) => Object g -> ON11 a b c d e f g h i j k
  O11h :: (Inst11 IsObjectType a b c d e f g h i j k) => Object h -> ON11 a b c d e f g h i j k
  O11i :: (Inst11 IsObjectType a b c d e f g h i j k) => Object i -> ON11 a b c d e f g h i j k
  O11j :: (Inst11 IsObjectType a b c d e f g h i j k) => Object j -> ON11 a b c d e f g h i j k
  O11k :: (Inst11 IsObjectType a b c d e f g h i j k) => Object k -> ON11 a b c d e f g h i j k
  ON11a :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 b c d e f g h i j k -> ON11 a b c d e f g h i j k
  ON11b :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a c d e f g h i j k -> ON11 a b c d e f g h i j k
  ON11c :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b d e f g h i j k -> ON11 a b c d e f g h i j k
  ON11d :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c e f g h i j k -> ON11 a b c d e f g h i j k
  ON11e :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c d f g h i j k -> ON11 a b c d e f g h i j k
  ON11f :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c d e g h i j k -> ON11 a b c d e f g h i j k
  ON11g :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c d e f h i j k -> ON11 a b c d e f g h i j k
  ON11h :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c d e f g i j k -> ON11 a b c d e f g h i j k
  ON11i :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c d e f g h j k -> ON11 a b c d e f g h i j k
  ON11j :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c d e f g h i k -> ON11 a b c d e f g h i j k
  ON11k :: (Inst11 IsObjectType a b c d e f g h i j k) => ON10 a b c d e f g h i j -> ON11 a b c d e f g h i j k
  --
  O12a :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object a -> ON12 a b c d e f g h i j k l
  O12b :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object b -> ON12 a b c d e f g h i j k l
  O12c :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object c -> ON12 a b c d e f g h i j k l
  O12d :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object d -> ON12 a b c d e f g h i j k l
  O12e :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object e -> ON12 a b c d e f g h i j k l
  O12f :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object f -> ON12 a b c d e f g h i j k l
  O12g :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object g -> ON12 a b c d e f g h i j k l
  O12h :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object h -> ON12 a b c d e f g h i j k l
  O12i :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object i -> ON12 a b c d e f g h i j k l
  O12j :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object j -> ON12 a b c d e f g h i j k l
  O12k :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object k -> ON12 a b c d e f g h i j k l
  O12l :: (Inst12 IsObjectType a b c d e f g h i j k l) => Object l -> ON12 a b c d e f g h i j k l
  ON12a :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 b c d e f g h i j k l -> ON12 a b c d e f g h i j k l
  ON12b :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a c d e f g h i j k l -> ON12 a b c d e f g h i j k l
  ON12c :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b d e f g h i j k l -> ON12 a b c d e f g h i j k l
  ON12d :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c e f g h i j k l -> ON12 a b c d e f g h i j k l
  ON12e :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d f g h i j k l -> ON12 a b c d e f g h i j k l
  ON12f :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d e g h i j k l -> ON12 a b c d e f g h i j k l
  ON12g :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d e f h i j k l -> ON12 a b c d e f g h i j k l
  ON12h :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d e f g i j k l -> ON12 a b c d e f g h i j k l
  ON12i :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d e f g h j k l -> ON12 a b c d e f g h i j k l
  ON12j :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d e f g h i k l -> ON12 a b c d e f g h i j k l
  ON12k :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d e f g h i j l -> ON12 a b c d e f g h i j k l
  ON12l :: (Inst12 IsObjectType a b c d e f g h i j k l) => ON11 a b c d e f g h i j k -> ON12 a b c d e f g h i j k l
  --
  deriving (Typeable)

deriving instance (Inst1 IsObjectType a) => Show (ObjectN (OT1 a))

deriving instance (Inst2 IsObjectType a b) => Show (ObjectN (OT2 a b))

deriving instance (Inst3 IsObjectType a b c) => Show (ObjectN (OT3 a b c))

deriving instance (Inst4 IsObjectType a b c d) => Show (ObjectN (OT4 a b c d))

deriving instance (Inst5 IsObjectType a b c d e) => Show (ObjectN (OT5 a b c d e))

deriving instance (Inst6 IsObjectType a b c d e f) => Show (ObjectN (OT6 a b c d e f))

deriving instance (Inst7 IsObjectType a b c d e f g) => Show (ObjectN (OT7 a b c d e f g))

deriving instance (Inst8 IsObjectType a b c d e f g h) => Show (ObjectN (OT8 a b c d e f g h))

deriving instance (Inst9 IsObjectType a b c d e f g h i) => Show (ObjectN (OT9 a b c d e f g h i))

deriving instance (Inst10 IsObjectType a b c d e f g h i j) => Show (ObjectN (OT10 a b c d e f g h i j))

deriving instance (Inst11 IsObjectType a b c d e f g h i j k) => Show (ObjectN (OT11 a b c d e f g h i j k))

deriving instance (Inst12 IsObjectType a b c d e f g h i j k l) => Show (ObjectN (OT12 a b c d e f g h i j k l))

{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.Object.ObjectN_ (
  ObjectN' (..),
  ON0',
  ON1',
  ON2',
  ON3',
  ON4',
  ON5',
  ON6',
  ON7',
  ON8',
  ON9',
  ON10',
  ON11',
  ON12',
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
import safe Data.Kind (Constraint, Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OTN_ (
  OT0',
  OT1',
  OT10',
  OT11',
  OT12',
  OT2',
  OT3',
  OT4',
  OT5',
  OT6',
  OT7',
  OT8',
  OT9',
 )

type ON0' uObj obj ot isOT = ObjectN' uObj obj ot isOT (OT0' ot isOT)

type ON1' uObj obj ot isOT a = ObjectN' uObj obj ot isOT (OT1' ot isOT a)

type ON2' uObj obj ot isOT a b = ObjectN' uObj obj ot isOT (OT2' ot isOT a b)

type ON3' uObj obj ot isOT a b c = ObjectN' uObj obj ot isOT (OT3' ot isOT a b c)

type ON4' uObj obj ot isOT a b c d = ObjectN' uObj obj ot isOT (OT4' ot isOT a b c d)

type ON5' uObj obj ot isOT a b c d e = ObjectN' uObj obj ot isOT (OT5' ot isOT a b c d e)

type ON6' uObj obj ot isOT a b c d e f = ObjectN' uObj obj ot isOT (OT6' ot isOT a b c d e f)

type ON7' uObj obj ot isOT a b c d e f g = ObjectN' uObj obj ot isOT (OT7' ot isOT a b c d e f g)

type ON8' uObj obj ot isOT a b c d e f g h = ObjectN' uObj obj ot isOT (OT8' ot isOT a b c d e f g h)

type ON9' uObj obj ot isOT a b c d e f g h i = ObjectN' uObj obj ot isOT (OT9' ot isOT a b c d e f g h i)

type ON10' uObj obj ot isOT a b c d e f g h i j = ObjectN' uObj obj ot isOT (OT10' ot isOT a b c d e f g h i j)

type ON11' uObj obj ot isOT a b c d e f g h i j k = ObjectN' uObj obj ot isOT (OT11' ot isOT a b c d e f g h i j k)

type ON12' uObj obj ot isOT a b c d e f g h i j k l = ObjectN' uObj obj ot isOT (OT12' ot isOT a b c d e f g h i j k l)

-- TODO:
-- The constructors should be private to disallow pattern matching during authoring
-- See `MtgPure.Model.Object.ObjectN` for smart constructors that play nice with type applications.
data ObjectN' (uObj :: Type) (obj :: k0 -> Type) (ot :: k1) (isOT :: k2 -> Constraint) (otn :: Type) :: Type where
  O0 :: uObj -> ON0' uObj obj ot isOT
  --
  O1 :: Inst1 isOT a => obj a -> ON1' uObj obj ot isOT a
  --
  O2a :: Inst2 isOT a b => obj a -> ON2' uObj obj ot isOT a b
  O2b :: Inst2 isOT a b => obj b -> ON2' uObj obj ot isOT a b
  ON2b :: Inst2 isOT a b => ON1' uObj obj ot isOT a -> ON2' uObj obj ot isOT a b
  ON2a :: Inst2 isOT a b => ON1' uObj obj ot isOT b -> ON2' uObj obj ot isOT a b
  --
  O3b :: Inst3 isOT a b c => obj b -> ON3' uObj obj ot isOT a b c
  O3a :: Inst3 isOT a b c => obj a -> ON3' uObj obj ot isOT a b c
  O3c :: Inst3 isOT a b c => obj c -> ON3' uObj obj ot isOT a b c
  ON3a :: Inst3 isOT a b c => ON2' uObj obj ot isOT b c -> ON3' uObj obj ot isOT a b c
  ON3b :: Inst3 isOT a b c => ON2' uObj obj ot isOT a c -> ON3' uObj obj ot isOT a b c
  ON3c :: Inst3 isOT a b c => ON2' uObj obj ot isOT a b -> ON3' uObj obj ot isOT a b c
  --
  O4a :: Inst4 isOT a b c d => obj a -> ON4' uObj obj ot isOT a b c d
  O4b :: Inst4 isOT a b c d => obj b -> ON4' uObj obj ot isOT a b c d
  O4c :: Inst4 isOT a b c d => obj c -> ON4' uObj obj ot isOT a b c d
  O4d :: Inst4 isOT a b c d => obj d -> ON4' uObj obj ot isOT a b c d
  ON4a :: Inst4 isOT a b c d => ON3' uObj obj ot isOT b c d -> ON4' uObj obj ot isOT a b c d
  ON4b :: Inst4 isOT a b c d => ON3' uObj obj ot isOT a c d -> ON4' uObj obj ot isOT a b c d
  ON4c :: Inst4 isOT a b c d => ON3' uObj obj ot isOT a b d -> ON4' uObj obj ot isOT a b c d
  ON4d :: Inst4 isOT a b c d => ON3' uObj obj ot isOT a b c -> ON4' uObj obj ot isOT a b c d
  --
  O5b :: Inst5 isOT a b c d e => obj b -> ON5' uObj obj ot isOT a b c d e
  O5a :: Inst5 isOT a b c d e => obj a -> ON5' uObj obj ot isOT a b c d e
  O5c :: Inst5 isOT a b c d e => obj c -> ON5' uObj obj ot isOT a b c d e
  O5d :: Inst5 isOT a b c d e => obj d -> ON5' uObj obj ot isOT a b c d e
  O5e :: Inst5 isOT a b c d e => obj e -> ON5' uObj obj ot isOT a b c d e
  ON5a :: Inst5 isOT a b c d e => ON4' uObj obj ot isOT b c d e -> ON5' uObj obj ot isOT a b c d e
  ON5b :: Inst5 isOT a b c d e => ON4' uObj obj ot isOT a c d e -> ON5' uObj obj ot isOT a b c d e
  ON5c :: Inst5 isOT a b c d e => ON4' uObj obj ot isOT a b d e -> ON5' uObj obj ot isOT a b c d e
  ON5d :: Inst5 isOT a b c d e => ON4' uObj obj ot isOT a b c e -> ON5' uObj obj ot isOT a b c d e
  ON5e :: Inst5 isOT a b c d e => ON4' uObj obj ot isOT a b c d -> ON5' uObj obj ot isOT a b c d e
  --
  O6a :: Inst6 isOT a b c d e f => obj a -> ON6' uObj obj ot isOT a b c d e f
  O6b :: Inst6 isOT a b c d e f => obj b -> ON6' uObj obj ot isOT a b c d e f
  O6c :: Inst6 isOT a b c d e f => obj c -> ON6' uObj obj ot isOT a b c d e f
  O6d :: Inst6 isOT a b c d e f => obj d -> ON6' uObj obj ot isOT a b c d e f
  O6e :: Inst6 isOT a b c d e f => obj e -> ON6' uObj obj ot isOT a b c d e f
  O6f :: Inst6 isOT a b c d e f => obj f -> ON6' uObj obj ot isOT a b c d e f
  ON6a :: Inst6 isOT a b c d e f => ON5' uObj obj ot isOT b c d e f -> ON6' uObj obj ot isOT a b c d e f
  ON6b :: Inst6 isOT a b c d e f => ON5' uObj obj ot isOT a c d e f -> ON6' uObj obj ot isOT a b c d e f
  ON6c :: Inst6 isOT a b c d e f => ON5' uObj obj ot isOT a b d e f -> ON6' uObj obj ot isOT a b c d e f
  ON6d :: Inst6 isOT a b c d e f => ON5' uObj obj ot isOT a b c e f -> ON6' uObj obj ot isOT a b c d e f
  ON6e :: Inst6 isOT a b c d e f => ON5' uObj obj ot isOT a b c d f -> ON6' uObj obj ot isOT a b c d e f
  ON6f :: Inst6 isOT a b c d e f => ON5' uObj obj ot isOT a b c d e -> ON6' uObj obj ot isOT a b c d e f
  --
  O7a :: Inst7 isOT a b c d e f g => obj a -> ON7' uObj obj ot isOT a b c d e f g
  O7b :: Inst7 isOT a b c d e f g => obj b -> ON7' uObj obj ot isOT a b c d e f g
  O7c :: Inst7 isOT a b c d e f g => obj c -> ON7' uObj obj ot isOT a b c d e f g
  O7d :: Inst7 isOT a b c d e f g => obj d -> ON7' uObj obj ot isOT a b c d e f g
  O7e :: Inst7 isOT a b c d e f g => obj e -> ON7' uObj obj ot isOT a b c d e f g
  O7f :: Inst7 isOT a b c d e f g => obj f -> ON7' uObj obj ot isOT a b c d e f g
  O7g :: Inst7 isOT a b c d e f g => obj g -> ON7' uObj obj ot isOT a b c d e f g
  ON7a :: Inst7 isOT a b c d e f g => ON6' uObj obj ot isOT b c d e f g -> ON7' uObj obj ot isOT a b c d e f g
  ON7b :: Inst7 isOT a b c d e f g => ON6' uObj obj ot isOT a c d e f g -> ON7' uObj obj ot isOT a b c d e f g
  ON7c :: Inst7 isOT a b c d e f g => ON6' uObj obj ot isOT a b d e f g -> ON7' uObj obj ot isOT a b c d e f g
  ON7d :: Inst7 isOT a b c d e f g => ON6' uObj obj ot isOT a b c e f g -> ON7' uObj obj ot isOT a b c d e f g
  ON7e :: Inst7 isOT a b c d e f g => ON6' uObj obj ot isOT a b c d f g -> ON7' uObj obj ot isOT a b c d e f g
  ON7f :: Inst7 isOT a b c d e f g => ON6' uObj obj ot isOT a b c d e g -> ON7' uObj obj ot isOT a b c d e f g
  ON7g :: Inst7 isOT a b c d e f g => ON6' uObj obj ot isOT a b c d e f -> ON7' uObj obj ot isOT a b c d e f g
  --
  O8a :: Inst8 isOT a b c d e f g h => obj a -> ON8' uObj obj ot isOT a b c d e f g h
  O8b :: Inst8 isOT a b c d e f g h => obj b -> ON8' uObj obj ot isOT a b c d e f g h
  O8c :: Inst8 isOT a b c d e f g h => obj c -> ON8' uObj obj ot isOT a b c d e f g h
  O8d :: Inst8 isOT a b c d e f g h => obj d -> ON8' uObj obj ot isOT a b c d e f g h
  O8e :: Inst8 isOT a b c d e f g h => obj e -> ON8' uObj obj ot isOT a b c d e f g h
  O8f :: Inst8 isOT a b c d e f g h => obj f -> ON8' uObj obj ot isOT a b c d e f g h
  O8g :: Inst8 isOT a b c d e f g h => obj g -> ON8' uObj obj ot isOT a b c d e f g h
  O8h :: Inst8 isOT a b c d e f g h => obj h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8a :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT b c d e f g h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8b :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT a c d e f g h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8c :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT a b d e f g h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8d :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT a b c e f g h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8e :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT a b c d f g h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8f :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT a b c d e g h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8g :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT a b c d e f h -> ON8' uObj obj ot isOT a b c d e f g h
  ON8h :: Inst8 isOT a b c d e f g h => ON7' uObj obj ot isOT a b c d e f g -> ON8' uObj obj ot isOT a b c d e f g h
  --
  O9a :: Inst9 isOT a b c d e f g h i => obj a -> ON9' uObj obj ot isOT a b c d e f g h i
  O9b :: Inst9 isOT a b c d e f g h i => obj b -> ON9' uObj obj ot isOT a b c d e f g h i
  O9c :: Inst9 isOT a b c d e f g h i => obj c -> ON9' uObj obj ot isOT a b c d e f g h i
  O9d :: Inst9 isOT a b c d e f g h i => obj d -> ON9' uObj obj ot isOT a b c d e f g h i
  O9e :: Inst9 isOT a b c d e f g h i => obj e -> ON9' uObj obj ot isOT a b c d e f g h i
  O9f :: Inst9 isOT a b c d e f g h i => obj f -> ON9' uObj obj ot isOT a b c d e f g h i
  O9g :: Inst9 isOT a b c d e f g h i => obj g -> ON9' uObj obj ot isOT a b c d e f g h i
  O9h :: Inst9 isOT a b c d e f g h i => obj h -> ON9' uObj obj ot isOT a b c d e f g h i
  O9i :: Inst9 isOT a b c d e f g h i => obj i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9a :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT b c d e f g h i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9b :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a c d e f g h i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9c :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a b d e f g h i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9d :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a b c e f g h i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9e :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a b c d f g h i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9f :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a b c d e g h i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9g :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a b c d e f h i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9h :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a b c d e f g i -> ON9' uObj obj ot isOT a b c d e f g h i
  ON9i :: Inst9 isOT a b c d e f g h i => ON8' uObj obj ot isOT a b c d e f g h -> ON9' uObj obj ot isOT a b c d e f g h i
  --
  O10b :: Inst10 isOT a b c d e f g h i j => obj b -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10a :: Inst10 isOT a b c d e f g h i j => obj a -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10c :: Inst10 isOT a b c d e f g h i j => obj c -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10d :: Inst10 isOT a b c d e f g h i j => obj d -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10e :: Inst10 isOT a b c d e f g h i j => obj e -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10f :: Inst10 isOT a b c d e f g h i j => obj f -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10g :: Inst10 isOT a b c d e f g h i j => obj g -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10h :: Inst10 isOT a b c d e f g h i j => obj h -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10i :: Inst10 isOT a b c d e f g h i j => obj i -> ON10' uObj obj ot isOT a b c d e f g h i j
  O10j :: Inst10 isOT a b c d e f g h i j => obj j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10a :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT b c d e f g h i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10b :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a c d e f g h i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10c :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b d e f g h i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10d :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b c e f g h i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10e :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b c d f g h i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10f :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b c d e g h i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10g :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b c d e f h i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10h :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b c d e f g i j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10i :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b c d e f g h j -> ON10' uObj obj ot isOT a b c d e f g h i j
  ON10j :: Inst10 isOT a b c d e f g h i j => ON9' uObj obj ot isOT a b c d e f g h i -> ON10' uObj obj ot isOT a b c d e f g h i j
  --
  O11b :: Inst11 isOT a b c d e f g h i j k => obj b -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11a :: Inst11 isOT a b c d e f g h i j k => obj a -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11c :: Inst11 isOT a b c d e f g h i j k => obj c -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11d :: Inst11 isOT a b c d e f g h i j k => obj d -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11e :: Inst11 isOT a b c d e f g h i j k => obj e -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11f :: Inst11 isOT a b c d e f g h i j k => obj f -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11g :: Inst11 isOT a b c d e f g h i j k => obj g -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11h :: Inst11 isOT a b c d e f g h i j k => obj h -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11i :: Inst11 isOT a b c d e f g h i j k => obj i -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11j :: Inst11 isOT a b c d e f g h i j k => obj j -> ON11' uObj obj ot isOT a b c d e f g h i j k
  O11k :: Inst11 isOT a b c d e f g h i j k => obj k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11a :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT b c d e f g h i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11b :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a c d e f g h i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11c :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b d e f g h i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11d :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c e f g h i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11e :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c d f g h i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11f :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c d e g h i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11g :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c d e f h i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11h :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c d e f g i j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11i :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c d e f g h j k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11j :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c d e f g h i k -> ON11' uObj obj ot isOT a b c d e f g h i j k
  ON11k :: Inst11 isOT a b c d e f g h i j k => ON10' uObj obj ot isOT a b c d e f g h i j -> ON11' uObj obj ot isOT a b c d e f g h i j k
  --
  O12b :: Inst12 isOT a b c d e f g h i j k l => obj b -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12a :: Inst12 isOT a b c d e f g h i j k l => obj a -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12c :: Inst12 isOT a b c d e f g h i j k l => obj c -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12d :: Inst12 isOT a b c d e f g h i j k l => obj d -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12e :: Inst12 isOT a b c d e f g h i j k l => obj e -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12f :: Inst12 isOT a b c d e f g h i j k l => obj f -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12g :: Inst12 isOT a b c d e f g h i j k l => obj g -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12h :: Inst12 isOT a b c d e f g h i j k l => obj h -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12i :: Inst12 isOT a b c d e f g h i j k l => obj i -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12j :: Inst12 isOT a b c d e f g h i j k l => obj j -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12k :: Inst12 isOT a b c d e f g h i j k l => obj k -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  O12l :: Inst12 isOT a b c d e f g h i j k l => obj l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12a :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT b c d e f g h i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12b :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a c d e f g h i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12c :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b d e f g h i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12d :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c e f g h i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12e :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d f g h i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12f :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d e g h i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12g :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d e f h i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12h :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d e f g i j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12i :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d e f g h j k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12j :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d e f g h i k l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12k :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d e f g h i j l -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  ON12l :: Inst12 isOT a b c d e f g h i j k l => ON11' uObj obj ot isOT a b c d e f g h i j k -> ON12' uObj obj ot isOT a b c d e f g h i j k l
  deriving (Typeable)

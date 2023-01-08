{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.OTN (
  OTN,
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
  mkOT0,
  mkOT1,
  mkOT2,
  mkOT3,
  mkOT4,
  mkOT5,
  mkOT6,
  mkOT7,
  mkOT8,
  mkOT9,
  mkOT10,
  mkOT11,
  mkOT12,
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
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OTKN (
  OTK0,
  OTK1,
  OTK10,
  OTK11,
  OTK12,
  OTK2,
  OTK3,
  OTK4,
  OTK5,
  OTK6,
  OTK7,
  OTK8,
  OTK9,
 )
import safe MtgPure.Model.Object.OTN_ (OTN' (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType)

type OTN = OTN' ObjectType IsObjectType

type OT0 = OTN OTK0

type OT1 a = OTN (OTK1 a)

type OT2 a b = OTN (OTK2 a b)

type OT3 a b c = OTN (OTK3 a b c)

type OT4 a b c d = OTN (OTK4 a b c d)

type OT5 a b c d e = OTN (OTK5 a b c d e)

type OT6 a b c d e f = OTN (OTK6 a b c d e f)

type OT7 a b c d e f g = OTN (OTK7 a b c d e f g)

type OT8 a b c d e f g h = OTN (OTK8 a b c d e f g h)

type OT9 a b c d e f g h i = OTN (OTK9 a b c d e f g h i)

type OT10 a b c d e f g h i j = OTN (OTK10 a b c d e f g h i j)

type OT11 a b c d e f g h i j k = OTN (OTK11 a b c d e f g h i j k)

type OT12 a b c d e f g h i j k l = OTN (OTK12 a b c d e f g h i j k l)

-- NOTE: These mkOT* functions exist to avoid need to apply `@ObjectType @IsObjectType` to constructors,
-- since these appear necessarily before any of the `a b c...` type arguments in the implied `forall` list.
-- (An explicit `forall` cannot workaround the issue due to type arguments needing to appear before erased ones).
mkOT0 :: OT0
mkOT0 = OT0

mkOT1 :: Inst1 IsObjectType a => OT1 a
mkOT1 = OT1

mkOT2 :: Inst2 IsObjectType a b => OT2 a b
mkOT2 = OT2

mkOT3 :: Inst3 IsObjectType a b c => OT3 a b c
mkOT3 = OT3

mkOT4 :: Inst4 IsObjectType a b c d => OT4 a b c d
mkOT4 = OT4

mkOT5 :: Inst5 IsObjectType a b c d e => OT5 a b c d e
mkOT5 = OT5

mkOT6 :: Inst6 IsObjectType a b c d e f => OT6 a b c d e f
mkOT6 = OT6

mkOT7 :: Inst7 IsObjectType a b c d e f g => OT7 a b c d e f g
mkOT7 = OT7

mkOT8 :: Inst8 IsObjectType a b c d e f g h => OT8 a b c d e f g h
mkOT8 = OT8

mkOT9 :: Inst9 IsObjectType a b c d e f g h i => OT9 a b c d e f g h i
mkOT9 = OT9

mkOT10 :: Inst10 IsObjectType a b c d e f g h i j => OT10 a b c d e f g h i j
mkOT10 = OT10

mkOT11 :: Inst11 IsObjectType a b c d e f g h i j k => OT11 a b c d e f g h i j k
mkOT11 = OT11

mkOT12 :: Inst12 IsObjectType a b c d e f g h i j k l => OT12 a b c d e f g h i j k l
mkOT12 = OT12

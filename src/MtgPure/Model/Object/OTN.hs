{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

-- NOTE: OTN stands for ObjectTypeN
module MtgPure.Model.Object.OTN (
  OTN (..),
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
  OT13,
) where

import safe Data.Inst (
  Inst1,
  Inst10,
  Inst11,
  Inst12,
  Inst13,
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
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OTKN (
  OTK0,
  OTK1,
  OTK10,
  OTK11,
  OTK12,
  OTK13,
  OTK2,
  OTK3,
  OTK4,
  OTK5,
  OTK6,
  OTK7,
  OTK8,
  OTK9,
 )

data OTN (otk :: k) :: Type where
  OT0 :: OTN OTK0
  OT1 :: (Inst1 IsObjectType a) => OTN (OTK1 a)
  OT2 :: (Inst2 IsObjectType a b) => OTN (OTK2 a b)
  OT3 :: (Inst3 IsObjectType a b c) => OTN (OTK3 a b c)
  OT4 :: (Inst4 IsObjectType a b c d) => OTN (OTK4 a b c d)
  OT5 :: (Inst5 IsObjectType a b c d e) => OTN (OTK5 a b c d e)
  OT6 :: (Inst6 IsObjectType a b c d e f) => OTN (OTK6 a b c d e f)
  OT7 :: (Inst7 IsObjectType a b c d e f g) => OTN (OTK7 a b c d e f g)
  OT8 :: (Inst8 IsObjectType a b c d e f g h) => OTN (OTK8 a b c d e f g h)
  OT9 :: (Inst9 IsObjectType a b c d e f g h i) => OTN (OTK9 a b c d e f g h i)
  OT10 :: (Inst10 IsObjectType a b c d e f g h i j) => OTN (OTK10 a b c d e f g h i j)
  OT11 :: (Inst11 IsObjectType a b c d e f g h i j k) => OTN (OTK11 a b c d e f g h i j k)
  OT12 :: (Inst12 IsObjectType a b c d e f g h i j k l) => OTN (OTK12 a b c d e f g h i j k l)
  OT13 :: (Inst13 IsObjectType a b c d e f g h i j k l m) => OTN (OTK13 a b c d e f g h i j k l m)

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

type OT13 a b c d e f g h i j k l m = OTN (OTK13 a b c d e f g h i j k l m)

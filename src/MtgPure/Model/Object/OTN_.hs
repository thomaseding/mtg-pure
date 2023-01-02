{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.OTN_ (
  OTN' (..),
  OT0',
  OT1',
  OT2',
  OT3',
  OT4',
  OT5',
  OT6',
  OT7',
  OT8',
  OT9',
  OT10',
  OT11',
  OT12',
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
import safe MtgPure.Model.Object.OTKN_ (
  OTK0',
  OTK1',
  OTK10',
  OTK11',
  OTK12',
  OTK2',
  OTK3',
  OTK4',
  OTK5',
  OTK6',
  OTK7',
  OTK8',
  OTK9',
 )

data OTN' (ot :: k0) (isOT :: k1 -> Constraint) (otk :: k2) :: Type where
  OT0 :: OTN' ot isOT (OTK0' ot)
  OT1 :: Inst1 isOT a => OTN' ot isOT (OTK1' ot a)
  OT2 :: Inst2 isOT a b => OTN' ot isOT (OTK2' ot a b)
  OT3 :: Inst3 isOT a b c => OTN' ot isOT (OTK3' ot a b c)
  OT4 :: Inst4 isOT a b c d => OTN' ot isOT (OTK4' ot a b c d)
  OT5 :: Inst5 isOT a b c d e => OTN' ot isOT (OTK5' ot a b c d e)
  OT6 :: Inst6 isOT a b c d e f => OTN' ot isOT (OTK6' ot a b c d e f)
  OT7 :: Inst7 isOT a b c d e f g => OTN' ot isOT (OTK7' ot a b c d e f g)
  OT8 :: Inst8 isOT a b c d e f g h => OTN' ot isOT (OTK8' ot a b c d e f g h)
  OT9 :: Inst9 isOT a b c d e f g h i => OTN' ot isOT (OTK9' ot a b c d e f g h i)
  OT10 :: Inst10 isOT a b c d e f g h i j => OTN' ot isOT (OTK10' ot a b c d e f g h i j)
  OT11 :: Inst11 isOT a b c d e f g h i j k => OTN' ot isOT (OTK11' ot a b c d e f g h i j k)
  OT12 :: Inst12 isOT a b c d e f g h i j k l => OTN' ot isOT (OTK12' ot a b c d e f g h i j k l)

type OT0' ot isOT = OTN' ot isOT (OTK0' ot)

type OT1' ot isOT a = OTN' ot isOT (OTK1' ot a)

type OT2' ot isOT a b = OTN' ot isOT (OTK2' ot a b)

type OT3' ot isOT a b c = OTN' ot isOT (OTK3' ot a b c)

type OT4' ot isOT a b c d = OTN' ot isOT (OTK4' ot a b c d)

type OT5' ot isOT a b c d e = OTN' ot isOT (OTK5' ot a b c d e)

type OT6' ot isOT a b c d e f = OTN' ot isOT (OTK6' ot a b c d e f)

type OT7' ot isOT a b c d e f g = OTN' ot isOT (OTK7' ot a b c d e f g)

type OT8' ot isOT a b c d e f g h = OTN' ot isOT (OTK8' ot a b c d e f g h)

type OT9' ot isOT a b c d e f g h i = OTN' ot isOT (OTK9' ot a b c d e f g h i)

type OT10' ot isOT a b c d e f g h i j = OTN' ot isOT (OTK10' ot a b c d e f g h i j)

type OT11' ot isOT a b c d e f g h i j k = OTN' ot isOT (OTK11' ot a b c d e f g h i j k)

type OT12' ot isOT a b c d e f g h i j k l = OTN' ot isOT (OTK12' ot a b c d e f g h i j k l)

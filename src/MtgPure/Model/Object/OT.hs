{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.OT (
  OT' (..),
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

data OT' (isOT :: k' -> Constraint) (otk :: k) :: Type where
  OT0 :: OT' isOT OTK0
  OT1 :: Inst1 isOT a => OT' isOT (OTK1 a)
  OT2 :: Inst2 isOT a b => OT' isOT (OTK2 a b)
  OT3 :: Inst3 isOT a b c => OT' isOT (OTK3 a b c)
  OT4 :: Inst4 isOT a b c d => OT' isOT (OTK4 a b c d)
  OT5 :: Inst5 isOT a b c d e => OT' isOT (OTK5 a b c d e)
  OT6 :: Inst6 isOT a b c d e f => OT' isOT (OTK6 a b c d e f)
  OT7 :: Inst7 isOT a b c d e f g => OT' isOT (OTK7 a b c d e f g)
  OT8 :: Inst8 isOT a b c d e f g h => OT' isOT (OTK8 a b c d e f g h)
  OT9 :: Inst9 isOT a b c d e f g h i => OT' isOT (OTK9 a b c d e f g h i)
  OT10 :: Inst10 isOT a b c d e f g h i j => OT' isOT (OTK10 a b c d e f g h i j)
  OT11 :: Inst11 isOT a b c d e f g h i j k => OT' isOT (OTK11 a b c d e f g h i j k)
  OT12 :: Inst12 isOT a b c d e f g h i j k l => OT' isOT (OTK12 a b c d e f g h i j k l)

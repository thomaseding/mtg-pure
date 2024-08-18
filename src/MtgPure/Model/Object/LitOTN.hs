{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.LitOTN (
  LitOTN (..),
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
  OTN (..),
 )

class LitOTN (ot :: Type) where
  litOTN :: ot
  mapOTN :: (forall k (otk :: k). (ot ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT ot

instance LitOTN OT0 where
  litOTN :: OT0
  litOTN = OT0

  mapOTN :: (forall k (otk :: k). (OT0 ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT OT0
  mapOTN f = f litOTN

instance (Inst1 IsObjectType a) => LitOTN (OT1 a) where
  litOTN :: (Inst1 IsObjectType a) => OT1 a
  litOTN = OT1

  mapOTN :: (Inst1 IsObjectType a) => (forall k (otk :: k). (OT1 a ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT1 a)
  mapOTN f = f litOTN

instance (Inst2 IsObjectType a b) => LitOTN (OT2 a b) where
  litOTN :: (Inst2 IsObjectType a b) => OT2 a b
  litOTN = OT2

  mapOTN :: (Inst2 IsObjectType a b) => (forall k (otk :: k). (OT2 a b ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT2 a b)
  mapOTN f = f litOTN

instance (Inst3 IsObjectType a b c) => LitOTN (OT3 a b c) where
  litOTN :: (Inst3 IsObjectType a b c) => OT3 a b c
  litOTN = OT3

  mapOTN :: (Inst3 IsObjectType a b c) => (forall k (otk :: k). (OT3 a b c ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT3 a b c)
  mapOTN f = f litOTN

instance (Inst4 IsObjectType a b c d) => LitOTN (OT4 a b c d) where
  litOTN :: (Inst4 IsObjectType a b c d) => OT4 a b c d
  litOTN = OT4

  mapOTN :: (Inst4 IsObjectType a b c d) => (forall k (otk :: k). (OT4 a b c d ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT4 a b c d)
  mapOTN f = f litOTN

instance (Inst5 IsObjectType a b c d e) => LitOTN (OT5 a b c d e) where
  litOTN :: (Inst5 IsObjectType a b c d e) => OT5 a b c d e
  litOTN = OT5

  mapOTN :: (Inst5 IsObjectType a b c d e) => (forall k (otk :: k). (OT5 a b c d e ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT5 a b c d e)
  mapOTN f = f litOTN

instance (Inst6 IsObjectType a b c d e f) => LitOTN (OT6 a b c d e f) where
  litOTN :: (Inst6 IsObjectType a b c d e f) => OT6 a b c d e f
  litOTN = OT6

  mapOTN :: (Inst6 IsObjectType a b c d e f) => (forall k (otk :: k). (OT6 a b c d e f ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT6 a b c d e f)
  mapOTN f = f litOTN

instance (Inst7 IsObjectType a b c d e f g) => LitOTN (OT7 a b c d e f g) where
  litOTN :: (Inst7 IsObjectType a b c d e f g) => OT7 a b c d e f g
  litOTN = OT7

  mapOTN :: (Inst7 IsObjectType a b c d e f g) => (forall k (otk :: k). (OT7 a b c d e f g ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT7 a b c d e f g)
  mapOTN f = f litOTN

instance (Inst8 IsObjectType a b c d e f g h) => LitOTN (OT8 a b c d e f g h) where
  litOTN :: (Inst8 IsObjectType a b c d e f g h) => OT8 a b c d e f g h
  litOTN = OT8

  mapOTN :: (Inst8 IsObjectType a b c d e f g h) => (forall k (otk :: k). (OT8 a b c d e f g h ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT8 a b c d e f g h)
  mapOTN f = f litOTN

instance (Inst9 IsObjectType a b c d e f g h i) => LitOTN (OT9 a b c d e f g h i) where
  litOTN :: (Inst9 IsObjectType a b c d e f g h i) => OT9 a b c d e f g h i
  litOTN = OT9

  mapOTN :: (Inst9 IsObjectType a b c d e f g h i) => (forall k (otk :: k). (OT9 a b c d e f g h i ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT9 a b c d e f g h i)
  mapOTN f = f litOTN

instance (Inst10 IsObjectType a b c d e f g h i j) => LitOTN (OT10 a b c d e f g h i j) where
  litOTN :: (Inst10 IsObjectType a b c d e f g h i j) => OT10 a b c d e f g h i j
  litOTN = OT10

  mapOTN :: (Inst10 IsObjectType a b c d e f g h i j) => (forall k (otk :: k). (OT10 a b c d e f g h i j ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT10 a b c d e f g h i j)
  mapOTN f = f litOTN

instance (Inst11 IsObjectType a b c d e f g h i j k) => LitOTN (OT11 a b c d e f g h i j k) where
  litOTN :: (Inst11 IsObjectType a b c d e f g h i j k) => OT11 a b c d e f g h i j k
  litOTN = OT11

  mapOTN :: (Inst11 IsObjectType a b c d e f g h i j k) => (forall k1 (otk :: k1). (OT11 a b c d e f g h i j k ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT11 a b c d e f g h i j k)
  mapOTN f = f litOTN

instance (Inst12 IsObjectType a b c d e f g h i j k l) => LitOTN (OT12 a b c d e f g h i j k l) where
  litOTN :: (Inst12 IsObjectType a b c d e f g h i j k l) => OT12 a b c d e f g h i j k l
  litOTN = OT12

  mapOTN :: (Inst12 IsObjectType a b c d e f g h i j k l) => (forall k1 (otk :: k1). (OT12 a b c d e f g h i j k l ~ OTN otk) => OTN otk -> liftOT (OTN otk)) -> liftOT (OT12 a b c d e f g h i j k l)
  mapOTN f = f litOTN

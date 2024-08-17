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
  litOTN = OT0
  mapOTN f = f litOTN

instance (Inst1 IsObjectType a) => LitOTN (OT1 a) where
  litOTN = OT1
  mapOTN f = f litOTN

instance (Inst2 IsObjectType a b) => LitOTN (OT2 a b) where
  litOTN = OT2
  mapOTN f = f litOTN

instance (Inst3 IsObjectType a b c) => LitOTN (OT3 a b c) where
  litOTN = OT3
  mapOTN f = f litOTN

instance (Inst4 IsObjectType a b c d) => LitOTN (OT4 a b c d) where
  litOTN = OT4
  mapOTN f = f litOTN

instance (Inst5 IsObjectType a b c d e) => LitOTN (OT5 a b c d e) where
  litOTN = OT5
  mapOTN f = f litOTN

instance (Inst6 IsObjectType a b c d e f) => LitOTN (OT6 a b c d e f) where
  litOTN = OT6
  mapOTN f = f litOTN

instance (Inst7 IsObjectType a b c d e f g) => LitOTN (OT7 a b c d e f g) where
  litOTN = OT7
  mapOTN f = f litOTN

instance (Inst8 IsObjectType a b c d e f g h) => LitOTN (OT8 a b c d e f g h) where
  litOTN = OT8
  mapOTN f = f litOTN

instance (Inst9 IsObjectType a b c d e f g h i) => LitOTN (OT9 a b c d e f g h i) where
  litOTN = OT9
  mapOTN f = f litOTN

instance (Inst10 IsObjectType a b c d e f g h i j) => LitOTN (OT10 a b c d e f g h i j) where
  litOTN = OT10
  mapOTN f = f litOTN

instance (Inst11 IsObjectType a b c d e f g h i j k) => LitOTN (OT11 a b c d e f g h i j k) where
  litOTN = OT11
  mapOTN f = f litOTN

instance (Inst12 IsObjectType a b c d e f g h i j k l) => LitOTN (OT12 a b c d e f g h i j k l) where
  litOTN = OT12
  mapOTN f = f litOTN

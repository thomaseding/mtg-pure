{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.LitOT (
  LitOT (..),
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
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.OT (OT' (..))
import safe MtgPure.Model.OTN (
  OT,
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

class LitOT (ot :: Type) where
  litOT :: ot
  mapOT :: (forall k (otk :: k). OT otk -> liftOT (OT otk)) -> liftOT ot

instance LitOT OT0 where
  litOT = OT0
  mapOT f = f litOT

instance Inst1 IsObjectType a => LitOT (OT1 a) where
  litOT = OT1
  mapOT f = f litOT

instance Inst2 IsObjectType a b => LitOT (OT2 a b) where
  litOT = OT2
  mapOT f = f litOT

instance Inst3 IsObjectType a b c => LitOT (OT3 a b c) where
  litOT = OT3
  mapOT f = f litOT

instance Inst4 IsObjectType a b c d => LitOT (OT4 a b c d) where
  litOT = OT4
  mapOT f = f litOT

instance Inst5 IsObjectType a b c d e => LitOT (OT5 a b c d e) where
  litOT = OT5
  mapOT f = f litOT

instance Inst6 IsObjectType a b c d e f => LitOT (OT6 a b c d e f) where
  litOT = OT6
  mapOT f = f litOT

instance Inst7 IsObjectType a b c d e f g => LitOT (OT7 a b c d e f g) where
  litOT = OT7
  mapOT f = f litOT

instance Inst8 IsObjectType a b c d e f g h => LitOT (OT8 a b c d e f g h) where
  litOT = OT8
  mapOT f = f litOT

instance Inst9 IsObjectType a b c d e f g h i => LitOT (OT9 a b c d e f g h i) where
  litOT = OT9
  mapOT f = f litOT

instance Inst10 IsObjectType a b c d e f g h i j => LitOT (OT10 a b c d e f g h i j) where
  litOT = OT10
  mapOT f = f litOT

instance Inst11 IsObjectType a b c d e f g h i j k => LitOT (OT11 a b c d e f g h i j k) where
  litOT = OT11
  mapOT f = f litOT

instance Inst12 IsObjectType a b c d e f g h i j k l => LitOT (OT12 a b c d e f g h i j k l) where
  litOT = OT12
  mapOT f = f litOT

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant bracket" #-}

module MtgPure.Model.ToObjectN.Classes (
  ToObject1' (..),
  ToObject2' (..),
  ToObject3' (..),
  ToObject4' (..),
  ToObject5' (..),
  ToObject6' (..),
  ToObject7' (..),
  ToObject8' (..),
  ToObject9' (..),
  ToObject10' (..),
  ToObject11' (..),
  ToObject12' (..),
  ToObject1 (..),
  ToObject2 (..),
  ToObject3 (..),
  ToObject4 (..),
  ToObject5 (..),
  ToObject6 (..),
  ToObject7 (..),
  ToObject8 (..),
  ToObject9 (..),
  ToObject10 (..),
  ToObject11 (..),
  ToObject12 (..),
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
import safe MtgPure.Model.Object (
  IsObjectType (idToObject),
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
  Object (..),
 )
import safe MtgPure.Model.ObjectN (ObjectN (..))

class (IsObjectType z, Inst1 IsObjectType a) => ToObject1' z a where
  -- NOTE: This is the same as `O1` for N=1, but uniqueness is not guaranteed
  -- for other values of N. This exists for uniformity.
  toObject1' :: Object z -> ObjectN (OT1 a)

class (IsObjectType z, Inst2 IsObjectType a b) => ToObject2' z a b where
  toObject2' :: Object z -> ObjectN (OT2 a b)

class (IsObjectType z, Inst3 IsObjectType a b c) => ToObject3' z a b c where
  toObject3' :: Object z -> ObjectN (OT3 a b c)

class (IsObjectType z, Inst4 IsObjectType a b c d) => ToObject4' z a b c d where
  toObject4' :: Object z -> ObjectN (OT4 a b c d)

class (IsObjectType z, Inst5 IsObjectType a b c d e) => ToObject5' z a b c d e where
  toObject5' :: Object z -> ObjectN (OT5 a b c d e)

class (IsObjectType z, Inst6 IsObjectType a b c d e f) => ToObject6' z a b c d e f where
  toObject6' :: Object z -> ObjectN (OT6 a b c d e f)

class (IsObjectType z, Inst7 IsObjectType a b c d e f g) => ToObject7' z a b c d e f g where
  toObject7' :: Object z -> ObjectN (OT7 a b c d e f g)

class (IsObjectType z, Inst8 IsObjectType a b c d e f g h) => ToObject8' z a b c d e f g h where
  toObject8' :: Object z -> ObjectN (OT8 a b c d e f g h)

class (IsObjectType z, Inst9 IsObjectType a b c d e f g h i) => ToObject9' z a b c d e f g h i where
  toObject9' :: Object z -> ObjectN (OT9 a b c d e f g h i)

class (IsObjectType z, Inst10 IsObjectType a b c d e f g h i j) => ToObject10' z a b c d e f g h i j where
  toObject10' :: Object z -> ObjectN (OT10 a b c d e f g h i j)

class (IsObjectType z, Inst11 IsObjectType a b c d e f g h i j k) => ToObject11' z a b c d e f g h i j k where
  toObject11' :: Object z -> ObjectN (OT11 a b c d e f g h i j k)

class (IsObjectType z, Inst12 IsObjectType a b c d e f g h i j k l) => ToObject12' z a b c d e f g h i j k l where
  toObject12' :: Object z -> ObjectN (OT12 a b c d e f g h i j k l)

class Inst1 IsObjectType a => ToObject1 ot a where
  toObject1 :: ObjectN ot -> ObjectN (OT1 a)

class Inst2 IsObjectType a b => ToObject2 ot a b where
  toObject2 :: ObjectN ot -> ObjectN (OT2 a b)

class Inst3 IsObjectType a b c => ToObject3 ot a b c where
  toObject3 :: ObjectN ot -> ObjectN (OT3 a b c)

class Inst4 IsObjectType a b c d => ToObject4 ot a b c d where
  toObject4 :: ObjectN ot -> ObjectN (OT4 a b c d)

class Inst5 IsObjectType a b c d e => ToObject5 ot a b c d e where
  toObject5 :: ObjectN ot -> ObjectN (OT5 a b c d e)

class Inst6 IsObjectType a b c d e f => ToObject6 ot a b c d e f where
  toObject6 :: ObjectN ot -> ObjectN (OT6 a b c d e f)

class Inst7 IsObjectType a b c d e f g => ToObject7 ot a b c d e f g where
  toObject7 :: ObjectN ot -> ObjectN (OT7 a b c d e f g)

class Inst8 IsObjectType a b c d e f g h => ToObject8 ot a b c d e f g h where
  toObject8 :: ObjectN ot -> ObjectN (OT8 a b c d e f g h)

class Inst9 IsObjectType a b c d e f g h i => ToObject9 ot a b c d e f g h i where
  toObject9 :: ObjectN ot -> ObjectN (OT9 a b c d e f g h i)

class Inst10 IsObjectType a b c d e f g h i j => ToObject10 ot a b c d e f g h i j where
  toObject10 :: ObjectN ot -> ObjectN (OT10 a b c d e f g h i j)

class Inst11 IsObjectType a b c d e f g h i j k => ToObject11 ot a b c d e f g h i j k where
  toObject11 :: ObjectN ot -> ObjectN (OT11 a b c d e f g h i j k)

class Inst12 IsObjectType a b c d e f g h i j k l => ToObject12 ot a b c d e f g h i j k l where
  toObject12 :: ObjectN ot -> ObjectN (OT12 a b c d e f g h i j k l)

instance IsObjectType a => ToObject1 OT0 a where
  toObject1 = \case
    O0 i -> O1 $ idToObject i

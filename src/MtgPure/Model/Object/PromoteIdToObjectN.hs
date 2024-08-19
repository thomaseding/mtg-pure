{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.PromoteIdToObjectN (
  PromoteIdToObjectN (..),
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
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (singObjectType))
import safe MtgPure.Model.Object.OTN (
  OT0,
  OT1,
  OT10,
  OT11,
  OT12,
  OT13,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OT8,
  OT9,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (..),
  UntypedObject (..),
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (ObjectN (..))

class PromoteIdToObjectN ot where
  promoteIdToObjectN :: ObjectId -> ObjectN ot

instance PromoteIdToObjectN OT0 where
  promoteIdToObjectN :: ObjectId -> ObjectN OT0
  promoteIdToObjectN = O0 . UntypedObject DefaultObjectDiscriminant

instance (Inst1 IsObjectType a) => PromoteIdToObjectN (OT1 a) where
  promoteIdToObjectN :: (Inst1 IsObjectType a) => ObjectId -> ObjectN (OT1 a)
  promoteIdToObjectN = O1 . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst2 IsObjectType a b) => PromoteIdToObjectN (OT2 a b) where
  promoteIdToObjectN :: (Inst2 IsObjectType a b) => ObjectId -> ObjectN (OT2 a b)
  promoteIdToObjectN = O2a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst3 IsObjectType a b c) => PromoteIdToObjectN (OT3 a b c) where
  promoteIdToObjectN :: (Inst3 IsObjectType a b c) => ObjectId -> ObjectN (OT3 a b c)
  promoteIdToObjectN = O3a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst4 IsObjectType a b c d) => PromoteIdToObjectN (OT4 a b c d) where
  promoteIdToObjectN :: (Inst4 IsObjectType a b c d) => ObjectId -> ObjectN (OT4 a b c d)
  promoteIdToObjectN = O4a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst5 IsObjectType a b c d e) => PromoteIdToObjectN (OT5 a b c d e) where
  promoteIdToObjectN :: (Inst5 IsObjectType a b c d e) => ObjectId -> ObjectN (OT5 a b c d e)
  promoteIdToObjectN = O5a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst6 IsObjectType a b c d e f) => PromoteIdToObjectN (OT6 a b c d e f) where
  promoteIdToObjectN :: (Inst6 IsObjectType a b c d e f) => ObjectId -> ObjectN (OT6 a b c d e f)
  promoteIdToObjectN = O6a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst7 IsObjectType a b c d e f g) => PromoteIdToObjectN (OT7 a b c d e f g) where
  promoteIdToObjectN :: (Inst7 IsObjectType a b c d e f g) => ObjectId -> ObjectN (OT7 a b c d e f g)
  promoteIdToObjectN = O7a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst8 IsObjectType a b c d e f g h) => PromoteIdToObjectN (OT8 a b c d e f g h) where
  promoteIdToObjectN :: (Inst8 IsObjectType a b c d e f g h) => ObjectId -> ObjectN (OT8 a b c d e f g h)
  promoteIdToObjectN = O8a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst9 IsObjectType a b c d e f g h i) => PromoteIdToObjectN (OT9 a b c d e f g h i) where
  promoteIdToObjectN :: (Inst9 IsObjectType a b c d e f g h i) => ObjectId -> ObjectN (OT9 a b c d e f g h i)
  promoteIdToObjectN = O9a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst10 IsObjectType a b c d e f g h i j) => PromoteIdToObjectN (OT10 a b c d e f g h i j) where
  promoteIdToObjectN :: (Inst10 IsObjectType a b c d e f g h i j) => ObjectId -> ObjectN (OT10 a b c d e f g h i j)
  promoteIdToObjectN = O10a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst11 IsObjectType a b c d e f g h i j k) => PromoteIdToObjectN (OT11 a b c d e f g h i j k) where
  promoteIdToObjectN :: (Inst11 IsObjectType a b c d e f g h i j k) => ObjectId -> ObjectN (OT11 a b c d e f g h i j k)
  promoteIdToObjectN = O11a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst12 IsObjectType a b c d e f g h i j k l) => PromoteIdToObjectN (OT12 a b c d e f g h i j k l) where
  promoteIdToObjectN :: (Inst12 IsObjectType a b c d e f g h i j k l) => ObjectId -> ObjectN (OT12 a b c d e f g h i j k l)
  promoteIdToObjectN = O12a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst13 IsObjectType a b c d e f g h i j k l m) => PromoteIdToObjectN (OT13 a b c d e f g h i j k l m) where
  promoteIdToObjectN :: (Inst13 IsObjectType a b c d e f g h i j k l m) => ObjectId -> ObjectN (OT13 a b c d e f g h i j k l m)
  promoteIdToObjectN = O13a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

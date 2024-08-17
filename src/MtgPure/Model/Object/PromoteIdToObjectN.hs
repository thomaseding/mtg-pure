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
  promoteIdToObjectN = O0 . UntypedObject DefaultObjectDiscriminant

instance (Inst1 IsObjectType a) => PromoteIdToObjectN (OT1 a) where
  promoteIdToObjectN = O1 . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst2 IsObjectType a b) => PromoteIdToObjectN (OT2 a b) where
  promoteIdToObjectN = O2a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst3 IsObjectType a b c) => PromoteIdToObjectN (OT3 a b c) where
  promoteIdToObjectN = O3a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst4 IsObjectType a b c d) => PromoteIdToObjectN (OT4 a b c d) where
  promoteIdToObjectN = O4a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst5 IsObjectType a b c d e) => PromoteIdToObjectN (OT5 a b c d e) where
  promoteIdToObjectN = O5a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst6 IsObjectType a b c d e f) => PromoteIdToObjectN (OT6 a b c d e f) where
  promoteIdToObjectN = O6a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst7 IsObjectType a b c d e f g) => PromoteIdToObjectN (OT7 a b c d e f g) where
  promoteIdToObjectN = O7a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst8 IsObjectType a b c d e f g h) => PromoteIdToObjectN (OT8 a b c d e f g h) where
  promoteIdToObjectN = O8a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst9 IsObjectType a b c d e f g h i) => PromoteIdToObjectN (OT9 a b c d e f g h i) where
  promoteIdToObjectN = O9a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst10 IsObjectType a b c d e f g h i j) => PromoteIdToObjectN (OT10 a b c d e f g h i j) where
  promoteIdToObjectN = O10a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst11 IsObjectType a b c d e f g h i j k) => PromoteIdToObjectN (OT11 a b c d e f g h i j k) where
  promoteIdToObjectN = O11a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

instance (Inst12 IsObjectType a b c d e f g h i j k l) => PromoteIdToObjectN (OT12 a b c d e f g h i j k l) where
  promoteIdToObjectN = O12a . Object (singObjectType @a) . UntypedObject DefaultObjectDiscriminant

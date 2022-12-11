{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object (
  Object (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ObjectId (
  GetObjectId (..),
  ObjectDiscriminant' (..),
  ObjectId (ObjectId),
  UntypedObject (..),
 )
import safe MtgPure.Model.ObjectType (
  ObjectType,
  SObjectType,
 )

data Object :: ObjectType -> Type where
  Object :: SObjectType a -> UntypedObject -> Object a
  deriving (Typeable)

instance Eq (Object ot) where
  Object _ (UntypedObject _ i) == Object _ (UntypedObject _ j) = i == j

instance Ord (Object ot) where
  compare (Object _ (UntypedObject _ i)) (Object _ (UntypedObject _ j)) = compare i j

instance GetObjectId (Object ot) where
  getUntypedObject = \case
    Object _ o -> o

instance Show (Object ot) where
  show = \case
    Object _ (UntypedObject (ObjectDiscriminant d) (ObjectId i)) -> case d of
      0 -> "O=" ++ show i
      _ -> "O=" ++ show i ++ "d" ++ show d

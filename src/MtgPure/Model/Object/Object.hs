{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.Object (
  Object (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OT (
  OT,
 )
import safe MtgPure.Model.Object.ObjectId (
  GetObjectId (..),
  ObjectDiscriminant' (..),
  ObjectId (ObjectId),
  UntypedObject (..),
 )
import safe MtgPure.Model.Object.SingOT (SingOT)

data Object :: OT -> Type where
  Object :: SingOT a -> UntypedObject -> Object a
  deriving (Typeable)

instance Eq (Object ot) where
  (==) :: Object ot -> Object ot -> Bool
  Object _ (UntypedObject _ i) == Object _ (UntypedObject _ j) = i == j

instance Ord (Object ot) where
  compare :: Object ot -> Object ot -> Ordering
  compare (Object _ (UntypedObject _ i)) (Object _ (UntypedObject _ j)) = compare i j

instance GetObjectId (Object ot) where
  getUntypedObject :: Object ot -> UntypedObject
  getUntypedObject = \case
    Object _ o -> o

instance Show (Object ot) where
  show :: Object ot -> String
  show = \case
    Object _ (UntypedObject (ObjectDiscriminant d) (ObjectId i)) -> case d of
      0 -> "O=" ++ show i
      _ -> "O=" ++ show i ++ "d" ++ show d

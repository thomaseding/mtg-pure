{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

-- TODO: Merge with Object.hs
module MtgPure.Model.ObjectId (
  UntypedObject (..),
  ObjectId (..),
  ObjectDiscriminant' (..),
  ObjectDiscriminant,
  pattern DefaultObjectDiscriminant,
  GetObjectId (..),
  getObjectId,
  getObjectDiscriminant,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

newtype ObjectId = ObjectId Int
  deriving (Show, Eq, Ord, Typeable)

newtype ObjectDiscriminant' a = ObjectDiscriminant a
  deriving (Eq, Functor, Ord, Show, Typeable)

type ObjectDiscriminant = ObjectDiscriminant' Int

pattern DefaultObjectDiscriminant :: ObjectDiscriminant
pattern DefaultObjectDiscriminant = ObjectDiscriminant 0

data UntypedObject :: Type where
  UntypedObject :: ObjectDiscriminant -> ObjectId -> UntypedObject
  deriving (Show, Eq, Ord, Typeable)

class GetObjectId a where
  getUntypedObject :: a -> UntypedObject

getObjectId :: GetObjectId a => a -> ObjectId
getObjectId x = case getUntypedObject x of
  UntypedObject _ i -> i

getObjectDiscriminant :: GetObjectId a => a -> ObjectDiscriminant
getObjectDiscriminant x = case getUntypedObject x of
  UntypedObject d _ -> d

instance GetObjectId ObjectId where
  getUntypedObject = UntypedObject DefaultObjectDiscriminant

instance (GetObjectId a, GetObjectId b) => GetObjectId (Either a b) where
  getUntypedObject = either getUntypedObject getUntypedObject

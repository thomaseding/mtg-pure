{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectId (
  ObjectId (..),
  GetObjectId (..),
) where

import safe Data.Typeable (Typeable)

newtype ObjectId = ObjectId Int
  deriving (Show, Eq, Ord, Typeable)

class GetObjectId a where
  getObjectId :: a -> ObjectId

instance GetObjectId ObjectId where
  getObjectId = id

instance (GetObjectId a, GetObjectId b) => GetObjectId (Either a b) where
  getObjectId = either getObjectId getObjectId

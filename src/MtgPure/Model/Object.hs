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

module MtgPure.Model.Object (
  Object (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ObjectId (ObjectId)
import safe MtgPure.Model.ObjectType (ObjectType (..), SObjectType)

data Object :: ObjectType -> Type where
  Object :: SObjectType a -> ObjectId -> Object a
  deriving (Show, Typeable)

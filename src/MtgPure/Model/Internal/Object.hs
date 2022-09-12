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

module MtgPure.Model.Internal.Object
  ( Object (..),
  )
where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import MtgPure.Model.Internal.ObjectId (ObjectId)
import MtgPure.Model.ObjectType
  ( ObjectType,
    SObjectType,
  )

data Object :: ObjectType -> Type where
  Object :: SObjectType a -> ObjectId -> Object a
  deriving (Show, Typeable)

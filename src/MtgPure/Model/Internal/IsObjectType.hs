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

module MtgPure.Model.Internal.IsObjectType
  ( IsObjectType (..),
    ObjectVisitor (..),
    visitObject',
  )
where

import Data.Typeable (Proxy, Typeable)
import MtgPure.Model.Internal.Object (Object (Object))
import MtgPure.Model.Internal.ObjectId (ObjectId)
import MtgPure.Model.Object
  ( OArtifact,
    OCreature,
    OEnchantment,
    OInstant,
    OLand,
    OPlaneswalker,
    OPlayer,
    OSorcery,
  )
import MtgPure.Model.ObjectType
  ( OTArtifact,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTPlayer,
    OTSorcery,
    ObjectType (..),
    SObjectType (..),
  )

data ObjectVisitor a = ObjectVisitor
  { visitArtifact :: OArtifact -> a,
    visitCreature :: OCreature -> a,
    visitEnchantment :: OEnchantment -> a,
    visitInstant :: OInstant -> a,
    visitLand :: OLand -> a,
    visitPlaneswalker :: OPlaneswalker -> a,
    visitPlayer :: OPlayer -> a,
    visitSorcery :: OSorcery -> a
  }

class Typeable a => IsObjectType (a :: ObjectType) where
  idToObject :: ObjectId -> Object a
  objectToId :: Object a -> ObjectId
  singObjectType :: Proxy a -> ObjectType
  visitObject :: ObjectVisitor b -> Object a -> b

visitObject' :: IsObjectType a => (forall b. IsObjectType b => Object b -> x) -> Object a -> x
visitObject' f = visitObject $ ObjectVisitor f f f f f f f f

instance IsObjectType OTArtifact where
  idToObject = Object SArtifact
  objectToId (Object SArtifact i) = i
  singObjectType _ = OTArtifact
  visitObject = visitArtifact

instance IsObjectType OTCreature where
  idToObject = Object SCreature
  objectToId (Object SCreature i) = i
  singObjectType _ = OTCreature
  visitObject = visitCreature

instance IsObjectType OTEnchantment where
  idToObject = Object SEnchantment
  objectToId (Object SEnchantment i) = i
  singObjectType _ = OTEnchantment
  visitObject = visitEnchantment

instance IsObjectType OTInstant where
  idToObject = Object SInstant
  objectToId (Object SInstant i) = i
  singObjectType _ = OTInstant
  visitObject = visitInstant

instance IsObjectType OTLand where
  idToObject = Object SLand
  objectToId (Object SLand i) = i
  singObjectType _ = OTLand
  visitObject = visitLand

instance IsObjectType OTPlaneswalker where
  idToObject = Object SPlaneswalker
  objectToId (Object SPlaneswalker i) = i
  singObjectType _ = OTPlaneswalker
  visitObject = visitPlaneswalker

instance IsObjectType OTPlayer where
  idToObject = Object SPlayer
  objectToId (Object SPlayer i) = i
  singObjectType _ = OTPlayer
  visitObject = visitPlayer

instance IsObjectType OTSorcery where
  idToObject = Object SSorcery
  objectToId (Object SSorcery i) = i
  singObjectType _ = OTSorcery
  visitObject = visitSorcery

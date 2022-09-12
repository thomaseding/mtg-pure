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

module MtgPure.Model.IsObjectType
  ( IsObjectType (..),
    ObjectVisitor (..),
    visitObject',
  )
where

import Data.Typeable (Proxy, Typeable)
import MtgPure.Model.Object
  ( Object (Object),
  )
import MtgPure.Model.ObjectId (ObjectId)
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
  { visitOArtifact :: Object 'OTArtifact -> a,
    visitOCreature :: Object 'OTCreature -> a,
    visitOEnchantment :: Object 'OTEnchantment -> a,
    visitOInstant :: Object 'OTInstant -> a,
    visitOLand :: Object 'OTLand -> a,
    visitOPlaneswalker :: Object 'OTPlaneswalker -> a,
    visitOPlayer :: Object 'OTPlayer -> a,
    visitOSorcery :: Object 'OTSorcery -> a
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
  visitObject = visitOArtifact

instance IsObjectType OTCreature where
  idToObject = Object SCreature
  objectToId (Object SCreature i) = i
  singObjectType _ = OTCreature
  visitObject = visitOCreature

instance IsObjectType OTEnchantment where
  idToObject = Object SEnchantment
  objectToId (Object SEnchantment i) = i
  singObjectType _ = OTEnchantment
  visitObject = visitOEnchantment

instance IsObjectType OTInstant where
  idToObject = Object SInstant
  objectToId (Object SInstant i) = i
  singObjectType _ = OTInstant
  visitObject = visitOInstant

instance IsObjectType OTLand where
  idToObject = Object SLand
  objectToId (Object SLand i) = i
  singObjectType _ = OTLand
  visitObject = visitOLand

instance IsObjectType OTPlaneswalker where
  idToObject = Object SPlaneswalker
  objectToId (Object SPlaneswalker i) = i
  singObjectType _ = OTPlaneswalker
  visitObject = visitOPlaneswalker

instance IsObjectType OTPlayer where
  idToObject = Object SPlayer
  objectToId (Object SPlayer i) = i
  singObjectType _ = OTPlayer
  visitObject = visitOPlayer

instance IsObjectType OTSorcery where
  idToObject = Object SSorcery
  objectToId (Object SSorcery i) = i
  singObjectType _ = OTSorcery
  visitObject = visitOSorcery

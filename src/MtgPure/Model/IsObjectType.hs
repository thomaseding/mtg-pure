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

module MtgPure.Model.IsObjectType (
  IsObjectType (..),
  ObjectVisitor (..),
  visitObject',
  objectTypeIndex,
) where

import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object (Object (Object))
import safe MtgPure.Model.ObjectId (ObjectId)
import safe MtgPure.Model.ObjectType (ObjectType (..), SObjectType (..))

data ObjectVisitor a = ObjectVisitor
  { visitOActivatedAbility :: Object 'OTActivatedAbility -> a
  , visitOArtifact :: Object 'OTArtifact -> a
  , visitOCreature :: Object 'OTCreature -> a
  , visitOEmblem :: Object 'OTEmblem -> a
  , visitOEnchantment :: Object 'OTEnchantment -> a
  , visitOInstant :: Object 'OTInstant -> a
  , visitOLand :: Object 'OTLand -> a
  , visitOPlaneswalker :: Object 'OTPlaneswalker -> a
  , visitOPlayer :: Object 'OTPlayer -> a
  , visitOSorcery :: Object 'OTSorcery -> a
  , visitOStaticAbility :: Object 'OTStaticAbility -> a
  , visitOTriggeredAbility :: Object 'OTTriggeredAbility -> a
  }
  deriving (Typeable)

class Typeable a => IsObjectType (a :: ObjectType) where
  idToObject :: ObjectId -> Object a
  objectToId :: Object a -> ObjectId
  singObjectType :: Proxy a -> SObjectType a
  litObjectType :: Proxy a -> ObjectType
  visitObject :: ObjectVisitor b -> Object a -> b

visitObject' ::
  IsObjectType a =>
  (forall b. IsObjectType b => Object b -> x) ->
  Object a ->
  x
visitObject' f = visitObject $ ObjectVisitor f f f f f f f f f f f f

objectTypeIndex :: IsObjectType a => Proxy a -> Int
objectTypeIndex = fromEnum . litObjectType

instance IsObjectType 'OTActivatedAbility where
  idToObject = Object SActivatedAbility
  objectToId (Object SActivatedAbility i) = i
  singObjectType _ = SActivatedAbility
  litObjectType _ = OTActivatedAbility
  visitObject = visitOActivatedAbility

instance IsObjectType 'OTArtifact where
  idToObject = Object SArtifact
  objectToId (Object SArtifact i) = i
  singObjectType _ = SArtifact
  litObjectType _ = OTArtifact
  visitObject = visitOArtifact

instance IsObjectType 'OTCreature where
  idToObject = Object SCreature
  objectToId (Object SCreature i) = i
  singObjectType _ = SCreature
  litObjectType _ = OTCreature
  visitObject = visitOCreature

instance IsObjectType 'OTEmblem where
  idToObject = Object SEmblem
  objectToId (Object SEmblem i) = i
  singObjectType _ = SEmblem
  litObjectType _ = OTEmblem
  visitObject = visitOEmblem

instance IsObjectType 'OTEnchantment where
  idToObject = Object SEnchantment
  objectToId (Object SEnchantment i) = i
  singObjectType _ = SEnchantment
  litObjectType _ = OTEnchantment
  visitObject = visitOEnchantment

instance IsObjectType 'OTInstant where
  idToObject = Object SInstant
  objectToId (Object SInstant i) = i
  singObjectType _ = SInstant
  litObjectType _ = OTInstant
  visitObject = visitOInstant

instance IsObjectType 'OTLand where
  idToObject = Object SLand
  objectToId (Object SLand i) = i
  singObjectType _ = SLand
  litObjectType _ = OTLand
  visitObject = visitOLand

instance IsObjectType 'OTPlaneswalker where
  idToObject = Object SPlaneswalker
  objectToId (Object SPlaneswalker i) = i
  singObjectType _ = SPlaneswalker
  litObjectType _ = OTPlaneswalker
  visitObject = visitOPlaneswalker

instance IsObjectType 'OTPlayer where
  idToObject = Object SPlayer
  objectToId (Object SPlayer i) = i
  singObjectType _ = SPlayer
  litObjectType _ = OTPlayer
  visitObject = visitOPlayer

instance IsObjectType 'OTSorcery where
  idToObject = Object SSorcery
  objectToId (Object SSorcery i) = i
  singObjectType _ = SSorcery
  litObjectType _ = OTSorcery
  visitObject = visitOSorcery

instance IsObjectType 'OTStaticAbility where
  idToObject = Object SStaticAbility
  objectToId (Object SStaticAbility i) = i
  singObjectType _ = SStaticAbility
  litObjectType _ = OTStaticAbility
  visitObject = visitOStaticAbility

instance IsObjectType 'OTTriggeredAbility where
  idToObject = Object STriggeredAbility
  objectToId (Object STriggeredAbility i) = i
  singObjectType _ = STriggeredAbility
  litObjectType _ = OTTriggeredAbility
  visitObject = visitOTriggeredAbility

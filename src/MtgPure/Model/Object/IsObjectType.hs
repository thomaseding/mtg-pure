{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.IsObjectType (
  IsObjectType (..),
  ObjectVisitor (..),
  visitObject',
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (ObjectId, UntypedObject (..))
import safe MtgPure.Model.Object.ObjectType (
  ObjectType (..),
 )
import safe MtgPure.Model.Object.SObjectType (
  SObjectType (..),
 )

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
  idToObject :: UntypedObject -> Object a
  objectToId :: Object a -> ObjectId
  singObjectType :: SObjectType a
  litObjectType :: ObjectType
  visitObject :: ObjectVisitor b -> Object a -> b

visitObject' ::
  IsObjectType a =>
  (forall b. IsObjectType b => Object b -> x) ->
  Object a ->
  x
visitObject' f = visitObject $ ObjectVisitor f f f f f f f f f f f f

instance IsObjectType 'OTActivatedAbility where
  idToObject = Object SActivatedAbility
  objectToId (Object SActivatedAbility (UntypedObject _ i)) = i
  singObjectType = SActivatedAbility
  litObjectType = OTActivatedAbility
  visitObject = visitOActivatedAbility

instance IsObjectType 'OTArtifact where
  idToObject = Object SArtifact
  objectToId (Object SArtifact (UntypedObject _ i)) = i
  singObjectType = SArtifact
  litObjectType = OTArtifact
  visitObject = visitOArtifact

instance IsObjectType 'OTCreature where
  idToObject = Object SCreature
  objectToId (Object SCreature (UntypedObject _ i)) = i
  singObjectType = SCreature
  litObjectType = OTCreature
  visitObject = visitOCreature

instance IsObjectType 'OTEmblem where
  idToObject = Object SEmblem
  objectToId (Object SEmblem (UntypedObject _ i)) = i
  singObjectType = SEmblem
  litObjectType = OTEmblem
  visitObject = visitOEmblem

instance IsObjectType 'OTEnchantment where
  idToObject = Object SEnchantment
  objectToId (Object SEnchantment (UntypedObject _ i)) = i
  singObjectType = SEnchantment
  litObjectType = OTEnchantment
  visitObject = visitOEnchantment

instance IsObjectType 'OTInstant where
  idToObject = Object SInstant
  objectToId (Object SInstant (UntypedObject _ i)) = i
  singObjectType = SInstant
  litObjectType = OTInstant
  visitObject = visitOInstant

instance IsObjectType 'OTLand where
  idToObject = Object SLand
  objectToId (Object SLand (UntypedObject _ i)) = i
  singObjectType = SLand
  litObjectType = OTLand
  visitObject = visitOLand

instance IsObjectType 'OTPlaneswalker where
  idToObject = Object SPlaneswalker
  objectToId (Object SPlaneswalker (UntypedObject _ i)) = i
  singObjectType = SPlaneswalker
  litObjectType = OTPlaneswalker
  visitObject = visitOPlaneswalker

instance IsObjectType 'OTPlayer where
  idToObject = Object SPlayer
  objectToId (Object SPlayer (UntypedObject _ i)) = i
  singObjectType = SPlayer
  litObjectType = OTPlayer
  visitObject = visitOPlayer

instance IsObjectType 'OTSorcery where
  idToObject = Object SSorcery
  objectToId (Object SSorcery (UntypedObject _ i)) = i
  singObjectType = SSorcery
  litObjectType = OTSorcery
  visitObject = visitOSorcery

instance IsObjectType 'OTStaticAbility where
  idToObject = Object SStaticAbility
  objectToId (Object SStaticAbility (UntypedObject _ i)) = i
  singObjectType = SStaticAbility
  litObjectType = OTStaticAbility
  visitObject = visitOStaticAbility

instance IsObjectType 'OTTriggeredAbility where
  idToObject = Object STriggeredAbility
  objectToId (Object STriggeredAbility (UntypedObject _ i)) = i
  singObjectType = STriggeredAbility
  litObjectType = OTTriggeredAbility
  visitObject = visitOTriggeredAbility

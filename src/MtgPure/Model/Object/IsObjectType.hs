{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.IsObjectType (
  IsObjectType (..),
  ObjectVisitor (..),
  visitObject',
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OT (
  OT (..),
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (ObjectId, UntypedObject (..))
import safe MtgPure.Model.Object.SingOT (SingOT (..))

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

class (Typeable a) => IsObjectType (a :: OT) where
  idToObject :: UntypedObject -> Object a
  objectToId :: Object a -> ObjectId
  singObjectType :: SingOT a
  litObjectType :: OT
  visitObject :: ObjectVisitor b -> Object a -> b

visitObject' ::
  (IsObjectType a) =>
  (forall b. (IsObjectType b) => Object b -> x) ->
  Object a ->
  x
visitObject' f = visitObject $ ObjectVisitor f f f f f f f f f f f f

instance IsObjectType 'OTActivatedAbility where
  idToObject = Object SingActivatedAbility
  objectToId (Object SingActivatedAbility (UntypedObject _ i)) = i
  singObjectType = SingActivatedAbility
  litObjectType = OTActivatedAbility
  visitObject = visitOActivatedAbility

instance IsObjectType 'OTArtifact where
  idToObject = Object SingArtifact
  objectToId (Object SingArtifact (UntypedObject _ i)) = i
  singObjectType = SingArtifact
  litObjectType = OTArtifact
  visitObject = visitOArtifact

instance IsObjectType 'OTCreature where
  idToObject = Object SingCreature
  objectToId (Object SingCreature (UntypedObject _ i)) = i
  singObjectType = SingCreature
  litObjectType = OTCreature
  visitObject = visitOCreature

instance IsObjectType 'OTEmblem where
  idToObject = Object SingEmblem
  objectToId (Object SingEmblem (UntypedObject _ i)) = i
  singObjectType = SingEmblem
  litObjectType = OTEmblem
  visitObject = visitOEmblem

instance IsObjectType 'OTEnchantment where
  idToObject = Object SingEnchantment
  objectToId (Object SingEnchantment (UntypedObject _ i)) = i
  singObjectType = SingEnchantment
  litObjectType = OTEnchantment
  visitObject = visitOEnchantment

instance IsObjectType 'OTInstant where
  idToObject = Object SingInstant
  objectToId (Object SingInstant (UntypedObject _ i)) = i
  singObjectType = SingInstant
  litObjectType = OTInstant
  visitObject = visitOInstant

instance IsObjectType 'OTLand where
  idToObject = Object SingLand
  objectToId (Object SingLand (UntypedObject _ i)) = i
  singObjectType = SingLand
  litObjectType = OTLand
  visitObject = visitOLand

instance IsObjectType 'OTPlaneswalker where
  idToObject = Object SingPlaneswalker
  objectToId (Object SingPlaneswalker (UntypedObject _ i)) = i
  singObjectType = SingPlaneswalker
  litObjectType = OTPlaneswalker
  visitObject = visitOPlaneswalker

instance IsObjectType 'OTPlayer where
  idToObject = Object SingPlayer
  objectToId (Object SingPlayer (UntypedObject _ i)) = i
  singObjectType = SingPlayer
  litObjectType = OTPlayer
  visitObject = visitOPlayer

instance IsObjectType 'OTSorcery where
  idToObject = Object SingSorcery
  objectToId (Object SingSorcery (UntypedObject _ i)) = i
  singObjectType = SingSorcery
  litObjectType = OTSorcery
  visitObject = visitOSorcery

instance IsObjectType 'OTStaticAbility where
  idToObject = Object SingStaticAbility
  objectToId (Object SingStaticAbility (UntypedObject _ i)) = i
  singObjectType = SingStaticAbility
  litObjectType = OTStaticAbility
  visitObject = visitOStaticAbility

instance IsObjectType 'OTTriggeredAbility where
  idToObject = Object SingTriggeredAbility
  objectToId (Object SingTriggeredAbility (UntypedObject _ i)) = i
  singObjectType = SingTriggeredAbility
  litObjectType = OTTriggeredAbility
  visitObject = visitOTriggeredAbility

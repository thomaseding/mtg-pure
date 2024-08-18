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
  idToObject :: UntypedObject -> Object 'OTActivatedAbility
  idToObject = Object SingActivatedAbility

  objectToId :: Object 'OTActivatedAbility -> ObjectId
  objectToId (Object SingActivatedAbility (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTActivatedAbility
  singObjectType = SingActivatedAbility

  litObjectType :: OT
  litObjectType = OTActivatedAbility

  visitObject :: ObjectVisitor b -> Object 'OTActivatedAbility -> b
  visitObject = visitOActivatedAbility

instance IsObjectType 'OTArtifact where
  idToObject :: UntypedObject -> Object 'OTArtifact
  idToObject = Object SingArtifact

  objectToId :: Object 'OTArtifact -> ObjectId
  objectToId (Object SingArtifact (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTArtifact
  singObjectType = SingArtifact

  litObjectType :: OT
  litObjectType = OTArtifact

  visitObject :: ObjectVisitor b -> Object 'OTArtifact -> b
  visitObject = visitOArtifact

instance IsObjectType 'OTCreature where
  idToObject :: UntypedObject -> Object 'OTCreature
  idToObject = Object SingCreature

  objectToId :: Object 'OTCreature -> ObjectId
  objectToId (Object SingCreature (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTCreature
  singObjectType = SingCreature

  litObjectType :: OT
  litObjectType = OTCreature

  visitObject :: ObjectVisitor b -> Object 'OTCreature -> b
  visitObject = visitOCreature

instance IsObjectType 'OTEmblem where
  idToObject :: UntypedObject -> Object 'OTEmblem
  idToObject = Object SingEmblem

  objectToId :: Object 'OTEmblem -> ObjectId
  objectToId (Object SingEmblem (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTEmblem
  singObjectType = SingEmblem

  litObjectType :: OT
  litObjectType = OTEmblem

  visitObject :: ObjectVisitor b -> Object 'OTEmblem -> b
  visitObject = visitOEmblem

instance IsObjectType 'OTEnchantment where
  idToObject :: UntypedObject -> Object 'OTEnchantment
  idToObject = Object SingEnchantment

  objectToId :: Object 'OTEnchantment -> ObjectId
  objectToId (Object SingEnchantment (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTEnchantment
  singObjectType = SingEnchantment

  litObjectType :: OT
  litObjectType = OTEnchantment

  visitObject :: ObjectVisitor b -> Object 'OTEnchantment -> b
  visitObject = visitOEnchantment

instance IsObjectType 'OTInstant where
  idToObject :: UntypedObject -> Object 'OTInstant
  idToObject = Object SingInstant

  objectToId :: Object 'OTInstant -> ObjectId
  objectToId (Object SingInstant (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTInstant
  singObjectType = SingInstant

  litObjectType :: OT
  litObjectType = OTInstant

  visitObject :: ObjectVisitor b -> Object 'OTInstant -> b
  visitObject = visitOInstant

instance IsObjectType 'OTLand where
  idToObject :: UntypedObject -> Object 'OTLand
  idToObject = Object SingLand

  objectToId :: Object 'OTLand -> ObjectId
  objectToId (Object SingLand (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTLand
  singObjectType = SingLand

  litObjectType :: OT
  litObjectType = OTLand

  visitObject :: ObjectVisitor b -> Object 'OTLand -> b
  visitObject = visitOLand

instance IsObjectType 'OTPlaneswalker where
  idToObject :: UntypedObject -> Object 'OTPlaneswalker
  idToObject = Object SingPlaneswalker

  objectToId :: Object 'OTPlaneswalker -> ObjectId
  objectToId (Object SingPlaneswalker (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTPlaneswalker
  singObjectType = SingPlaneswalker

  litObjectType :: OT
  litObjectType = OTPlaneswalker

  visitObject :: ObjectVisitor b -> Object 'OTPlaneswalker -> b
  visitObject = visitOPlaneswalker

instance IsObjectType 'OTPlayer where
  idToObject :: UntypedObject -> Object 'OTPlayer
  idToObject = Object SingPlayer

  objectToId :: Object 'OTPlayer -> ObjectId
  objectToId (Object SingPlayer (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTPlayer
  singObjectType = SingPlayer

  litObjectType :: OT
  litObjectType = OTPlayer

  visitObject :: ObjectVisitor b -> Object 'OTPlayer -> b
  visitObject = visitOPlayer

instance IsObjectType 'OTSorcery where
  idToObject :: UntypedObject -> Object 'OTSorcery
  idToObject = Object SingSorcery

  objectToId :: Object 'OTSorcery -> ObjectId
  objectToId (Object SingSorcery (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTSorcery
  singObjectType = SingSorcery

  litObjectType :: OT
  litObjectType = OTSorcery

  visitObject :: ObjectVisitor b -> Object 'OTSorcery -> b
  visitObject = visitOSorcery

instance IsObjectType 'OTStaticAbility where
  idToObject :: UntypedObject -> Object 'OTStaticAbility
  idToObject = Object SingStaticAbility

  objectToId :: Object 'OTStaticAbility -> ObjectId
  objectToId (Object SingStaticAbility (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTStaticAbility
  singObjectType = SingStaticAbility

  litObjectType :: OT
  litObjectType = OTStaticAbility

  visitObject :: ObjectVisitor b -> Object 'OTStaticAbility -> b
  visitObject = visitOStaticAbility

instance IsObjectType 'OTTriggeredAbility where
  idToObject :: UntypedObject -> Object 'OTTriggeredAbility
  idToObject = Object SingTriggeredAbility

  objectToId :: Object 'OTTriggeredAbility -> ObjectId
  objectToId (Object SingTriggeredAbility (UntypedObject _ i)) = i

  singObjectType :: SingOT 'OTTriggeredAbility
  singObjectType = SingTriggeredAbility

  litObjectType :: OT
  litObjectType = OTTriggeredAbility

  visitObject :: ObjectVisitor b -> Object 'OTTriggeredAbility -> b
  visitObject = visitOTriggeredAbility

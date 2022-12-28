{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.Singleton.Permanent (
  WPermanent (..),
  PermanentType (..),
  IsPermanentType (..),
  PermanentVisitor (..),
  visitPermanent',
  CoPermanent (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OTKind (
  OTArtifact,
  OTCreature,
  OTEnchantment,
  OTLand,
  OTPermanent,
  OTPlaneswalker,
 )
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
  OT4,
 )
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOT, ZO)

data PermanentType
  = PTArtifact
  | PTCreature
  | PTEnchantment
  | PTLand
  | PTPlaneswalker
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

-- Witness type
data WPermanent :: Type -> Type where
  WPermanentArtifact :: WPermanent OTArtifact
  WPermanentCreature :: WPermanent OTCreature
  WPermanentEnchantment :: WPermanent OTEnchantment
  WPermanentLand :: WPermanent OTLand
  WPermanentPlaneswalker :: WPermanent OTPlaneswalker
  WPermanent :: WPermanent OTPermanent
  WPermanent2 :: Inst2 IsPermanentType a b => WPermanent (OT2 a b)
  WPermanent3 :: Inst3 IsPermanentType a b c => WPermanent (OT3 a b c)
  WPermanent4 :: Inst4 IsPermanentType a b c d => WPermanent (OT4 a b c d)
  deriving (Typeable)

deriving instance Show (WPermanent a)

data PermanentVisitor zone z = PermanentVisitor
  { visitPArtifact :: ZO zone OTArtifact -> z
  , visitPCreature :: ZO zone OTCreature -> z
  , visitPEnchantment :: ZO zone OTEnchantment -> z
  , visitPLand :: ZO zone OTLand -> z
  , visitPPlaneswalker :: ZO zone OTPlaneswalker -> z
  }
  deriving (Typeable)

class IsObjectType a => IsPermanentType a where
  singPermanentType :: Proxy a -> PermanentType
  singPermanent :: Proxy a -> WPermanent (OT1 a)
  visitPermanent :: PermanentVisitor zone z -> WPermanent (OT1 a) -> ZO zone (OT1 a) -> z

visitPermanent' ::
  IsPermanentType a =>
  (forall a'. IsPermanentType a' => ZO zone (OT1 a') -> z) ->
  WPermanent (OT1 a) ->
  ZO zone (OT1 a) ->
  z
visitPermanent' f = visitPermanent $ PermanentVisitor f f f f f

instance IsPermanentType 'OTArtifact where
  singPermanentType _ = PTArtifact
  singPermanent _ = WPermanentArtifact
  visitPermanent v _ = visitPArtifact v

instance IsPermanentType 'OTCreature where
  singPermanentType _ = PTCreature
  singPermanent _ = WPermanentCreature
  visitPermanent v _ = visitPCreature v

instance IsPermanentType 'OTEnchantment where
  singPermanentType _ = PTEnchantment
  singPermanent _ = WPermanentEnchantment
  visitPermanent v _ = visitPEnchantment v

instance IsPermanentType 'OTLand where
  singPermanentType _ = PTLand
  singPermanent _ = WPermanentLand
  visitPermanent v _ = visitPLand v

instance IsPermanentType 'OTPlaneswalker where
  singPermanentType _ = PTPlaneswalker
  singPermanent _ = WPermanentPlaneswalker
  visitPermanent v _ = visitPPlaneswalker v

class IsOT ot => CoPermanent ot where
  coPermanent :: WPermanent ot

instance CoPermanent OTArtifact where
  coPermanent = WPermanentArtifact

instance CoPermanent OTCreature where
  coPermanent = WPermanentCreature

instance CoPermanent OTEnchantment where
  coPermanent = WPermanentEnchantment

instance CoPermanent OTLand where
  coPermanent = WPermanentLand

instance CoPermanent OTPlaneswalker where
  coPermanent = WPermanentPlaneswalker

instance CoPermanent OTPermanent where
  coPermanent = WPermanent

instance Inst2 IsPermanentType a b => CoPermanent (OT2 a b) where
  coPermanent = WPermanent2 :: WPermanent (OT2 a b)

instance Inst3 IsPermanentType a b c => CoPermanent (OT3 a b c) where
  coPermanent = WPermanent3 :: WPermanent (OT3 a b c)

instance Inst4 IsPermanentType a b c d => CoPermanent (OT4 a b c d) where
  coPermanent = WPermanent4 :: WPermanent (OT4 a b c d)

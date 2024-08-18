{-# LANGUAGE Safe #-}
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
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
  OT4,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNArtifact,
  OTNCreature,
  OTNEnchantment,
  OTNLand,
  OTNPermanent,
  OTNPlaneswalker,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO)

data PermanentType
  = PTArtifact
  | PTCreature
  | PTEnchantment
  | PTLand
  | PTPlaneswalker
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

-- Witness type
data WPermanent :: Type -> Type where
  WPermanentArtifact :: WPermanent OTNArtifact
  WPermanentCreature :: WPermanent OTNCreature
  WPermanentEnchantment :: WPermanent OTNEnchantment
  WPermanentLand :: WPermanent OTNLand
  WPermanentPlaneswalker :: WPermanent OTNPlaneswalker
  WPermanent :: WPermanent OTNPermanent
  WPermanent2 :: (Inst2 IsPermanentType a b) => WPermanent (OT2 a b)
  WPermanent3 :: (Inst3 IsPermanentType a b c) => WPermanent (OT3 a b c)
  WPermanent4 :: (Inst4 IsPermanentType a b c d) => WPermanent (OT4 a b c d)
  deriving (Typeable)

deriving instance Show (WPermanent a)

data PermanentVisitor zone z = PermanentVisitor
  { visitPArtifact :: ZO zone OTNArtifact -> z
  , visitPCreature :: ZO zone OTNCreature -> z
  , visitPEnchantment :: ZO zone OTNEnchantment -> z
  , visitPLand :: ZO zone OTNLand -> z
  , visitPPlaneswalker :: ZO zone OTNPlaneswalker -> z
  }
  deriving (Typeable)

class (IsObjectType a) => IsPermanentType a where
  singPermanentType :: Proxy a -> PermanentType
  singPermanent :: Proxy a -> WPermanent (OT1 a)
  visitPermanent :: PermanentVisitor zone z -> WPermanent (OT1 a) -> ZO zone (OT1 a) -> z

visitPermanent' ::
  (IsPermanentType a) =>
  (forall a'. (IsPermanentType a') => ZO zone (OT1 a') -> z) ->
  WPermanent (OT1 a) ->
  ZO zone (OT1 a) ->
  z
visitPermanent' f = visitPermanent $ PermanentVisitor f f f f f

instance IsPermanentType 'OTArtifact where
  singPermanentType :: Proxy 'OTArtifact -> PermanentType
  singPermanentType _ = PTArtifact

  singPermanent :: Proxy 'OTArtifact -> WPermanent (OT1 'OTArtifact)
  singPermanent _ = WPermanentArtifact

  visitPermanent :: PermanentVisitor zone z -> WPermanent (OT1 'OTArtifact) -> ZO zone (OT1 'OTArtifact) -> z
  visitPermanent v _ = visitPArtifact v

instance IsPermanentType 'OTCreature where
  singPermanentType :: Proxy 'OTCreature -> PermanentType
  singPermanentType _ = PTCreature

  singPermanent :: Proxy 'OTCreature -> WPermanent (OT1 'OTCreature)
  singPermanent _ = WPermanentCreature

  visitPermanent :: PermanentVisitor zone z -> WPermanent (OT1 'OTCreature) -> ZO zone (OT1 'OTCreature) -> z
  visitPermanent v _ = visitPCreature v

instance IsPermanentType 'OTEnchantment where
  singPermanentType :: Proxy 'OTEnchantment -> PermanentType
  singPermanentType _ = PTEnchantment

  singPermanent :: Proxy 'OTEnchantment -> WPermanent (OT1 'OTEnchantment)
  singPermanent _ = WPermanentEnchantment

  visitPermanent :: PermanentVisitor zone z -> WPermanent (OT1 'OTEnchantment) -> ZO zone (OT1 'OTEnchantment) -> z
  visitPermanent v _ = visitPEnchantment v

instance IsPermanentType 'OTLand where
  singPermanentType :: Proxy 'OTLand -> PermanentType
  singPermanentType _ = PTLand

  singPermanent :: Proxy 'OTLand -> WPermanent (OT1 'OTLand)
  singPermanent _ = WPermanentLand

  visitPermanent :: PermanentVisitor zone z -> WPermanent (OT1 'OTLand) -> ZO zone (OT1 'OTLand) -> z
  visitPermanent v _ = visitPLand v

instance IsPermanentType 'OTPlaneswalker where
  singPermanentType :: Proxy 'OTPlaneswalker -> PermanentType
  singPermanentType _ = PTPlaneswalker

  singPermanent :: Proxy 'OTPlaneswalker -> WPermanent (OT1 'OTPlaneswalker)
  singPermanent _ = WPermanentPlaneswalker

  visitPermanent :: PermanentVisitor zone z -> WPermanent (OT1 'OTPlaneswalker) -> ZO zone (OT1 'OTPlaneswalker) -> z
  visitPermanent v _ = visitPPlaneswalker v

class (IsOTN ot) => CoPermanent ot where
  coPermanent :: WPermanent ot

instance CoPermanent OTNArtifact where
  coPermanent :: WPermanent OTNArtifact
  coPermanent = WPermanentArtifact

instance CoPermanent OTNCreature where
  coPermanent :: WPermanent OTNCreature
  coPermanent = WPermanentCreature

instance CoPermanent OTNEnchantment where
  coPermanent :: WPermanent OTNEnchantment
  coPermanent = WPermanentEnchantment

instance CoPermanent OTNLand where
  coPermanent :: WPermanent OTNLand
  coPermanent = WPermanentLand

instance CoPermanent OTNPlaneswalker where
  coPermanent :: WPermanent OTNPlaneswalker
  coPermanent = WPermanentPlaneswalker

instance CoPermanent OTNPermanent where
  coPermanent :: WPermanent OTNPermanent
  coPermanent = WPermanent

instance (Inst2 IsPermanentType a b) => CoPermanent (OT2 a b) where
  coPermanent :: (Inst2 IsPermanentType a b) => WPermanent (OT2 a b)
  coPermanent = WPermanent2 :: WPermanent (OT2 a b)

instance (Inst3 IsPermanentType a b c) => CoPermanent (OT3 a b c) where
  coPermanent :: (Inst3 IsPermanentType a b c) => WPermanent (OT3 a b c)
  coPermanent = WPermanent3 :: WPermanent (OT3 a b c)

instance (Inst4 IsPermanentType a b c d) => CoPermanent (OT4 a b c d) where
  coPermanent :: (Inst4 IsPermanentType a b c d) => WPermanent (OT4 a b c d)
  coPermanent = WPermanent4 :: WPermanent (OT4 a b c d)

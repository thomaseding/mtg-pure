{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.Singleton.Any (
  IsAnyType,
  CoAny (..),
  SingCoAny (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OTN (OT2, OT3, OT4, OT5, OT6)
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNArtifact,
  OTNCreature,
  OTNEnchantment,
  OTNInstant,
  OTNLand,
  OTNPlaneswalker,
  OTNPlayer,
  OTNSorcery,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN)

data SingCoAny ot where
  CoAnyArtifact :: SingCoAny OTNArtifact
  CoAnyCreature :: SingCoAny OTNCreature
  CoAnyEnchantment :: SingCoAny OTNEnchantment
  CoAnyInstant :: SingCoAny OTNInstant
  CoAnyLand :: SingCoAny OTNLand
  CoAnyPlaneswalker :: SingCoAny OTNPlaneswalker
  CoAnyPlayer :: SingCoAny OTNPlayer
  CoAnySorcery :: SingCoAny OTNSorcery
  CoAny :: SingCoAny OTNAny
  CoAny2 :: (Inst2 IsAnyType a b) => SingCoAny (OT2 a b)
  CoAny3 :: (Inst3 IsAnyType a b c) => SingCoAny (OT3 a b c)
  CoAny4 :: (Inst4 IsAnyType a b c d) => SingCoAny (OT4 a b c d)
  CoAny5 :: (Inst5 IsAnyType a b c d e) => SingCoAny (OT5 a b c d e)
  CoAny6 :: (Inst6 IsAnyType a b c d e f) => SingCoAny (OT6 a b c d e f)
  deriving (Typeable)

deriving instance Show (SingCoAny ot)

type IsAnyType = IsObjectType

class (IsOTN ot) => CoAny ot where
  coAny :: SingCoAny ot

instance CoAny OTNInstant where
  coAny = CoAnyInstant

instance CoAny OTNSorcery where
  coAny = CoAnySorcery

instance CoAny OTNPlayer where
  coAny = CoAnyPlayer

instance CoAny OTNArtifact where
  coAny = CoAnyArtifact

instance CoAny OTNCreature where
  coAny = CoAnyCreature

instance CoAny OTNEnchantment where
  coAny = CoAnyEnchantment

instance CoAny OTNLand where
  coAny = CoAnyLand

instance CoAny OTNPlaneswalker where
  coAny = CoAnyPlaneswalker

instance (Inst2 IsAnyType a b) => CoAny (OT2 a b) where
  coAny = CoAny2

instance (Inst3 IsAnyType a b c) => CoAny (OT3 a b c) where
  coAny = CoAny3

instance (Inst4 IsAnyType a b c d) => CoAny (OT4 a b c d) where
  coAny = CoAny4

instance (Inst5 IsAnyType a b c d e) => CoAny (OT5 a b c d e) where
  coAny = CoAny5

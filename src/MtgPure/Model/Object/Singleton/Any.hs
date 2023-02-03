{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.Singleton.Any (
  WAny (..),
  IsAnyType,
  CoAny (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
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

-- Witness type
data WAny :: Type -> Type where
  WAnyArtifact :: WAny OTNArtifact
  WAnyCreature :: WAny OTNCreature
  WAnyEnchantment :: WAny OTNEnchantment
  WAnyInstant :: WAny OTNInstant
  WAnyLand :: WAny OTNLand
  WAnyPlaneswalker :: WAny OTNPlaneswalker
  WAnyPlayer :: WAny OTNPlayer
  WAnySorcery :: WAny OTNSorcery
  WAny :: WAny OTNAny
  WAny2 :: Inst2 IsAnyType a b => WAny (OT2 a b)
  WAny3 :: Inst3 IsAnyType a b c => WAny (OT3 a b c)
  WAny4 :: Inst4 IsAnyType a b c d => WAny (OT4 a b c d)
  WAny5 :: Inst5 IsAnyType a b c d e => WAny (OT5 a b c d e)
  WAny6 :: Inst6 IsAnyType a b c d e f => WAny (OT6 a b c d e f)
  deriving (Typeable)

deriving instance Show (WAny ot)

type IsAnyType = IsObjectType

class IsOTN ot => CoAny ot where
  coAny :: WAny ot

instance CoAny OTNInstant where
  coAny = WAnyInstant

instance CoAny OTNSorcery where
  coAny = WAnySorcery

instance CoAny OTNPlayer where
  coAny = WAnyPlayer

instance CoAny OTNArtifact where
  coAny = WAnyArtifact

instance CoAny OTNCreature where
  coAny = WAnyCreature

instance CoAny OTNEnchantment where
  coAny = WAnyEnchantment

instance CoAny OTNLand where
  coAny = WAnyLand

instance CoAny OTNPlaneswalker where
  coAny = WAnyPlaneswalker

instance Inst2 IsAnyType a b => CoAny (OT2 a b) where
  coAny = WAny2

instance Inst3 IsAnyType a b c => CoAny (OT3 a b c) where
  coAny = WAny3

instance Inst4 IsAnyType a b c d => CoAny (OT4 a b c d) where
  coAny = WAny4

instance Inst5 IsAnyType a b c d e => CoAny (OT5 a b c d e) where
  coAny = WAny5

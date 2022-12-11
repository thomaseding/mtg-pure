{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType.Any (
  WAny (..),
  IsAnyType,
  CoAny (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.OTN (OT2, OT3, OT4, OT5, OT6)
import safe MtgPure.Model.ObjectType.Kind (
  OTAny,
  OTArtifact,
  OTCreature,
  OTEnchantment,
  OTInstant,
  OTLand,
  OTPlaneswalker,
  OTPlayer,
  OTSorcery,
 )
import safe MtgPure.Model.ZoneObject (IsOT)

-- Witness type
data WAny :: Type -> Type where
  WAnyArtifact :: WAny OTArtifact
  WAnyCreature :: WAny OTCreature
  WAnyEnchantment :: WAny OTEnchantment
  WAnyInstant :: WAny OTInstant
  WAnyLand :: WAny OTLand
  WAnyPlaneswalker :: WAny OTPlaneswalker
  WAnyPlayer :: WAny OTPlayer
  WAnySorcery :: WAny OTSorcery
  WAny :: WAny OTAny
  WAny2 :: Inst2 IsAnyType a b => WAny (OT2 a b)
  WAny3 :: Inst3 IsAnyType a b c => WAny (OT3 a b c)
  WAny4 :: Inst4 IsAnyType a b c d => WAny (OT4 a b c d)
  WAny5 :: Inst5 IsAnyType a b c d e => WAny (OT5 a b c d e)
  WAny6 :: Inst6 IsAnyType a b c d e f => WAny (OT6 a b c d e f)
  deriving (Typeable)

deriving instance Show (WAny ot)

type IsAnyType = IsObjectType

class IsOT ot => CoAny ot where
  coAny :: WAny ot

instance CoAny OTInstant where
  coAny = WAnyInstant

instance CoAny OTSorcery where
  coAny = WAnySorcery

instance CoAny OTPlayer where
  coAny = WAnyPlayer

instance CoAny OTArtifact where
  coAny = WAnyArtifact

instance CoAny OTCreature where
  coAny = WAnyCreature

instance CoAny OTEnchantment where
  coAny = WAnyEnchantment

instance CoAny OTLand where
  coAny = WAnyLand

instance CoAny OTPlaneswalker where
  coAny = WAnyPlaneswalker

instance Inst2 IsAnyType a b => CoAny (OT2 a b) where
  coAny = WAny2

instance Inst3 IsAnyType a b c => CoAny (OT3 a b c) where
  coAny = WAny3

instance Inst4 IsAnyType a b c d => CoAny (OT4 a b c d) where
  coAny = WAny4

instance Inst5 IsAnyType a b c d e => CoAny (OT5 a b c d e) where
  coAny = WAny5

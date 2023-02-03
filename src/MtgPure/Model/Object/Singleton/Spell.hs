{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.Singleton.Spell (
  WSpell (..),
  SpellType (..),
  IsSpellType (..),
  CoSpell (..),
  SpellVisitor (..),
  visitSpell',
) where

import safe Data.Inst (Inst2, Inst3, Inst4)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
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
  OTNInstant,
  OTNPlaneswalker,
  OTNSorcery,
  OTNSpell,
 )
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO)

data SpellType
  = STArtifact
  | STCreature
  | STEnchantment
  | STInstant
  | STPlaneswalker
  | STSorcery
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

-- Witness type
data WSpell :: Type -> Type where
  WSpellArtifact :: WSpell OTNArtifact
  WSpellCreature :: WSpell OTNCreature
  WSpellEnchantment :: WSpell OTNEnchantment
  WSpellInstant :: WSpell OTNInstant
  WSpellPlaneswalker :: WSpell OTNPlaneswalker
  WSpellSorcery :: WSpell OTNSorcery
  WSpell :: WSpell OTNSpell
  WSpell2 :: Inst2 IsSpellType a b => WSpell (OT2 a b)
  WSpell3 :: Inst3 IsSpellType a b c => WSpell (OT3 a b c)
  WSpell4 :: Inst4 IsSpellType a b c d => WSpell (OT4 a b c d)
  deriving (Typeable)

deriving instance Show (WSpell a)

data SpellVisitor zone z = SpellVisitor
  { visitSArtifact :: ZO zone OTNArtifact -> z
  , visitSCreature :: ZO zone OTNCreature -> z
  , visitSEnchantment :: ZO zone OTNEnchantment -> z
  , visitSInstant :: ZO zone OTNInstant -> z
  , visitSPlaneswalker :: ZO zone OTNPlaneswalker -> z
  , visitSSorcery :: ZO zone OTNSorcery -> z
  }
  deriving (Typeable)

class IsObjectType a => IsSpellType a where
  singSpellType :: Proxy a -> SpellType
  singSpell :: Proxy a -> WSpell (OT1 a)
  visitSpell :: SpellVisitor zone z -> WSpell (OT1 a) -> ZO zone (OT1 a) -> z

visitSpell' ::
  IsSpellType a =>
  (forall a'. IsSpellType a' => ZO zone (OT1 a') -> z) ->
  WSpell (OT1 a) ->
  ZO zone (OT1 a) ->
  z
visitSpell' f = visitSpell $ SpellVisitor f f f f f f

instance IsSpellType 'OTArtifact where
  singSpellType _ = STArtifact
  singSpell _ = WSpellArtifact
  visitSpell v _ = visitSArtifact v

instance IsSpellType 'OTCreature where
  singSpellType _ = STCreature
  singSpell _ = WSpellCreature
  visitSpell v _ = visitSCreature v

instance IsSpellType 'OTEnchantment where
  singSpellType _ = STEnchantment
  singSpell _ = WSpellEnchantment
  visitSpell v _ = visitSEnchantment v

instance IsSpellType 'OTInstant where
  singSpellType _ = STInstant
  singSpell _ = WSpellInstant
  visitSpell v _ = visitSInstant v

instance IsSpellType 'OTPlaneswalker where
  singSpellType _ = STPlaneswalker
  singSpell _ = WSpellPlaneswalker
  visitSpell v _ = visitSPlaneswalker v

instance IsSpellType 'OTSorcery where
  singSpellType _ = STSorcery
  singSpell _ = WSpellSorcery
  visitSpell v _ = visitSSorcery v

class IsOTN ot => CoSpell ot where
  coSpell :: WSpell ot

instance CoSpell OTNArtifact where
  coSpell = WSpellArtifact

instance CoSpell OTNCreature where
  coSpell = WSpellCreature

instance CoSpell OTNEnchantment where
  coSpell = WSpellEnchantment

instance CoSpell OTNInstant where
  coSpell = WSpellInstant

instance CoSpell OTNPlaneswalker where
  coSpell = WSpellPlaneswalker

instance CoSpell OTNSorcery where
  coSpell = WSpellSorcery

instance CoSpell OTNSpell where
  coSpell = WSpell

instance Inst2 IsSpellType a b => CoSpell (OT2 a b) where
  coSpell = WSpell2 :: WSpell (OT2 a b)

instance Inst3 IsSpellType a b c => CoSpell (OT3 a b c) where
  coSpell = WSpell3 :: WSpell (OT3 a b c)

instance Inst4 IsSpellType a b c d => CoSpell (OT4 a b c d) where
  coSpell = WSpell4 :: WSpell (OT4 a b c d)

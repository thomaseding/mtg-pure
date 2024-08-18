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
  OTNInstant,
  OTNPlaneswalker,
  OTNSorcery,
  OTNSpell,
 )
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
  WSpell2 :: (Inst2 IsSpellType a b) => WSpell (OT2 a b)
  WSpell3 :: (Inst3 IsSpellType a b c) => WSpell (OT3 a b c)
  WSpell4 :: (Inst4 IsSpellType a b c d) => WSpell (OT4 a b c d)
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

class (IsObjectType a) => IsSpellType a where
  singSpellType :: Proxy a -> SpellType
  singSpell :: Proxy a -> WSpell (OT1 a)
  visitSpell :: SpellVisitor zone z -> WSpell (OT1 a) -> ZO zone (OT1 a) -> z

visitSpell' ::
  (IsSpellType a) =>
  (forall a'. (IsSpellType a') => ZO zone (OT1 a') -> z) ->
  WSpell (OT1 a) ->
  ZO zone (OT1 a) ->
  z
visitSpell' f = visitSpell $ SpellVisitor f f f f f f

instance IsSpellType 'OTArtifact where
  singSpellType :: Proxy 'OTArtifact -> SpellType
  singSpellType _ = STArtifact

  singSpell :: Proxy 'OTArtifact -> WSpell (OT1 'OTArtifact)
  singSpell _ = WSpellArtifact

  visitSpell :: SpellVisitor zone z -> WSpell (OT1 'OTArtifact) -> ZO zone (OT1 'OTArtifact) -> z
  visitSpell v _ = visitSArtifact v

instance IsSpellType 'OTCreature where
  singSpellType :: Proxy 'OTCreature -> SpellType
  singSpellType _ = STCreature

  singSpell :: Proxy 'OTCreature -> WSpell (OT1 'OTCreature)
  singSpell _ = WSpellCreature

  visitSpell :: SpellVisitor zone z -> WSpell (OT1 'OTCreature) -> ZO zone (OT1 'OTCreature) -> z
  visitSpell v _ = visitSCreature v

instance IsSpellType 'OTEnchantment where
  singSpellType :: Proxy 'OTEnchantment -> SpellType
  singSpellType _ = STEnchantment

  singSpell :: Proxy 'OTEnchantment -> WSpell (OT1 'OTEnchantment)
  singSpell _ = WSpellEnchantment

  visitSpell :: SpellVisitor zone z -> WSpell (OT1 'OTEnchantment) -> ZO zone (OT1 'OTEnchantment) -> z
  visitSpell v _ = visitSEnchantment v

instance IsSpellType 'OTInstant where
  singSpellType :: Proxy 'OTInstant -> SpellType
  singSpellType _ = STInstant

  singSpell :: Proxy 'OTInstant -> WSpell (OT1 'OTInstant)
  singSpell _ = WSpellInstant

  visitSpell :: SpellVisitor zone z -> WSpell (OT1 'OTInstant) -> ZO zone (OT1 'OTInstant) -> z
  visitSpell v _ = visitSInstant v

instance IsSpellType 'OTPlaneswalker where
  singSpellType :: Proxy 'OTPlaneswalker -> SpellType
  singSpellType _ = STPlaneswalker

  singSpell :: Proxy 'OTPlaneswalker -> WSpell (OT1 'OTPlaneswalker)
  singSpell _ = WSpellPlaneswalker

  visitSpell :: SpellVisitor zone z -> WSpell (OT1 'OTPlaneswalker) -> ZO zone (OT1 'OTPlaneswalker) -> z
  visitSpell v _ = visitSPlaneswalker v

instance IsSpellType 'OTSorcery where
  singSpellType :: Proxy 'OTSorcery -> SpellType
  singSpellType _ = STSorcery

  singSpell :: Proxy 'OTSorcery -> WSpell (OT1 'OTSorcery)
  singSpell _ = WSpellSorcery

  visitSpell :: SpellVisitor zone z -> WSpell (OT1 'OTSorcery) -> ZO zone (OT1 'OTSorcery) -> z
  visitSpell v _ = visitSSorcery v

class (IsOTN ot) => CoSpell ot where
  coSpell :: WSpell ot

instance CoSpell OTNArtifact where
  coSpell :: WSpell OTNArtifact
  coSpell = WSpellArtifact

instance CoSpell OTNCreature where
  coSpell :: WSpell OTNCreature
  coSpell = WSpellCreature

instance CoSpell OTNEnchantment where
  coSpell :: WSpell OTNEnchantment
  coSpell = WSpellEnchantment

instance CoSpell OTNInstant where
  coSpell :: WSpell OTNInstant
  coSpell = WSpellInstant

instance CoSpell OTNPlaneswalker where
  coSpell :: WSpell OTNPlaneswalker
  coSpell = WSpellPlaneswalker

instance CoSpell OTNSorcery where
  coSpell :: WSpell OTNSorcery
  coSpell = WSpellSorcery

instance CoSpell OTNSpell where
  coSpell :: WSpell OTNSpell
  coSpell = WSpell

instance (Inst2 IsSpellType a b) => CoSpell (OT2 a b) where
  coSpell :: (Inst2 IsSpellType a b) => WSpell (OT2 a b)
  coSpell = WSpell2 :: WSpell (OT2 a b)

instance (Inst3 IsSpellType a b c) => CoSpell (OT3 a b c) where
  coSpell :: (Inst3 IsSpellType a b c) => WSpell (OT3 a b c)
  coSpell = WSpell3 :: WSpell (OT3 a b c)

instance (Inst4 IsSpellType a b c d) => CoSpell (OT4 a b c d) where
  coSpell :: (Inst4 IsSpellType a b c d) => WSpell (OT4 a b c d)
  coSpell = WSpell4 :: WSpell (OT4 a b c d)

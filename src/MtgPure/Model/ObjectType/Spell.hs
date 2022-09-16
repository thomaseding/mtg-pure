{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType.Spell (
  WSpell (..),
  SpellType (..),
  IsSpellType (..),
  SpellVisitor (..),
  visitSpell',
) where

import safe Data.Inst (Inst2, Inst3, Inst4)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ObjectType (OT1, OT2, OT3, OT4, ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTArtifact,
  OTCreature,
  OTEnchantment,
  OTInstant,
  OTPlaneswalker,
  OTSorcery,
  OTSpell,
 )
import safe MtgPure.Model.ZoneObject (ZO)

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
  WSpellArtifact :: WSpell OTArtifact
  WSpellCreature :: WSpell OTCreature
  WSpellEnchantment :: WSpell OTEnchantment
  WSpellInstant :: WSpell OTInstant
  WSpellPlaneswalker :: WSpell OTPlaneswalker
  WSpellSorcery :: WSpell OTSorcery
  WSpell :: WSpell OTSpell
  WSpell2 :: Inst2 IsSpellType a b => WSpell (OT2 a b)
  WSpell3 :: Inst3 IsSpellType a b c => WSpell (OT3 a b c)
  WSpell4 :: Inst4 IsSpellType a b c d => WSpell (OT4 a b c d)
  deriving (Typeable)

deriving instance Show (WSpell a)

data SpellVisitor zone z = SpellVisitor
  { visitSArtifact :: ZO zone OTArtifact -> z
  , visitSCreature :: ZO zone OTCreature -> z
  , visitSEnchantment :: ZO zone OTEnchantment -> z
  , visitSInstant :: ZO zone OTInstant -> z
  , visitSPlaneswalker :: ZO zone OTPlaneswalker -> z
  , visitSSorcery :: ZO zone OTSorcery -> z
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

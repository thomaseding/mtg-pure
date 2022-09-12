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

module MtgPure.Model.ObjectType.Spell
  ( WSpell (..),
    SpellType (..),
    IsSpellType (..),
    SpellVisitor (..),
    visitSpell',
  )
where

import safe Data.Inst (Inst2, Inst3, Inst4)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectN.Type
  ( OArtifact,
    OCreature,
    OEnchantment,
    OInstant,
    OPlaneswalker,
    OSorcery,
  )
import MtgPure.Model.ObjectType (OT, ObjectType (..))
import MtgPure.Model.ObjectType.Kind
  ( OTArtifact,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTPlaneswalker,
    OTSorcery,
    OTSpell,
  )

data SpellType
  = STArtifact
  | STCreature
  | STEnchantment
  | STInstant
  | STPlaneswalker
  | STSorcery
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Witness type
data WSpell :: forall ot. ot -> Type where
  WSpellArtifact :: WSpell OTArtifact
  WSpellCreature :: WSpell OTCreature
  WSpellEnchantment :: WSpell OTEnchantment
  WSpellInstant :: WSpell OTInstant
  WSpellPlaneswalker :: WSpell OTPlaneswalker
  WSpellSorcery :: WSpell OTSorcery
  WSpell :: WSpell OTSpell
  WSpell2 :: Inst2 IsSpellType a b => WSpell '(OT, a, b)
  WSpell3 :: Inst3 IsSpellType a b c => WSpell '(OT, a, b, c)
  WSpell4 :: Inst4 IsSpellType a b c d => WSpell '(OT, a, b, c, d)

deriving instance Show (WSpell a)

data SpellVisitor a = SpellVisitor
  { visitSArtifact :: OArtifact -> a,
    visitSCreature :: OCreature -> a,
    visitSEnchantment :: OEnchantment -> a,
    visitSInstant :: OInstant -> a,
    visitSPlaneswalker :: OPlaneswalker -> a,
    visitSSorcery :: OSorcery -> a
  }

class IsObjectType a => IsSpellType a where
  singSpellType :: Proxy a -> SpellType
  singSpell :: Proxy a -> WSpell '(OT, a)
  visitSpell :: SpellVisitor b -> WSpell '(OT, a) -> ObjectN '(OT, a) -> b

visitSpell' :: IsSpellType a => (forall b. IsSpellType b => ObjectN '(OT, b) -> x) -> WSpell '(OT, a) -> ObjectN '(OT, a) -> x
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

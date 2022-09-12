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
import safe MtgPure.Model.ObjectN.Type
  ( OArtifact,
    OCreature,
    OEnchantment,
    OInstant,
    ON1,
    ON2,
    ON3,
    ON4,
    OPlaneswalker,
    OSorcery,
    OSpell,
  )
import MtgPure.Model.ObjectType (ObjectType (..))

data SpellType
  = STArtifact
  | STCreature
  | STEnchantment
  | STInstant
  | STPlaneswalker
  | STSorcery
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Witness type
data WSpell :: Type -> Type where
  WSpellArtifact :: WSpell OArtifact
  WSpellCreature :: WSpell OCreature
  WSpellEnchantment :: WSpell OEnchantment
  WSpellInstant :: WSpell OInstant
  WSpellPlaneswalker :: WSpell OPlaneswalker
  WSpellSorcery :: WSpell OSorcery
  WSpell :: WSpell OSpell
  WSpell2 :: Inst2 IsSpellType a b => WSpell (ON2 a b)
  WSpell3 :: Inst3 IsSpellType a b c => WSpell (ON3 a b c)
  WSpell4 :: Inst4 IsSpellType a b c d => WSpell (ON4 a b c d)

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
  singSpell :: Proxy a -> WSpell (ON1 a)
  visitSpell :: SpellVisitor b -> WSpell (ON1 a) -> ON1 a -> b

visitSpell' :: IsSpellType a => (forall b. IsSpellType b => ON1 b -> x) -> WSpell (ON1 a) -> ON1 a -> x
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

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

module MtgPure.Model.Spell
  ( Spell (..),
    SpellType (..),
    IsSpellType (..),
    SpellVisitor (..),
    visitSpell',
  )
where

import Data.Inst (Inst2, Inst3, Inst4)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.ObjectN
  ( OArtifact,
    OCreature,
    OEnchantment,
    OInstant,
    OPlaneswalker,
    OSorcery,
    ObjectN,
  )
import MtgPure.Model.ObjectType
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
data Spell :: forall a. a -> Type where
  SpellArtifact :: Spell OTArtifact
  SpellCreature :: Spell OTCreature
  SpellEnchantment :: Spell OTEnchantment
  SpellInstant :: Spell OTInstant
  SpellPlaneswalker :: Spell OTPlaneswalker
  SpellSorcery :: Spell OTSorcery
  Spell :: Spell OTSpell
  Spell2 :: Inst2 IsSpellType a b => Spell '(a, b)
  Spell3 :: Inst3 IsSpellType a b c => Spell '(a, b, c)
  Spell4 :: Inst4 IsSpellType a b c d => Spell '(a, b, c, d)

deriving instance Show (Spell a)

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
  singSpell :: Proxy a -> Spell a
  visitSpell :: SpellVisitor b -> Spell a -> ObjectN a -> b

visitSpell' :: IsSpellType a => (forall b. IsSpellType b => ObjectN b -> x) -> Spell a -> ObjectN a -> x
visitSpell' f = visitSpell $ SpellVisitor f f f f f f

instance IsSpellType OTArtifact where
  singSpellType _ = STArtifact
  singSpell _ = SpellArtifact
  visitSpell v _ = visitSArtifact v

instance IsSpellType OTCreature where
  singSpellType _ = STCreature
  singSpell _ = SpellCreature
  visitSpell v _ = visitSCreature v

instance IsSpellType OTEnchantment where
  singSpellType _ = STEnchantment
  singSpell _ = SpellEnchantment
  visitSpell v _ = visitSEnchantment v

instance IsSpellType OTInstant where
  singSpellType _ = STInstant
  singSpell _ = SpellInstant
  visitSpell v _ = visitSInstant v

instance IsSpellType OTPlaneswalker where
  singSpellType _ = STPlaneswalker
  singSpell _ = SpellPlaneswalker
  visitSpell v _ = visitSPlaneswalker v

instance IsSpellType OTSorcery where
  singSpellType _ = STSorcery
  singSpell _ = SpellSorcery
  visitSpell v _ = visitSSorcery v

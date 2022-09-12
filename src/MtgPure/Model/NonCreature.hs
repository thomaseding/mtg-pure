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

module MtgPure.Model.NonCreature
  ( NonCreature (..),
    NonCreatureType (..),
    IsNonCreatureType (..),
    NonCreatureVisitor (..),
    visitNonCreature',
  )
where

import Data.Inst (Inst2, Inst3)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.ObjectN
  ( OArtifact,
    OEnchantment,
    OInstant,
    OLand,
    OPlaneswalker,
    OSorcery,
    ObjectN,
  )
import MtgPure.Model.ObjectType
  ( OTArtifact,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTNonCreature,
    OTPlaneswalker,
    OTSorcery,
  )

data NonCreatureType
  = NCTArtifact
  | NCTEnchantment
  | NCTInstant
  | NCTLand
  | NCTPlaneswalker
  | NCTSorcery
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Witness type
data NonCreature :: forall a. a -> Type where
  NonCreatureArtifact :: NonCreature OTArtifact
  NonCreatureEnchantment :: NonCreature OTEnchantment
  NonCreatureInstant :: NonCreature OTInstant
  NonCreatureLand :: NonCreature OTLand
  NonCreaturePlaneswalker :: NonCreature OTPlaneswalker
  NonCreatureSorcery :: NonCreature OTSorcery
  NonCreature :: NonCreature OTNonCreature
  NonCreature2 :: Inst2 IsNonCreatureType a b => NonCreature '(a, b)
  NonCreature3 :: Inst3 IsNonCreatureType a b c => NonCreature '(a, b, c)

deriving instance Show (NonCreature a)

data NonCreatureVisitor a = NonCreatureVisitor
  { visitNCArtifact :: OArtifact -> a,
    visitNCInstant :: OInstant -> a,
    visitNCEnchantment :: OEnchantment -> a,
    visitNCLand :: OLand -> a,
    visitNCPlaneswalker :: OPlaneswalker -> a,
    visitNCSorcery :: OSorcery -> a
  }

class IsObjectType a => IsNonCreatureType a where
  singNonCreatureType :: Proxy a -> NonCreatureType
  singNonCreature :: Proxy a -> NonCreature a
  visitNonCreature :: NonCreatureVisitor b -> NonCreature a -> ObjectN a -> b

visitNonCreature' :: IsNonCreatureType a => (forall b. IsNonCreatureType b => ObjectN b -> x) -> NonCreature a -> ObjectN a -> x
visitNonCreature' f = visitNonCreature $ NonCreatureVisitor f f f f f f

instance IsNonCreatureType OTArtifact where
  singNonCreatureType _ = NCTArtifact
  singNonCreature _ = NonCreatureArtifact
  visitNonCreature v _ = visitNCArtifact v

instance IsNonCreatureType OTEnchantment where
  singNonCreatureType _ = NCTEnchantment
  singNonCreature _ = NonCreatureEnchantment
  visitNonCreature v _ = visitNCEnchantment v

instance IsNonCreatureType OTInstant where
  singNonCreatureType _ = NCTInstant
  singNonCreature _ = NonCreatureInstant
  visitNonCreature v _ = visitNCInstant v

instance IsNonCreatureType OTLand where
  singNonCreatureType _ = NCTLand
  singNonCreature _ = NonCreatureLand
  visitNonCreature v _ = visitNCLand v

instance IsNonCreatureType OTPlaneswalker where
  singNonCreatureType _ = NCTPlaneswalker
  singNonCreature _ = NonCreaturePlaneswalker
  visitNonCreature v _ = visitNCPlaneswalker v

instance IsNonCreatureType OTSorcery where
  singNonCreatureType _ = NCTInstant
  singNonCreature _ = NonCreatureSorcery
  visitNonCreature v _ = visitNCSorcery v

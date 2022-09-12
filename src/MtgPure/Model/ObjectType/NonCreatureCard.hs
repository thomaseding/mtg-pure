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

module MtgPure.Model.ObjectType.NonCreatureCard
  ( WNonCreatureCard (..),
    NonCreatureCardType (..),
    IsNonCreatureCardType (..),
    NonCreatureCardVisitor (..),
    visitNonCreature',
  )
where

import safe Data.Inst (Inst2, Inst3)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ObjectN.Type
  ( OArtifact,
    OEnchantment,
    OInstant,
    OLand,
    ON1,
    OPlaneswalker,
    OSorcery,
  )
import safe MtgPure.Model.ObjectType (OT1, OT2, OT3, ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind
  ( OTArtifact,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTNonCreature,
    OTPlaneswalker,
    OTSorcery,
  )

data NonCreatureCardType
  = NCTArtifact
  | NCTEnchantment
  | NCTInstant
  | NCTLand
  | NCTPlaneswalker
  | NCTSorcery
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Witness type
data WNonCreatureCard :: Type -> Type where
  WNonCreatureArtifact :: WNonCreatureCard OTArtifact
  WNonCreatureEnchantment :: WNonCreatureCard OTEnchantment
  WNonCreatureInstant :: WNonCreatureCard OTInstant
  WNonCreatureLand :: WNonCreatureCard OTLand
  WNonCreaturePlaneswalker :: WNonCreatureCard OTPlaneswalker
  WNonCreatureSorcery :: WNonCreatureCard OTSorcery
  WNonCreatureCard :: WNonCreatureCard OTNonCreature
  WNonCreatureCard2 :: Inst2 IsNonCreatureCardType a b => WNonCreatureCard (OT2 a b)
  WNonCreatureCard3 :: Inst3 IsNonCreatureCardType a b c => WNonCreatureCard (OT3 a b c)

deriving instance Show (WNonCreatureCard a)

data NonCreatureCardVisitor a = NonCreatureCardVisitor
  { visitNCArtifact :: OArtifact -> a,
    visitNCInstant :: OInstant -> a,
    visitNCEnchantment :: OEnchantment -> a,
    visitNCLand :: OLand -> a,
    visitNCPlaneswalker :: OPlaneswalker -> a,
    visitNCSorcery :: OSorcery -> a
  }

class IsObjectType a => IsNonCreatureCardType a where
  singNonCreatureCardType :: Proxy a -> NonCreatureCardType
  singNonCreatureCard :: Proxy a -> WNonCreatureCard (OT1 a)
  visitNonCreatureCard :: NonCreatureCardVisitor b -> WNonCreatureCard (OT1 a) -> ON1 a -> b

visitNonCreature' ::
  IsNonCreatureCardType a =>
  (forall b. IsNonCreatureCardType b => ON1 b -> x) ->
  WNonCreatureCard (OT1 a) ->
  ON1 a ->
  x
visitNonCreature' f = visitNonCreatureCard $ NonCreatureCardVisitor f f f f f f

instance IsNonCreatureCardType 'OTArtifact where
  singNonCreatureCardType _ = NCTArtifact
  singNonCreatureCard _ = WNonCreatureArtifact
  visitNonCreatureCard v _ = visitNCArtifact v

instance IsNonCreatureCardType 'OTEnchantment where
  singNonCreatureCardType _ = NCTEnchantment
  singNonCreatureCard _ = WNonCreatureEnchantment
  visitNonCreatureCard v _ = visitNCEnchantment v

instance IsNonCreatureCardType 'OTInstant where
  singNonCreatureCardType _ = NCTInstant
  singNonCreatureCard _ = WNonCreatureInstant
  visitNonCreatureCard v _ = visitNCInstant v

instance IsNonCreatureCardType 'OTLand where
  singNonCreatureCardType _ = NCTLand
  singNonCreatureCard _ = WNonCreatureLand
  visitNonCreatureCard v _ = visitNCLand v

instance IsNonCreatureCardType 'OTPlaneswalker where
  singNonCreatureCardType _ = NCTPlaneswalker
  singNonCreatureCard _ = WNonCreaturePlaneswalker
  visitNonCreatureCard v _ = visitNCPlaneswalker v

instance IsNonCreatureCardType 'OTSorcery where
  singNonCreatureCardType _ = NCTSorcery
  singNonCreatureCard _ = WNonCreatureSorcery
  visitNonCreatureCard v _ = visitNCSorcery v

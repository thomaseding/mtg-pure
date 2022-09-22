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

module MtgPure.Model.ObjectType.NonCreatureCard (
  WNonCreatureCard (..),
  NonCreatureCardType (..),
  IsNonCreatureCardType (..),
  NonCreatureCardVisitor (..),
  visitNonCreature',
) where

import safe Data.Inst (Inst2, Inst3)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object (
  IsObjectType,
  OT1,
  OT2,
  OT3,
  ObjectType (..),
 )
import safe MtgPure.Model.ObjectType.Kind (
  OTArtifact,
  OTEnchantment,
  OTInstant,
  OTLand,
  OTNonCreature,
  OTPlaneswalker,
  OTSorcery,
 )
import safe MtgPure.Model.ZoneObject (ZO)

data NonCreatureCardType
  = NCTArtifact
  | NCTEnchantment
  | NCTInstant
  | NCTLand
  | NCTPlaneswalker
  | NCTSorcery
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

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
  deriving (Typeable)

deriving instance Show (WNonCreatureCard a)

data NonCreatureCardVisitor zone z = NonCreatureCardVisitor
  { visitNCArtifact :: ZO zone OTArtifact -> z
  , visitNCInstant :: ZO zone OTInstant -> z
  , visitNCEnchantment :: ZO zone OTEnchantment -> z
  , visitNCLand :: ZO zone OTLand -> z
  , visitNCPlaneswalker :: ZO zone OTPlaneswalker -> z
  , visitNCSorcery :: ZO zone OTSorcery -> z
  }
  deriving (Typeable)

class IsObjectType a => IsNonCreatureCardType a where
  singNonCreatureCardType :: Proxy a -> NonCreatureCardType
  singNonCreatureCard :: Proxy a -> WNonCreatureCard (OT1 a)
  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 a) -> ZO zone (OT1 a) -> z

visitNonCreature' ::
  IsNonCreatureCardType a =>
  (forall a'. IsNonCreatureCardType a' => ZO zone (OT1 a') -> z) ->
  WNonCreatureCard (OT1 a) ->
  ZO zone (OT1 a) ->
  z
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

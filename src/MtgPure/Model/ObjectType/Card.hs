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

module MtgPure.Model.ObjectType.Card
  ( WCard (..),
    CardType (..),
    IsCardType (..),
    CardVisitor (..),
    visitCard',
  )
where

import safe Data.Inst (Inst2, Inst3)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe MtgPure.Model.CardType (CardType (..))
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ObjectType (OT1, OT2, OT3, ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind
  ( OTArtifact,
    OTCard,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTSorcery,
  )
import safe MtgPure.Model.ZoneObject (ZO)

-- Witness type
data WCard :: Type -> Type where
  WCardArtifact :: WCard OTArtifact
  WCardCreature :: WCard OTCreature
  WCardEnchantment :: WCard OTEnchantment
  WCardInstant :: WCard OTInstant
  WCardLand :: WCard OTLand
  WCardPlaneswalker :: WCard OTPlaneswalker
  WCardSorcery :: WCard OTSorcery
  WCard :: WCard OTCard
  WCard2 :: Inst2 IsCardType a b => WCard (OT2 a b)
  WCard3 :: Inst3 IsCardType a b c => WCard (OT3 a b c)

deriving instance Show (WCard a)

data CardVisitor zone z = CardVisitor
  { visitCArtifact :: ZO zone OTArtifact -> z,
    visitCCreature :: ZO zone OTCreature -> z,
    visitCInstant :: ZO zone OTInstant -> z,
    visitCEnchantment :: ZO zone OTEnchantment -> z,
    visitCLand :: ZO zone OTLand -> z,
    visitCPlaneswalker :: ZO zone OTPlaneswalker -> z,
    visitCSorcery :: ZO zone OTSorcery -> z
  }

class IsObjectType a => IsCardType a where
  singCardType :: Proxy a -> CardType
  singCard :: Proxy a -> WCard (OT1 a)
  visitCard :: CardVisitor zone z -> WCard (OT1 a) -> ZO zone (OT1 a) -> z

visitCard' ::
  IsCardType a =>
  (forall a'. IsCardType a' => ZO zone (OT1 a') -> z) ->
  WCard (OT1 a) ->
  ZO zone (OT1 a) ->
  z
visitCard' f = visitCard $ CardVisitor f f f f f f f

instance IsCardType 'OTArtifact where
  singCardType _ = CTArtifact
  singCard _ = WCardArtifact
  visitCard v _ = visitCArtifact v

instance IsCardType 'OTCreature where
  singCardType _ = CTEnchantment
  singCard _ = WCardCreature
  visitCard v _ = visitCCreature v

instance IsCardType 'OTEnchantment where
  singCardType _ = CTEnchantment
  singCard _ = WCardEnchantment
  visitCard v _ = visitCEnchantment v

instance IsCardType 'OTInstant where
  singCardType _ = CTInstant
  singCard _ = WCardInstant
  visitCard v _ = visitCInstant v

instance IsCardType 'OTLand where
  singCardType _ = CTLand
  singCard _ = WCardLand
  visitCard v _ = visitCLand v

instance IsCardType 'OTPlaneswalker where
  singCardType _ = CTPlaneswalker
  singCard _ = WCardPlaneswalker
  visitCard v _ = visitCPlaneswalker v

instance IsCardType 'OTSorcery where
  singCardType _ = CTSorcery
  singCard _ = WCardSorcery
  visitCard v _ = visitCSorcery v

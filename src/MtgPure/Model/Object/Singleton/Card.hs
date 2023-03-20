{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.Singleton.Card (
  WCard (..),
  CardType (..),
  IsCardType (..),
  CardVisitor (..),
  visitCard',
  CoCard (..),
) where

import safe Data.Inst (Inst2, Inst3)
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.CardType (
  CardType (..),
  ObjectTypeToCardType,
  SCardType (..),
 )
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNArtifact,
  OTNCard,
  OTNCreature,
  OTNEnchantment,
  OTNInstant,
  OTNLand,
  OTNPlaneswalker,
  OTNSorcery,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO)

-- Witness type
data WCard :: Type -> Type where
  WCardArtifact :: WCard OTNArtifact
  WCardCreature :: WCard OTNCreature
  WCardEnchantment :: WCard OTNEnchantment
  WCardInstant :: WCard OTNInstant
  WCardLand :: WCard OTNLand
  WCardPlaneswalker :: WCard OTNPlaneswalker
  WCardSorcery :: WCard OTNSorcery
  WCard :: WCard OTNCard
  WCard2 :: Inst2 IsCardType a b => WCard (OT2 a b)
  WCard3 :: Inst3 IsCardType a b c => WCard (OT3 a b c)
  deriving (Typeable)

deriving instance Show (WCard a)

data CardVisitor zone z = CardVisitor
  { visitCArtifact :: ZO zone OTNArtifact -> z
  , visitCCreature :: ZO zone OTNCreature -> z
  , visitCInstant :: ZO zone OTNInstant -> z
  , visitCEnchantment :: ZO zone OTNEnchantment -> z
  , visitCLand :: ZO zone OTNLand -> z
  , visitCPlaneswalker :: ZO zone OTNPlaneswalker -> z
  , visitCSorcery :: ZO zone OTNSorcery -> z
  }
  deriving (Typeable)

class IsObjectType a => IsCardType a where
  litCardType :: CardType
  singCardType :: SCardType (ObjectTypeToCardType a)
  singCard :: WCard (OT1 a)
  visitCard :: CardVisitor zone z -> WCard (OT1 a) -> ZO zone (OT1 a) -> z

visitCard' ::
  IsCardType a =>
  (forall a'. IsCardType a' => ZO zone (OT1 a') -> z) ->
  WCard (OT1 a) ->
  ZO zone (OT1 a) ->
  z
visitCard' f = visitCard $ CardVisitor f f f f f f f

instance IsCardType 'OTArtifact where
  litCardType = CTArtifact
  singCardType = SCTArtifact
  singCard = WCardArtifact
  visitCard v _ = visitCArtifact v

instance IsCardType 'OTCreature where
  litCardType = CTCreature
  singCardType = SCTCreature
  singCard = WCardCreature
  visitCard v _ = visitCCreature v

instance IsCardType 'OTEnchantment where
  litCardType = CTEnchantment
  singCardType = SCTEnchantment
  singCard = WCardEnchantment
  visitCard v _ = visitCEnchantment v

instance IsCardType 'OTInstant where
  litCardType = CTInstant
  singCardType = SCTInstant
  singCard = WCardInstant
  visitCard v _ = visitCInstant v

instance IsCardType 'OTLand where
  litCardType = CTLand
  singCardType = SCTLand
  singCard = WCardLand
  visitCard v _ = visitCLand v

instance IsCardType 'OTPlaneswalker where
  litCardType = CTPlaneswalker
  singCardType = SCTPlaneswalker
  singCard = WCardPlaneswalker
  visitCard v _ = visitCPlaneswalker v

instance IsCardType 'OTSorcery where
  litCardType = CTSorcery
  singCardType = SCTSorcery
  singCard = WCardSorcery
  visitCard v _ = visitCSorcery v

class IsOTN ot => CoCard ot where
  coCard :: WCard ot

instance CoCard OTNArtifact where
  coCard = WCardArtifact

instance CoCard OTNCreature where
  coCard = WCardCreature

instance CoCard OTNEnchantment where
  coCard = WCardEnchantment

instance CoCard OTNInstant where
  coCard = WCardInstant

instance CoCard OTNLand where
  coCard = WCardLand

instance CoCard OTNPlaneswalker where
  coCard = WCardPlaneswalker

instance CoCard OTNSorcery where
  coCard = WCardSorcery

instance CoCard OTNCard where
  coCard = WCard

instance Inst2 IsCardType a b => CoCard (OT2 a b) where
  coCard = WCard2 :: WCard (OT2 a b)

instance Inst3 IsCardType a b c => CoCard (OT3 a b c) where
  coCard = WCard3 :: WCard (OT3 a b c)

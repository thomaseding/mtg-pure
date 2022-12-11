{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType.Card (
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
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.OTN (
  OT1,
  OT2,
  OT3,
 )
import safe MtgPure.Model.ObjectType (ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTArtifact,
  OTCard,
  OTCreature,
  OTEnchantment,
  OTInstant,
  OTLand,
  OTPlaneswalker,
  OTSorcery,
 )
import safe MtgPure.Model.ZoneObject (IsOT, ZO)

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
  deriving (Typeable)

deriving instance Show (WCard a)

data CardVisitor zone z = CardVisitor
  { visitCArtifact :: ZO zone OTArtifact -> z
  , visitCCreature :: ZO zone OTCreature -> z
  , visitCInstant :: ZO zone OTInstant -> z
  , visitCEnchantment :: ZO zone OTEnchantment -> z
  , visitCLand :: ZO zone OTLand -> z
  , visitCPlaneswalker :: ZO zone OTPlaneswalker -> z
  , visitCSorcery :: ZO zone OTSorcery -> z
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

class IsOT ot => CoCard ot where
  coCard :: WCard ot

instance CoCard OTArtifact where
  coCard = WCardArtifact

instance CoCard OTCreature where
  coCard = WCardCreature

instance CoCard OTEnchantment where
  coCard = WCardEnchantment

instance CoCard OTInstant where
  coCard = WCardInstant

instance CoCard OTLand where
  coCard = WCardLand

instance CoCard OTPlaneswalker where
  coCard = WCardPlaneswalker

instance CoCard OTSorcery where
  coCard = WCardSorcery

instance CoCard OTCard where
  coCard = WCard

instance Inst2 IsCardType a b => CoCard (OT2 a b) where
  coCard = WCard2 :: WCard (OT2 a b)

instance Inst3 IsCardType a b c => CoCard (OT3 a b c) where
  coCard = WCard3 :: WCard (OT3 a b c)

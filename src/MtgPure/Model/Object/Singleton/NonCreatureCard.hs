{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.Singleton.NonCreatureCard (
  WNonCreatureCard (..),
  NonCreatureCardType (..),
  IsNonCreatureCardType (..),
  NonCreatureCardVisitor (..),
  visitNonCreature',
  CoNonCreatureCard (..),
) where

import safe Data.Inst (Inst2, Inst3)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNArtifact,
  OTNEnchantment,
  OTNInstant,
  OTNLand,
  OTNNonCreature,
  OTNPlaneswalker,
  OTNSorcery,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO)

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
  WNonCreatureArtifact :: WNonCreatureCard OTNArtifact
  WNonCreatureEnchantment :: WNonCreatureCard OTNEnchantment
  WNonCreatureInstant :: WNonCreatureCard OTNInstant
  WNonCreatureLand :: WNonCreatureCard OTNLand
  WNonCreaturePlaneswalker :: WNonCreatureCard OTNPlaneswalker
  WNonCreatureSorcery :: WNonCreatureCard OTNSorcery
  WNonCreatureCard :: WNonCreatureCard OTNNonCreature
  WNonCreatureCard2 :: (Inst2 IsNonCreatureCardType a b) => WNonCreatureCard (OT2 a b)
  WNonCreatureCard3 :: (Inst3 IsNonCreatureCardType a b c) => WNonCreatureCard (OT3 a b c)
  deriving (Typeable)

deriving instance Show (WNonCreatureCard a)

data NonCreatureCardVisitor zone z = NonCreatureCardVisitor
  { visitNCArtifact :: ZO zone OTNArtifact -> z
  , visitNCInstant :: ZO zone OTNInstant -> z
  , visitNCEnchantment :: ZO zone OTNEnchantment -> z
  , visitNCLand :: ZO zone OTNLand -> z
  , visitNCPlaneswalker :: ZO zone OTNPlaneswalker -> z
  , visitNCSorcery :: ZO zone OTNSorcery -> z
  }
  deriving (Typeable)

class (IsObjectType a) => IsNonCreatureCardType a where
  singNonCreatureCardType :: Proxy a -> NonCreatureCardType
  singNonCreatureCard :: Proxy a -> WNonCreatureCard (OT1 a)
  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 a) -> ZO zone (OT1 a) -> z

visitNonCreature' ::
  (IsNonCreatureCardType a) =>
  (forall a'. (IsNonCreatureCardType a') => ZO zone (OT1 a') -> z) ->
  WNonCreatureCard (OT1 a) ->
  ZO zone (OT1 a) ->
  z
visitNonCreature' f = visitNonCreatureCard $ NonCreatureCardVisitor f f f f f f

instance IsNonCreatureCardType 'OTArtifact where
  singNonCreatureCardType :: Proxy 'OTArtifact -> NonCreatureCardType
  singNonCreatureCardType _ = NCTArtifact

  singNonCreatureCard :: Proxy 'OTArtifact -> WNonCreatureCard (OT1 'OTArtifact)
  singNonCreatureCard _ = WNonCreatureArtifact

  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 'OTArtifact) -> ZO zone (OT1 'OTArtifact) -> z
  visitNonCreatureCard v _ = visitNCArtifact v

instance IsNonCreatureCardType 'OTEnchantment where
  singNonCreatureCardType :: Proxy 'OTEnchantment -> NonCreatureCardType
  singNonCreatureCardType _ = NCTEnchantment

  singNonCreatureCard :: Proxy 'OTEnchantment -> WNonCreatureCard (OT1 'OTEnchantment)
  singNonCreatureCard _ = WNonCreatureEnchantment

  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 'OTEnchantment) -> ZO zone (OT1 'OTEnchantment) -> z
  visitNonCreatureCard v _ = visitNCEnchantment v

instance IsNonCreatureCardType 'OTInstant where
  singNonCreatureCardType :: Proxy 'OTInstant -> NonCreatureCardType
  singNonCreatureCardType _ = NCTInstant

  singNonCreatureCard :: Proxy 'OTInstant -> WNonCreatureCard (OT1 'OTInstant)
  singNonCreatureCard _ = WNonCreatureInstant

  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 'OTInstant) -> ZO zone (OT1 'OTInstant) -> z
  visitNonCreatureCard v _ = visitNCInstant v

instance IsNonCreatureCardType 'OTLand where
  singNonCreatureCardType :: Proxy 'OTLand -> NonCreatureCardType
  singNonCreatureCardType _ = NCTLand

  singNonCreatureCard :: Proxy 'OTLand -> WNonCreatureCard (OT1 'OTLand)
  singNonCreatureCard _ = WNonCreatureLand

  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 'OTLand) -> ZO zone (OT1 'OTLand) -> z
  visitNonCreatureCard v _ = visitNCLand v

instance IsNonCreatureCardType 'OTPlaneswalker where
  singNonCreatureCardType :: Proxy 'OTPlaneswalker -> NonCreatureCardType
  singNonCreatureCardType _ = NCTPlaneswalker

  singNonCreatureCard :: Proxy 'OTPlaneswalker -> WNonCreatureCard (OT1 'OTPlaneswalker)
  singNonCreatureCard _ = WNonCreaturePlaneswalker

  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 'OTPlaneswalker) -> ZO zone (OT1 'OTPlaneswalker) -> z
  visitNonCreatureCard v _ = visitNCPlaneswalker v

instance IsNonCreatureCardType 'OTSorcery where
  singNonCreatureCardType :: Proxy 'OTSorcery -> NonCreatureCardType
  singNonCreatureCardType _ = NCTSorcery

  singNonCreatureCard :: Proxy 'OTSorcery -> WNonCreatureCard (OT1 'OTSorcery)
  singNonCreatureCard _ = WNonCreatureSorcery

  visitNonCreatureCard :: NonCreatureCardVisitor zone z -> WNonCreatureCard (OT1 'OTSorcery) -> ZO zone (OT1 'OTSorcery) -> z
  visitNonCreatureCard v _ = visitNCSorcery v

class (IsOTN ot) => CoNonCreatureCard ot where
  coNonCreatureCard :: WNonCreatureCard ot

instance CoNonCreatureCard OTNArtifact where
  coNonCreatureCard :: WNonCreatureCard OTNArtifact
  coNonCreatureCard = WNonCreatureArtifact

instance CoNonCreatureCard OTNEnchantment where
  coNonCreatureCard :: WNonCreatureCard OTNEnchantment
  coNonCreatureCard = WNonCreatureEnchantment

instance CoNonCreatureCard OTNInstant where
  coNonCreatureCard :: WNonCreatureCard OTNInstant
  coNonCreatureCard = WNonCreatureInstant

instance CoNonCreatureCard OTNLand where
  coNonCreatureCard :: WNonCreatureCard OTNLand
  coNonCreatureCard = WNonCreatureLand

instance CoNonCreatureCard OTNPlaneswalker where
  coNonCreatureCard :: WNonCreatureCard OTNPlaneswalker
  coNonCreatureCard = WNonCreaturePlaneswalker

instance CoNonCreatureCard OTNSorcery where
  coNonCreatureCard :: WNonCreatureCard OTNSorcery
  coNonCreatureCard = WNonCreatureSorcery

instance (Inst2 IsNonCreatureCardType a b) => CoNonCreatureCard (OT2 a b) where
  coNonCreatureCard :: (Inst2 IsNonCreatureCardType a b) => WNonCreatureCard (OT2 a b)
  coNonCreatureCard = WNonCreatureCard2 :: WNonCreatureCard (OT2 a b)

instance (Inst3 IsNonCreatureCardType a b c) => CoNonCreatureCard (OT3 a b c) where
  coNonCreatureCard :: (Inst3 IsNonCreatureCardType a b c) => WNonCreatureCard (OT3 a b c)
  coNonCreatureCard = WNonCreatureCard3 :: WNonCreatureCard (OT3 a b c)

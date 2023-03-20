{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.OTNAliases (
  OTNAbility,
  OTNActivatedAbility,
  OTNActivatedOrTriggeredAbility,
  OTNAny,
  OTNArtifact,
  OTNArtifactCreature,
  OTNArtifactLand,
  OTNCard,
  OTNCreature,
  OTNCreaturePlaneswalker,
  OTNCreaturePlayer,
  OTNCreaturePlayerPlaneswalker,
  OTNDamageSource,
  OTNEmblem,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNInstant,
  OTNLand,
  OTNNonArtifactPermanent,
  OTNNonCreature,
  OTNNonCreaturePermanent,
  OTNNonEnchantmentPermanent,
  OTNNonLandPermanent,
  OTNNonPlaneswalkerPermanent,
  OTNPermanent,
  OTNPlaneswalker,
  OTNPlayer,
  OTNPlayerPlaneswalker,
  OTNSorcery,
  OTNSpell,
  OTNStaticAbility,
  OTNToken,
  OTNTriggeredAbility,
) where

import safe MtgPure.Model.Object.OT (
  OT (..),
 )
import safe MtgPure.Model.Object.OTN (OTN)

-- GHC doesn't seem to do the injectivity... simplify for bug report
--
-- type family MkOT (x :: k1) = (y :: k2) | y -> x where
--   MkOT a = '(OTN, a :: OT)
--   MkOT '(a, b) = '(OTN, a :: OT, b :: OT)
--   MkOT '(a, b, c) = '(OTN, a :: OT, b :: OT, c :: OT)
--   MkOT '(a, b, c, d) = '(OTN, a :: OT, b :: OT, c :: OT, d :: OT)
--   MkOT '(a, b, c, d, e) = '(OTN, a :: OT, b :: OT, c :: OT, d :: OT, e :: OT)
--   MkOT '(a, b, c, d, e, f) = '(OTN, a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT)
--   MkOT '(a, b, c, d, e, f, g) = '(OTN, a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT, g :: OT)

type OTNActivatedAbility = OTN '( '(), 'OTActivatedAbility)

type OTNArtifact = OTN '( '(), 'OTArtifact)

type OTNCreature = OTN '( '(), 'OTCreature)

type OTNEmblem = OTN '( '(), 'OTEmblem)

type OTNEnchantment = OTN '( '(), 'OTEnchantment)

type OTNInstant = OTN '( '(), 'OTInstant)

type OTNLand = OTN '( '(), 'OTLand)

type OTNPlaneswalker = OTN '( '(), 'OTPlaneswalker)

type OTNPlayer = OTN '( '(), 'OTPlayer)

type OTNSorcery = OTN '( '(), 'OTSorcery)

type OTNStaticAbility = OTN '( '(), 'OTStaticAbility)

type OTNTriggeredAbility = OTN '( '(), 'OTTriggeredAbility)

type OTNToken = OTNPermanent

type OTNAbility =
  OTN
    '( '()
     , 'OTActivatedAbility
     , 'OTStaticAbility
     , 'OTTriggeredAbility
     )

type OTNActivatedOrTriggeredAbility =
  OTN
    '( '()
     , 'OTActivatedAbility
     , 'OTTriggeredAbility
     )

type OTNArtifactCreature =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     )

type OTNArtifactLand =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTLand
     )

type OTNCreaturePlayer =
  OTN
    '( '()
     , 'OTCreature
     , 'OTPlayer
     )

type OTNCreaturePlaneswalker =
  OTN
    '( '()
     , 'OTCreature
     , 'OTPlaneswalker
     )

type OTNPlayerPlaneswalker =
  OTN
    '( '()
     , 'OTPlaneswalker
     , 'OTPlayer
     )

type OTNEnchantmentCreature =
  OTN
    '( '()
     , 'OTCreature
     , 'OTEnchantment
     )

type OTNCreaturePlayerPlaneswalker =
  OTN
    '( '()
     , 'OTCreature
     , 'OTPlaneswalker
     , 'OTPlayer
     )

type OTNNonArtifactPermanent =
  OTN
    '( '()
     , 'OTCreature
     , 'OTEnchantment
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNNonCreaturePermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTEnchantment
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNNonEnchantmentPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNNonLandPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTPlaneswalker
     )

type OTNNonPlaneswalkerPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTLand
     )

type OTNPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNNonCreature =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTEnchantment
     , 'OTInstant
     , 'OTLand
     , 'OTPlaneswalker
     , 'OTSorcery
     )

type OTNSpell =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTInstant
     , 'OTPlaneswalker
     , 'OTSorcery
     )

type OTNCard =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTInstant
     , 'OTLand
     , 'OTPlaneswalker
     , 'OTSorcery
     )

type OTNDamageSource =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTInstant
     , 'OTLand
     , 'OTPlaneswalker
     , 'OTPlayer
     , 'OTSorcery
     )

type OTNAny =
  OTN
    '( '()
     , 'OTActivatedAbility
     , 'OTArtifact
     , 'OTCreature
     , 'OTEmblem
     , 'OTEnchantment
     , 'OTInstant
     , 'OTLand
     , 'OTPlaneswalker
     , 'OTPlayer
     , 'OTSorcery
     , 'OTStaticAbility
     , 'OTTriggeredAbility
     )

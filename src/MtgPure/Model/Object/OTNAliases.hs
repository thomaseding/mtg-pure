{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.OTNAliases (
  OTAbility,
  OTActivatedAbility,
  OTActivatedOrTriggeredAbility,
  OTAny,
  OTArtifact,
  OTArtifactCreature,
  OTArtifactLand,
  OTCard,
  OTCreature,
  OTCreaturePlaneswalker,
  OTCreaturePlayer,
  OTCreaturePlayerPlaneswalker,
  OTDamageSource,
  OTEmblem,
  OTEnchantment,
  OTEnchantmentCreature,
  OTInstant,
  OTLand,
  OTNonArtifactPermanent,
  OTNonCreature,
  OTNonCreaturePermanent,
  OTNonEnchantmentPermanent,
  OTNonLandPermanent,
  OTNonPlaneswalkerPermanent,
  OTPermanent,
  OTPlaneswalker,
  OTPlayer,
  OTPlayerPlaneswalker,
  OTSorcery,
  OTSpell,
  OTStaticAbility,
  OTToken,
  OTTriggeredAbility,
) where

import safe MtgPure.Model.Object.OTN (OTN)
import safe MtgPure.Model.Object.ObjectType (
  ObjectType (..),
 )

-- GHC doesn't seem to do the injectivity... simplify for bug report
--
-- type family MkOT (x :: k1) = (y :: k2) | y -> x where
--   MkOT a = '(OTN, a :: ObjectType)
--   MkOT '(a, b) = '(OTN, a :: ObjectType, b :: ObjectType)
--   MkOT '(a, b, c) = '(OTN, a :: ObjectType, b :: ObjectType, c :: ObjectType)
--   MkOT '(a, b, c, d) = '(OTN, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
--   MkOT '(a, b, c, d, e) = '(OTN, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
--   MkOT '(a, b, c, d, e, f) = '(OTN, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
--   MkOT '(a, b, c, d, e, f, g) = '(OTN, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)

type OTActivatedAbility = OTN '( '(), 'OTActivatedAbility)

type OTArtifact = OTN '( '(), 'OTArtifact)

type OTCreature = OTN '( '(), 'OTCreature)

type OTEmblem = OTN '( '(), 'OTEmblem)

type OTEnchantment = OTN '( '(), 'OTEnchantment)

type OTInstant = OTN '( '(), 'OTInstant)

type OTLand = OTN '( '(), 'OTLand)

type OTPlaneswalker = OTN '( '(), 'OTPlaneswalker)

type OTPlayer = OTN '( '(), 'OTPlayer)

type OTSorcery = OTN '( '(), 'OTSorcery)

type OTStaticAbility = OTN '( '(), 'OTStaticAbility)

type OTTriggeredAbility = OTN '( '(), 'OTTriggeredAbility)

type OTToken = OTPermanent

type OTAbility =
  OTN
    '( '()
     , 'OTActivatedAbility
     , 'OTStaticAbility
     , 'OTTriggeredAbility
     )

type OTActivatedOrTriggeredAbility =
  OTN
    '( '()
     , 'OTActivatedAbility
     , 'OTTriggeredAbility
     )

type OTArtifactCreature =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     )

type OTArtifactLand =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTLand
     )

type OTCreaturePlayer =
  OTN
    '( '()
     , 'OTCreature
     , 'OTPlayer
     )

type OTCreaturePlaneswalker =
  OTN
    '( '()
     , 'OTCreature
     , 'OTPlaneswalker
     )

type OTPlayerPlaneswalker =
  OTN
    '( '()
     , 'OTPlaneswalker
     , 'OTPlayer
     )

type OTEnchantmentCreature =
  OTN
    '( '()
     , 'OTCreature
     , 'OTEnchantment
     )

type OTCreaturePlayerPlaneswalker =
  OTN
    '( '()
     , 'OTCreature
     , 'OTPlaneswalker
     , 'OTPlayer
     )

type OTNonArtifactPermanent =
  OTN
    '( '()
     , 'OTCreature
     , 'OTEnchantment
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNonCreaturePermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTEnchantment
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNonEnchantmentPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNonLandPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTPlaneswalker
     )

type OTNonPlaneswalkerPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTLand
     )

type OTPermanent =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTLand
     , 'OTPlaneswalker
     )

type OTNonCreature =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTEnchantment
     , 'OTInstant
     , 'OTLand
     , 'OTPlaneswalker
     , 'OTSorcery
     )

type OTSpell =
  OTN
    '( '()
     , 'OTArtifact
     , 'OTCreature
     , 'OTEnchantment
     , 'OTInstant
     , 'OTPlaneswalker
     , 'OTSorcery
     )

type OTCard =
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

type OTDamageSource =
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

type OTAny =
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

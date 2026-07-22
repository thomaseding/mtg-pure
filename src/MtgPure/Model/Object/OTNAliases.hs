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
  OTNBattle,
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

import safe MtgPure.Model.Object.OTKAliases (
  OTKAbility,
  OTKActivatedAbility,
  OTKActivatedOrTriggeredAbility,
  OTKAny,
  OTKArtifact,
  OTKArtifactCreature,
  OTKArtifactLand,
  OTKBattle,
  OTKCard,
  OTKCreature,
  OTKCreaturePlaneswalker,
  OTKCreaturePlayer,
  OTKCreaturePlayerPlaneswalker,
  OTKDamageSource,
  OTKEmblem,
  OTKEnchantment,
  OTKEnchantmentCreature,
  OTKInstant,
  OTKLand,
  OTKNonArtifactPermanent,
  OTKNonCreature,
  OTKNonCreaturePermanent,
  OTKNonEnchantmentPermanent,
  OTKNonLandPermanent,
  OTKNonPlaneswalkerPermanent,
  OTKPermanent,
  OTKPlaneswalker,
  OTKPlayer,
  OTKPlayerPlaneswalker,
  OTKSorcery,
  OTKSpell,
  OTKStaticAbility,
  OTKTriggeredAbility,
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

type OTNActivatedAbility = OTN OTKActivatedAbility

type OTNArtifact = OTN OTKArtifact

type OTNBattle = OTN OTKBattle

type OTNCreature = OTN OTKCreature

type OTNEmblem = OTN OTKEmblem

type OTNEnchantment = OTN OTKEnchantment

type OTNInstant = OTN OTKInstant

type OTNLand = OTN OTKLand

type OTNPlaneswalker = OTN OTKPlaneswalker

type OTNPlayer = OTN OTKPlayer

type OTNSorcery = OTN OTKSorcery

type OTNStaticAbility = OTN OTKStaticAbility

type OTNTriggeredAbility = OTN OTKTriggeredAbility

type OTNToken = OTNPermanent

type OTNAbility = OTN OTKAbility

type OTNActivatedOrTriggeredAbility = OTN OTKActivatedOrTriggeredAbility

type OTNArtifactCreature = OTN OTKArtifactCreature

type OTNArtifactLand = OTN OTKArtifactLand

type OTNCreaturePlayer = OTN OTKCreaturePlayer

type OTNCreaturePlaneswalker = OTN OTKCreaturePlaneswalker

type OTNPlayerPlaneswalker = OTN OTKPlayerPlaneswalker

type OTNEnchantmentCreature = OTN OTKEnchantmentCreature

type OTNCreaturePlayerPlaneswalker = OTN OTKCreaturePlayerPlaneswalker

type OTNNonArtifactPermanent = OTN OTKNonArtifactPermanent

type OTNNonCreaturePermanent = OTN OTKNonCreaturePermanent

type OTNNonEnchantmentPermanent = OTN OTKNonEnchantmentPermanent

type OTNNonLandPermanent = OTN OTKNonLandPermanent

type OTNNonPlaneswalkerPermanent = OTN OTKNonPlaneswalkerPermanent

type OTNPermanent = OTN OTKPermanent

type OTNNonCreature = OTN OTKNonCreature

type OTNSpell = OTN OTKSpell

type OTNCard = OTN OTKCard

type OTNDamageSource = OTN OTKDamageSource

type OTNAny = OTN OTKAny

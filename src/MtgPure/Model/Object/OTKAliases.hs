{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.OTKAliases (
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
  OTKToken,
  OTKTriggeredAbility,
) where

import safe MtgPure.Model.Object.OT (
  OT (..),
 )

type OTKActivatedAbility = '[ 'OTActivatedAbility] :: [OT]

type OTKArtifact = '[ 'OTArtifact] :: [OT]

type OTKBattle = '[ 'OTBattle] :: [OT]

type OTKCreature = '[ 'OTCreature] :: [OT]

type OTKEmblem = '[ 'OTEmblem] :: [OT]

type OTKEnchantment = '[ 'OTEnchantment] :: [OT]

type OTKInstant = '[ 'OTInstant] :: [OT]

type OTKLand = '[ 'OTLand] :: [OT]

type OTKPlaneswalker = '[ 'OTPlaneswalker] :: [OT]

type OTKPlayer = '[ 'OTPlayer] :: [OT]

type OTKSorcery = '[ 'OTSorcery] :: [OT]

type OTKStaticAbility = '[ 'OTStaticAbility] :: [OT]

type OTKTriggeredAbility = '[ 'OTTriggeredAbility] :: [OT]

type OTKToken = OTKPermanent

type OTKAbility =
  '[ 'OTActivatedAbility
   , 'OTStaticAbility
   , 'OTTriggeredAbility
   ] ::
    [OT]

type OTKActivatedOrTriggeredAbility =
  '[ 'OTActivatedAbility
   , 'OTTriggeredAbility
   ] ::
    [OT]

type OTKArtifactCreature =
  '[ 'OTArtifact
   , 'OTCreature
   ] ::
    [OT]

type OTKArtifactLand =
  '[ 'OTArtifact
   , 'OTLand
   ] ::
    [OT]

type OTKCreaturePlayer =
  '[ 'OTCreature
   , 'OTPlayer
   ] ::
    [OT]

type OTKCreaturePlaneswalker =
  '[ 'OTCreature
   , 'OTPlaneswalker
   ] ::
    [OT]

type OTKPlayerPlaneswalker =
  '[ 'OTPlaneswalker
   , 'OTPlayer
   ] ::
    [OT]

type OTKEnchantmentCreature =
  '[ 'OTCreature
   , 'OTEnchantment
   ] ::
    [OT]

type OTKCreaturePlayerPlaneswalker =
  '[ 'OTCreature
   , 'OTPlaneswalker
   , 'OTPlayer
   ] ::
    [OT]

type OTKNonArtifactPermanent =
  '[ 'OTCreature
   , 'OTBattle
   , 'OTEnchantment
   , 'OTLand
   , 'OTPlaneswalker
   ] ::
    [OT]

type OTKNonCreaturePermanent =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTEnchantment
   , 'OTLand
   , 'OTPlaneswalker
   ] ::
    [OT]

type OTKNonEnchantmentPermanent =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTCreature
   , 'OTLand
   , 'OTPlaneswalker
   ] ::
    [OT]

type OTKNonLandPermanent =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTCreature
   , 'OTEnchantment
   , 'OTPlaneswalker
   ] ::
    [OT]

type OTKNonPlaneswalkerPermanent =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTCreature
   , 'OTEnchantment
   , 'OTLand
   ] ::
    [OT]

type OTKPermanent =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTCreature
   , 'OTEnchantment
   , 'OTLand
   , 'OTPlaneswalker
   ] ::
    [OT]

type OTKNonCreature =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTEnchantment
   , 'OTInstant
   , 'OTLand
   , 'OTPlaneswalker
   , 'OTSorcery
   ] ::
    [OT]

type OTKSpell =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTCreature
   , 'OTEnchantment
   , 'OTInstant
   , 'OTPlaneswalker
   , 'OTSorcery
   ] ::
    [OT]

type OTKCard =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTCreature
   , 'OTEnchantment
   , 'OTInstant
   , 'OTLand
   , 'OTPlaneswalker
   , 'OTSorcery
   ] ::
    [OT]

type OTKDamageSource =
  '[ 'OTArtifact
   , 'OTBattle
   , 'OTCreature
   , 'OTEnchantment
   , 'OTInstant
   , 'OTLand
   , 'OTPlaneswalker
   , 'OTPlayer
   , 'OTSorcery
   ] ::
    [OT]

type OTKAny =
  '[ 'OTActivatedAbility
   , 'OTArtifact
   , 'OTBattle
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
   ] ::
    [OT]

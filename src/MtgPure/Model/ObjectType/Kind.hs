{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType.Kind
  ( OTAbility,
    OTActivatedAbility,
    OTActivatedOrTriggeredAbility,
    OTAny,
    OTArtifact,
    OTArtifactCreature,
    OTCard,
    OTCreature,
    OTCreaturePlaneswalker,
    OTCreaturePlayer,
    OTCreaturePlayerPlaneswalker,
    OTDamageSource,
    OTEmblem,
    OTEnchantment,
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
    OTTriggeredAbility,
  )
where

import MtgPure.Model.ObjectType (ObjectType (..))

type OTActivatedAbility = 'OTActivatedAbility

type OTArtifact = 'OTArtifact

type OTCreature = 'OTCreature

type OTEmblem = 'OTEmblem

type OTEnchantment = 'OTEnchantment

type OTInstant = 'OTInstant

type OTLand = 'OTLand

type OTPlaneswalker = 'OTPlaneswalker

type OTPlayer = 'OTPlayer

type OTSorcery = 'OTSorcery

type OTStaticAbility = 'OTStaticAbility

type OTTriggeredAbility = 'OTTriggeredAbility

type OTAbility =
  '( OTActivatedAbility,
     OTStaticAbility,
     OTTriggeredAbility
   )

type OTActivatedOrTriggeredAbility =
  '( OTActivatedAbility,
     OTTriggeredAbility
   )

type OTArtifactCreature =
  '( OTArtifact,
     OTCreature
   )

type OTCreaturePlayer =
  '( OTCreature,
     OTPlayer
   )

type OTCreaturePlaneswalker =
  '( OTCreature,
     OTPlaneswalker
   )

type OTPlayerPlaneswalker =
  '( OTPlaneswalker,
     OTPlayer
   )

type OTCreaturePlayerPlaneswalker =
  '( OTCreature,
     OTPlaneswalker,
     OTPlayer
   )

type OTNonArtifactPermanent =
  '( OTCreature,
     OTEnchantment,
     OTLand,
     OTPlaneswalker
   )

type OTNonCreaturePermanent =
  '( OTArtifact,
     OTEnchantment,
     OTLand,
     OTPlaneswalker
   )

type OTNonEnchantmentPermanent =
  '( OTArtifact,
     OTCreature,
     OTLand,
     OTPlaneswalker
   )

type OTNonLandPermanent =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTPlaneswalker
   )

type OTNonPlaneswalkerPermanent =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTLand
   )

type OTPermanent =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTLand,
     OTPlaneswalker
   )

type OTNonCreature =
  '( OTArtifact,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTSorcery
   )

type OTSpell =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTPlaneswalker,
     OTSorcery
   )

type OTCard =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTSorcery
   )

type OTDamageSource =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTPlayer,
     OTSorcery
   )

type OTAny =
  '( OTActivatedAbility,
     OTArtifact,
     OTCreature,
     OTEmblem,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTPlayer,
     OTSorcery,
     OTStaticAbility,
     OTTriggeredAbility
   )

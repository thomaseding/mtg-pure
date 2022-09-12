{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.ObjectN.Type
  ( OAbility,
    OActivatedAbility,
    OActivatedOrTriggeredAbility,
    OAny,
    OArtifact,
    OArtifactCreature,
    OCreature,
    OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OEmblem,
    OEnchantment,
    OInstant,
    OLand,
    OPermanent,
    OPlaneswalker,
    OPlayer,
    OPlayerPlaneswalker,
    OSorcery,
    OSpell,
    OStaticAbility,
    OTriggeredAbility,
  )
where

import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectType.Kind
  ( OTAbility,
    OTActivatedAbility,
    OTActivatedOrTriggeredAbility,
    OTAny,
    OTArtifact,
    OTArtifactCreature,
    OTCreature,
    OTCreaturePlaneswalker,
    OTCreaturePlayer,
    OTCreaturePlayerPlaneswalker,
    OTDamageSource,
    OTEmblem,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPermanent,
    OTPlaneswalker,
    OTPlayer,
    OTPlayerPlaneswalker,
    OTSorcery,
    OTSpell,
    OTStaticAbility,
    OTTriggeredAbility,
  )

type OAny = ObjectN OTAny

type OAbility = ObjectN OTAbility

type OActivatedAbility = ObjectN OTActivatedAbility

type OActivatedOrTriggeredAbility = ObjectN OTActivatedOrTriggeredAbility

type OArtifact = ObjectN OTArtifact

type OArtifactCreature = ObjectN OTArtifactCreature

type OCreature = ObjectN OTCreature

type OCreaturePlaneswalker = ObjectN OTCreaturePlaneswalker

type OCreaturePlayer = ObjectN OTCreaturePlayer

type OCreaturePlayerPlaneswalker = ObjectN OTCreaturePlayerPlaneswalker

type ODamageSource = ObjectN OTDamageSource

type OEmblem = ObjectN OTEmblem

type OEnchantment = ObjectN OTEnchantment

type OInstant = ObjectN OTInstant

type OLand = ObjectN OTLand

type OPermanent = ObjectN OTPermanent

type OPlaneswalker = ObjectN OTPlaneswalker

type OPlayer = ObjectN OTPlayer

type OPlayerPlaneswalker = ObjectN OTPlayerPlaneswalker

type OSorcery = ObjectN OTSorcery

type OSpell = ObjectN OTSpell

type OStaticAbility = ObjectN OTStaticAbility

type OTriggeredAbility = ObjectN OTTriggeredAbility

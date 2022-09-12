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
  ( ON1,
    ON2,
    ON3,
    ON4,
    ON5,
    ON6,
    ON7,
    ON8,
    ON9,
    ON10,
    ON11,
    ON12,
    OAbility,
    OActivatedAbility,
    OActivatedOrTriggeredAbility,
    OAny,
    OArtifact,
    OArtifactCreature,
    OCard,
    OCreature,
    OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OEmblem,
    OEnchantment,
    OInstant,
    OLand,
    ONonCreature,
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
import safe MtgPure.Model.ObjectType
  ( OT1,
    OT10,
    OT11,
    OT12,
    OT2,
    OT3,
    OT4,
    OT5,
    OT6,
    OT7,
    OT8,
    OT9,
  )
import safe MtgPure.Model.ObjectType.Kind
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
    OTNonCreature,
    OTPermanent,
    OTPlaneswalker,
    OTPlayer,
    OTPlayerPlaneswalker,
    OTSorcery,
    OTSpell,
    OTStaticAbility,
    OTTriggeredAbility,
  )

type ON1 a = ObjectN (OT1 a)

type ON2 a b = ObjectN (OT2 a b)

type ON3 a b c = ObjectN (OT3 a b c)

type ON4 a b c d = ObjectN (OT4 a b c d)

type ON5 a b c d e = ObjectN (OT5 a b c d e)

type ON6 a b c d e f = ObjectN (OT6 a b c d e f)

type ON7 a b c d e f g = ObjectN (OT7 a b c d e f g)

type ON8 a b c d e f g h = ObjectN (OT8 a b c d e f g h)

type ON9 a b c d e f g h i = ObjectN (OT9 a b c d e f g h i)

type ON10 a b c d e f g h i j = ObjectN (OT10 a b c d e f g h i j)

type ON11 a b c d e f g h i j k = ObjectN (OT11 a b c d e f g h i j k)

type ON12 a b c d e f g h i j k l = ObjectN (OT12 a b c d e f g h i j k l)

type OAny = ObjectN OTAny

type OAbility = ObjectN OTAbility

type OActivatedAbility = ObjectN OTActivatedAbility

type OActivatedOrTriggeredAbility = ObjectN OTActivatedOrTriggeredAbility

type OArtifact = ObjectN OTArtifact

type OArtifactCreature = ObjectN OTArtifactCreature

type OCard = ObjectN OTCard

type OCreature = ObjectN OTCreature

type OCreaturePlaneswalker = ObjectN OTCreaturePlaneswalker

type OCreaturePlayer = ObjectN OTCreaturePlayer

type OCreaturePlayerPlaneswalker = ObjectN OTCreaturePlayerPlaneswalker

type ODamageSource = ObjectN OTDamageSource

type OEmblem = ObjectN OTEmblem

type OEnchantment = ObjectN OTEnchantment

type OInstant = ObjectN OTInstant

type OLand = ObjectN OTLand

type ONonCreature = ObjectN OTNonCreature

type OPermanent = ObjectN OTPermanent

type OPlaneswalker = ObjectN OTPlaneswalker

type OPlayer = ObjectN OTPlayer

type OPlayerPlaneswalker = ObjectN OTPlayerPlaneswalker

type OSorcery = ObjectN OTSorcery

type OSpell = ObjectN OTSpell

type OStaticAbility = ObjectN OTStaticAbility

type OTriggeredAbility = ObjectN OTTriggeredAbility

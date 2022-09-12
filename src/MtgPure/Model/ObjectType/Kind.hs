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

import safe MtgPure.Model.ObjectType (OT, ObjectType (..))

-- GHC doesn't seem to do the injectivity... simplify for bug report
--
-- type family MkOT (x :: k1) = (y :: k2) | y -> x where
--   MkOT a = '(OT, a :: ObjectType)
--   MkOT '(a, b) = '(OT, a :: ObjectType, b :: ObjectType)
--   MkOT '(a, b, c) = '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType)
--   MkOT '(a, b, c, d) = '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)
--   MkOT '(a, b, c, d, e) = '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)
--   MkOT '(a, b, c, d, e, f) = '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)
--   MkOT '(a, b, c, d, e, f, g) = '(OT, a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)

type OTActivatedAbility = '(OT, 'OTActivatedAbility)

type OTArtifact = '(OT, 'OTArtifact)

type OTCreature = '(OT, 'OTCreature)

type OTEmblem = '(OT, 'OTEmblem)

type OTEnchantment = '(OT, 'OTEnchantment)

type OTInstant = '(OT, 'OTInstant)

type OTLand = '(OT, 'OTLand)

type OTPlaneswalker = '(OT, 'OTPlaneswalker)

type OTPlayer = '(OT, 'OTPlayer)

type OTSorcery = '(OT, 'OTSorcery)

type OTStaticAbility = '(OT, 'OTStaticAbility)

type OTTriggeredAbility = '(OT, 'OTTriggeredAbility)

type OTAbility =
  '( OT,
     'OTActivatedAbility,
     'OTStaticAbility,
     'OTTriggeredAbility
   )

type OTActivatedOrTriggeredAbility =
  '( OT,
     'OTActivatedAbility,
     'OTTriggeredAbility
   )

type OTArtifactCreature =
  '( OT,
     'OTArtifact,
     'OTCreature
   )

type OTCreaturePlayer =
  '( OT,
     'OTCreature,
     'OTPlayer
   )

type OTCreaturePlaneswalker =
  '( OT,
     'OTCreature,
     'OTPlaneswalker
   )

type OTPlayerPlaneswalker =
  '( OT,
     'OTPlaneswalker,
     'OTPlayer
   )

type OTCreaturePlayerPlaneswalker =
  '( OT,
     'OTCreature,
     'OTPlaneswalker,
     'OTPlayer
   )

type OTNonArtifactPermanent =
  '( OT,
     'OTCreature,
     'OTEnchantment,
     'OTLand,
     'OTPlaneswalker
   )

type OTNonCreaturePermanent =
  '( OT,
     'OTArtifact,
     'OTEnchantment,
     'OTLand,
     'OTPlaneswalker
   )

type OTNonEnchantmentPermanent =
  '( OT,
     'OTArtifact,
     'OTCreature,
     'OTLand,
     'OTPlaneswalker
   )

type OTNonLandPermanent =
  '( OT,
     'OTArtifact,
     'OTCreature,
     'OTEnchantment,
     'OTPlaneswalker
   )

type OTNonPlaneswalkerPermanent =
  '( OT,
     'OTArtifact,
     'OTCreature,
     'OTEnchantment,
     'OTLand
   )

type OTPermanent =
  '( OT,
     'OTArtifact,
     'OTCreature,
     'OTEnchantment,
     'OTLand,
     'OTPlaneswalker
   )

type OTNonCreature =
  '( OT,
     'OTArtifact,
     'OTEnchantment,
     'OTInstant,
     'OTLand,
     'OTPlaneswalker,
     'OTSorcery
   )

type OTSpell =
  '( OT,
     'OTArtifact,
     'OTCreature,
     'OTEnchantment,
     'OTInstant,
     'OTPlaneswalker,
     'OTSorcery
   )

type OTCard =
  '( OT,
     'OTArtifact,
     'OTCreature,
     'OTEnchantment,
     'OTInstant,
     'OTLand,
     'OTPlaneswalker,
     'OTSorcery
   )

type OTDamageSource =
  '( OT,
     'OTArtifact,
     'OTCreature,
     'OTEnchantment,
     'OTInstant,
     'OTLand,
     'OTPlaneswalker,
     'OTPlayer,
     'OTSorcery
   )

type OTAny =
  '( OT,
     'OTActivatedAbility,
     'OTArtifact,
     'OTCreature,
     'OTEmblem,
     'OTEnchantment,
     'OTInstant,
     'OTLand,
     'OTPlaneswalker,
     'OTPlayer,
     'OTSorcery,
     'OTStaticAbility,
     'OTTriggeredAbility
   )

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

module MtgPure.Model.Object
  ( Object,
    OArtifact,
    OCreature,
    OEnchantment,
    OInstant,
    OLand,
    OPlaneswalker,
    OPlayer,
    OSorcery,
  )
where

import MtgPure.Model.Internal.Object (Object)
import MtgPure.Model.ObjectType
  ( OTArtifact,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTPlayer,
    OTSorcery,
  )

type OArtifact = Object OTArtifact

type OCreature = Object OTCreature

type OEnchantment = Object OTEnchantment

type OInstant = Object OTInstant

type OLand = Object OTLand

type OPlaneswalker = Object OTPlaneswalker

type OPlayer = Object OTPlayer

type OSorcery = Object OTSorcery

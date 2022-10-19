{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Fwd.Impl (
  fwdImpl,
) where

import safe MtgPure.Engine.ActivateCast (
  activateAbility,
  askActivateAbility,
  castSpell,
 )
import safe MtgPure.Engine.CaseOf (caseOf)
import safe MtgPure.Engine.Core (
  allZOs,
  findHandCard,
  findLibraryCard,
  findPermanent,
  findPlayer,
  getAPNAP,
  getActivatedAbilities,
  getActivePlayer,
  getAlivePlayerCount,
  getAllActivatedAbilities,
  getPermanent,
  getPermanents,
  getPlayer,
  getPlayers,
  newObjectId,
  pushHandCard,
  pushLibraryCard,
  removeHandCard,
  removeLibraryCard,
  rewindIllegal,
  setPermanent,
  setPlayer,
  withEachControlledPermanent_,
  withEachPermanent,
  withEachPermanent_,
  withEachPlayer_,
 )
import safe MtgPure.Engine.Enact (enact)
import safe MtgPure.Engine.Fwd.Type (Fwd' (..))
import safe MtgPure.Engine.Pay (pay)
import safe MtgPure.Engine.PerformElections (performElections)
import safe MtgPure.Engine.PlayLand (askPlayLand)
import safe MtgPure.Engine.Priority (
  gainPriority,
  getHasPriority,
  getPlayerWithPriority,
 )
import safe MtgPure.Engine.Resolve (resolveTopOfStack)
import safe MtgPure.Engine.Satisfies (satisfies, zosSatisfying)
import safe MtgPure.Engine.State (Fwd)
import safe MtgPure.Engine.StateBasedActions (performStateBasedActions)
import safe MtgPure.Engine.Turn (startGame)

fwdImpl :: Monad m => Fwd m
fwdImpl =
  Fwd
    { fwd_ = ()
    , fwd_activateAbility = activateAbility
    , fwd_allZOs = allZOs
    , fwd_askActivateAbility = askActivateAbility
    , fwd_askPlayLand = askPlayLand
    , fwd_caseOf = caseOf
    , fwd_castSpell = castSpell
    , fwd_enact = enact
    , fwd_findHandCard = findHandCard
    , fwd_findLibraryCard = findLibraryCard
    , fwd_findPermanent = findPermanent
    , fwd_findPlayer = findPlayer
    , fwd_gainPriority = gainPriority
    , fwd_getActivatedAbilities = getActivatedAbilities
    , fwd_getActivePlayer = getActivePlayer
    , fwd_getAlivePlayerCount = getAlivePlayerCount
    , fwd_getAllActivatedAbilities = getAllActivatedAbilities
    , fwd_getAPNAP = getAPNAP
    , fwd_getHasPriority = getHasPriority
    , fwd_getPermanent = getPermanent
    , fwd_getPermanents = getPermanents
    , fwd_getPlayer = getPlayer
    , fwd_getPlayers = getPlayers
    , fwd_getPlayerWithPriority = getPlayerWithPriority
    , fwd_newObjectId = newObjectId
    , fwd_pay = pay
    , fwd_performElections = performElections
    , fwd_performStateBasedActions = performStateBasedActions
    , fwd_pushHandCard = pushHandCard
    , fwd_pushLibraryCard = pushLibraryCard
    , fwd_removeHandCard = removeHandCard
    , fwd_removeLibraryCard = removeLibraryCard
    , fwd_resolveTopOfStack = resolveTopOfStack
    , fwd_rewindIllegal = rewindIllegal
    , fwd_satisfies = satisfies
    , fwd_setPermanent = setPermanent
    , fwd_setPlayer = setPlayer
    , fwd_startGame = startGame
    , fwd_withEachControlledPermanent_ = withEachControlledPermanent_
    , fwd_withEachPermanent = withEachPermanent
    , fwd_withEachPermanent_ = withEachPermanent_
    , fwd_withEachPlayer_ = withEachPlayer_
    , fwd_zosSatisfying = zosSatisfying
    }

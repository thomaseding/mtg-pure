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
  activateAbilityImpl,
  askActivateAbilityImpl,
  castSpellImpl,
 )
import safe MtgPure.Engine.CaseOf (caseOfImpl)
import safe MtgPure.Engine.Core (
  allZOsImpl,
  findHandCardImpl,
  findLibraryCardImpl,
  findPermanentImpl,
  findPlayerImpl,
  getAPNAPImpl,
  getActivatedAbilitiesImpl,
  getActivePlayerImpl,
  getAlivePlayerCountImpl,
  getAllActivatedAbilitiesImpl,
  getPermanentImpl,
  getPermanentsImpl,
  getPlayerImpl,
  getPlayersImpl,
  newObjectIdImpl,
  pushHandCardImpl,
  pushLibraryCardImpl,
  removeHandCardImpl,
  removeLibraryCardImpl,
  rewindIllegalImpl,
  setPermanentImpl,
  setPlayerImpl,
  withEachControlledPermanentImpl_,
  withEachPermanentImpl,
  withEachPermanentImpl_,
  withEachPlayerImpl_,
 )
import safe MtgPure.Engine.Enact (enactImpl)
import safe MtgPure.Engine.Fwd (Fwd' (..))
import safe MtgPure.Engine.Pay (payImpl)
import safe MtgPure.Engine.PerformElections (performElectionsImpl)
import safe MtgPure.Engine.PlayLand (askPlayLandImpl)
import safe MtgPure.Engine.Priority (
  gainPriorityImpl,
  getHasPriorityImpl,
  getPlayerWithPriorityImpl,
 )
import safe MtgPure.Engine.Resolve (resolveTopOfStackImpl)
import safe MtgPure.Engine.Satisfies (satisfiesImpl, zosSatisfyingImpl)
import safe MtgPure.Engine.State (Fwd)
import safe MtgPure.Engine.StateBasedActions (performStateBasedActionsImpl)
import safe MtgPure.Engine.Turn (startGameImpl)

fwdImpl :: Monad m => Fwd m
fwdImpl =
  Fwd
    { fwd_ = ()
    , fwd_activateAbility = activateAbilityImpl
    , fwd_allZOs = allZOsImpl
    , fwd_askActivateAbility = askActivateAbilityImpl
    , fwd_askPlayLand = askPlayLandImpl
    , fwd_caseOf = caseOfImpl
    , fwd_castSpell = castSpellImpl
    , fwd_enact = enactImpl
    , fwd_findHandCard = findHandCardImpl
    , fwd_findLibraryCard = findLibraryCardImpl
    , fwd_findPermanent = findPermanentImpl
    , fwd_findPlayer = findPlayerImpl
    , fwd_gainPriority = gainPriorityImpl
    , fwd_getActivatedAbilities = getActivatedAbilitiesImpl
    , fwd_getActivePlayer = getActivePlayerImpl
    , fwd_getAlivePlayerCount = getAlivePlayerCountImpl
    , fwd_getAllActivatedAbilities = getAllActivatedAbilitiesImpl
    , fwd_getAPNAP = getAPNAPImpl
    , fwd_getHasPriority = getHasPriorityImpl
    , fwd_getPermanent = getPermanentImpl
    , fwd_getPermanents = getPermanentsImpl
    , fwd_getPlayer = getPlayerImpl
    , fwd_getPlayers = getPlayersImpl
    , fwd_getPlayerWithPriority = getPlayerWithPriorityImpl
    , fwd_newObjectId = newObjectIdImpl
    , fwd_pay = payImpl
    , fwd_performElections = performElectionsImpl
    , fwd_performStateBasedActions = performStateBasedActionsImpl
    , fwd_pushHandCard = pushHandCardImpl
    , fwd_pushLibraryCard = pushLibraryCardImpl
    , fwd_removeHandCard = removeHandCardImpl
    , fwd_removeLibraryCard = removeLibraryCardImpl
    , fwd_resolveTopOfStack = resolveTopOfStackImpl
    , fwd_rewindIllegal = rewindIllegalImpl
    , fwd_satisfies = satisfiesImpl
    , fwd_setPermanent = setPermanentImpl
    , fwd_setPlayer = setPlayerImpl
    , fwd_startGame = startGameImpl
    , fwd_withEachControlledPermanent_ = withEachControlledPermanentImpl_
    , fwd_withEachPermanent = withEachPermanentImpl
    , fwd_withEachPermanent_ = withEachPermanentImpl_
    , fwd_withEachPlayer_ = withEachPlayerImpl_
    , fwd_zosSatisfying = zosSatisfyingImpl
    }

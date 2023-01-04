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
  castSpell,
 )
import safe MtgPure.Engine.CaseOf (caseOf)
import safe MtgPure.Engine.Core (
  activatedToIndex,
  allControlledPermanentsOf,
  allPermanents,
  allZOActivatedAbilities,
  allZOs,
  doesZoneObjectExist,
  findHandCard,
  findLibraryCard,
  findPermanent,
  findPlayer,
  getAPNAP,
  getActivatedAbilitiesOf,
  getActivePlayer,
  getAlivePlayerCount,
  getAlivePlayers,
  getBasicLandTypes,
  getPermanent,
  getPlayer,
  indexToActivated,
  newObjectId,
  pushHandCard,
  pushLibraryCard,
  removeHandCard,
  removeLibraryCard,
  rewindIllegal,
  setPermanent,
  setPlayer,
  toZO,
 )
import safe MtgPure.Engine.Enact (enact)
import safe MtgPure.Engine.Fwd.Type (Fwd' (..))
import safe MtgPure.Engine.Pay (pay)
import safe MtgPure.Engine.PerformElections (controllerOf, performElections, requiresTargets)
import safe MtgPure.Engine.PlayLand (playLand)
import safe MtgPure.Engine.Priority (
  askPriorityAction,
  gainPriority,
  getHasPriority,
  getPlayerWithPriority,
 )
import safe MtgPure.Engine.Resolve (resolveOneShot, resolveTopOfStack)
import safe MtgPure.Engine.Satisfies (satisfies, zosSatisfying)
import safe MtgPure.Engine.State (Fwd)
import safe MtgPure.Engine.StateBasedActions (performStateBasedActions)
import safe MtgPure.Engine.Turn (startGame)

fwdImpl :: Monad m => Fwd m
fwdImpl =
  Fwd
    { fwd_ = ()
    , fwd_abilityToIndex = activatedToIndex
    , fwd_activateAbility = activateAbility
    , fwd_activatedAbilitiesOf = getActivatedAbilitiesOf
    , fwd_allControlledPermanentsOf = allControlledPermanentsOf
    , fwd_allPermanents = allPermanents
    , fwd_allZOActivatedAbilities = allZOActivatedAbilities
    , fwd_allZOs = allZOs
    , fwd_askPriorityAction = askPriorityAction
    , fwd_caseOf = caseOf
    , fwd_castSpell = castSpell
    , fwd_controllerOf = controllerOf
    , fwd_doesZoneObjectExist = doesZoneObjectExist
    , fwd_enact = enact
    , fwd_findHandCard = findHandCard
    , fwd_findLibraryCard = findLibraryCard
    , fwd_findPermanent = findPermanent
    , fwd_findPlayer = findPlayer
    , fwd_gainPriority = gainPriority
    , fwd_getActivePlayer = getActivePlayer
    , fwd_getAlivePlayers = getAlivePlayers
    , fwd_getAlivePlayerCount = getAlivePlayerCount
    , fwd_getAPNAP = getAPNAP
    , fwd_getBasicLandTypes = getBasicLandTypes
    , fwd_getHasPriority = getHasPriority
    , fwd_getPermanent = getPermanent
    , fwd_getPlayer = getPlayer
    , fwd_getPlayerWithPriority = getPlayerWithPriority
    , fwd_indexToAbility = indexToActivated
    , fwd_newObjectId = newObjectId
    , fwd_pay = pay
    , fwd_performElections = performElections
    , fwd_performStateBasedActions = performStateBasedActions
    , fwd_playLand = playLand
    , fwd_pushHandCard = pushHandCard
    , fwd_pushLibraryCard = pushLibraryCard
    , fwd_removeHandCard = removeHandCard
    , fwd_removeLibraryCard = removeLibraryCard
    , fwd_requiresTargets = requiresTargets
    , fwd_resolveOneShot = resolveOneShot
    , fwd_resolveTopOfStack = resolveTopOfStack
    , fwd_rewindIllegal = rewindIllegal
    , fwd_satisfies = satisfies
    , fwd_setPermanent = setPermanent
    , fwd_setPlayer = setPlayer
    , fwd_startGame = startGame
    , fwd_toZO = toZO
    , fwd_zosSatisfying = zosSatisfying
    }

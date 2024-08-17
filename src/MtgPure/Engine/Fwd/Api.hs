{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Fwd.Api (
  Api (..),
  ApiCont (..),
  run,
  runCont,
  rewindIllegal,
  rewindIllegalActivation,
  rewindNothing,
  eachLogged,
  eachLogged_,
  --
  activateAbility,
  activatedAbilitiesOf,
  activatedToIndex,
  allControlledPermanentsOf,
  allPermanents,
  allZOActivatedAbilities,
  allZOs,
  askPriorityAction,
  bailGainPriority,
  caseOf,
  castSpell,
  controllerOf,
  doesZoneObjectExist,
  enact,
  endTheGame,
  endTheTurn,
  findGraveyardCard,
  findHandCard,
  findLibraryCard,
  findPermanent,
  findPlayer,
  gainPriority,
  getActivePlayer,
  getAlivePlayers,
  getAlivePlayerCount,
  getAPNAP,
  getBasicLandTypes,
  getHasPriority,
  getIntrinsicManaAbilities,
  getPermanent,
  getPlayer,
  getPlayerWithPriority,
  getTrivialManaAbilities,
  indexToActivated,
  isSatisfied,
  localNewObjectId,
  modifyPlayer,
  newObjectId,
  newVariableId,
  ownerOf,
  pay,
  performIntrinsicElections,
  performResolveElections,
  performStateBasedActions,
  performTargetElections,
  pickOneZO,
  playLand,
  pushGraveyardCard,
  pushHandCard,
  pushLibraryCard,
  putOntoBattlefield,
  queryObjectId,
  removeGraveyardCard,
  removeHandCard,
  removeLibraryCard,
  resolveTopOfStack,
  resolveTopOfStackCont,
  resolveElected,
  satisfies,
  setPermanent,
  setPlayer,
  startGame,
  staticAbilitiesOf,
  triggeredAbilitiesOf,
  toZO,
  zosSatisfying,
) where

import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe qualified Data.Foldable as F
import safe Data.Kind (Type)
import safe qualified Data.Stream as Stream
import safe qualified Data.Traversable as T
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd.Type (Fwd' (..))
import safe MtgPure.Engine.Legality (Legality)
import safe MtgPure.Engine.Monad (
  PriorityEnd,
  fromRO,
  gets,
  internalFromPrivate,
  liftCont,
 )
import safe MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex,
  ActivateAbility,
  ActivateResult,
  CastSpell,
  Elected,
  ElectionInput,
  Ev,
  PlayLand,
  PlayerCount (..),
  PriorityAction,
  QueryObjectResult,
  ResolveElected,
  SomeActivatedAbility,
  SomeStaticAbility,
  SomeTriggeredAbility,
  SourceZO,
  SpecialAction,
 )
import safe MtgPure.Engine.State (
  Fwd,
  GameResult,
  GameState (..),
  Magic,
  MagicCont,
  logCall,
 )
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.Combinators (CanHaveTrivialManaAbility)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.OTNAliases (OTNCard, OTNPermanent)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (ObjectId)
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (Permanent)
import safe MtgPure.Model.Player (Player)
import safe MtgPure.Model.Recursive (
  AnyCard,
  Case,
  Condition,
  Cost,
  Effect,
  Elect,
  Requirement,
 )
import safe MtgPure.Model.Variable (VariableId)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO)

getFwd :: (Monad m, IsReadWrite rw) => Magic v rw m (Fwd m)
getFwd = internalFromPrivate $ fromRO $ gets magicFwd

fwd0 :: (IsReadWrite rw, Monad m) => (Fwd m -> Magic v rw m z) -> Magic v rw m z
fwd0 go = do
  fwd <- getFwd
  go fwd

fwd1 :: (IsReadWrite rw, Monad m) => (Fwd m -> (a -> Magic v rw m z)) -> a -> Magic v rw m z
fwd1 go a = do
  fwd <- getFwd
  go fwd a

fwd2 :: (IsReadWrite rw, Monad m) => (Fwd m -> (a -> b -> Magic v rw m z)) -> a -> b -> Magic v rw m z
fwd2 go a b = do
  fwd <- getFwd
  go fwd a b

fwd3 :: (IsReadWrite rw, Monad m) => (Fwd m -> (a -> b -> c -> Magic v rw m z)) -> a -> b -> c -> Magic v rw m z
fwd3 go a b c = do
  fwd <- getFwd
  go fwd a b c

-- fwd4 :: (IsReadWrite rw, Monad m) => (Fwd m -> (a -> b -> c -> d -> Magic v rw m z)) -> a -> b -> c -> d -> Magic v rw m z
-- fwd4 go a b c d = do
--   fwd <- getFwd
--   go fwd a b c d

data Api (m :: Type -> Type) (v :: Visibility) (rw :: ReadWrite) (ret :: Type) :: Type where
  ActivatedToIndex :: (IsZO zone ot) => SomeActivatedAbility zone ot -> Api m 'Private 'RO AbsoluteActivatedAbilityIndex
  ActivatedAbilitiesOf :: (IsZO zone ot) => ZO zone ot -> Api m 'Private 'RO [SomeActivatedAbility zone ot]
  ActivePlayer :: Api m 'Public 'RO (Object 'OTPlayer)
  AlivePlayers :: Api m 'Public 'RO [Object 'OTPlayer]
  AlivePlayerCount :: Api m 'Public 'RO PlayerCount
  AllPermanents :: Api m 'Public 'RO [ZO 'ZBattlefield OTNPermanent]
  AllZOActivatedAbilities :: (IsZO zone ot) => Api m 'Private 'RO [SomeActivatedAbility zone ot]
  AllZOs :: (IsZO zone ot) => Api m 'Private 'RO [ZO zone ot]
  CaseOf :: (x -> Api m 'Private 'RW a) -> Case x -> Api m 'Private 'RW a
  ControllerOf :: (IsZO zone ot) => ZO zone ot -> Api m 'Private 'RO (Object 'OTPlayer)
  DoesZoneObjectExist :: (IsZO zone ot) => ZO zone ot -> Api m 'Private 'RO Bool
  Enact :: Maybe SourceZO -> Effect 'OneShot -> Api m 'Private 'RW [Ev]
  FindHandCard :: Object 'OTPlayer -> ZO 'ZHand OTNCard -> Api m 'Private 'RO (Maybe AnyCard)
  FindLibraryCard :: Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Api m 'Private 'RO (Maybe AnyCard)
  FindPermanent :: ZO 'ZBattlefield OTNPermanent -> Api m 'Private 'RO (Maybe Permanent)
  FindPlayer :: Object 'OTPlayer -> Api m 'Private 'RO (Maybe Player)
  GetAPNAP :: Api m v 'RO (Stream.Stream (Object 'OTPlayer))
  GetZoneOf :: ObjectId -> Api m 'Private 'RO Zone
  GetPermanent :: ZO 'ZBattlefield OTNPermanent -> Api m 'Private 'RO Permanent
  GetPlayer :: Object 'OTPlayer -> Api m 'Private 'RO Player
  HasPriority :: Object 'OTPlayer -> Api m 'Public 'RO Bool
  IndexToActivated :: (IsZO zone ot) => AbsoluteActivatedAbilityIndex -> Api m 'Private 'RO (Maybe (SomeActivatedAbility zone ot))
  ModifyPlayer :: Object 'OTPlayer -> (Player -> Player) -> Api m 'Private 'RW ()
  NewObjectId :: Api m 'Private 'RW ObjectId
  Pay :: Object 'OTPlayer -> Cost -> Api m 'Private 'RW Legality
  PerformStateBasedActions :: Api m 'Private 'RW ()
  PlayerWithPriority :: Api m 'Public 'RO (Maybe (Object 'OTPlayer))
  PushHandCard :: Object 'OTPlayer -> AnyCard -> Api m 'Private 'RW (ZO 'ZHand OTNCard)
  PushLibraryCard :: Object 'OTPlayer -> AnyCard -> Api m 'Private 'RW (ZO 'ZLibrary OTNCard)
  RemoveHandCard :: Object 'OTPlayer -> ZO 'ZHand OTNCard -> Api m 'Private 'RW (Maybe AnyCard)
  RemoveLibraryCard :: Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Api m 'Private 'RW (Maybe AnyCard)
  Satisfies :: (IsZO zone ot) => ZO zone ot -> Requirement zone ot -> Api m 'Private 'RO Bool
  SetPermanent :: ZO 'ZBattlefield OTNPermanent -> Maybe Permanent -> Api m 'Private 'RW ()
  SetPlayer :: Object 'OTPlayer -> Player -> Api m 'Private 'RW ()
  StartGame :: Api m 'Private 'RW Void
  ToZO :: (IsZO zone ot) => ObjectId -> Api m 'Private 'RO (Maybe (ZO zone ot))
  ZOsSatisfying :: (IsZO zone ot) => Requirement zone ot -> Api m 'Private 'RO [ZO zone ot]

data ApiCont (m :: Type -> Type) (v :: Visibility) (rw :: ReadWrite) (bail :: Type) (a :: Type) :: Type where
  AskPriorityAction :: Object 'OTPlayer -> ApiCont m 'Private 'RW PriorityEnd ()
  GainPriority :: Object 'OTPlayer -> ApiCont m 'Private 'RW Void ()
  ResolveTopOfStack :: ApiCont m 'Private 'RW PriorityEnd Void

run :: (IsReadWrite rw, Monad m) => Api m v rw z -> Magic v rw m z
run = \case
  ActivatedToIndex a -> activatedToIndex a
  ActivatedAbilitiesOf a -> activatedAbilitiesOf a
  ActivePlayer -> getActivePlayer
  AlivePlayers -> getAlivePlayers
  AlivePlayerCount -> getAlivePlayerCount
  AllPermanents -> allPermanents
  AllZOActivatedAbilities -> allZOActivatedAbilities
  AllZOs -> allZOs
  CaseOf a b -> caseOf (run . a) b
  ControllerOf a -> controllerOf a
  DoesZoneObjectExist a -> doesZoneObjectExist a
  Enact a b -> enact a b
  FindHandCard a b -> findHandCard a b
  FindLibraryCard a b -> findLibraryCard a b
  FindPermanent a -> findPermanent a
  FindPlayer a -> findPlayer a
  GetAPNAP -> getAPNAP
  GetPermanent a -> getPermanent a
  GetPlayer a -> getPlayer a
  GetZoneOf a -> undefined a
  HasPriority a -> getHasPriority a
  IndexToActivated a -> indexToActivated a
  ModifyPlayer a b -> modifyPlayer a b
  NewObjectId -> newObjectId
  Pay a b -> pay a b
  PerformStateBasedActions -> performStateBasedActions
  PlayerWithPriority -> getPlayerWithPriority
  PushHandCard a b -> pushHandCard a b
  PushLibraryCard a b -> pushLibraryCard a b
  RemoveHandCard a b -> removeHandCard a b
  RemoveLibraryCard a b -> removeLibraryCard a b
  Satisfies a b -> satisfies a b
  SetPermanent a b -> setPermanent a b
  SetPlayer a b -> setPlayer a b
  StartGame -> startGame
  ToZO a -> toZO a
  ZOsSatisfying a -> zosSatisfying a

runCont :: (Monad m) => ApiCont m v rw bail a -> MagicCont v rw bail m a
runCont = \case
  AskPriorityAction a -> askPriorityAction a
  GainPriority a -> gainPriority a
  ResolveTopOfStack -> resolveTopOfStackCont

rewindIllegal :: (Monad m) => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegal = fwd1 fwd_rewindIllegal

rewindIllegalActivation :: (Monad m) => Magic 'Private 'RW m ActivateResult -> Magic 'Private 'RW m ActivateResult
rewindIllegalActivation = fwd1 fwd_rewindIllegalActivation

rewindNothing :: (Monad m) => Magic 'Private 'RW m (Maybe a) -> Magic 'Private 'RW m (Maybe a)
rewindNothing = fwd1 fwd_rewindNothing

eachLogged :: (IsReadWrite rw, Monad m) => [a] -> (a -> Magic v rw m z) -> Magic v rw m [z]
eachLogged f = logCall 'eachLogged . T.for f

eachLogged_ :: (IsReadWrite rw, Monad m) => [a] -> (a -> Magic v rw m ()) -> Magic v rw m ()
eachLogged_ f = logCall 'eachLogged_ . F.for_ f

----------------------------------------

askPriorityAction :: (Monad m) => Object 'OTPlayer -> MagicCont 'Private 'RW PriorityEnd m ()
askPriorityAction a = do
  fwd <- liftCont getFwd
  fwd_askPriorityAction fwd a

bailGainPriority :: (Monad m) => Object 'OTPlayer -> MagicCont 'Private 'RW PriorityEnd m a
bailGainPriority a = do
  fwd <- liftCont getFwd
  fwd_bailGainPriority fwd a

endTheTurn :: (Monad m) => MagicCont 'Private 'RW Void m Void
endTheTurn = do
  fwd <- liftCont getFwd
  fwd_endTheTurn fwd

gainPriority :: (Monad m) => Object 'OTPlayer -> MagicCont 'Private 'RW Void m ()
gainPriority a = do
  fwd <- liftCont getFwd
  fwd_gainPriority fwd a

resolveTopOfStackCont :: (Monad m) => MagicCont 'Private 'RW PriorityEnd m Void
resolveTopOfStackCont = do
  fwd <- liftCont getFwd
  fwd_resolveTopOfStackCont fwd

----------------------------------------

activatedToIndex :: (IsZO zone ot, Monad m) => SomeActivatedAbility zone ot -> Magic 'Private 'RO m AbsoluteActivatedAbilityIndex
activatedToIndex = fwd1 fwd_abilityToIndex

activatedAbilitiesOf :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m [SomeActivatedAbility zone ot]
activatedAbilitiesOf = fwd1 fwd_activatedAbilitiesOf

allZOActivatedAbilities :: (IsZO zone ot, Monad m) => Magic 'Private 'RO m [SomeActivatedAbility zone ot]
allZOActivatedAbilities = fwd0 fwd_allZOActivatedAbilities

-- TODO: This prolly should be Private instead because of things like face-down morph creatures.
allControlledPermanentsOf ::
  (Monad m) =>
  Object 'OTPlayer ->
  Magic 'Public 'RO m [ZO 'ZBattlefield OTNPermanent]
allControlledPermanentsOf = fwd1 fwd_allControlledPermanentsOf

-- TODO: This prolly should be Private instead because of things like face-down morph creatures.
allPermanents :: (Monad m) => Magic 'Public 'RO m [ZO 'ZBattlefield OTNPermanent]
allPermanents = fwd0 fwd_allPermanents

allZOs :: (IsZO zone ot, Monad m) => Magic 'Private 'RO m [ZO zone ot]
allZOs = fwd0 fwd_allZOs

caseOf :: (IsReadWrite rw, Monad m) => (x -> Magic 'Private rw m a) -> Case x -> Magic 'Private rw m a
caseOf = fwd2 fwd_caseOf

activateAbility :: forall m. (Monad m) => Object 'OTPlayer -> PriorityAction ActivateAbility -> Magic 'Private 'RW m ActivateResult
activateAbility = fwd2 fwd_activateAbility

castSpell :: forall m. (Monad m) => Object 'OTPlayer -> PriorityAction CastSpell -> Magic 'Private 'RW m Legality
castSpell = fwd2 fwd_castSpell

controllerOf :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m (Object 'OTPlayer)
controllerOf = fwd1 fwd_controllerOf

doesZoneObjectExist :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m Bool
doesZoneObjectExist = fwd1 fwd_doesZoneObjectExist

enact :: (Monad m) => Maybe SourceZO -> Effect 'OneShot -> Magic 'Private 'RW m [Ev]
enact = fwd2 fwd_enact

endTheGame :: (Monad m) => GameResult m -> Magic 'Public 'RO m Void
endTheGame = fwd1 fwd_endTheGame

findGraveyardCard :: (Monad m) => Object 'OTPlayer -> ZO 'ZGraveyard OTNCard -> Magic 'Private 'RO m (Maybe AnyCard)
findGraveyardCard = fwd2 fwd_findGraveyardCard

findHandCard :: (Monad m) => Object 'OTPlayer -> ZO 'ZHand OTNCard -> Magic 'Private 'RO m (Maybe AnyCard)
findHandCard = fwd2 fwd_findHandCard

findLibraryCard :: (Monad m) => Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Magic 'Private 'RO m (Maybe AnyCard)
findLibraryCard = fwd2 fwd_findLibraryCard

findPermanent :: (Monad m) => ZO 'ZBattlefield OTNPermanent -> Magic 'Private 'RO m (Maybe Permanent)
findPermanent = fwd1 fwd_findPermanent

findPlayer :: (Monad m) => Object 'OTPlayer -> Magic 'Private 'RO m (Maybe Player)
findPlayer = fwd1 fwd_findPlayer

getAlivePlayers :: (Monad m) => Magic 'Public 'RO m [Object 'OTPlayer]
getAlivePlayers = fwd0 fwd_getAlivePlayers

getActivePlayer :: (Monad m) => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayer = fwd0 fwd_getActivePlayer

getAlivePlayerCount :: (Monad m) => Magic 'Public 'RO m PlayerCount
getAlivePlayerCount = fwd0 fwd_getAlivePlayerCount

getAPNAP :: (Monad m) => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAP = fwd0 fwd_getAPNAP

getBasicLandTypes :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m [BasicLandType]
getBasicLandTypes = fwd1 fwd_getBasicLandTypes

getHasPriority :: (Monad m) => Object 'OTPlayer -> Magic 'Public 'RO m Bool
getHasPriority = fwd1 fwd_getHasPriority

getIntrinsicManaAbilities :: (CanHaveTrivialManaAbility ot, Monad m) => ZO 'ZBattlefield ot -> Magic 'Private 'RO m [SomeActivatedAbility 'ZBattlefield ot]
getIntrinsicManaAbilities = fwd1 fwd_getIntrinsicManaAbilities

getPermanent :: (Monad m) => ZO 'ZBattlefield OTNPermanent -> Magic 'Private 'RO m Permanent
getPermanent = fwd1 fwd_getPermanent

getPlayer :: (Monad m) => Object 'OTPlayer -> Magic 'Private 'RO m Player
getPlayer = fwd1 fwd_getPlayer

getPlayerWithPriority :: (Monad m) => Magic 'Public 'RO m (Maybe (Object 'OTPlayer))
getPlayerWithPriority = fwd0 fwd_getPlayerWithPriority

getTrivialManaAbilities :: (CanHaveTrivialManaAbility ot, Monad m) => ZO 'ZBattlefield ot -> Magic 'Private 'RO m [SomeActivatedAbility 'ZBattlefield ot]
getTrivialManaAbilities = fwd1 fwd_getTrivialManaAbilities

indexToActivated :: (IsZO zone ot, Monad m) => AbsoluteActivatedAbilityIndex -> Magic 'Private 'RO m (Maybe (SomeActivatedAbility zone ot))
indexToActivated = fwd1 fwd_indexToActivated

isSatisfied :: (Monad m) => Condition -> Magic 'Private 'RO m Bool
isSatisfied = fwd1 fwd_isSatisfied

localNewObjectId :: (IsReadWrite rw, Monad m) => Object 'OTPlayer -> (ObjectId -> Magic 'Private rw m a) -> Magic 'Private rw m a
localNewObjectId = fwd2 fwd_localNewObjectId

modifyPlayer :: (Monad m) => Object 'OTPlayer -> (Player -> Player) -> Magic 'Private 'RW m ()
modifyPlayer o f = do
  p <- fromRO $ getPlayer o
  setPlayer o $ f p

newObjectId :: (Monad m) => Magic 'Private 'RW m ObjectId
newObjectId = fwd0 fwd_newObjectId

newVariableId :: (Monad m) => Magic 'Private 'RW m VariableId
newVariableId = fwd0 fwd_newVariableId

ownerOf :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m (Object 'OTPlayer)
ownerOf = fwd1 fwd_ownerOf

pay :: (Monad m) => Object 'OTPlayer -> Cost -> Magic 'Private 'RW m Legality
pay = fwd2 fwd_pay

performIntrinsicElections ::
  forall ot m el x.
  (Monad m) =>
  ElectionInput 'IntrinsicStage ->
  (el -> Magic 'Private 'RO m x) ->
  Elect 'IntrinsicStage el ot ->
  Magic 'Private 'RO m x
performIntrinsicElections = fwd3 fwd_performIntrinsicElections

performResolveElections ::
  forall ot m el x.
  (Monad m) =>
  ElectionInput 'ResolveStage ->
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect 'ResolveStage el ot ->
  Magic 'Private 'RW m (Maybe x)
performResolveElections = fwd3 fwd_performResolveElections

performStateBasedActions :: (Monad m) => Magic 'Private 'RW m ()
performStateBasedActions = fwd0 fwd_performStateBasedActions

performTargetElections ::
  forall ot m el x.
  (Monad m) =>
  ElectionInput 'TargetStage ->
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect 'TargetStage el ot ->
  Magic 'Private 'RW m (Maybe x)
performTargetElections = fwd3 fwd_performTargetElections

pickOneZO :: (IsZO zone ot, Monad m) => Object 'OTPlayer -> [ZO zone ot] -> Magic 'Public 'RW m (Maybe (ZO zone ot))
pickOneZO = fwd2 fwd_pickOneZO

playLand :: (Monad m) => Object 'OTPlayer -> SpecialAction PlayLand -> Magic 'Private 'RW m Legality
playLand = fwd2 fwd_playLand

pushGraveyardCard :: (Monad m) => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZGraveyard OTNCard)
pushGraveyardCard = fwd2 fwd_pushGraveyardCard

pushHandCard :: (Monad m) => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZHand OTNCard)
pushHandCard = fwd2 fwd_pushHandCard

pushLibraryCard :: (Monad m) => Object 'OTPlayer -> AnyCard -> Magic 'Private 'RW m (ZO 'ZLibrary OTNCard)
pushLibraryCard = fwd2 fwd_pushLibraryCard

putOntoBattlefield ::
  (IsZO zone ot, CoPermanent ot, Monad m) =>
  Object 'OTPlayer ->
  ZO zone ot ->
  Magic 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
putOntoBattlefield = fwd2 fwd_putOntoBattlefield

queryObjectId :: (Monad m) => ObjectId -> Magic 'Private 'RO m (Maybe QueryObjectResult)
queryObjectId = fwd1 fwd_queryObjectId

removeGraveyardCard :: (Monad m) => Object 'OTPlayer -> ZO 'ZGraveyard OTNCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeGraveyardCard = fwd2 fwd_removeGraveyardCard

removeHandCard :: (Monad m) => Object 'OTPlayer -> ZO 'ZHand OTNCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeHandCard = fwd2 fwd_removeHandCard

removeLibraryCard :: (Monad m) => Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Magic 'Private 'RW m (Maybe AnyCard)
removeLibraryCard = fwd2 fwd_removeLibraryCard

resolveElected :: (IsOTN ot, Monad m) => ZO 'ZStack OT0 -> Elected 'TargetStage ot -> Magic 'Private 'RW m ResolveElected
resolveElected = fwd2 fwd_resolveElected

resolveTopOfStack :: (Monad m) => Magic 'Private 'RW m (Maybe ResolveElected)
resolveTopOfStack = fwd0 fwd_resolveTopOfStack

satisfies :: (IsZO zone ot, Monad m) => ZO zone ot -> Requirement zone ot -> Magic 'Private 'RO m Bool
satisfies = fwd2 fwd_satisfies

setPermanent :: (Monad m) => ZO 'ZBattlefield OTNPermanent -> Maybe Permanent -> Magic 'Private 'RW m ()
setPermanent = fwd2 fwd_setPermanent

setPlayer :: (Monad m) => Object 'OTPlayer -> Player -> Magic 'Private 'RW m ()
setPlayer = fwd2 fwd_setPlayer

startGame :: (Monad m) => Magic 'Private 'RW m Void
startGame = fwd0 fwd_startGame

staticAbilitiesOf :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m [SomeStaticAbility zone ot]
staticAbilitiesOf = fwd1 fwd_staticAbilitiesOf

toZO :: (IsZO zone ot, Monad m) => ObjectId -> Magic 'Private 'RO m (Maybe (ZO zone ot))
toZO = fwd1 fwd_toZO

triggeredAbilitiesOf :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m [SomeTriggeredAbility zone ot]
triggeredAbilitiesOf = fwd1 fwd_triggeredAbilitiesOf

zosSatisfying :: (IsZO zone ot, Monad m) => Requirement zone ot -> Magic 'Private 'RO m [ZO zone ot]
zosSatisfying = fwd1 fwd_zosSatisfying

{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Fwd.Type (
  Fwd' (..),
) where

import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe qualified Data.Stream as Stream
import safe Data.Void (Void)
import safe MtgPure.Engine.Legality (Legality)
import safe MtgPure.Engine.Monad (Magic', MagicCont', PriorityEnd)
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

data Fwd' ex st m where
  Fwd ::
    { fwd_ :: ()
    , fwd_abilityToIndex :: forall zone ot. IsZO zone ot => SomeActivatedAbility zone ot -> Magic' ex st 'Private 'RO m AbsoluteActivatedAbilityIndex
    , fwd_activateAbility :: Object 'OTPlayer -> PriorityAction ActivateAbility -> Magic' ex st 'Private 'RW m ActivateResult
    , fwd_activatedAbilitiesOf :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m [SomeActivatedAbility zone ot]
    , fwd_allControlledPermanentsOf :: Object 'OTPlayer -> Magic' ex st 'Public 'RO m [ZO 'ZBattlefield OTNPermanent]
    , fwd_allPermanents :: Magic' ex st 'Public 'RO m [ZO 'ZBattlefield OTNPermanent]
    , fwd_allZOActivatedAbilities :: forall zone ot. IsZO zone ot => Magic' ex st 'Private 'RO m [SomeActivatedAbility zone ot]
    , fwd_allZOs :: forall zone ot. IsZO zone ot => Magic' ex st 'Private 'RO m [ZO zone ot]
    , fwd_askPriorityAction :: Object 'OTPlayer -> MagicCont' ex st 'Private 'RW PriorityEnd m ()
    , fwd_bailGainPriority :: forall a. Object 'OTPlayer -> MagicCont' ex st 'Private 'RW PriorityEnd m a
    , fwd_caseOf :: forall rw x a. IsReadWrite rw => (x -> Magic' ex st 'Private rw m a) -> Case x -> Magic' ex st 'Private rw m a
    , fwd_castSpell :: Object 'OTPlayer -> PriorityAction CastSpell -> Magic' ex st 'Private 'RW m Legality
    , fwd_controllerOf :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m (Object 'OTPlayer)
    , fwd_doesZoneObjectExist :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m Bool
    , fwd_enact :: Maybe SourceZO -> Effect 'OneShot -> Magic' ex st 'Private 'RW m [Ev]
    , -- | RO is ok-ish here because the return type is Void and the game just ends.
      --  It's still a bit of a hack, but it's better than hacking in some Public RW user API.
      fwd_endTheGame :: ex -> Magic' ex st 'Public 'RO m Void
    , fwd_endTheTurn :: MagicCont' ex st 'Private 'RW Void m Void
    , fwd_findGraveyardCard :: Object 'OTPlayer -> ZO 'ZGraveyard OTNCard -> Magic' ex st 'Private 'RO m (Maybe AnyCard)
    , fwd_findHandCard :: Object 'OTPlayer -> ZO 'ZHand OTNCard -> Magic' ex st 'Private 'RO m (Maybe AnyCard)
    , fwd_findLibraryCard :: Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Magic' ex st 'Private 'RO m (Maybe AnyCard)
    , fwd_findPermanent :: ZO 'ZBattlefield OTNPermanent -> Magic' ex st 'Private 'RO m (Maybe Permanent)
    , fwd_findPlayer :: Object 'OTPlayer -> Magic' ex st 'Private 'RO m (Maybe Player)
    , fwd_gainPriority :: Object 'OTPlayer -> MagicCont' ex st 'Private 'RW Void m ()
    , fwd_getActivePlayer :: Magic' ex st 'Public 'RO m (Object 'OTPlayer)
    , fwd_getAlivePlayers :: Magic' ex st 'Public 'RO m [Object 'OTPlayer]
    , fwd_getAlivePlayerCount :: Magic' ex st 'Public 'RO m PlayerCount
    , fwd_getAPNAP :: forall v. Magic' ex st v 'RO m (Stream.Stream (Object 'OTPlayer))
    , fwd_getBasicLandTypes :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m [BasicLandType]
    , fwd_getHasPriority :: Object 'OTPlayer -> Magic' ex st 'Public 'RO m Bool
    , fwd_getIntrinsicManaAbilities :: forall ot. CanHaveTrivialManaAbility ot => ZO 'ZBattlefield ot -> Magic' ex st 'Private 'RO m [SomeActivatedAbility 'ZBattlefield ot]
    , fwd_getPermanent :: ZO 'ZBattlefield OTNPermanent -> Magic' ex st 'Private 'RO m Permanent
    , fwd_getPlayer :: Object 'OTPlayer -> Magic' ex st 'Private 'RO m Player
    , fwd_getPlayerWithPriority :: Magic' ex st 'Public 'RO m (Maybe (Object 'OTPlayer))
    , fwd_getTrivialManaAbilities :: forall ot. CanHaveTrivialManaAbility ot => ZO 'ZBattlefield ot -> Magic' ex st 'Private 'RO m [SomeActivatedAbility 'ZBattlefield ot]
    , fwd_indexToActivated :: forall zone ot. IsZO zone ot => AbsoluteActivatedAbilityIndex -> Magic' ex st 'Private 'RO m (Maybe (SomeActivatedAbility zone ot))
    , fwd_isSatisfied :: Condition -> Magic' ex st 'Private 'RO m Bool
    , fwd_localNewObjectId :: forall rw a. IsReadWrite rw => Object 'OTPlayer -> (ObjectId -> Magic' ex st 'Private rw m a) -> Magic' ex st 'Private rw m a
    , fwd_newObjectId :: Magic' ex st 'Private 'RW m ObjectId
    , fwd_newVariableId :: Magic' ex st 'Private 'RW m VariableId
    , fwd_ownerOf :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m (Object 'OTPlayer)
    , fwd_pay :: Object 'OTPlayer -> Cost -> Magic' ex st 'Private 'RW m Legality
    , fwd_performIntrinsicElections :: forall ot el x. ElectionInput 'IntrinsicStage -> (el -> Magic' ex st 'Private 'RO m x) -> Elect 'IntrinsicStage el ot -> Magic' ex st 'Private 'RO m x
    , fwd_performResolveElections :: forall ot el x. ElectionInput 'ResolveStage -> (el -> Magic' ex st 'Private 'RW m (Maybe x)) -> Elect 'ResolveStage el ot -> Magic' ex st 'Private 'RW m (Maybe x)
    , fwd_performTargetElections :: forall ot el x. ElectionInput 'TargetStage -> (el -> Magic' ex st 'Private 'RW m (Maybe x)) -> Elect 'TargetStage el ot -> Magic' ex st 'Private 'RW m (Maybe x)
    , fwd_performStateBasedActions :: Magic' ex st 'Private 'RW m ()
    , fwd_pickOneZO :: forall zone ot. IsZO zone ot => Object 'OTPlayer -> [ZO zone ot] -> Magic' ex st 'Public 'RW m (Maybe (ZO zone ot))
    , fwd_playLand :: Object 'OTPlayer -> SpecialAction PlayLand -> Magic' ex st 'Private 'RW m Legality
    , fwd_pushGraveyardCard :: Object 'OTPlayer -> AnyCard -> Magic' ex st 'Private 'RW m (ZO 'ZGraveyard OTNCard)
    , fwd_pushHandCard :: Object 'OTPlayer -> AnyCard -> Magic' ex st 'Private 'RW m (ZO 'ZHand OTNCard)
    , fwd_pushLibraryCard :: Object 'OTPlayer -> AnyCard -> Magic' ex st 'Private 'RW m (ZO 'ZLibrary OTNCard)
    , fwd_putOntoBattlefield :: forall zone ot. (IsZO zone ot, CoPermanent ot) => Object 'OTPlayer -> ZO zone ot -> Magic' ex st 'Private 'RW m (Maybe (ZO 'ZBattlefield OTNPermanent))
    , fwd_queryObjectId :: ObjectId -> Magic' ex st 'Private 'RO m (Maybe QueryObjectResult)
    , fwd_removeGraveyardCard :: Object 'OTPlayer -> ZO 'ZGraveyard OTNCard -> Magic' ex st 'Private 'RW m (Maybe AnyCard)
    , fwd_removeHandCard :: Object 'OTPlayer -> ZO 'ZHand OTNCard -> Magic' ex st 'Private 'RW m (Maybe AnyCard)
    , fwd_removeLibraryCard :: Object 'OTPlayer -> ZO 'ZLibrary OTNCard -> Magic' ex st 'Private 'RW m (Maybe AnyCard)
    , fwd_resolveElected :: forall ot. IsOTN ot => ZO 'ZStack OT0 -> Elected 'TargetStage ot -> Magic' ex st 'Private 'RW m ResolveElected
    , fwd_resolveTopOfStack :: Monad m => Magic' ex st 'Private 'RW m (Maybe ResolveElected)
    , fwd_resolveTopOfStackCont :: MagicCont' ex st 'Private 'RW PriorityEnd m Void
    , fwd_rewindIllegal :: Magic' ex st 'Private 'RW m Legality -> Magic' ex st 'Private 'RW m Bool
    , fwd_rewindIllegalActivation :: Magic' ex st 'Private 'RW m ActivateResult -> Magic' ex st 'Private 'RW m ActivateResult
    , fwd_rewindNothing :: forall a. Magic' ex st 'Private 'RW m (Maybe a) -> Magic' ex st 'Private 'RW m (Maybe a)
    , fwd_satisfies :: forall zone ot. IsZO zone ot => ZO zone ot -> Requirement zone ot -> Magic' ex st 'Private 'RO m Bool
    , fwd_setPermanent :: ZO 'ZBattlefield OTNPermanent -> Maybe Permanent -> Magic' ex st 'Private 'RW m ()
    , fwd_setPlayer :: Object 'OTPlayer -> Player -> Magic' ex st 'Private 'RW m ()
    , fwd_startGame :: Magic' ex st 'Private 'RW m Void
    , fwd_staticAbilitiesOf :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m [SomeStaticAbility zone ot]
    , fwd_toZO :: forall zone ot. IsZO zone ot => ObjectId -> Magic' ex st 'Private 'RO m (Maybe (ZO zone ot))
    , fwd_triggeredAbilitiesOf :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m [SomeTriggeredAbility zone ot]
    , fwd_zosSatisfying :: forall zone ot. IsZO zone ot => Requirement zone ot -> Magic' ex st 'Private 'RO m [ZO zone ot]
    } ->
    Fwd' ex st m

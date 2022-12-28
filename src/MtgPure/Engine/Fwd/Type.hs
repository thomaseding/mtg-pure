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

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Util (AndLike)
import safe qualified Data.Stream as Stream
import safe Data.Void (Void)
import safe MtgPure.Engine.Legality (Legality)
import safe MtgPure.Engine.Monad (Magic', MagicCont')
import safe MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex,
  Play,
  PlayerCount (..),
  SomeActivatedAbility,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object.OTKind (OTCard, OTPermanent, OTSpell)
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (ObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Permanent (Permanent)
import safe MtgPure.Model.Player (Player)
import safe MtgPure.Model.Recursive (
  AnyCard,
  Case,
  Cost,
  Effect,
  Elect,
  Requirement,
 )
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)

data Fwd' ex st m where
  Fwd ::
    { fwd_ :: ()
    , fwd_abilityToIndex :: forall zone ot. IsZO zone ot => SomeActivatedAbility zone ot -> Magic' ex st 'Private 'RO m AbsoluteActivatedAbilityIndex
    , fwd_activatedAbilitiesOf :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m [SomeActivatedAbility zone ot]
    , fwd_allControlledPermanentsOf :: Object 'OTPlayer -> Magic' ex st 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
    , fwd_allPermanents :: Magic' ex st 'Public 'RO m [ZO 'ZBattlefield OTPermanent]
    , fwd_allPlayers :: Magic' ex st 'Public 'RO m [Object 'OTPlayer]
    , fwd_allZOActivatedAbilities :: forall zone ot. IsZO zone ot => Magic' ex st 'Private 'RO m [SomeActivatedAbility zone ot]
    , fwd_allZOs :: forall zone ot. IsZO zone ot => Magic' ex st 'Private 'RO m [ZO zone ot]
    , fwd_askActivateAbility :: Object 'OTPlayer -> MagicCont' ex st 'Private 'RW m () ()
    , fwd_askCastSpell :: Object 'OTPlayer -> MagicCont' ex st 'Private 'RW m () ()
    , fwd_askPlayLand :: Object 'OTPlayer -> MagicCont' ex st 'Private 'RW m () ()
    , fwd_caseOf :: forall x a. (x -> Magic' ex st 'Private 'RW m a) -> Case x -> Magic' ex st 'Private 'RW m a
    , fwd_castSpell :: Object 'OTPlayer -> Play OTSpell -> Magic' ex st 'Private 'RW m Legality
    , fwd_controllerOf :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m (Object 'OTPlayer)
    , fwd_doesZoneObjectExist :: forall zone ot. IsZO zone ot => ZO zone ot -> Magic' ex st 'Private 'RO m Bool
    , fwd_enact :: Effect 'OneShot -> Magic' ex st 'Private 'RW m ()
    , fwd_findHandCard :: Object 'OTPlayer -> ZO 'ZHand OTCard -> Magic' ex st 'Private 'RW m (Maybe AnyCard)
    , fwd_findLibraryCard :: Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic' ex st 'Private 'RW m (Maybe AnyCard)
    , fwd_findPermanent :: ZO 'ZBattlefield OTPermanent -> Magic' ex st 'Private 'RO m (Maybe Permanent)
    , fwd_findPlayer :: Object 'OTPlayer -> Magic' ex st 'Private 'RO m (Maybe Player)
    , fwd_gainPriority :: Object 'OTPlayer -> Magic' ex st 'Private 'RW m ()
    , fwd_getActivePlayer :: Magic' ex st 'Public 'RO m (Object 'OTPlayer)
    , fwd_getAlivePlayerCount :: Magic' ex st 'Public 'RO m PlayerCount
    , fwd_getAPNAP :: forall v. Magic' ex st v 'RO m (Stream.Stream (Object 'OTPlayer))
    , fwd_getHasPriority :: Object 'OTPlayer -> Magic' ex st 'Public 'RO m Bool
    , fwd_getPermanent :: ZO 'ZBattlefield OTPermanent -> Magic' ex st 'Private 'RO m Permanent
    , fwd_getPlayer :: Object 'OTPlayer -> Magic' ex st 'Private 'RO m Player
    , fwd_getPlayerWithPriority :: Magic' ex st 'Public 'RO m (Maybe (Object 'OTPlayer))
    , fwd_indexToAbility :: forall zone ot. IsZO zone ot => AbsoluteActivatedAbilityIndex -> Magic' ex st 'Private 'RO m (Maybe (SomeActivatedAbility zone ot))
    , fwd_newObjectId :: Magic' ex st 'Private 'RW m ObjectId
    , fwd_pay :: forall ot. Object 'OTPlayer -> Cost ot -> Magic' ex st 'Private 'RW m Legality
    , fwd_performElections :: forall ot p el x. AndLike (Maybe x) => ZO 'ZStack OT0 -> (el -> Magic' ex st 'Private 'RW m (Maybe x)) -> Elect p el ot -> Magic' ex st 'Private 'RW m (Maybe x)
    , fwd_performStateBasedActions :: Magic' ex st 'Private 'RW m ()
    , fwd_play :: forall ot. Object 'OTPlayer -> Play ot -> Magic' ex st 'Private 'RW m Legality
    , fwd_pushHandCard :: Object 'OTPlayer -> AnyCard -> Magic' ex st 'Private 'RW m (ZO 'ZHand OTCard)
    , fwd_pushLibraryCard :: Object 'OTPlayer -> AnyCard -> Magic' ex st 'Private 'RW m (ZO 'ZLibrary OTCard)
    , fwd_removeHandCard :: Object 'OTPlayer -> ZO 'ZHand OTCard -> Magic' ex st 'Private 'RW m (Maybe AnyCard)
    , fwd_removeLibraryCard :: Object 'OTPlayer -> ZO 'ZLibrary OTCard -> Magic' ex st 'Private 'RW m (Maybe AnyCard)
    , fwd_resolveTopOfStack :: Magic' ex st 'Private 'RW m ()
    , fwd_rewindIllegal :: Magic' ex st 'Private 'RW m Legality -> Magic' ex st 'Private 'RW m Bool
    , fwd_satisfies :: forall zone ot. IsZO zone ot => ZO zone ot -> Requirement zone ot -> Magic' ex st 'Private 'RO m Bool
    , fwd_setPermanent :: ZO 'ZBattlefield OTPermanent -> Maybe Permanent -> Magic' ex st 'Private 'RW m ()
    , fwd_setPlayer :: Object 'OTPlayer -> Player -> Magic' ex st 'Private 'RW m ()
    , fwd_startGame :: Magic' ex st 'Private 'RW m Void
    , fwd_toZO :: forall zone ot. IsZO zone ot => ObjectId -> Magic' ex st 'Private 'RO m (Maybe (ZO zone ot))
    , fwd_zosSatisfying :: forall zone ot. IsZO zone ot => Requirement zone ot -> Magic' ex st 'Private 'RO m [ZO zone ot]
    } ->
    Fwd' ex st m

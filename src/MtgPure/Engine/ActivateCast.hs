{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.ActivateCast (
  activateAbilityImpl,
  castSpellImpl,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Util (AndLike (..))
import safe Data.Functor ((<&>))
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Engine.Fwd.Wrap (newObjectId, pay, performElections)
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (modify)
import safe MtgPure.Engine.Prompt (ActivateAbility (..), CastSpell (..))
import safe MtgPure.Engine.State (
  AnyElected (..),
  Elected (..),
  GameState (..),
  Magic,
  Pending,
  PendingReady (..),
  StackEntry (..),
  electedObject_controller,
  electedObject_cost,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object (IsObjectType (..), OT, OT0, OT1, Object, ObjectType (..))
import safe MtgPure.Model.ObjectId (ObjectId)
import safe MtgPure.Model.ObjectN (ObjectN (O1))
import safe MtgPure.Model.ObjectType.Card (WCard (..))
import safe MtgPure.Model.ObjectType.Kind (OTInstant, OTSorcery)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  Card (..),
  CardTypeDef (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectPrePost,
  Some (..),
  SomeTerm (..),
  WithThis (..),
 )
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.ToObjectN (ToObject6' (..))
import safe MtgPure.Model.ToObjectN.Classes (ToObject2' (..))
import safe MtgPure.Model.Tribal (
  IsMaybeTribal (..),
  IsTribal,
  SMaybeTribal (..),
  Tribal (..),
 )
import safe MtgPure.Model.Variable (Variable (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (ZO, ZoneObject (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0)

activateAbilityImpl :: forall m. Monad m => Object 'OTPlayer -> ActivateAbility -> Magic 'Private 'RW m Legality
activateAbilityImpl oPlayer = \case
  ActivateAbility _wit oThis ability -> go oThis ability
 where
  go :: forall zone ot. ZO zone ot -> ActivatedAbility zone ot -> Magic 'Private 'RW m Legality
  go oThis = \case
    Ability inCost inEffect -> do
      let isThisInCorrectZone = True -- TODO
          isController = True -- TODO
      case (isThisInCorrectZone, isController) of
        (False, _) -> pure Illegal -- TODO prompt complaint
        (_, False) -> pure Illegal -- TODO prompt complaint
        (True, True) -> do
          abilityId <- newObjectId
          let zoAbility = toZO0 @ 'ZStack abilityId

              toMaybe = \case
                Legal -> Just ()
                Illegal -> Nothing

              cont cost effect =
                fmap toMaybe $
                  payElectedAndPutOnStack @ 'Nothing $
                    ElectedActivatedAbility oPlayer oThis cost effect

          playPendingStackItem @ 'Nothing zoAbility inCost inEffect cont <&> \case
            Nothing -> Illegal
            Just{} -> Legal

playPendingStackItem ::
  forall mTribal m ot x.
  (IsMaybeTribal mTribal, AndLike x, AndLike (Maybe x)) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  ElectPrePost (Cost ot) ot ->
  ElectPrePost (Effect 'OneShot) ot ->
  (Pending (Cost ot) ot -> Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingStackItem zoStack inCost inEffect goCont = performElections @mTribal andM zoStack (goCost . Pending) inCost
 where
  goCost :: Pending (Cost ot) ot -> Magic 'Private 'RW m (Maybe x)
  goCost cost =
    let goElectEffect :: ElectPrePost (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)
        goElectEffect = performElections @mTribal andM zoStack (goEffect . Pending)

        goEffect :: Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)
        goEffect = goCont cost
     in goElectEffect inEffect

data CastEnv (ot :: Type) :: Type where
  CastEnv ::
    { castEnv_ElectedTribal ::
        Object 'OTPlayer ->
        Card ot ->
        CardTypeDef 'Tribal ot ->
        PendingReady 'Pre (Cost ot) ot ->
        PendingReady 'Pre (Effect 'OneShot) ot ->
        Elected 'Pre 'Pre ( 'Just 'Tribal) ot
    , castEnv_ElectedNonTribal ::
        Object 'OTPlayer ->
        Card ot ->
        CardTypeDef 'NonTribal ot ->
        PendingReady 'Pre (Cost ot) ot ->
        PendingReady 'Pre (Effect 'OneShot) ot ->
        Elected 'Pre 'Pre ( 'Just 'NonTribal) ot
    , castEnv_effect :: CardTypeDef 'NonTribal ot -> ElectPrePost (Effect 'OneShot) ot
    , castEnv_cost :: CardTypeDef 'NonTribal ot -> ElectPrePost (Cost ot) ot
    } ->
    CastEnv ot

instantCastEnv :: CastEnv OTInstant
instantCastEnv =
  CastEnv
    { castEnv_ElectedTribal = ElectedInstant
    , castEnv_ElectedNonTribal = ElectedInstant
    , castEnv_effect = instant_effect
    , castEnv_cost = instant_cost
    }

sorceryCastEnv :: CastEnv OTSorcery
sorceryCastEnv =
  CastEnv
    { castEnv_ElectedTribal = ElectedSorcery
    , castEnv_ElectedNonTribal = ElectedSorcery
    , castEnv_effect = sorcery_effect
    , castEnv_cost = sorcery_cost
    }

castSpellImpl :: forall m. Monad m => Object 'OTPlayer -> CastSpell -> Magic 'Private 'RW m Legality
castSpellImpl oCaster (CastSpell someCard) = case someCard of
  Some6a (SomeArtifact card) -> undefined card
  Some6b (SomeCreature card) -> undefined card
  Some6c (SomeEnchantment card) -> undefined card
  Some6d (SomeInstant card@Card{}) -> castSpell' @ 'NonTribal oCaster card instantCastEnv
  Some6d (SomeInstant card@TribalCard{}) -> castSpell' @ 'Tribal oCaster card instantCastEnv
  Some6e (SomePlaneswalker card) -> undefined card
  Some6f (SomeSorcery card@Card{}) -> castSpell' @ 'NonTribal oCaster card sorceryCastEnv
  Some6f (SomeSorcery card@TribalCard{}) -> castSpell' @ 'Tribal oCaster card sorceryCastEnv
  Some6ab (SomeArtifactCreature card) -> undefined card
  Some6bc (SomeEnchantmentCreature card) -> undefined card

castSpell' ::
  forall tribal otk ot m.
  (ot ~ OT otk, Monad m) =>
  IsTribal tribal =>
  Object 'OTPlayer ->
  Card ot ->
  CastEnv ot ->
  Magic 'Private 'RW m Legality
castSpell' oCaster card env = case card of
  Card _name wCard (T1 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId)
  Card _name wCard (T2 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId, viewThis thisId)
  TribalCard _name wCard (T1 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId)
  TribalCard _name wCard (T2 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId, viewThis thisId)
 where
  viewThis :: (IsZone zone, IsObjectType a) => ObjectId -> ZO zone (OT1 a)
  viewThis = ZO singZone . O1 . idToObject

  goLegality ::
    forall ret tribal' this.
    IsTribal tribal' =>
    ret ~ Maybe (Elected 'Pre 'Pre ( 'Just tribal') ot) =>
    WCard ot ->
    (this -> Elect 'Pre (CardTypeDef tribal' ot) ot) ->
    this ->
    Magic 'Private 'RW m Legality
  goLegality wCard thisToElectDef zoThis = do
    mElected <- goMaybe wCard thisToElectDef zoThis
    pure $ case mElected of
      Nothing -> Illegal
      Just () -> Legal

  goMaybe ::
    forall ret tribal' this.
    IsTribal tribal' =>
    ret ~ Maybe () =>
    WCard ot ->
    (this -> Elect 'Pre (CardTypeDef tribal' ot) ot) ->
    this ->
    Magic 'Private 'RW m ret
  goMaybe wCard thisToElectDef zoThis = do
    spellId <- newObjectId
    let zoSpell = toZO0 @ 'ZStack spellId

        goElectDef :: Elect 'Pre (CardTypeDef tribal' ot) ot -> Magic 'Private 'RW m ret
        goElectDef = performElections @( 'Just tribal) andM zoSpell goDef

        goDef :: CardTypeDef tribal' ot -> Magic 'Private 'RW m ret
        goDef def = goDef' def def
         where
          goDef' topDef bottomDef = case bottomDef of
            ArtifactLandDef{} -> undefined -- TODO: Not a spell
            LandDef{} -> undefined -- TODO: Not a spell
            --
            ArtifactDef{} -> goDefNonTribal topDef bottomDef
            ArtifactCreatureDef{} -> goDefNonTribal topDef bottomDef
            CreatureDef{} -> goDefNonTribal topDef bottomDef
            EnchantmentDef{} -> goDefNonTribal topDef bottomDef
            EnchantmentCreatureDef{} -> goDefNonTribal topDef bottomDef
            PlaneswalkerDef{} -> goDefNonTribal topDef bottomDef
            --
            InstantDef{} -> goDefNonTribal topDef bottomDef
            SorceryDef{} -> goDefNonTribal topDef bottomDef
            --
            TribalDef{} -> goDefTribal topDef bottomDef
            --
            VariableDef varToDef -> do
              let var = ReifiedVariable undefined undefined
              goDef' topDef $ varToDef var

        goDefNonTribal :: CardTypeDef 'NonTribal ot -> CardTypeDef 'NonTribal ot -> Magic 'Private 'RW m ret
        goDefNonTribal topDef def = playPendingStackItem @( 'Just 'NonTribal) zoSpell (castEnv_cost env def) (castEnv_effect env def) $ \cost effect -> do
          case wCard of
            WCardSorcery -> do
              legality <- payElectedAndPutOnStack @( 'Just 'NonTribal) $ castEnv_ElectedNonTribal env oCaster card topDef cost effect
              pure $ case legality of
                Illegal -> Nothing
                Legal -> Just ()
            _ -> undefined

        goDefTribal :: CardTypeDef 'Tribal ot -> CardTypeDef 'Tribal ot -> Magic 'Private 'RW m ret
        goDefTribal topDef (TribalDef _ _ def) = playPendingStackItem @( 'Just 'Tribal) zoSpell (castEnv_cost env def) (castEnv_effect env def) $ \cost effect -> do
          case wCard of
            WCardSorcery -> do
              legality <- payElectedAndPutOnStack @( 'Just 'Tribal) $ castEnv_ElectedTribal env oCaster card topDef cost effect
              pure $ case legality of
                Illegal -> Nothing
                Legal -> Just ()
            _ -> undefined

    goElectDef $ thisToElectDef zoThis

setElectedObject_cost ::
  Elected pCost pEffect tribal ot ->
  PendingReady pCost' (Cost ot) ot ->
  Elected pCost' pEffect tribal ot
setElectedObject_cost elected cost = case elected of
  ElectedActivatedAbility{} -> elected{electedActivatedAbility_cost = cost}
  ElectedInstant{} -> elected{electedInstant_cost = cost}
  ElectedSorcery{} -> elected{electedSorcery_cost = cost}

class IsMaybeTribal mTribal => PayElected (mTribal :: Maybe Tribal) (ot :: Type) where
  payElectedAndPutOnStack :: Monad m => Elected 'Pre 'Pre mTribal ot -> Magic 'Private 'RW m Legality

instance PayElected 'Nothing ot where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackAbility . ZO SZStack . toObject2' . idToObject @ 'OTActivatedAbility

instance IsTribal tribal => PayElected ( 'Just tribal) OTInstant where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTInstant

instance IsTribal tribal => PayElected ( 'Just tribal) OTSorcery where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTSorcery

payElectedAndPutOnStack' ::
  forall mTribal ot m.
  (IsMaybeTribal mTribal, Monad m) =>
  (ObjectId -> StackObject) ->
  Elected 'Pre 'Pre mTribal ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStack' idToStackObject elected = do
  stackId <- newObjectId
  let stackItem = idToStackObject stackId

      goElectCost :: Monad m => Elect 'Post (Cost ot) ot -> Magic 'Private 'RW m Legality
      goElectCost elect =
        performElections @mTribal andM (toZO0 stackId) goCost elect <&> \case
          Nothing -> Illegal
          Just () -> Legal

      goCost :: Monad m => Cost ot -> Magic 'Private 'RW m (Maybe ())
      goCost cost =
        pay (electedObject_controller elected) cost >>= \case
          Illegal -> pure Nothing
          Legal -> do
            let entry =
                  StackEntry
                    { stackEntryTargets = []
                    , stackEntryElected = AnyElected $ setElectedObject_cost elected $ Ready cost
                    }
            modify $ \st -> st{magicStack = Stack $ stackItem : unStack (magicStack st)}
            modify $ \st -> case singMaybeTribal @mTribal of
              SNothingTribal -> st{magicStackEntryAbilityMap = Map.insert (toZO0 stackId) entry $ magicStackEntryAbilityMap st}
              SJustNonTribal -> st{magicStackEntryNonTribalMap = Map.insert (toZO0 stackId) entry $ magicStackEntryNonTribalMap st}
              SJustTribal -> st{magicStackEntryTribalMap = Map.insert (toZO0 stackId) entry $ magicStackEntryTribalMap st}
            pure $ Just ()
  legality <- goElectCost $ unPending $ electedObject_cost elected
  case legality of
    Legal -> pure Legal
    Illegal -> pure Illegal -- TODO: GC stack object stuff if Illegal

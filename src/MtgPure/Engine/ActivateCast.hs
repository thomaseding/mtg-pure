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
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Engine.Fwd.Wrap (newObjectId, pay, performElections)
import safe MtgPure.Engine.Legality (Legality (..), legalityToMaybe, maybeToLegality)
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
import safe MtgPure.Model.Object (IsObjectType (..), OT0, OT1, Object, ObjectType (..))
import safe MtgPure.Model.ObjectId (ObjectId)
import safe MtgPure.Model.ObjectN (ObjectN (O1))
import safe MtgPure.Model.ObjectType.Kind (
  OTInstant,
  OTSorcery,
 )
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  Card (..),
  CardFacet (..),
  Cost (..),
  Effect (..),
  Elect (..),
  IsSpecificCard (singSpecificCard),
  Some (..),
  SomeTerm (..),
  SpecificCard (..),
  WithThis (..),
  WithThisActivated,
  WithThisCard,
 )
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.ToObjectN.Classes (ToObject2' (..), ToObject6' (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO, ZoneObject (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0)

type Legality' = Maybe ()

data CastMeta (ot :: Type) :: Type where
  CastMeta ::
    { castMeta_Elected ::
        Object 'OTPlayer ->
        Card ot ->
        CardFacet ot ->
        Cost ot ->
        PendingReady 'Pre (Effect 'OneShot) ot ->
        Elected 'Pre ot
    , castMeta_effect :: CardFacet ot -> Elect 'Post (Effect 'OneShot) ot
    , castMeta_cost :: CardFacet ot -> Cost ot
    } ->
    CastMeta ot

instantCastMeta :: CastMeta OTInstant
instantCastMeta =
  CastMeta
    { castMeta_Elected = ElectedInstant
    , castMeta_effect = instant_effect
    , castMeta_cost = instant_cost
    }

sorceryCastMeta :: CastMeta OTSorcery
sorceryCastMeta =
  CastMeta
    { castMeta_Elected = ElectedSorcery
    , castMeta_effect = sorcery_effect
    , castMeta_cost = sorcery_cost
    }

castSpellImpl :: forall m. Monad m => Object 'OTPlayer -> CastSpell -> Magic 'Private 'RW m Legality
castSpellImpl oCaster (CastSpell someCard) = case someCard of
  Some6a (SomeArtifact card) -> undefined card
  Some6b (SomeCreature card) -> undefined card
  Some6c (SomeEnchantment card) -> undefined card
  Some6d (SomeInstant card) -> castSpellImpl' oCaster card instantCastMeta
  Some6e (SomePlaneswalker card) -> undefined card
  Some6f (SomeSorcery card) -> castSpellImpl' oCaster card sorceryCastMeta
  Some6ab (SomeArtifactCreature card) -> undefined card
  Some6bc (SomeEnchantmentCreature card) -> undefined card

lensedThis :: (IsZone zone, IsObjectType a) => ObjectId -> ZO zone (OT1 a)
lensedThis = ZO singZone . O1 . idToObject

castSpellImpl' ::
  forall ot m.
  Monad m =>
  Object 'OTPlayer ->
  Card ot ->
  CastMeta ot ->
  Magic 'Private 'RW m Legality
castSpellImpl' oCaster card meta = case card of
  Card _name withThis -> goWithThis withThis
 where
  goWithThis :: IsSpecificCard ot => WithThisCard ot -> Magic 'Private 'RW m Legality
  goWithThis = \case
    T1 thisToElectFacet -> do
      thisId <- newObjectId
      goLegality thisToElectFacet (lensedThis thisId)
    T2 thisToElectFacet -> do
      thisId <- newObjectId
      goLegality thisToElectFacet (lensedThis thisId, lensedThis thisId)

  goLegality ::
    forall this.
    IsSpecificCard ot =>
    (this -> Elect 'Pre (CardFacet ot) ot) ->
    this ->
    Magic 'Private 'RW m Legality
  goLegality thisToElectFacet this = do
    spellId <- newObjectId
    let zoSpell = toZO0 @ 'ZStack spellId

        goElectFacet :: Elect 'Pre (CardFacet ot) ot -> Magic 'Private 'RW m Legality
        goElectFacet = fmap maybeToLegality . performElections andM zoSpell goFacet

        goFacet :: CardFacet ot -> Magic 'Private 'RW m Legality'
        goFacet facet = case facet of
          ArtifactLandFacet{} -> undefined -- TODO: Not a spell
          LandFacet{} -> undefined -- TODO: Not a spell
          --
          ArtifactFacet{} -> goFacet' facet
          ArtifactCreatureFacet{} -> goFacet' facet
          CreatureFacet{} -> goFacet' facet
          EnchantmentFacet{} -> goFacet' facet
          EnchantmentCreatureFacet{} -> goFacet' facet
          PlaneswalkerFacet{} -> goFacet' facet
          --
          InstantFacet{} -> goFacet' facet
          SorceryFacet{} -> goFacet' facet

        goFacet' :: CardFacet ot -> Magic 'Private 'RW m Legality'
        goFacet' facet = playPendingStackItem zoSpell (castMeta_cost meta facet) (castMeta_effect meta facet) $ \cost effect -> do
          case singSpecificCard @ot of
            SorceryCard ->
              legalityToMaybe <$> do
                payElectedAndPutOnStack @ 'Cast $ castMeta_Elected meta oCaster card facet cost effect
            _ -> undefined

    goElectFacet $ thisToElectFacet this

-- TODO: Generalize for TriggeredAbility as well. Prolly make an AbilityMeta type that is analogous to CastMeta.
activateAbilityImpl :: forall m. Monad m => Object 'OTPlayer -> ActivateAbility -> Magic 'Private 'RW m Legality
activateAbilityImpl oPlayer = \case
  ActivateAbility _wit zoThis withThisAbility -> goWithThis zoThis withThisAbility
 where
  goWithThis :: forall zone ot. IsZO zone ot => ZO zone ot -> WithThisActivated zone ot -> Magic 'Private 'RW m Legality
  goWithThis zoThis = \case
    T1 thisToElectActivated -> do
      thisId <- newObjectId
      goLegality thisToElectActivated (lensedThis thisId)
    T2 thisToElectActivated -> do
      thisId <- newObjectId
      goLegality thisToElectActivated (lensedThis thisId, lensedThis thisId)
   where
    goLegality ::
      forall this.
      (this -> Elect 'Pre (ActivatedAbility zone ot) ot) ->
      this ->
      Magic 'Private 'RW m Legality
    goLegality thisToElectAbility this = do
      abilityId <- newObjectId
      let zoAbility = toZO0 @ 'ZStack abilityId

          goElectActivated :: Elect 'Pre (ActivatedAbility zone ot) ot -> Magic 'Private 'RW m Legality
          goElectActivated = fmap maybeToLegality . performElections andM zoAbility goActivated

          goActivated :: ActivatedAbility zone ot -> Magic 'Private 'RW m Legality'
          goActivated activated =
            legalityToMaybe <$> do
              let isThisInCorrectZone = True -- TODO
                  isController = True -- TODO
              case (isThisInCorrectZone, isController) of
                (False, _) -> pure Illegal -- TODO prompt complaint
                (_, False) -> pure Illegal -- TODO prompt complaint
                (True, True) -> do
                  fmap maybeToLegality $
                    playPendingStackItem zoAbility (activated_cost activated) (activated_effect activated) $ \cost effect ->
                      fmap legalityToMaybe $
                        payElectedAndPutOnStack @ 'Activate $
                          ElectedActivatedAbility oPlayer zoThis cost effect

      goElectActivated $ thisToElectAbility this

playPendingStackItem ::
  forall m ot x.
  (AndLike x, AndLike (Maybe x)) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  Cost ot ->
  Elect 'Post (Effect 'OneShot) ot ->
  (Cost ot -> Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingStackItem _zoStack cost electEffect cont = do
  cont cost $ Pending electEffect

setElectedObject_cost ::
  Elected pEffect ot ->
  Cost ot ->
  Elected pEffect ot
setElectedObject_cost elected cost = case elected of
  ElectedActivatedAbility{} -> elected{electedActivatedAbility_cost = cost}
  ElectedInstant{} -> elected{electedInstant_cost = cost}
  ElectedSorcery{} -> elected{electedSorcery_cost = cost}

data ActivateCast = Activate | Cast

class PayElected (ac :: ActivateCast) (ot :: Type) where
  payElectedAndPutOnStack :: Monad m => Elected 'Pre ot -> Magic 'Private 'RW m Legality

instance PayElected 'Activate ot where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackAbility . ZO SZStack . toObject2' . idToObject @ 'OTActivatedAbility

instance PayElected 'Cast OTInstant where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTInstant

instance PayElected 'Cast OTSorcery where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTSorcery

payElectedAndPutOnStack' ::
  forall ot m.
  Monad m =>
  (ObjectId -> StackObject) ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStack' idToStackObject elected = do
  stackId <- newObjectId
  let stackItem = idToStackObject stackId

      goCost :: Monad m => Cost ot -> Magic 'Private 'RW m Legality
      goCost cost =
        pay (electedObject_controller elected) cost >>= \case
          Illegal -> pure Illegal
          Legal -> do
            let entry =
                  StackEntry
                    { stackEntryTargets = []
                    , stackEntryElected = AnyElected $ setElectedObject_cost elected cost
                    }
            modify $ \st -> st{magicStack = Stack $ stackItem : unStack (magicStack st)}
            modify $ \st -> st{magicStackEntryMap = Map.insert (toZO0 stackId) entry $ magicStackEntryMap st}
            pure Legal

  legality <- goCost $ electedObject_cost elected
  case legality of
    Legal -> pure Legal
    Illegal -> pure Illegal -- TODO: GC stack object stuff if Illegal

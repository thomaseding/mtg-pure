{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Recursive.Eq where

import safe qualified Control.Monad as M
import safe qualified Control.Monad.State.Strict as State
import safe Data.Inst (Inst1, Inst2)
import safe qualified Data.List as List
import safe Data.Maybe (isJust)
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaPool (ManaPool)
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectId (ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectN.Type (OPermanent, OPlayer)
import safe MtgPure.Model.ObjectType (OT, ObjectType (..))
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import safe MtgPure.Model.ObjectType.NonCreatureCard (WNonCreatureCard)
import safe MtgPure.Model.ObjectType.Permanent (IsPermanentType, WPermanent (..))
import safe MtgPure.Model.Recursive
  ( Ability,
    Card,
    CardTypeDef,
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    EventListener (..),
    Requirement (..),
    SetCard,
    SetToken,
    StaticAbility (..),
    Token (..),
    TriggeredAbility (..),
    TypeableOT,
    WithObject (..),
  )
import safe MtgPure.Model.Selection (Selection (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToObjectN (ToObject1 (..))
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))

instance Eq (Effect e) where
  (==) x y = runEnvM $ eqEffect x y

type EnvM = State.State Env

data Env = Env
  { nextRawId :: Int,
    envThing :: ()
  }

mkEnv :: Env
mkEnv =
  Env
    { nextRawId = 0,
      envThing = ()
    }

runEnvM :: EnvM a -> a
runEnvM m = State.evalState m mkEnv

andM :: Monad m => [m Bool] -> m Bool
andM = List.foldl' (M.liftM2 (&&)) $ pure True

listM :: (a -> a -> EnvM Bool) -> [a] -> [a] -> EnvM Bool
listM eqM xs ys = case length xs == length ys of
  True -> and <$> M.zipWithM eqM xs ys
  False -> pure False

newObject :: forall a. IsObjectType a => EnvM (Object a)
newObject = do
  raw <- State.gets nextRawId
  let i = ObjectId raw
      obj = idToObject @a i
  State.modify' $ \st -> st {nextRawId = nextRawId st + 1}
  pure obj

newObjectN ::
  forall a ot.
  -- NOTE: The `Typeable (ObjectN ot)` not needed, but keep it for now to prove it's possible
  (Typeable (ObjectN ot), IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  EnvM (ObjectN ot)
newObjectN make = do
  obj <- newObject @a
  let objN = make obj
  pure objN

----------------------------------------

eqAbilility :: Ability ot -> Ability ot -> EnvM Bool
eqAbilility = undefined

eqCard :: Card ot -> Card ot -> EnvM Bool
eqCard = undefined

eqCardTypeDef :: CardTypeDef t ot -> CardTypeDef t ot -> EnvM Bool
eqCardTypeDef = undefined

eqColors :: Colors -> Colors -> EnvM Bool
eqColors = undefined

eqCondition :: Condition -> Condition -> EnvM Bool
eqCondition = \case
  CAnd conds1 -> \case
    CAnd conds2 -> eqConditions conds1 conds2
    _ -> pure False
  COr conds1 -> \case
    COr conds2 -> eqConditions conds1 conds2
    _ -> pure False
  Satisfies any1 obj1 reqs1 -> \case
    Satisfies any2 obj2 reqs2 ->
      let go :: forall k ot. TypeableOT k ot => WAny ot -> ObjectN ot -> [Requirement ot] -> EnvM Bool
          go any1 obj1 reqs1 = case cast (any2, obj2, reqs2) of
            Nothing -> pure False
            Just (any2, obj2, reqs2) -> andM [eqWAny any1 any2, eqObjectN obj1 obj2, eqRequirements reqs1 reqs2]
       in go any1 obj1 reqs1
    _ -> pure False

eqConditions :: [Condition] -> [Condition] -> EnvM Bool
eqConditions = listM eqCondition

eqCost :: Cost ot -> Cost ot -> EnvM Bool
eqCost = \case
  AndCosts costs1 -> \case
    AndCosts costs2 -> eqCosts costs1 costs2
    _ -> pure False
  DiscardRandomCost amount1 -> \case
    DiscardRandomCost amount2 -> pure $ amount1 == amount2
    _ -> pure False
  LoyaltyCost amount1 -> \case
    LoyaltyCost amount2 -> pure $ amount1 == amount2
    _ -> pure False
  ManaCost mana1 -> \case
    ManaCost mana2 -> eqManaCost mana1 mana2
    _ -> pure False
  OrCosts costs1 -> \case
    OrCosts costs2 -> eqCosts costs1 costs2
    _ -> pure False
  PayLife amount1 -> \case
    PayLife amount2 -> pure $ amount1 == amount2
    _ -> pure False
  SacrificeCost perm1 reqs1 -> \case
    SacrificeCost perm2 reqs2 -> andM [eqWPermanent perm1 perm2, eqRequirements reqs1 reqs2]
    _ -> pure False
  TapCost obj1 -> \case
    TapCost obj2 -> eqOPermanent obj1 obj2
    _ -> pure False

eqCosts :: [Cost ot] -> [Cost ot] -> EnvM Bool
eqCosts = listM eqCost

eqEffect :: Effect e -> Effect e -> EnvM Bool
eqEffect = \case
  AddMana mana1 player1 -> \case
    AddMana mana2 player2 ->
      andM [eqManaPool mana1 mana2, eqOPlayer player1 player2]
    _ -> pure False
  AddToBattlefield wPerm1 player1 token1 -> \case
    AddToBattlefield wPerm2 player2 token2 ->
      let go :: forall k ot. TypeableOT k ot => WPermanent ot -> Token ot -> EnvM Bool
          go wPerm1 token1 = case cast (wPerm2, token2) of
            Nothing -> pure False
            Just (wPerm2, token2) -> andM [eqWPermanent wPerm1 wPerm2, eqOPlayer player1 player2, eqToken token1 token2]
       in go wPerm1 token1
    _ -> pure False
  _ -> undefined

eqElectE :: Elect e ot -> Elect e ot -> EnvM Bool
eqElectE = \case
  A sel1 with1 -> \case
    A sel2 with2 ->
      andM [eqSelection sel1 sel2, eqWithObjectElectE with1 with2]
    _ -> pure False
  ActivePlayer cont1 -> \case
    ActivePlayer cont2 -> do
      obj <- newObjectN @'OTPlayer toObject1
      eqElectE (cont1 obj) (cont2 obj)
    _ -> pure False
  All with1 -> \case
    All with2 -> eqWithObjectElectE with1 with2
    _ -> pure False
  _ -> undefined

eqElectEventListener :: Elect (EventListener ot) ot -> Elect (EventListener ot) ot -> EnvM Bool
eqElectEventListener = \case
  A sel1 with1 -> \case
    A sel2 with2 ->
      andM [eqSelection sel1 sel2, eqWithObjectElectE with1 with2]
    _ -> pure False
  ActivePlayer cont1 -> \case
    ActivePlayer cont2 -> do
      obj <- newObjectN @'OTPlayer toObject1
      eqElectEventListener (cont1 obj) (cont2 obj)
    _ -> pure False
  All with1 -> \case
    All with2 -> eqWithObjectElectE with1 with2
    _ -> pure False
  _ -> undefined

eqEventListener :: EventListener ot -> EventListener ot -> EnvM Bool
eqEventListener = \case
  Events listeners1 -> \case
    Events listeners2 -> eqEventListeners listeners1 listeners2
    _ -> pure False
  SpellIsCast with1 -> \case
    SpellIsCast with2 -> eqWithObjectElectE with1 with2
    _ -> pure False
  TimePoint time1 elect1 -> \case
    TimePoint time2 elect2 ->
      let go :: forall p. Typeable p => TimePoint p -> EnvM Bool
          go time1 = case cast time2 of
            Nothing -> pure False
            Just time2 -> andM [eqTimePoint time1 time2, eqElectE elect1 elect2]
       in go time1
    _ -> pure False

eqEventListeners :: [EventListener ot] -> [EventListener ot] -> EnvM Bool
eqEventListeners = listM eqEventListener

eqManaCost :: ManaCost -> ManaCost -> EnvM Bool
eqManaCost = undefined

eqManaPool :: ManaPool -> ManaPool -> EnvM Bool
eqManaPool = undefined

eqObjectN :: VisitObjectN ot => ObjectN ot -> ObjectN ot -> EnvM Bool
eqObjectN objN1 objN2 = do
  let i1 = visitObjectN' objectToId objN1
      i2 = visitObjectN' objectToId objN2
  pure $ i1 == i2

eqOPermanent :: OPermanent -> OPermanent -> EnvM Bool
eqOPermanent = eqObjectN

eqOPlayer :: OPlayer -> OPlayer -> EnvM Bool
eqOPlayer = eqObjectN

eqRequirement :: Requirement ot -> Requirement ot -> EnvM Bool
eqRequirement = \case
  ControlledBy player1 -> \case
    ControlledBy player2 -> eqOPlayer player1 player2
    _ -> pure False
  HasAbility ability1 -> \case
    HasAbility ability2 -> eqAbilility ability1 ability2
    _ -> pure False
  HasBasicLandType type1 -> \case
    HasBasicLandType type2 -> pure $ type1 == type2
    _ -> pure False
  Impossible -> \case
    Impossible -> pure True
    _ -> pure False
  Is any1 obj1 -> \case
    Is any2 obj2 -> andM [eqWAny any1 any2, eqObjectN obj1 obj2]
    _ -> pure False
  Not req1 -> \case
    Not req2 -> eqRequirement req1 req2
    _ -> pure False
  OfColors colors1 -> \case
    OfColors colors2 -> eqColors colors1 colors2
    _ -> pure False
  OwnedBy player1 -> \case
    OwnedBy player2 -> eqOPlayer player1 player2
    _ -> pure False
  PlayerPays cost1 -> \case
    PlayerPays cost2 -> eqCost cost1 cost2
    _ -> pure False
  RAnd reqs1 -> \case
    RAnd reqs2 -> eqRequirements reqs1 reqs2
    _ -> pure False
  ROr reqs1 -> \case
    ROr reqs2 -> eqRequirements reqs1 reqs2
    _ -> pure False
  Tapped perm1 -> \case
    Tapped perm2 -> eqWPermanent perm1 perm2
    _ -> pure False
  R2 reqsA1 reqsB1 -> \case
    R2 reqsA2 reqsB2 ->
      andM
        [ eqRequirements reqsA1 reqsA2,
          eqRequirements reqsB1 reqsB2
        ]
    _ -> pure False
  R3 reqsA1 reqsB1 reqsC1 -> \case
    R3 reqsA2 reqsB2 reqsC2 ->
      andM
        [ eqRequirements reqsA1 reqsA2,
          eqRequirements reqsB1 reqsB2,
          eqRequirements reqsC1 reqsC2
        ]
    _ -> pure False
  R4 reqsA1 reqsB1 reqsC1 reqsD1 -> \case
    R4 reqsA2 reqsB2 reqsC2 reqsD2 ->
      andM
        [ eqRequirements reqsA1 reqsA2,
          eqRequirements reqsB1 reqsB2,
          eqRequirements reqsC1 reqsC2,
          eqRequirements reqsD1 reqsD2
        ]
    _ -> pure False
  R5 reqsA1 reqsB1 reqsC1 reqsD1 reqsE1 -> \case
    R5 reqsA2 reqsB2 reqsC2 reqsD2 reqsE2 ->
      andM
        [ eqRequirements reqsA1 reqsA2,
          eqRequirements reqsB1 reqsB2,
          eqRequirements reqsC1 reqsC2,
          eqRequirements reqsD1 reqsD2,
          eqRequirements reqsE1 reqsE2
        ]
    _ -> pure False

eqRequirements :: [Requirement ot] -> [Requirement ot] -> EnvM Bool
eqRequirements = listM eqRequirement

eqSelection :: Selection -> Selection -> EnvM Bool
eqSelection = \case
  Choose player1 -> \case
    Choose player2 -> eqOPlayer player1 player2
    _ -> pure False
  Target player1 -> \case
    Target player2 -> eqOPlayer player1 player2
    _ -> pure False
  Random -> \case
    Random -> pure True
    _ -> pure False

eqSetCard :: SetCard ot -> SetCard ot -> EnvM Bool
eqSetCard = undefined

eqSetToken :: SetToken ot -> SetToken ot -> EnvM Bool
eqSetToken = undefined

eqStaticAbility :: StaticAbility ot -> StaticAbility ot -> EnvM Bool
eqStaticAbility = \case
  As with1 -> \case
    As with2 -> eqWithObjectEventListener with1 with2
    _ -> pure False
  _ -> undefined

eqTimePoint :: TimePoint p -> TimePoint p -> EnvM Bool
eqTimePoint = undefined

eqToken :: Token ot -> Token ot -> EnvM Bool
eqToken = undefined

eqTriggeredAbility :: forall k ot. TypeableOT k ot => TriggeredAbility ot -> TriggeredAbility ot -> EnvM Bool
eqTriggeredAbility = \case
  When listener1 -> \case
    When listener2 -> eqElectE @(EventListener ot) @ot listener1 listener2

-- Can't figure out how to DRY this with `eqWithObject*` 😢
eqWithObjectElectE :: forall e ot. WithObject (Elect e) ot -> WithObject (Elect e) ot -> EnvM Bool
eqWithObjectElectE = \case
  O1 reqs1 cont1 -> \case
    O1 reqs2 cont2 ->
      let go :: forall a. Inst1 IsObjectType a => [Requirement '(OT, a)] -> (ObjectN '(OT, a) -> Elect e ot) -> EnvM Bool
          go reqs1 cont1 = case cast (reqs2, cont2) of
            Nothing -> pure False
            Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a O cont1 cont2]
       in go reqs1 cont1
    _ -> pure False
  O2 reqs1 cont1 -> \case
    O2 reqs2 cont2 ->
      let go :: forall a b. Inst2 IsObjectType a b => [Requirement '(OT, a, b)] -> (ObjectN '(OT, a, b) -> Elect e ot) -> EnvM Bool
          go reqs1 cont1 = case cast (reqs2, cont2) of
            Nothing -> pure False
            Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a O2a cont1 cont2]
       in go reqs1 cont1
    _ -> pure False
  _ -> undefined
  where
    withObjectCont ::
      forall (a :: ObjectType) k ot' ot e.
      (IsObjectType a, TypeableOT k ot') =>
      (Object a -> ObjectN ot') ->
      (ObjectN ot' -> Elect e ot) ->
      (ObjectN ot' -> Elect e ot) ->
      EnvM Bool
    withObjectCont cons cont1 cont2 = do
      objN <- newObjectN @a cons
      eqElectE (cont1 objN) (cont2 objN)

-- Can't figure out how to DRY this with `eqWithObject*` 😢
eqWithObjectEventListener :: forall ot. WithObject EventListener ot -> WithObject EventListener ot -> EnvM Bool
eqWithObjectEventListener = \case
  O1 reqs1 cont1 -> \case
    O1 reqs2 cont2 ->
      let go :: forall a. Inst1 IsObjectType a => [Requirement '(OT, a)] -> (ObjectN '(OT, a) -> EventListener ot) -> EnvM Bool
          go reqs1 cont1 = case cast (reqs2, cont2) of
            Nothing -> pure False
            Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a O cont1 cont2]
       in go reqs1 cont1
    _ -> pure False
  O2 reqs1 cont1 -> \case
    O2 reqs2 cont2 ->
      let go :: forall a b. Inst2 IsObjectType a b => [Requirement '(OT, a, b)] -> (ObjectN '(OT, a, b) -> EventListener ot) -> EnvM Bool
          go reqs1 cont1 = case cast (reqs2, cont2) of
            Nothing -> pure False
            Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a O2a cont1 cont2]
       in go reqs1 cont1
    _ -> pure False
  _ -> undefined
  where
    withObjectCont ::
      forall (a :: ObjectType) k ot' ot.
      (IsObjectType a, TypeableOT k ot') =>
      (Object a -> ObjectN ot') ->
      (ObjectN ot' -> EventListener ot) ->
      (ObjectN ot' -> EventListener ot) ->
      EnvM Bool
    withObjectCont cons cont1 cont2 = do
      objN <- newObjectN @a cons
      eqEventListener (cont1 objN) (cont2 objN)

eqWAny :: WAny ot -> WAny ot -> EnvM Bool
eqWAny = \case
  WAnyInstant -> \case
    WAnyInstant -> pure True
    WAnyPermanent {} -> pure False
  WAnySorcery -> \case
    WAnySorcery -> pure True
    WAnyPermanent {} -> pure False
  WAnyPlayer -> \case
    WAnyPlayer -> pure True
    WAnyPermanent {} -> pure False
  WAnyPermanent perm1 -> \case
    WAnyPermanent perm2 -> eqWPermanent perm1 perm2
    _ -> pure False

eqWPermanent :: WPermanent ot -> WPermanent ot -> EnvM Bool
eqWPermanent = \case
  WPermanentArtifact -> \case
    WPermanentArtifact -> pure True
  WPermanentCreature -> \case
    WPermanentCreature -> pure True
  WPermanentEnchantment -> \case
    WPermanentEnchantment -> pure True
  WPermanentLand -> \case
    WPermanentLand -> pure True
  WPermanentPlaneswalker -> \case
    WPermanentPlaneswalker -> pure True
  WPermanent -> \case
    WPermanent -> pure True
  perm1@WPermanent2 -> \case
    perm2@WPermanent2 ->
      let go :: forall a b. Inst2 IsPermanentType a b => WPermanent '(OT, a, b) -> Bool
          go _ = isJust (cast perm2 :: Maybe (WPermanent '(OT, a, b)))
       in pure $ go perm1
  _ -> undefined

eqNonCreatureCard :: WNonCreatureCard ot -> WNonCreatureCard ot -> EnvM Bool
eqNonCreatureCard = undefined

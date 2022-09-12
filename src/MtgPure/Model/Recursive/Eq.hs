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

module MtgPure.Model.Recursive.Eq () where

import safe qualified Control.Monad as M
import safe qualified Control.Monad.State.Strict as State
import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5, Inst6)
import safe qualified Data.List as List
import safe Data.Maybe (isJust)
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaPool (ManaPool)
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectId (ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreature,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OPermanent,
    OPlayer,
    OSpell,
  )
import safe MtgPure.Model.ObjectType
  ( OT,
    ObjectType (..),
    ObjectType2,
    ObjectType3,
    ObjectType4,
    ObjectType5,
    ObjectType6,
  )
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import safe MtgPure.Model.ObjectType.NonCreatureCard (WNonCreatureCard (..))
import safe MtgPure.Model.ObjectType.Permanent (WPermanent (..))
import safe MtgPure.Model.Recursive
  ( Ability (..),
    Card (..),
    CardTypeDef (..),
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    EventListener (..),
    Requirement (..),
    SetCard (..),
    SetToken (SetToken),
    StaticAbility (..),
    Token (..),
    TriggeredAbility (..),
    TypeableOT,
    TypeableOT2,
    WithObject (..),
    WithThis (..),
  )
import safe MtgPure.Model.Selection (Selection (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToObjectN.Classes (ToObject1 (..))
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.Variable (Variable (..))
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))

instance Eq (Card ot) where
  (==) x y = runEnvM $ eqCard x y

instance Eq (Token ot) where
  (==) x y = runEnvM $ eqToken x y

instance Eq (SetCard ot) where
  (==) x y = runEnvM $ eqSetCard x y

instance Eq (SetToken e) where
  (==) x y = runEnvM $ eqSetToken x y

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

newVariable :: EnvM Variable
newVariable = do
  raw <- State.gets nextRawId
  let var = ReifiedVariable raw
  State.modify' $ \st -> st {nextRawId = nextRawId st + 1}
  pure var

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

withObjectCont ::
  forall (a :: ObjectType) k ot' x.
  (IsObjectType a, TypeableOT k ot') =>
  (x -> x -> EnvM Bool) ->
  (Object a -> ObjectN ot') ->
  (ObjectN ot' -> x) ->
  (ObjectN ot' -> x) ->
  EnvM Bool
withObjectCont eqM cons cont1 cont2 = do
  objN <- newObjectN @a cons
  eqM (cont1 objN) (cont2 objN)

----------------------------------------

eqAbility :: Ability ot -> Ability ot -> EnvM Bool
eqAbility = \case
  Activated cost1 effect1 -> \case
    Activated cost2 effect2 -> andM [eqElectE cost1 cost2, eqElectE effect1 effect2]
    _ -> pure False
  Static static1 -> \case
    Static static2 -> eqStaticAbility static1 static2
    _ -> pure False
  Triggered triggered1 -> \case
    Triggered triggered2 -> eqTriggeredAbility triggered1 triggered2
    _ -> pure False

eqAbilities :: [Ability ot] -> [Ability ot] -> EnvM Bool
eqAbilities = listM eqAbility

eqCard :: Card ot -> Card ot -> EnvM Bool
eqCard = \case
  Card name1 def1 -> \case
    Card name2 def2 -> andM [pure $ name1 == name2, eqWithThisCardTypeDef def1 def2]
    _ -> pure False
  TribalCard name1 def1 -> \case
    TribalCard name2 def2 -> andM [pure $ name1 == name2, eqWithThisCardTypeDef def1 def2]
    _ -> pure False
  ArtifactCard card1 -> \case
    ArtifactCard card2 -> eqCard card1 card2
    _ -> pure False
  CreatureCard card1 -> \case
    CreatureCard card2 -> eqCard card1 card2
    _ -> pure False
  EnchantmentCard card1 -> \case
    EnchantmentCard card2 -> eqCard card1 card2
    _ -> pure False
  InstantCard card1 -> \case
    InstantCard card2 -> eqCard card1 card2
    _ -> pure False
  LandCard card1 -> \case
    LandCard card2 -> eqCard card1 card2
    _ -> pure False
  PlaneswalkerCard card1 -> \case
    PlaneswalkerCard card2 -> eqCard card1 card2
    _ -> pure False
  SorceryCard card1 -> \case
    SorceryCard card2 -> eqCard card1 card2
    _ -> pure False

eqCardTypeDef :: CardTypeDef t ot -> CardTypeDef t ot -> EnvM Bool
eqCardTypeDef = \case
  ArtifactDef colors1 cost1 abilities1 -> \case
    ArtifactDef colors2 cost2 abilities2 ->
      andM
        [ eqColors colors1 colors2,
          eqElectE cost1 cost2,
          eqAbilities abilities1 abilities2
        ]
    _ -> pure False
  ArtifactCreatureDef colors1 cost1 types1 power1 toughness1 abilities1 -> \case
    ArtifactCreatureDef colors2 cost2 types2 power2 toughness2 abilities2 ->
      andM
        [ eqColors colors1 colors2,
          eqElectE cost1 cost2,
          pure $ types1 == types2,
          pure $ power1 == power2,
          pure $ toughness1 == toughness2,
          eqAbilities abilities1 abilities2
        ]
    _ -> pure False
  CreatureDef colors1 cost1 types1 power1 toughness1 abilities1 -> \case
    CreatureDef colors2 cost2 types2 power2 toughness2 abilities2 ->
      andM
        [ eqColors colors1 colors2,
          eqElectE cost1 cost2,
          pure $ types1 == types2,
          pure $ power1 == power2,
          pure $ toughness1 == toughness2,
          eqAbilities abilities1 abilities2
        ]
    _ -> pure False
  EnchantmentDef colors1 cost1 abilities1 -> \case
    EnchantmentDef colors2 cost2 abilities2 ->
      andM
        [ eqColors colors1 colors2,
          eqElectE cost1 cost2,
          eqAbilities abilities1 abilities2
        ]
    _ -> pure False
  InstantDef colors1 cost1 abilities1 effect1 -> \case
    InstantDef colors2 cost2 abilities2 effect2 ->
      andM
        [ eqColors colors1 colors2,
          eqElectE cost1 cost2,
          eqAbilities abilities1 abilities2,
          eqElectE effect1 effect2
        ]
    _ -> pure False
  LandDef abilities1 -> \case
    LandDef abilities2 -> eqAbilities abilities1 abilities2
    _ -> pure False
  PlaneswalkerDef colors1 cost1 loyalty1 abilities1 -> \case
    PlaneswalkerDef colors2 cost2 loyalty2 abilities2 ->
      andM
        [ eqColors colors1 colors2,
          eqElectE cost1 cost2,
          pure $ loyalty1 == loyalty2,
          eqAbilities abilities1 abilities2
        ]
    _ -> pure False
  SorceryDef colors1 cost1 abilities1 effect1 -> \case
    SorceryDef colors2 cost2 abilities2 effect2 ->
      andM
        [ eqColors colors1 colors2,
          eqElectE cost1 cost2,
          eqAbilities abilities1 abilities2,
          eqElectE effect1 effect2
        ]
    _ -> pure False
  TribalDef types1 witness1 def1 -> \case
    TribalDef types2 witness2 def2 ->
      andM
        [ pure $ types1 == types2,
          eqWNonCreatureCard witness1 witness2,
          eqCardTypeDef def1 def2
        ]
    _ -> pure False
  VariableDef varToDef1 -> \case
    VariableDef varToDef2 -> do
      var <- newVariable
      let def1 = varToDef1 var
          def2 = varToDef2 var
      eqCardTypeDef def1 def2
    _ -> pure False

eqColors :: Colors -> Colors -> EnvM Bool
eqColors x y = pure $ x == y

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

eqDamage :: Damage -> Damage -> EnvM Bool
eqDamage x y = pure $ x == y

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
  ChangeTo perm1 obj1 card1 -> \case
    ChangeTo perm2 obj2 card2 ->
      let go :: forall k ot. TypeableOT k ot => WPermanent ot -> Card ot -> EnvM Bool
          go perm1 card1 = case cast (perm2, card2) of
            Nothing -> pure False
            Just (perm2, card2) -> andM [eqWPermanent perm1 perm2, eqOPermanent obj1 obj2, eqCard card1 card2]
       in go perm1 card1
  CounterAbility ability1 -> \case
    CounterAbility ability2 -> eqOActivatedOrTriggeredAbility ability1 ability2
    _ -> pure False
  CounterSpell spell1 -> \case
    CounterSpell spell2 -> eqOSpell spell1 spell2
    _ -> pure False
  DealDamage source1 victim1 damage1 -> \case
    DealDamage source2 victim2 damage2 ->
      andM
        [ eqODamageSource source1 source2,
          eqOCreaturePlayerPlaneswalker victim1 victim2,
          eqDamage damage1 damage2
        ]
    _ -> pure False
  Destroy victim1 -> \case
    Destroy victim2 -> eqOPermanent victim1 victim2
    _ -> pure False
  DrawCards player1 amount1 -> \case
    DrawCards player2 amount2 -> andM [pure $ amount1 == amount2, eqOPlayer player1 player2]
    _ -> pure False
  Sacrifice perm1 player1 reqs1 -> \case
    Sacrifice perm2 player2 reqs2 ->
      let go :: forall k ot. TypeableOT k ot => WPermanent ot -> [Requirement ot] -> EnvM Bool
          go perm1 reqs1 = case cast (perm2, reqs2) of
            Nothing -> pure False
            Just (perm2, reqs2) -> andM [eqWPermanent perm1 perm2, eqOPlayer player1 player2, eqRequirements reqs1 reqs2]
       in go perm1 reqs1
    _ -> pure False

eqElectE :: Elect e ot -> Elect e ot -> EnvM Bool
eqElectE = \case
  A sel1 with1 -> \case
    A sel2 with2 ->
      andM [eqSelection sel1 sel2, eqWithObjectElectE with1 with2]
    _ -> pure False
  ActivePlayer playerToElect1 -> \case
    ActivePlayer playerToElect2 -> do
      player <- newObjectN @'OTPlayer toObject1
      let elect1 = playerToElect1 player
          elect2 = playerToElect2 player
      eqElectE elect1 elect2
    _ -> pure False
  All with1 -> \case
    All with2 -> eqWithObjectElectE with1 with2
    _ -> pure False
  Condition cond1 -> \case
    Condition cond2 -> eqCondition cond1 cond2
    _ -> pure False
  ControllerOf obj1 playerToElect1 -> \case
    ControllerOf obj2 playerToElect2 -> do
      player <- newObjectN @'OTPlayer toObject1
      let elect1 = playerToElect1 player
          elect2 = playerToElect2 player
      andM [eqOAny obj1 obj2, eqElectE elect1 elect2]
    _ -> pure False
  Cost cost1 -> \case
    Cost cost2 -> eqCost cost1 cost2
    _ -> pure False
  Effect effects1 -> \case
    Effect effects2 -> listM eqEffect effects1 effects2
    _ -> pure False
  Event listener1 -> \case
    Event listener2 -> eqEventListener listener1 listener2
    _ -> pure False
  If cond1 then1 else1 -> \case
    If cond2 then2 else2 -> andM [eqCondition cond1 cond2, eqElectE then1 then2, eqElectE else1 else2]
    _ -> pure False
  VariableFromPower obj1 varToElect1 -> \case
    VariableFromPower obj2 varToElect2 -> do
      var <- newVariable
      let elect1 = varToElect1 var
          elect2 = varToElect2 var
      andM [eqOCreature obj1 obj2, eqElectE elect1 elect2]
    _ -> pure False

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
eqManaCost x y = pure $ x == y

eqManaPool :: ManaPool -> ManaPool -> EnvM Bool
eqManaPool x y = pure $ x == y

eqO1 ::
  forall a kot kot' x ot ot'.
  (TypeableOT2 kot ot x, TypeableOT kot' ot', Inst1 IsObjectType a) =>
  (x ot -> x ot -> EnvM Bool) ->
  [Requirement '(OT, a)] ->
  [Requirement ot'] ->
  (ObjectN '(OT, a) -> x ot) ->
  (ObjectN ot' -> x ot) ->
  EnvM Bool
eqO1 eqM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> pure False
  Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a eqM O cont1 cont2]

eqO2 ::
  forall a b kot kot' x ot ot'.
  (TypeableOT2 kot ot x, TypeableOT kot' ot', Inst2 IsObjectType a b) =>
  (x ot -> x ot -> EnvM Bool) ->
  [Requirement '(OT, a, b)] ->
  [Requirement ot'] ->
  (ObjectN '(OT, a, b) -> x ot) ->
  (ObjectN ot' -> x ot) ->
  EnvM Bool
eqO2 eqM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> pure False
  Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a eqM O2a cont1 cont2]

eqO3 ::
  forall a b c kot kot' x ot ot'.
  (TypeableOT2 kot ot x, TypeableOT kot' ot', Inst3 IsObjectType a b c) =>
  (x ot -> x ot -> EnvM Bool) ->
  [Requirement '(OT, a, b, c)] ->
  [Requirement ot'] ->
  (ObjectN '(OT, a, b, c) -> x ot) ->
  (ObjectN ot' -> x ot) ->
  EnvM Bool
eqO3 eqM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> pure False
  Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a eqM O3a cont1 cont2]

eqO4 ::
  forall a b c d kot kot' x ot ot'.
  (TypeableOT2 kot ot x, TypeableOT kot' ot', Inst4 IsObjectType a b c d) =>
  (x ot -> x ot -> EnvM Bool) ->
  [Requirement '(OT, a, b, c, d)] ->
  [Requirement ot'] ->
  (ObjectN '(OT, a, b, c, d) -> x ot) ->
  (ObjectN ot' -> x ot) ->
  EnvM Bool
eqO4 eqM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> pure False
  Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a eqM O4a cont1 cont2]

eqO5 ::
  forall a b c d e kot kot' x ot ot'.
  (TypeableOT2 kot ot x, TypeableOT kot' ot', Inst5 IsObjectType a b c d e) =>
  (x ot -> x ot -> EnvM Bool) ->
  [Requirement '(OT, a, b, c, d, e)] ->
  [Requirement ot'] ->
  (ObjectN '(OT, a, b, c, d, e) -> x ot) ->
  (ObjectN ot' -> x ot) ->
  EnvM Bool
eqO5 eqM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> pure False
  Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont @a eqM O5a cont1 cont2]

eqObjectN :: VisitObjectN ot => ObjectN ot -> ObjectN ot -> EnvM Bool
eqObjectN objN1 objN2 = do
  let i1 = visitObjectN' objectToId objN1
      i2 = visitObjectN' objectToId objN2
  pure $ i1 == i2

eqOActivatedOrTriggeredAbility :: OActivatedOrTriggeredAbility -> OActivatedOrTriggeredAbility -> EnvM Bool
eqOActivatedOrTriggeredAbility = eqObjectN

eqOAny :: OAny -> OAny -> EnvM Bool
eqOAny = eqObjectN

eqOCreature :: OCreature -> OCreature -> EnvM Bool
eqOCreature = eqObjectN

eqOCreaturePlayerPlaneswalker :: OCreaturePlayerPlaneswalker -> OCreaturePlayerPlaneswalker -> EnvM Bool
eqOCreaturePlayerPlaneswalker = eqObjectN

eqODamageSource :: ODamageSource -> ODamageSource -> EnvM Bool
eqODamageSource = eqObjectN

eqOPermanent :: OPermanent -> OPermanent -> EnvM Bool
eqOPermanent = eqObjectN

eqOPlayer :: OPlayer -> OPlayer -> EnvM Bool
eqOPlayer = eqObjectN

eqOSpell :: OSpell -> OSpell -> EnvM Bool
eqOSpell = eqObjectN

eqRequirement :: Requirement ot -> Requirement ot -> EnvM Bool
eqRequirement = \case
  ControlledBy player1 -> \case
    ControlledBy player2 -> eqOPlayer player1 player2
    _ -> pure False
  HasAbility ability1 -> \case
    HasAbility ability2 -> eqWithThisAbility ability1 ability2
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
eqSetCard = \case
  SetCard set1 rarity1 card1 -> \case
    SetCard set2 rarity2 card2 -> andM [pure $ set1 == set2 && rarity1 == rarity2, eqCard card1 card2]

eqSetToken :: SetToken ot -> SetToken ot -> EnvM Bool
eqSetToken = \case
  SetToken set1 rarity1 token1 -> \case
    SetToken set2 rarity2 token2 -> andM [pure $ set1 == set2 && rarity1 == rarity2, eqToken token1 token2]

eqStaticAbility :: StaticAbility ot -> StaticAbility ot -> EnvM Bool
eqStaticAbility = \case
  As with1 -> \case
    As with2 -> eqWithObjectEventListener with1 with2
    _ -> pure False
  ContinuousEffect elect1 -> \case
    ContinuousEffect elect2 -> eqElectE elect1 elect2
    _ -> pure False
  FirstStrike -> \case
    FirstStrike -> pure True
    _ -> pure False
  Flying -> \case
    Flying -> pure True
    _ -> pure False
  Haste -> \case
    Haste -> pure True
    _ -> pure False
  Suspend duration1 elect1 -> \case
    Suspend duration2 elect2 -> andM [pure $ duration1 == duration2, eqElectE elect1 elect2]
    _ -> pure False

eqTimePoint :: TimePoint p -> TimePoint p -> EnvM Bool
eqTimePoint = \case
  PhaseBegin phase1 -> \case
    PhaseBegin phase2 -> pure $ phase1 == phase2
    _ -> pure False
  PhaseEnd phase1 -> \case
    PhaseEnd phase2 -> pure $ phase1 == phase2
    _ -> pure False
  StepBegin step1 -> \case
    StepBegin step2 -> pure $ step1 == step2
    _ -> pure False
  StepEnd step1 -> \case
    StepEnd step2 -> pure $ step1 == step2
    _ -> pure False

eqToken :: Token ot -> Token ot -> EnvM Bool
eqToken = \case
  Token card1 -> \case
    Token card2 -> eqCard card1 card2

eqTriggeredAbility :: forall ot. TriggeredAbility ot -> TriggeredAbility ot -> EnvM Bool
eqTriggeredAbility = \case
  When listener1 -> \case
    When listener2 -> eqElectE @(EventListener ot) @ot listener1 listener2

eqWithObjectElectE :: WithObject (Elect e) ot -> WithObject (Elect e) ot -> EnvM Bool
eqWithObjectElectE = \case
  O1 reqs1 cont1 -> \case
    O1 reqs2 cont2 -> eqO1 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O2 reqs1 cont1 -> \case
    O2 reqs2 cont2 -> eqO2 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O3 reqs1 cont1 -> \case
    O3 reqs2 cont2 -> eqO3 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O4 reqs1 cont1 -> \case
    O4 reqs2 cont2 -> eqO4 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O5 reqs1 cont1 -> \case
    O5 reqs2 cont2 -> eqO5 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  where
    eqM = eqElectE

eqWithObjectEventListener :: WithObject EventListener ot -> WithObject EventListener ot -> EnvM Bool
eqWithObjectEventListener = \case
  O1 reqs1 cont1 -> \case
    O1 reqs2 cont2 -> eqO1 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O2 reqs1 cont1 -> \case
    O2 reqs2 cont2 -> eqO2 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O3 reqs1 cont1 -> \case
    O3 reqs2 cont2 -> eqO3 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O4 reqs1 cont1 -> \case
    O4 reqs2 cont2 -> eqO4 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  O5 reqs1 cont1 -> \case
    O5 reqs2 cont2 -> eqO5 eqM reqs1 reqs2 cont1 cont2
    _ -> pure False
  where
    eqM = eqEventListener

eqWithThisAbility :: WithThis Ability ot -> WithThis Ability ot -> EnvM Bool
eqWithThisAbility = \case
  T1 cont1 -> \case
    T1 cont2 -> eqO1 eqM [] [] cont1 cont2
  T2 cont1 -> \case
    T2 cont2 -> eqO2 eqM [] [] cont1 cont2
  T3 cont1 -> \case
    T3 cont2 -> eqO3 eqM [] [] cont1 cont2
  T4 cont1 -> \case
    T4 cont2 -> eqO4 eqM [] [] cont1 cont2
  T5 cont1 -> \case
    T5 cont2 -> eqO5 eqM [] [] cont1 cont2
  where
    eqM = eqAbility

eqWithThisCardTypeDef :: Typeable tribal => WithThis (CardTypeDef tribal) ot -> WithThis (CardTypeDef tribal) ot -> EnvM Bool
eqWithThisCardTypeDef = \case
  T1 cont1 -> \case
    T1 cont2 -> eqO1 eqM [] [] cont1 cont2
  T2 cont1 -> \case
    T2 cont2 -> eqO2 eqM [] [] cont1 cont2
  T3 cont1 -> \case
    T3 cont2 -> eqO3 eqM [] [] cont1 cont2
  T4 cont1 -> \case
    T4 cont2 -> eqO4 eqM [] [] cont1 cont2
  T5 cont1 -> \case
    T5 cont2 -> eqO5 eqM [] [] cont1 cont2
  where
    eqM = eqCardTypeDef

eqW2 ::
  forall witness ot1 a1 b1 ot2 a2 b2.
  ( Typeable witness,
    ot1 ~ '(OT, a1, b1),
    ot2 ~ '(OT, a2, b2),
    TypeableOT ObjectType2 ot1,
    TypeableOT ObjectType2 ot2,
    Inst2 IsObjectType a1 b1,
    Inst2 IsObjectType a2 b2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Bool
eqW2 _wit1 wit2 = pure $ isJust (cast wit2 :: Maybe (witness ot1))

eqW3 ::
  forall witness ot1 a1 b1 c1 ot2 a2 b2 c2.
  ( Typeable witness,
    ot1 ~ '(OT, a1, b1, c1),
    ot2 ~ '(OT, a2, b2, c2),
    TypeableOT ObjectType3 ot1,
    TypeableOT ObjectType3 ot2,
    Inst3 IsObjectType a1 b1 c1,
    Inst3 IsObjectType a2 b2 c2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Bool
eqW3 _wit1 wit2 = pure $ isJust (cast wit2 :: Maybe (witness ot1))

eqW4 ::
  forall witness ot1 a1 b1 c1 d1 ot2 a2 b2 c2 d2.
  ( Typeable witness,
    ot1 ~ '(OT, a1, b1, c1, d1),
    ot2 ~ '(OT, a2, b2, c2, d2),
    TypeableOT ObjectType4 ot1,
    TypeableOT ObjectType4 ot2,
    Inst4 IsObjectType a1 b1 c1 d1,
    Inst4 IsObjectType a2 b2 c2 d2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Bool
eqW4 _wit1 wit2 = pure $ isJust (cast wit2 :: Maybe (witness ot1))

eqW5 ::
  forall witness ot1 a1 b1 c1 d1 e1 ot2 a2 b2 c2 d2 e2.
  ( Typeable witness,
    ot1 ~ '(OT, a1, b1, c1, d1, e1),
    ot2 ~ '(OT, a2, b2, c2, d2, e2),
    TypeableOT ObjectType5 ot1,
    TypeableOT ObjectType5 ot2,
    Inst5 IsObjectType a1 b1 c1 d1 e1,
    Inst5 IsObjectType a2 b2 c2 d2 e2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Bool
eqW5 _wit1 wit2 = pure $ isJust (cast wit2 :: Maybe (witness ot1))

eqW6 ::
  forall witness ot1 a1 b1 c1 d1 e1 f1 ot2 a2 b2 c2 d2 e2 f2.
  ( Typeable witness,
    ot1 ~ '(OT, a1, b1, c1, d1, e1, f1),
    ot2 ~ '(OT, a2, b2, c2, d2, e2, f2),
    TypeableOT ObjectType6 ot1,
    TypeableOT ObjectType6 ot2,
    Inst6 IsObjectType a1 b1 c1 d1 e1 f1,
    Inst6 IsObjectType a2 b2 c2 d2 e2 f2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Bool
eqW6 _wit1 wit2 = pure $ isJust (cast wit2 :: Maybe (witness ot1))

eqWAny :: WAny ot -> WAny ot -> EnvM Bool
eqWAny = \case
  WAnyArtifact -> \case
    WAnyArtifact -> pure True
  WAnyCreature -> \case
    WAnyCreature -> pure True
  WAnyEnchantment -> \case
    WAnyEnchantment -> pure True
  WAnyInstant -> \case
    WAnyInstant -> pure True
  WAnyLand -> \case
    WAnyLand -> pure True
  WAnyPlaneswalker -> \case
    WAnyPlaneswalker -> pure True
  WAnyPlayer -> \case
    WAnyPlayer -> pure True
  WAnySorcery -> \case
    WAnySorcery -> pure True
  WAny -> \case
    WAny -> pure True
  x@WAny2 -> \case
    y@WAny2 -> eqW2 x y
  x@WAny3 -> \case
    y@WAny3 -> eqW3 x y
  x@WAny4 -> \case
    y@WAny4 -> eqW4 x y
  x@WAny5 -> \case
    y@WAny5 -> eqW5 x y
  x@WAny6 -> \case
    y@WAny6 -> eqW6 x y

eqWNonCreatureCard :: WNonCreatureCard ot -> WNonCreatureCard ot -> EnvM Bool
eqWNonCreatureCard = \case
  WNonCreatureArtifact -> \case
    WNonCreatureArtifact -> pure True
  WNonCreatureEnchantment -> \case
    WNonCreatureEnchantment -> pure True
  WNonCreatureInstant -> \case
    WNonCreatureInstant -> pure True
  WNonCreatureLand -> \case
    WNonCreatureLand -> pure True
  WNonCreaturePlaneswalker -> \case
    WNonCreaturePlaneswalker -> pure True
  WNonCreatureSorcery -> \case
    WNonCreatureSorcery -> pure True
  WNonCreatureCard -> \case
    WNonCreatureCard -> pure True
  x@WNonCreatureCard2 -> \case
    y@WNonCreatureCard2 -> eqW2 x y
  x@WNonCreatureCard3 -> \case
    y@WNonCreatureCard3 -> eqW3 x y

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
  x@WPermanent2 -> \case
    y@WPermanent2 -> eqW2 x y
  x@WPermanent3 -> \case
    y@WPermanent3 -> eqW3 x y
  x@WPermanent4 -> \case
    y@WPermanent4 -> eqW4 x y

{-# LANGUAGE AllowAmbiguousTypes #-}
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

module MtgPure.Model.Recursive.Ord () where

import safe qualified Control.Monad.State.Strict as State
import safe Data.ConsIndex (ConsIndex (consIndex))
import safe Data.Inst
  ( Inst1,
    Inst2,
    Inst3,
    Inst4,
    Inst5,
    Inst6,
  )
import safe Data.Proxy (Proxy (Proxy))
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
    ON1,
    ON2,
    ON3,
    ON4,
    ON5,
    OPermanent,
    OPlayer,
    OSpell,
  )
import safe MtgPure.Model.ObjectType
  ( OT1,
    OT2,
    OT3,
    OT4,
    OT5,
    OT6,
    ObjectType (..),
  )
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import MtgPure.Model.ObjectType.Card (WCard (..))
import safe MtgPure.Model.ObjectType.Index (IndexOT (indexOT))
import safe MtgPure.Model.ObjectType.NonCreatureCard (WNonCreatureCard (..))
import safe MtgPure.Model.ObjectType.Permanent (WPermanent (..))
import safe MtgPure.Model.ObjectType.Spell (WSpell (..))
import safe MtgPure.Model.Recursive
  ( Ability (..),
    Card (..),
    CardTypeDef (..),
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    Else (..),
    Event,
    EventListener,
    EventListener' (..),
    NonProxy (..),
    Requirement (..),
    SetCard (..),
    SetToken (SetToken),
    StaticAbility (..),
    Token (..),
    TriggeredAbility (..),
    TypeableOT,
    TypeableOT2,
    WithLinkedCard (..),
    WithLinkedObject (..),
    WithMaskedObject (..),
    WithThis (..),
    ZoneCard (..),
  )
import safe MtgPure.Model.Selection (Selection (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToObjectN.Classes (ToObject1 (..))
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.Variable (Variable (..))
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))
import MtgPure.Model.Zone (Zone (..))

----------------------------------------

instance Eq (Ability ot) where
  (==) x y = runEnvM (ordAbility x y) == EQ

instance Eq (Card ot) where
  (==) x y = runEnvM (ordCard x y) == EQ

instance Eq (CardTypeDef tribal ot) where
  (==) x y = runEnvM (ordCardTypeDef x y) == EQ

instance Eq Condition where
  (==) x y = runEnvM (ordCondition x y) == EQ

instance Eq (Cost ot) where
  (==) x y = runEnvM (ordCost x y) == EQ

instance Eq (Effect e) where
  (==) x y = runEnvM (ordEffect x y) == EQ

instance Eq (Elect e ot) where
  (==) x y = runEnvM (ordElectE x y) == EQ

instance Eq EventListener where
  (==) x y = runEnvM (ordEventListener x y) == EQ

instance VisitObjectN ot => Eq (ObjectN ot) where
  (==) x y = runEnvM (ordObjectN x y) == EQ

instance Eq (Requirement (Card ot)) where
  (==) x y = runEnvM (ordRequirement x y) == EQ

instance Eq (Requirement (ObjectN ot)) where
  (==) x y = runEnvM (ordRequirement x y) == EQ

instance Eq Selection where
  (==) x y = runEnvM (ordSelection x y) == EQ

instance Eq (SetCard ot) where
  (==) x y = runEnvM (ordSetCard x y) == EQ

instance Eq (SetToken ot) where
  (==) x y = runEnvM (ordSetToken x y) == EQ

instance Eq (StaticAbility ot) where
  (==) x y = runEnvM (ordStaticAbility x y) == EQ

instance Eq (TimePoint p) where
  (==) x y = runEnvM (ordTimePoint x y) == EQ

instance Eq (Token ot) where
  (==) x y = runEnvM (ordToken x y) == EQ

instance Eq (TriggeredAbility ot) where
  (==) x y = runEnvM (ordTriggeredAbility x y) == EQ

instance Eq (WithMaskedObject (Elect e ot)) where
  (==) x y = runEnvM (ordWithMaskedObjectElectE x y) == EQ

instance Eq (WithThis Ability ot) where
  (==) x y = runEnvM (ordWithThisAbility x y) == EQ

instance Typeable tribal => Eq (WithThis (CardTypeDef tribal) ot) where
  (==) x y = runEnvM (ordWithThisCardTypeDef x y) == EQ

instance Eq (WAny ot) where
  (==) x y = runEnvM (ordWAny x y) == EQ

instance Eq (WNonCreatureCard ot) where
  (==) x y = runEnvM (ordWNonCreatureCard x y) == EQ

instance Eq (WPermanent ot) where
  (==) x y = runEnvM (ordWPermanent x y) == EQ

----------------------------------------

instance Ord (Ability ot) where
  compare x y = runEnvM (ordAbility x y)

instance Ord (Card ot) where
  compare x y = runEnvM (ordCard x y)

instance Ord (CardTypeDef tribal ot) where
  compare x y = runEnvM (ordCardTypeDef x y)

instance Ord Condition where
  compare x y = runEnvM (ordCondition x y)

instance Ord (Cost ot) where
  compare x y = runEnvM (ordCost x y)

instance Ord (Effect e) where
  compare x y = runEnvM (ordEffect x y)

instance Ord (Elect e ot) where
  compare x y = runEnvM (ordElectE x y)

instance Ord EventListener where
  compare x y = runEnvM (ordEventListener x y)

instance VisitObjectN ot => Ord (ObjectN ot) where
  compare x y = runEnvM (ordObjectN x y)

instance Ord (Requirement (Card ot)) where
  compare x y = runEnvM (ordRequirement x y)

instance Ord (Requirement (ObjectN ot)) where
  compare x y = runEnvM (ordRequirement x y)

instance Ord Selection where
  compare x y = runEnvM (ordSelection x y)

instance Ord (SetCard ot) where
  compare x y = runEnvM (ordSetCard x y)

instance Ord (SetToken ot) where
  compare x y = runEnvM (ordSetToken x y)

instance Ord (StaticAbility ot) where
  compare x y = runEnvM (ordStaticAbility x y)

instance Ord (TimePoint p) where
  compare x y = runEnvM (ordTimePoint x y)

instance Ord (Token ot) where
  compare x y = runEnvM (ordToken x y)

instance Ord (TriggeredAbility ot) where
  compare x y = runEnvM (ordTriggeredAbility x y)

instance Ord (WithMaskedObject (Elect e ot)) where
  compare x y = runEnvM (ordWithMaskedObjectElectE x y)

instance Ord (WithThis Ability ot) where
  compare x y = runEnvM (ordWithThisAbility x y)

instance Typeable tribal => Ord (WithThis (CardTypeDef tribal) ot) where
  compare x y = runEnvM (ordWithThisCardTypeDef x y)

instance Ord (WAny ot) where
  compare x y = runEnvM (ordWAny x y)

instance Ord (WNonCreatureCard ot) where
  compare x y = runEnvM (ordWNonCreatureCard x y)

instance Ord (WPermanent ot) where
  compare x y = runEnvM (ordWPermanent x y)

----------------------------------------

type EnvM = State.State Env

-- TODO: Smarts need to be added to handle Card loops
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

seqM :: Monad m => [m Ordering] -> m Ordering
seqM = \case
  [] -> pure EQ
  ordM : ordMs ->
    ordM >>= \case
      EQ -> seqM ordMs
      ord -> pure ord

listM :: (a -> a -> EnvM Ordering) -> [a] -> [a] -> EnvM Ordering
listM compareM xs ys = case (xs, ys) of
  ([], []) -> pure EQ
  ([], _) -> pure LT
  (_, []) -> pure GT
  (x : xs', y : ys') ->
    compareM x y >>= \case
      EQ -> listM compareM xs' ys'
      ord -> pure ord

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
  (Typeable ot, IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  EnvM (ObjectN ot)
newObjectN make = do
  obj <- newObject @a
  let objN = make obj
  pure objN

withObjectCont ::
  forall (a :: ObjectType) ot' x.
  (IsObjectType a, TypeableOT ot') =>
  (x -> x -> EnvM Ordering) ->
  (Object a -> ObjectN ot') ->
  (ObjectN ot' -> x) ->
  (ObjectN ot' -> x) ->
  EnvM Ordering
withObjectCont ordM cons cont1 cont2 = do
  objN <- newObjectN @a cons
  ordM (cont1 objN) (cont2 objN)

compareIndexM :: ConsIndex a => a -> a -> EnvM Ordering
compareIndexM x y = pure $ compare (consIndex x) (consIndex y)

compareOT :: forall ot ot' proxy. (TypeableOT ot, TypeableOT ot') => proxy ot' -> EnvM Ordering
compareOT _proxy = pure $ compare (indexOT (Proxy @ot)) (indexOT (Proxy @ot'))

----------------------------------------

ordAbility :: Ability ot -> Ability ot -> EnvM Ordering
ordAbility x = case x of
  Activated cost1 effect1 -> \case
    Activated cost2 effect2 -> seqM [ordElectE cost1 cost2, ordElectE effect1 effect2]
    y -> compareIndexM x y
  Static static1 -> \case
    Static static2 -> ordStaticAbility static1 static2
    y -> compareIndexM x y
  Triggered triggered1 -> \case
    Triggered triggered2 -> ordTriggeredAbility triggered1 triggered2
    y -> compareIndexM x y

ordAbilities :: [Ability ot] -> [Ability ot] -> EnvM Ordering
ordAbilities = listM ordAbility

ordCard :: Card ot -> Card ot -> EnvM Ordering
ordCard x = case x of
  Card name1 wCard1 def1 -> \case
    Card name2 wCard2 def2 ->
      seqM
        [ pure $ compare name1 name2,
          ordWCard wCard1 wCard2,
          ordWithThisCardTypeDef def1 def2
        ]
    y -> compareIndexM x y
  TribalCard name1 wCard1 def1 -> \case
    TribalCard name2 wCard2 def2 ->
      seqM
        [ pure $ compare name1 name2,
          ordWCard wCard1 wCard2,
          ordWithThisCardTypeDef def1 def2
        ]
    y -> compareIndexM x y
  --
  ArtifactCard card1 -> \case
    ArtifactCard card2 -> ordCard card1 card2
    y -> compareIndexM x y
  CreatureCard card1 -> \case
    CreatureCard card2 -> ordCard card1 card2
    y -> compareIndexM x y
  EnchantmentCard card1 -> \case
    EnchantmentCard card2 -> ordCard card1 card2
    y -> compareIndexM x y
  InstantCard card1 -> \case
    InstantCard card2 -> ordCard card1 card2
    y -> compareIndexM x y
  LandCard card1 -> \case
    LandCard card2 -> ordCard card1 card2
    y -> compareIndexM x y
  PlaneswalkerCard card1 -> \case
    PlaneswalkerCard card2 -> ordCard card1 card2
    y -> compareIndexM x y
  SorceryCard card1 -> \case
    SorceryCard card2 -> ordCard card1 card2
    y -> compareIndexM x y

ordCardTypeDef :: CardTypeDef t ot -> CardTypeDef t ot -> EnvM Ordering
ordCardTypeDef x = case x of
  ArtifactDef colors1 cost1 abilities1 -> \case
    ArtifactDef colors2 cost2 abilities2 ->
      seqM
        [ ordColors colors1 colors2,
          ordElectE cost1 cost2,
          ordAbilities abilities1 abilities2
        ]
    y -> compareIndexM x y
  ArtifactCreatureDef colors1 cost1 types1 power1 toughness1 abilities1 -> \case
    ArtifactCreatureDef colors2 cost2 types2 power2 toughness2 abilities2 ->
      seqM
        [ ordColors colors1 colors2,
          ordElectE cost1 cost2,
          pure $ compare types1 types2,
          pure $ compare power1 power2,
          pure $ compare toughness1 toughness2,
          ordAbilities abilities1 abilities2
        ]
    y -> compareIndexM x y
  CreatureDef colors1 cost1 types1 power1 toughness1 abilities1 -> \case
    CreatureDef colors2 cost2 types2 power2 toughness2 abilities2 ->
      seqM
        [ ordColors colors1 colors2,
          ordElectE cost1 cost2,
          pure $ compare types1 types2,
          pure $ compare power1 power2,
          pure $ compare toughness1 toughness2,
          ordAbilities abilities1 abilities2
        ]
    y -> compareIndexM x y
  EnchantmentDef colors1 cost1 abilities1 -> \case
    EnchantmentDef colors2 cost2 abilities2 ->
      seqM
        [ ordColors colors1 colors2,
          ordElectE cost1 cost2,
          ordAbilities abilities1 abilities2
        ]
    y -> compareIndexM x y
  InstantDef colors1 cost1 abilities1 effect1 -> \case
    InstantDef colors2 cost2 abilities2 effect2 ->
      seqM
        [ ordColors colors1 colors2,
          ordElectE cost1 cost2,
          ordAbilities abilities1 abilities2,
          ordElectE effect1 effect2
        ]
    y -> compareIndexM x y
  LandDef types1 abilities1 -> \case
    LandDef types2 abilities2 -> seqM [pure $ compare types1 types2, ordAbilities abilities1 abilities2]
    y -> compareIndexM x y
  PlaneswalkerDef colors1 cost1 loyalty1 abilities1 -> \case
    PlaneswalkerDef colors2 cost2 loyalty2 abilities2 ->
      seqM
        [ ordColors colors1 colors2,
          ordElectE cost1 cost2,
          pure $ compare loyalty1 loyalty2,
          ordAbilities abilities1 abilities2
        ]
    y -> compareIndexM x y
  SorceryDef colors1 cost1 abilities1 effect1 -> \case
    SorceryDef colors2 cost2 abilities2 effect2 ->
      seqM
        [ ordColors colors1 colors2,
          ordElectE cost1 cost2,
          ordAbilities abilities1 abilities2,
          ordElectE effect1 effect2
        ]
    y -> compareIndexM x y
  TribalDef types1 witness1 def1 -> \case
    TribalDef types2 witness2 def2 ->
      seqM
        [ pure $ compare types1 types2,
          ordWNonCreatureCard witness1 witness2,
          ordCardTypeDef def1 def2
        ]
    y -> compareIndexM x y
  VariableDef varToDef1 -> \case
    VariableDef varToDef2 -> do
      var <- newVariable
      let def1 = varToDef1 var
          def2 = varToDef2 var
      ordCardTypeDef def1 def2
    y -> compareIndexM x y

ordColors :: Colors -> Colors -> EnvM Ordering
ordColors x y = pure $ compare x y

ordCondition :: Condition -> Condition -> EnvM Ordering
ordCondition x = case x of
  CAnd conds1 -> \case
    CAnd conds2 -> ordConditions conds1 conds2
    y -> compareIndexM x y
  CNot cond1 -> \case
    CNot cond2 -> ordCondition cond1 cond2
    y -> compareIndexM x y
  COr conds1 -> \case
    COr conds2 -> ordConditions conds1 conds2
    y -> compareIndexM x y
  Satisfies any1 obj1 reqs1 -> \case
    Satisfies any2 obj2 reqs2 ->
      let go :: forall ot. TypeableOT ot => WAny ot -> EnvM Ordering
          go _ = case cast (any2, obj2, reqs2) of
            Nothing -> compareOT @ot any2
            Just (any2, obj2, reqs2) ->
              seqM
                [ ordWAny any1 any2,
                  ordObjectN obj1 obj2,
                  ordRequirements reqs1 reqs2
                ]
       in go any1
    y -> compareIndexM x y

ordConditions :: [Condition] -> [Condition] -> EnvM Ordering
ordConditions = listM ordCondition

ordCost :: Cost ot -> Cost ot -> EnvM Ordering
ordCost x = case x of
  AndCosts costs1 -> \case
    AndCosts costs2 -> ordCosts costs1 costs2
    y -> compareIndexM x y
  DiscardRandomCost amount1 -> \case
    DiscardRandomCost amount2 -> pure $ compare amount1 amount2
    y -> compareIndexM x y
  LoyaltyCost amount1 -> \case
    LoyaltyCost amount2 -> pure $ compare amount1 amount2
    y -> compareIndexM x y
  ManaCost mana1 -> \case
    ManaCost mana2 -> ordManaCost mana1 mana2
    y -> compareIndexM x y
  OrCosts costs1 -> \case
    OrCosts costs2 -> ordCosts costs1 costs2
    y -> compareIndexM x y
  PayLife amount1 -> \case
    PayLife amount2 -> pure $ compare amount1 amount2
    y -> compareIndexM x y
  SacrificeCost perm1 reqs1 -> \case
    SacrificeCost perm2 reqs2 -> seqM [ordWPermanent perm1 perm2, ordRequirements reqs1 reqs2]
    y -> compareIndexM x y
  TapCost obj1 -> \case
    TapCost obj2 -> ordOPermanent obj1 obj2
    y -> compareIndexM x y

ordCosts :: [Cost ot] -> [Cost ot] -> EnvM Ordering
ordCosts = listM ordCost

ordDamage :: Damage -> Damage -> EnvM Ordering
ordDamage x y = pure $ compare x y

ordEffect :: Effect e -> Effect e -> EnvM Ordering
ordEffect x = case x of
  AddMana player1 mana1 -> \case
    AddMana player2 mana2 ->
      seqM [ordOPlayer player1 player2, ordManaPool mana1 mana2]
    y -> compareIndexM x y
  AddToBattlefield perm1 player1 token1 -> \case
    AddToBattlefield perm2 player2 token2 ->
      let go :: forall ot. TypeableOT ot => WPermanent ot -> EnvM Ordering
          go _ = case cast (perm2, token2) of
            Nothing -> compareOT @ot perm2
            Just (perm2, token2) ->
              seqM
                [ ordWPermanent perm1 perm2,
                  ordOPlayer player1 player2,
                  ordToken token1 token2
                ]
       in go perm1
    y -> compareIndexM x y
  ChangeTo perm1 obj1 card1 -> \case
    ChangeTo perm2 obj2 card2 ->
      let go :: forall ot. TypeableOT ot => WPermanent ot -> EnvM Ordering
          go _ = case cast (perm2, card2) of
            Nothing -> compareOT @ot perm2
            Just (perm2, card2) ->
              seqM
                [ ordWPermanent perm1 perm2,
                  ordOPermanent obj1 obj2,
                  ordCard card1 card2
                ]
       in go perm1
    y -> compareIndexM x y
  CounterAbility ability1 -> \case
    CounterAbility ability2 -> ordOActivatedOrTriggeredAbility ability1 ability2
    y -> compareIndexM x y
  CounterSpell spell1 -> \case
    CounterSpell spell2 -> ordOSpell spell1 spell2
    y -> compareIndexM x y
  DealDamage source1 victim1 damage1 -> \case
    DealDamage source2 victim2 damage2 ->
      seqM
        [ ordODamageSource source1 source2,
          ordOCreaturePlayerPlaneswalker victim1 victim2,
          ordDamage damage1 damage2
        ]
    y -> compareIndexM x y
  Destroy victim1 -> \case
    Destroy victim2 -> ordOPermanent victim1 victim2
    y -> compareIndexM x y
  DrawCards player1 amount1 -> \case
    DrawCards player2 amount2 -> seqM [pure $ compare amount1 amount2, ordOPlayer player1 player2]
    y -> compareIndexM x y
  EffectContinuous effect1 -> \case
    EffectContinuous effect2 -> ordEffect effect1 effect2
    y -> compareIndexM x y
  EOr effects1 -> \case
    EOr effects2 -> listM ordEffect effects1 effects2
    y -> compareIndexM x y
  Gain any1 obj1 ability1 -> \case
    Gain any2 obj2 ability2 ->
      let go :: forall ot. TypeableOT ot => WAny ot -> EnvM Ordering
          go _ = case cast (any2, obj2, ability2) of
            Nothing -> compareOT @ot any2
            Just (any2, obj2, ability2) ->
              seqM
                [ ordWAny any1 any2,
                  ordObjectN obj1 obj2,
                  ordAbility ability1 ability2
                ]
       in go any1
    y -> compareIndexM x y
  Lose any1 obj1 ability1 -> \case
    Lose any2 obj2 ability2 ->
      let go :: forall ot. TypeableOT ot => WAny ot -> EnvM Ordering
          go _ = case cast (any2, obj2, ability2) of
            Nothing -> compareOT @ot any2
            Just (any2, obj2, ability2) ->
              seqM
                [ ordWAny any1 any2,
                  ordObjectN obj1 obj2,
                  ordAbility ability1 ability2
                ]
       in go any1
    y -> compareIndexM x y
  PutOntoBattlefield wPerm1 player1 card1 -> \case
    PutOntoBattlefield wPerm2 player2 card2 ->
      let go :: forall ot. TypeableOT ot => WPermanent ot -> EnvM Ordering
          go _ = case cast (wPerm2, card2) of
            Nothing -> compareOT @ot wPerm2
            Just (wPerm2, card2) ->
              seqM
                [ ordWPermanent wPerm1 wPerm2,
                  ordOPlayer player1 player2,
                  ordZoneCard card1 card2
                ]
       in go wPerm1
    y -> compareIndexM x y
  Sacrifice perm1 player1 reqs1 -> \case
    Sacrifice perm2 player2 reqs2 ->
      let go :: forall ot. TypeableOT ot => WPermanent ot -> EnvM Ordering
          go _ = case cast (perm2, reqs2) of
            Nothing -> compareOT @ot perm2
            Just (perm2, reqs2) ->
              seqM
                [ ordWPermanent perm1 perm2,
                  ordOPlayer player1 player2,
                  ordRequirements reqs1 reqs2
                ]
       in go perm1
    y -> compareIndexM x y
  SearchLibrary wCard1 player1 card1 -> \case
    SearchLibrary wCard2 player2 card2 ->
      let go :: forall ot. TypeableOT ot => WCard ot -> EnvM Ordering
          go _ = case cast (wCard2, card2) of
            Nothing -> compareOT @ot wCard2
            Just (wCard2, card2) ->
              seqM
                [ ordWCard wCard1 wCard2,
                  ordOPlayer player1 player2,
                  ordWithLinkedCard ordElectE card1 card2
                ]
       in go wCard1
    y -> compareIndexM x y
  StatDelta creature1 power1 toughness1 -> \case
    StatDelta creature2 power2 toughness2 ->
      seqM
        [ ordOCreature creature1 creature2,
          pure $ compare power1 power2,
          pure $ compare toughness1 toughness2
        ]
    y -> compareIndexM x y
  Until event1 effect1 -> \case
    Until event2 effect2 -> seqM [ordElectE event1 event2, ordEffect effect1 effect2]
    y -> compareIndexM x y

ordElectE :: Elect e ot -> Elect e ot -> EnvM Ordering
ordElectE x = case x of
  A sel1 player1 with1 -> \case
    A sel2 player2 with2 ->
      seqM [ordSelection sel1 sel2, ordOPlayer player1 player2, ordWithMaskedObjectElectE with1 with2]
    y -> compareIndexM x y
  ActivePlayer playerToElect1 -> \case
    ActivePlayer playerToElect2 -> do
      player <- newObjectN @'OTPlayer toObject1
      let elect1 = playerToElect1 player
          elect2 = playerToElect2 player
      ordElectE elect1 elect2
    y -> compareIndexM x y
  All with1 -> \case
    All with2 -> ordWithMaskedObjectElectE with1 with2
    y -> compareIndexM x y
  Condition cond1 -> \case
    Condition cond2 -> ordCondition cond1 cond2
    y -> compareIndexM x y
  ControllerOf obj1 playerToElect1 -> \case
    ControllerOf obj2 playerToElect2 -> do
      player <- newObjectN @'OTPlayer toObject1
      let elect1 = playerToElect1 player
          elect2 = playerToElect2 player
      seqM [ordOAny obj1 obj2, ordElectE elect1 elect2]
    y -> compareIndexM x y
  Cost cost1 -> \case
    Cost cost2 -> ordCost cost1 cost2
    y -> compareIndexM x y
  Effect effects1 -> \case
    Effect effects2 -> listM ordEffect effects1 effects2
    y -> compareIndexM x y
  Event event1 -> \case
    Event event2 -> ordEvent event1 event2
    y -> compareIndexM x y
  If cond1 then1 else1 -> \case
    If cond2 then2 else2 -> seqM [ordCondition cond1 cond2, ordElectE then1 then2, ordElseE else1 else2]
    y -> compareIndexM x y
  Listen listener1 -> \case
    Listen listener2 -> ordEventListener listener1 listener2
    y -> compareIndexM x y
  Random with1 -> \case
    Random with2 -> ordWithMaskedObjectElectE with1 with2
    y -> compareIndexM x y
  VariableFromPower obj1 varToElect1 -> \case
    VariableFromPower obj2 varToElect2 -> do
      var <- newVariable
      let elect1 = varToElect1 var
          elect2 = varToElect2 var
      seqM [ordOCreature obj1 obj2, ordElectE elect1 elect2]
    y -> compareIndexM x y

ordElseE :: Else e ot -> Else e ot -> EnvM Ordering
ordElseE = \case
  ElseCost elect1 -> \case
    ElseCost elect2 -> ordElectE elect1 elect2
  ElseEffect elect1 -> \case
    ElseEffect elect2 -> ordElectE elect1 elect2
  ElseEvent -> \case
    ElseEvent -> pure EQ

ordEventListener' ::
  forall w.
  Typeable w =>
  (forall ot. w ot -> w ot -> EnvM Ordering) ->
  EventListener' w ->
  EventListener' w ->
  EnvM Ordering
ordEventListener' ordM x = case x of
  BecomesTapped perm1 with1 -> \case
    BecomesTapped perm2 with2 ->
      let go :: forall ot. TypeableOT2 ot w => WPermanent ot -> EnvM Ordering
          go _ = case cast (perm2, with2) of
            Nothing -> compareOT @ot perm2
            Just (perm2, with2) -> seqM [ordWPermanent perm1 perm2, ordWithLinkedObject ordM with1 with2]
       in go perm1
    y -> compareIndexM x y
  Events listeners1 -> \case
    Events listeners2 -> listM (ordEventListener' ordM) listeners1 listeners2
    y -> compareIndexM x y
  SpellIsCast spell1 with1 -> \case
    SpellIsCast spell2 with2 ->
      let go :: forall ot. TypeableOT ot => WSpell ot -> EnvM Ordering
          go _ = case cast (spell2, with2) of
            Nothing -> compareOT @ot spell2
            Just (spell2, with2) -> seqM [ordWSpell spell1 spell2, ordWithLinkedObject ordM with1 with2]
       in go spell1
    y -> compareIndexM x y
  TimePoint time1 elect1 -> \case
    TimePoint time2 elect2 -> case cast (time2, elect2) of
      Nothing -> pure $ compare (consIndex time1) (consIndex time2)
      Just (time2, elect2) -> seqM [ordTimePoint time1 time2, ordM elect1 elect2]
    y -> compareIndexM x y

ordEvent :: Event -> Event -> EnvM Ordering
ordEvent = ordEventListener' $ \Proxy -> pure $ pure EQ

ordEventListener :: EventListener -> EventListener -> EnvM Ordering
ordEventListener = ordEventListener' ordElectE

ordManaCost :: ManaCost -> ManaCost -> EnvM Ordering
ordManaCost x y = pure $ compare x y

ordManaPool :: ManaPool -> ManaPool -> EnvM Ordering
ordManaPool x y = pure $ compare x y

ordCardO1 ::
  forall zone a x ot.
  (Typeable zone, Typeable x, TypeableOT ot, Inst1 IsObjectType a, TypeableOT (OT1 a)) =>
  (forall ot'. TypeableOT ot' => ObjectN ot' -> ZoneCard zone ot') ->
  (x -> x -> EnvM Ordering) ->
  [Requirement (ZoneCard zone (OT1 a))] ->
  [Requirement (ZoneCard zone ot)] ->
  (ZoneCard zone (OT1 a) -> x) ->
  (ZoneCard zone ot -> x) ->
  EnvM Ordering
ordCardO1 toZone ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT1 a) (Proxy :: Proxy ot)
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2,
        withObjectCont @a ordM O (cont1 . toZone) (cont2 . toZone)
      ]

ordCardO2 ::
  forall zone a b x ot.
  (Typeable zone, Typeable x, TypeableOT ot, Inst2 IsObjectType a b, TypeableOT (OT2 a b)) =>
  (forall ot'. TypeableOT ot' => ObjectN ot' -> ZoneCard zone ot') ->
  (x -> x -> EnvM Ordering) ->
  [Requirement (ZoneCard zone (OT2 a b))] ->
  [Requirement (ZoneCard zone ot)] ->
  (ZoneCard zone (OT2 a b) -> x) ->
  (ZoneCard zone ot -> x) ->
  EnvM Ordering
ordCardO2 toZone ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT2 a b) (Proxy :: Proxy ot)
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2,
        withObjectCont @a ordM O2a (cont1 . toZone) (cont2 . toZone)
      ]

ordCardO3 ::
  forall zone a b c x ot.
  (Typeable zone, Typeable x, TypeableOT ot, Inst3 IsObjectType a b c, TypeableOT (OT3 a b c)) =>
  (forall ot'. TypeableOT ot' => ObjectN ot' -> ZoneCard zone ot') ->
  (x -> x -> EnvM Ordering) ->
  [Requirement (ZoneCard zone (OT3 a b c))] ->
  [Requirement (ZoneCard zone ot)] ->
  (ZoneCard zone (OT3 a b c) -> x) ->
  (ZoneCard zone ot -> x) ->
  EnvM Ordering
ordCardO3 toZone ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT3 a b c) (Proxy :: Proxy ot)
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2,
        withObjectCont @a ordM O3a (cont1 . toZone) (cont2 . toZone)
      ]

ordCardO4 ::
  forall zone a b c d x ot.
  (Typeable zone, Typeable x, TypeableOT ot, Inst4 IsObjectType a b c d, TypeableOT (OT4 a b c d)) =>
  (forall ot'. TypeableOT ot' => ObjectN ot' -> ZoneCard zone ot') ->
  (x -> x -> EnvM Ordering) ->
  [Requirement (ZoneCard zone (OT4 a b c d))] ->
  [Requirement (ZoneCard zone ot)] ->
  (ZoneCard zone (OT4 a b c d) -> x) ->
  (ZoneCard zone ot -> x) ->
  EnvM Ordering
ordCardO4 toZone ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT4 a b c d) (Proxy :: Proxy ot)
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2,
        withObjectCont @a ordM O4a (cont1 . toZone) (cont2 . toZone)
      ]

ordCardO5 ::
  forall zone a b c d e x ot.
  (Typeable zone, Typeable x, TypeableOT ot, Inst5 IsObjectType a b c d e, TypeableOT (OT5 a b c d e)) =>
  (forall ot'. TypeableOT ot' => ObjectN ot' -> ZoneCard zone ot') ->
  (x -> x -> EnvM Ordering) ->
  [Requirement (ZoneCard zone (OT5 a b c d e))] ->
  [Requirement (ZoneCard zone ot)] ->
  (ZoneCard zone (OT5 a b c d e) -> x) ->
  (ZoneCard zone ot -> x) ->
  EnvM Ordering
ordCardO5 toZone ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT5 a b c d e) (Proxy :: Proxy ot)
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2,
        withObjectCont @a ordM O5a (cont1 . toZone) (cont2 . toZone)
      ]

ordObjectO1 ::
  forall a x ot.
  (Typeable x, TypeableOT ot, Inst1 IsObjectType a, TypeableOT (OT1 a)) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement (ON1 a)] ->
  [Requirement (ObjectN ot)] ->
  (ON1 a -> x) ->
  (ObjectN ot -> x) ->
  EnvM Ordering
ordObjectO1 ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT1 a) (Proxy :: Proxy ot)
  Just (reqs2, cont2) -> seqM [ordRequirements reqs1 reqs2, withObjectCont @a ordM O cont1 cont2]

ordObjectO2 ::
  forall a b x ot.
  (Typeable x, TypeableOT ot, Inst2 IsObjectType a b, TypeableOT (OT2 a b)) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement (ON2 a b)] ->
  [Requirement (ObjectN ot)] ->
  (ON2 a b -> x) ->
  (ObjectN ot -> x) ->
  EnvM Ordering
ordObjectO2 ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT2 a b) (Proxy :: Proxy ot)
  Just (reqs2, cont2) -> seqM [ordRequirements reqs1 reqs2, withObjectCont @a ordM O2a cont1 cont2]

ordObjectO3 ::
  forall a b c x ot.
  (Typeable x, TypeableOT ot, Inst3 IsObjectType a b c, TypeableOT (OT3 a b c)) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement (ON3 a b c)] ->
  [Requirement (ObjectN ot)] ->
  (ON3 a b c -> x) ->
  (ObjectN ot -> x) ->
  EnvM Ordering
ordObjectO3 ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT3 a b c) (Proxy :: Proxy ot)
  Just (reqs2, cont2) -> seqM [ordRequirements reqs1 reqs2, withObjectCont @a ordM O3a cont1 cont2]

ordObjectO4 ::
  forall a b c d x ot.
  (Typeable x, TypeableOT ot, Inst4 IsObjectType a b c d, TypeableOT (OT4 a b c d)) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement (ON4 a b c d)] ->
  [Requirement (ObjectN ot)] ->
  (ON4 a b c d -> x) ->
  (ObjectN ot -> x) ->
  EnvM Ordering
ordObjectO4 ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT4 a b c d) (Proxy :: Proxy ot)
  Just (reqs2, cont2) -> seqM [ordRequirements reqs1 reqs2, withObjectCont @a ordM O4a cont1 cont2]

ordObjectO5 ::
  forall a b c d e x ot.
  (Typeable x, TypeableOT ot, Inst5 IsObjectType a b c d e, TypeableOT (OT5 a b c d e)) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement (ON5 a b c d e)] ->
  [Requirement (ObjectN ot)] ->
  (ON5 a b c d e -> x) ->
  (ObjectN ot -> x) ->
  EnvM Ordering
ordObjectO5 ordM reqs1 reqs2 cont1 cont2 = case cast (reqs2, cont2) of
  Nothing -> compareOT @(OT5 a b c d e) (Proxy :: Proxy ot)
  Just (reqs2, cont2) -> seqM [ordRequirements reqs1 reqs2, withObjectCont @a ordM O5a cont1 cont2]

ordObjectN :: VisitObjectN ot => ObjectN ot -> ObjectN ot -> EnvM Ordering
ordObjectN objN1 objN2 = do
  let i1 = visitObjectN' objectToId objN1
      i2 = visitObjectN' objectToId objN2
  pure $ compare i1 i2

ordOActivatedOrTriggeredAbility :: OActivatedOrTriggeredAbility -> OActivatedOrTriggeredAbility -> EnvM Ordering
ordOActivatedOrTriggeredAbility = ordObjectN

ordOAny :: OAny -> OAny -> EnvM Ordering
ordOAny = ordObjectN

ordOCreature :: OCreature -> OCreature -> EnvM Ordering
ordOCreature = ordObjectN

ordOCreaturePlayerPlaneswalker :: OCreaturePlayerPlaneswalker -> OCreaturePlayerPlaneswalker -> EnvM Ordering
ordOCreaturePlayerPlaneswalker = ordObjectN

ordODamageSource :: ODamageSource -> ODamageSource -> EnvM Ordering
ordODamageSource = ordObjectN

ordOPermanent :: OPermanent -> OPermanent -> EnvM Ordering
ordOPermanent = ordObjectN

ordOPlayer :: OPlayer -> OPlayer -> EnvM Ordering
ordOPlayer = ordObjectN

ordOSpell :: OSpell -> OSpell -> EnvM Ordering
ordOSpell = ordObjectN

ordRequirement :: Requirement a -> Requirement a -> EnvM Ordering
ordRequirement x = case x of
  ControlledBy player1 -> \case
    ControlledBy player2 -> ordOPlayer player1 player2
    y -> compareIndexM x y
  HasAbility ability1 -> \case
    HasAbility ability2 -> ordWithThisAbility ability1 ability2
    y -> compareIndexM x y
  HasLandType type1 -> \case
    HasLandType type2 -> pure $ compare type1 type2
    y -> compareIndexM x y
  Impossible -> \case
    Impossible -> pure EQ
    y -> compareIndexM x y
  Is any1 obj1 -> \case
    Is any2 obj2 -> seqM [ordWAny any1 any2, ordObjectN obj1 obj2]
    y -> compareIndexM x y
  Not req1 -> \case
    Not req2 -> ordRequirement req1 req2
    y -> compareIndexM x y
  OfColors colors1 -> \case
    OfColors colors2 -> ordColors colors1 colors2
    y -> compareIndexM x y
  OwnedBy player1 -> \case
    OwnedBy player2 -> ordOPlayer player1 player2
    y -> compareIndexM x y
  PlayerPays cost1 -> \case
    PlayerPays cost2 -> ordCost cost1 cost2
    y -> compareIndexM x y
  RAnd reqs1 -> \case
    RAnd reqs2 -> ordRequirements reqs1 reqs2
    y -> compareIndexM x y
  ROr reqs1 -> \case
    ROr reqs2 -> ordRequirements reqs1 reqs2
    y -> compareIndexM x y
  Tapped perm1 -> \case
    Tapped perm2 -> ordWPermanent perm1 perm2
    y -> compareIndexM x y
  R2 reqsA1 reqsB1 -> \case
    R2 reqsA2 reqsB2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2,
          ordRequirements reqsB1 reqsB2
        ]
    y -> compareIndexM x y
  R3 reqsA1 reqsB1 reqsC1 -> \case
    R3 reqsA2 reqsB2 reqsC2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2,
          ordRequirements reqsB1 reqsB2,
          ordRequirements reqsC1 reqsC2
        ]
    y -> compareIndexM x y
  R4 reqsA1 reqsB1 reqsC1 reqsD1 -> \case
    R4 reqsA2 reqsB2 reqsC2 reqsD2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2,
          ordRequirements reqsB1 reqsB2,
          ordRequirements reqsC1 reqsC2,
          ordRequirements reqsD1 reqsD2
        ]
    y -> compareIndexM x y
  R5 reqsA1 reqsB1 reqsC1 reqsD1 reqsE1 -> \case
    R5 reqsA2 reqsB2 reqsC2 reqsD2 reqsE2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2,
          ordRequirements reqsB1 reqsB2,
          ordRequirements reqsC1 reqsC2,
          ordRequirements reqsD1 reqsD2,
          ordRequirements reqsE1 reqsE2
        ]
    y -> compareIndexM x y

ordRequirements :: [Requirement a] -> [Requirement a] -> EnvM Ordering
ordRequirements = listM ordRequirement

ordSelection :: Selection -> Selection -> EnvM Ordering
ordSelection x = case x of
  Choose -> \case
    Choose -> pure EQ
    y -> compareIndexM x y
  Target -> \case
    Target -> pure EQ
    y -> compareIndexM x y

ordSetCard :: SetCard ot -> SetCard ot -> EnvM Ordering
ordSetCard = \case
  SetCard set1 rarity1 card1 -> \case
    SetCard set2 rarity2 card2 ->
      seqM
        [ pure $ compare set1 set2,
          pure $ compare rarity1 rarity2,
          ordCard card1 card2
        ]

ordSetToken :: SetToken ot -> SetToken ot -> EnvM Ordering
ordSetToken = \case
  SetToken set1 rarity1 token1 -> \case
    SetToken set2 rarity2 token2 ->
      seqM
        [ pure $ compare set1 set2,
          pure $ compare rarity1 rarity2,
          ordToken token1 token2
        ]

ordStaticAbility :: StaticAbility ot -> StaticAbility ot -> EnvM Ordering
ordStaticAbility x = case x of
  As electListener1 -> \case
    As electListener2 -> ordElectE electListener1 electListener2
    y -> compareIndexM x y
  StaticContinuous elect1 -> \case
    StaticContinuous elect2 -> ordElectE elect1 elect2
    y -> compareIndexM x y
  FirstStrike -> \case
    FirstStrike -> pure EQ
    y -> compareIndexM x y
  Flying -> \case
    Flying -> pure EQ
    y -> compareIndexM x y
  Haste -> \case
    Haste -> pure EQ
    y -> compareIndexM x y
  Suspend duration1 elect1 -> \case
    Suspend duration2 elect2 -> seqM [pure $ compare duration1 duration2, ordElectE elect1 elect2]
    y -> compareIndexM x y

ordTimePoint :: TimePoint p -> TimePoint p -> EnvM Ordering
ordTimePoint x = case x of
  PhaseBegin phase1 -> \case
    PhaseBegin phase2 -> pure $ compare phase1 phase2
    y -> compareIndexM x y
  PhaseEnd phase1 -> \case
    PhaseEnd phase2 -> pure $ compare phase1 phase2
    y -> compareIndexM x y
  StepBegin step1 -> \case
    StepBegin step2 -> pure $ compare step1 step2
    y -> compareIndexM x y
  StepEnd step1 -> \case
    StepEnd step2 -> pure $ compare step1 step2
    y -> compareIndexM x y

ordToken :: Token ot -> Token ot -> EnvM Ordering
ordToken = \case
  Token card1 -> \case
    Token card2 -> ordCard card1 card2

ordTriggeredAbility :: forall ot. TriggeredAbility ot -> TriggeredAbility ot -> EnvM Ordering
ordTriggeredAbility = \case
  When listener1 -> \case
    When listener2 -> ordElectE @EventListener @ot listener1 listener2

ordWithLinkedCard ::
  Typeable x =>
  (x ot -> x ot -> EnvM Ordering) ->
  WithLinkedCard 'LibraryZone x ot ->
  WithLinkedCard 'LibraryZone x ot ->
  EnvM Ordering
ordWithLinkedCard ordM x = case x of
  LcProxy reqs1 -> \case
    LcProxy reqs2 -> ordRequirements reqs1 reqs2
    y -> compareIndexM x y
  Lc1 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    Lc1 NonProxyElectEffectOneShot reqs2 cont2 -> ordCardO1 LibraryCard ordM reqs1 reqs2 cont1 cont2
  Lc2 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    Lc2 NonProxyElectEffectOneShot reqs2 cont2 -> ordCardO2 LibraryCard ordM reqs1 reqs2 cont1 cont2
  Lc3 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    Lc3 NonProxyElectEffectOneShot reqs2 cont2 -> ordCardO3 LibraryCard ordM reqs1 reqs2 cont1 cont2
  Lc4 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    Lc4 NonProxyElectEffectOneShot reqs2 cont2 -> ordCardO4 LibraryCard ordM reqs1 reqs2 cont1 cont2
  Lc5 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    Lc5 NonProxyElectEffectOneShot reqs2 cont2 -> ordCardO5 LibraryCard ordM reqs1 reqs2 cont1 cont2

ordWithLinkedObject ::
  Typeable x =>
  (x ot -> x ot -> EnvM Ordering) ->
  WithLinkedObject x ot ->
  WithLinkedObject x ot ->
  EnvM Ordering
ordWithLinkedObject ordM x = case x of
  LProxy reqs1 -> \case
    LProxy reqs2 -> ordRequirements reqs1 reqs2
    y -> compareIndexM x y
  L1 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    L1 NonProxyElectEffectOneShot reqs2 cont2 -> ordObjectO1 ordM reqs1 reqs2 cont1 cont2
  L2 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    L2 NonProxyElectEffectOneShot reqs2 cont2 -> ordObjectO2 ordM reqs1 reqs2 cont1 cont2
  L3 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    L3 NonProxyElectEffectOneShot reqs2 cont2 -> ordObjectO3 ordM reqs1 reqs2 cont1 cont2
  L4 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    L4 NonProxyElectEffectOneShot reqs2 cont2 -> ordObjectO4 ordM reqs1 reqs2 cont1 cont2
  L5 NonProxyElectEffectOneShot reqs1 cont1 -> \case
    L5 NonProxyElectEffectOneShot reqs2 cont2 -> ordObjectO5 ordM reqs1 reqs2 cont1 cont2

-- ordWithMaskedLibraryCardElectE ::
--   WithMaskedCard 'LibraryZone (Elect e ot) ->
--   WithMaskedCard 'LibraryZone (Elect e ot) ->
--   EnvM Ordering
-- ordWithMaskedLibraryCardElectE x = case x of
--   Mc1 reqs1 cont1 -> \case
--     Mc1 reqs2 cont2 -> ordCardO1 ordM reqs1 reqs2 cont1 cont2
--     y -> pure $ compare (consIndex x) (consIndex y)
--   Mc2 reqs1 cont1 -> \case
--     Mc2 reqs2 cont2 -> ordCardO2 ordM reqs1 reqs2 cont1 cont2
--     y -> pure $ compare (consIndex x) (consIndex y)
--   Mc3 reqs1 cont1 -> \case
--     Mc3 reqs2 cont2 -> ordCardO3 ordM reqs1 reqs2 cont1 cont2
--     y -> pure $ compare (consIndex x) (consIndex y)
--   Mc4 reqs1 cont1 -> \case
--     Mc4 reqs2 cont2 -> ordCardO4 ordM reqs1 reqs2 cont1 cont2
--     y -> pure $ compare (consIndex x) (consIndex y)
--   Mc5 reqs1 cont1 -> \case
--     Mc5 reqs2 cont2 -> ordCardO5 ordM reqs1 reqs2 cont1 cont2
--     y -> pure $ compare (consIndex x) (consIndex y)
--   where
--     ordM = ordElectE

ordWithMaskedObjectElectE ::
  WithMaskedObject (Elect e ot) ->
  WithMaskedObject (Elect e ot) ->
  EnvM Ordering
ordWithMaskedObjectElectE x = case x of
  M1 reqs1 cont1 -> \case
    M1 reqs2 cont2 -> ordObjectO1 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  M2 reqs1 cont1 -> \case
    M2 reqs2 cont2 -> ordObjectO2 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  M3 reqs1 cont1 -> \case
    M3 reqs2 cont2 -> ordObjectO3 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  M4 reqs1 cont1 -> \case
    M4 reqs2 cont2 -> ordObjectO4 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  M5 reqs1 cont1 -> \case
    M5 reqs2 cont2 -> ordObjectO5 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  where
    ordM = ordElectE

ordWithThisAbility ::
  WithThis Ability ot ->
  WithThis Ability ot ->
  EnvM Ordering
ordWithThisAbility = \case
  T1 cont1 -> \case
    T1 cont2 -> ordObjectO1 ordM reqs1 reqs2 cont1 cont2
  T2 cont1 -> \case
    T2 cont2 -> ordObjectO2 ordM reqs1 reqs2 cont1 cont2
  T3 cont1 -> \case
    T3 cont2 -> ordObjectO3 ordM reqs1 reqs2 cont1 cont2
  T4 cont1 -> \case
    T4 cont2 -> ordObjectO4 ordM reqs1 reqs2 cont1 cont2
  T5 cont1 -> \case
    T5 cont2 -> ordObjectO5 ordM reqs1 reqs2 cont1 cont2
  where
    ordM = ordAbility
    reqs1 = []
    reqs2 = []

ordWithThisCardTypeDef ::
  Typeable tribal =>
  WithThis (CardTypeDef tribal) ot ->
  WithThis (CardTypeDef tribal) ot ->
  EnvM Ordering
ordWithThisCardTypeDef = \case
  T1 cont1 -> \case
    T1 cont2 -> ordObjectO1 ordM reqs1 reqs2 cont1 cont2
  T2 cont1 -> \case
    T2 cont2 -> ordObjectO2 ordM reqs1 reqs2 cont1 cont2
  T3 cont1 -> \case
    T3 cont2 -> ordObjectO3 ordM reqs1 reqs2 cont1 cont2
  T4 cont1 -> \case
    T4 cont2 -> ordObjectO4 ordM reqs1 reqs2 cont1 cont2
  T5 cont1 -> \case
    T5 cont2 -> ordObjectO5 ordM reqs1 reqs2 cont1 cont2
  where
    ordM = ordCardTypeDef
    reqs1 = []
    reqs2 = []

ordW2 ::
  forall witness ot1 a1 b1 ot2 a2 b2.
  ( Typeable witness,
    ot1 ~ OT2 a1 b1,
    ot2 ~ OT2 a2 b2,
    TypeableOT ot1,
    TypeableOT ot2,
    Inst2 IsObjectType a1 b1,
    Inst2 IsObjectType a2 b2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW2 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 wit2
  Just {} -> pure EQ

ordW3 ::
  forall witness ot1 a1 b1 c1 ot2 a2 b2 c2.
  ( Typeable witness,
    ot1 ~ OT3 a1 b1 c1,
    ot2 ~ OT3 a2 b2 c2,
    TypeableOT ot1,
    TypeableOT ot2,
    Inst3 IsObjectType a1 b1 c1,
    Inst3 IsObjectType a2 b2 c2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW3 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 wit2
  Just {} -> pure EQ

ordW4 ::
  forall witness ot1 a1 b1 c1 d1 ot2 a2 b2 c2 d2.
  ( Typeable witness,
    ot1 ~ OT4 a1 b1 c1 d1,
    ot2 ~ OT4 a2 b2 c2 d2,
    TypeableOT ot1,
    TypeableOT ot2,
    Inst4 IsObjectType a1 b1 c1 d1,
    Inst4 IsObjectType a2 b2 c2 d2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW4 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 wit2
  Just {} -> pure EQ

ordW5 ::
  forall witness ot1 a1 b1 c1 d1 e1 ot2 a2 b2 c2 d2 e2.
  ( Typeable witness,
    ot1 ~ OT5 a1 b1 c1 d1 e1,
    ot2 ~ OT5 a2 b2 c2 d2 e2,
    TypeableOT ot1,
    TypeableOT ot2,
    Inst5 IsObjectType a1 b1 c1 d1 e1,
    Inst5 IsObjectType a2 b2 c2 d2 e2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW5 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 wit2
  Just {} -> pure EQ

ordW6 ::
  forall witness ot1 a1 b1 c1 d1 e1 f1 ot2 a2 b2 c2 d2 e2 f2.
  ( Typeable witness,
    ot1 ~ OT6 a1 b1 c1 d1 e1 f1,
    ot2 ~ OT6 a2 b2 c2 d2 e2 f2,
    TypeableOT ot1,
    TypeableOT ot2,
    Inst6 IsObjectType a1 b1 c1 d1 e1 f1,
    Inst6 IsObjectType a2 b2 c2 d2 e2 f2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW6 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 wit2
  Just {} -> pure EQ

ordWAny :: WAny ot -> WAny ot -> EnvM Ordering
ordWAny = \case
  WAnyArtifact -> \case
    WAnyArtifact -> pure EQ
  WAnyCreature -> \case
    WAnyCreature -> pure EQ
  WAnyEnchantment -> \case
    WAnyEnchantment -> pure EQ
  WAnyInstant -> \case
    WAnyInstant -> pure EQ
  WAnyLand -> \case
    WAnyLand -> pure EQ
  WAnyPlaneswalker -> \case
    WAnyPlaneswalker -> pure EQ
  WAnyPlayer -> \case
    WAnyPlayer -> pure EQ
  WAnySorcery -> \case
    WAnySorcery -> pure EQ
  WAny -> \case
    WAny -> pure EQ
  x@WAny2 -> \case
    y@WAny2 -> ordW2 x y
  x@WAny3 -> \case
    y@WAny3 -> ordW3 x y
  x@WAny4 -> \case
    y@WAny4 -> ordW4 x y
  x@WAny5 -> \case
    y@WAny5 -> ordW5 x y
  x@WAny6 -> \case
    y@WAny6 -> ordW6 x y

ordWCard :: WCard ot -> WCard ot -> EnvM Ordering
ordWCard = \case
  WCardArtifact -> \case
    WCardArtifact -> pure EQ
  WCardCreature -> \case
    WCardCreature -> pure EQ
  WCardEnchantment -> \case
    WCardEnchantment -> pure EQ
  WCardInstant -> \case
    WCardInstant -> pure EQ
  WCardLand -> \case
    WCardLand -> pure EQ
  WCardPlaneswalker -> \case
    WCardPlaneswalker -> pure EQ
  WCardSorcery -> \case
    WCardSorcery -> pure EQ
  WCard -> \case
    WCard -> pure EQ
  x@WCard2 -> \case
    y@WCard2 -> ordW2 x y
  x@WCard3 -> \case
    y@WCard3 -> ordW3 x y

ordWNonCreatureCard :: WNonCreatureCard ot -> WNonCreatureCard ot -> EnvM Ordering
ordWNonCreatureCard = \case
  WNonCreatureArtifact -> \case
    WNonCreatureArtifact -> pure EQ
  WNonCreatureEnchantment -> \case
    WNonCreatureEnchantment -> pure EQ
  WNonCreatureInstant -> \case
    WNonCreatureInstant -> pure EQ
  WNonCreatureLand -> \case
    WNonCreatureLand -> pure EQ
  WNonCreaturePlaneswalker -> \case
    WNonCreaturePlaneswalker -> pure EQ
  WNonCreatureSorcery -> \case
    WNonCreatureSorcery -> pure EQ
  WNonCreatureCard -> \case
    WNonCreatureCard -> pure EQ
  x@WNonCreatureCard2 -> \case
    y@WNonCreatureCard2 -> ordW2 x y
  x@WNonCreatureCard3 -> \case
    y@WNonCreatureCard3 -> ordW3 x y

ordWPermanent :: WPermanent ot -> WPermanent ot -> EnvM Ordering
ordWPermanent = \case
  WPermanentArtifact -> \case
    WPermanentArtifact -> pure EQ
  WPermanentCreature -> \case
    WPermanentCreature -> pure EQ
  WPermanentEnchantment -> \case
    WPermanentEnchantment -> pure EQ
  WPermanentLand -> \case
    WPermanentLand -> pure EQ
  WPermanentPlaneswalker -> \case
    WPermanentPlaneswalker -> pure EQ
  WPermanent -> \case
    WPermanent -> pure EQ
  x@WPermanent2 -> \case
    y@WPermanent2 -> ordW2 x y
  x@WPermanent3 -> \case
    y@WPermanent3 -> ordW3 x y
  x@WPermanent4 -> \case
    y@WPermanent4 -> ordW4 x y

ordWSpell :: WSpell ot -> WSpell ot -> EnvM Ordering
ordWSpell = \case
  WSpellArtifact -> \case
    WSpellArtifact -> pure EQ
  WSpellCreature -> \case
    WSpellCreature -> pure EQ
  WSpellEnchantment -> \case
    WSpellEnchantment -> pure EQ
  WSpellInstant -> \case
    WSpellInstant -> pure EQ
  WSpellPlaneswalker -> \case
    WSpellPlaneswalker -> pure EQ
  WSpellSorcery -> \case
    WSpellSorcery -> pure EQ
  WSpell -> \case
    WSpell -> pure EQ
  x@WSpell2 -> \case
    y@WSpell2 -> ordW2 x y
  x@WSpell3 -> \case
    y@WSpell3 -> ordW3 x y
  x@WSpell4 -> \case
    y@WSpell4 -> ordW4 x y

ordZoneCard :: ZoneCard zone ot -> ZoneCard zone ot -> EnvM Ordering
ordZoneCard x = case x of
  LibraryCard objN1 -> \case
    LibraryCard objN2 -> ordObjectN objN1 objN2

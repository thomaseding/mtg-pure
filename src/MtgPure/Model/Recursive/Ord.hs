{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Recursive.Ord (
  EnvM,
  runEnvM,
  seqM,
  listM,
  compareIndexM,
  compareOT,
  compareZone,
  compareZoneOT,
  --
  ordAbility,
  ordWithThisActivated,
  ordZoneObject,
) where

import safe qualified Control.Monad.State.Strict as State
import safe Data.ConsIndex (ConsIndex (consIndex))
import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Nat (Fin (..), IsNat (..), NatList (..))
import safe Data.Proxy (Proxy (Proxy))
import safe Data.Typeable (Typeable, cast, typeRep)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.Damage (Damage)
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaPool (ManaPool)
import safe MtgPure.Model.Object.IndexOT (IndexOT (indexOT))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTN (
  OT0,
  OT1,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
 )
import safe MtgPure.Model.Object.OTNAliases (OTNAny, OTNDamageSource)
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (..),
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (ObjectN (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Any (WAny (..))
import safe MtgPure.Model.Object.Singleton.Card (WCard (..))
import safe MtgPure.Model.Object.Singleton.NonCreatureCard (WNonCreatureCard (..))
import safe MtgPure.Model.Object.Singleton.Permanent (WPermanent (..))
import safe MtgPure.Model.Object.Singleton.Spell (WSpell (..))
import safe MtgPure.Model.Object.ToObjectN.Classes (ToObject1' (toObject1'))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Object.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.PrePost (IsPrePost (..), PrePost (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  CardFacet (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  Else (..),
  Enchant (..),
  EnchantmentType (..),
  Event,
  EventListener,
  EventListener' (..),
  IsUser,
  List (..),
  NonProxy (..),
  Requirement (..),
  SetCard (..),
  SetToken (SetToken),
  StaticAbility (..),
  Token (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisActivated,
  WithThisTriggered,
  YourCardFacet (..),
 )
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Variable (
  Variable (..),
  VariableId,
  VariableId' (..),
 )
import safe MtgPure.Model.Zone (IsZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOT, IsZO, ZO, ZoneObject (..), toZone)

----------------------------------------

instance Eq (Ability ot) where
  (==) x y = runEnvM (ordAbility x y) == EQ

instance Eq AnyCard where
  (==) x y = runEnvM (ordAnyCard x y) == EQ

instance Eq AnyToken where
  (==) x y = runEnvM (ordAnyToken x y) == EQ

instance Eq (Card ot) where
  (==) x y = runEnvM (ordCard x y) == EQ

instance Eq (CardFacet ot) where
  (==) x y = runEnvM (ordCardFacet x y) == EQ

instance Eq Condition where
  (==) x y = runEnvM (ordCondition x y) == EQ

instance IndexOT ot => Eq (Cost ot) where
  (==) x y = runEnvM (ordCost x y) == EQ

instance Typeable ef => Eq (Effect ef) where
  (==) x y = runEnvM (ordEffect x y) == EQ

instance (Typeable el, Typeable p, IsOT ot) => Eq (Elect p el ot) where
  (==) x y = runEnvM (ordElectEl x y) == EQ

instance Eq EventListener where
  (==) x y = runEnvM (ordEventListener x y) == EQ

instance Eq (ObjectN ot) where
  (==) x y = runEnvM (ordObjectN x y) == EQ

instance IndexOT ot => Eq (Requirement zone ot) where
  (==) x y = runEnvM (ordRequirement x y) == EQ

instance Eq (SetCard ot) where
  (==) x y = runEnvM (ordSetCard x y) == EQ

instance Eq (SetToken ot) where
  (==) x y = runEnvM (ordSetToken x y) == EQ

instance Eq (StaticAbility zone ot) where
  (==) x y = runEnvM (ordStaticAbility x y) == EQ

instance Eq (TimePoint p) where
  (==) x y = runEnvM (ordTimePoint x y) == EQ

instance Eq (Token ot) where
  (==) x y = runEnvM (ordToken x y) == EQ

instance IndexOT ot => Eq (TriggeredAbility zone ot) where
  (==) x y = runEnvM (ordTriggeredAbility x y) == EQ

instance (Typeable el, Typeable p, IsZO zone ot) => Eq (WithMaskedObject zone (Elect p el ot)) where
  (==) x y = runEnvM (ordWithMaskedObjectElectEl x y) == EQ

instance IsZO zone ot => Eq (WithThis zone Ability ot) where
  (==) x y = runEnvM (ordWithThis ordAbility x y) == EQ

instance IsZO zone ot => Eq (WithThisActivated zone ot) where
  (==) x y = runEnvM (ordWithThisActivated x y) == EQ

instance IsZO zone ot => Eq (WithThisTriggered zone ot) where
  (==) x y = runEnvM (ordWithThisTriggered x y) == EQ

instance Eq (WAny ot) where
  (==) x y = runEnvM (ordWAny x y) == EQ

instance Eq (WNonCreatureCard ot) where
  (==) x y = runEnvM (ordWNonCreatureCard x y) == EQ

instance Eq (WPermanent ot) where
  (==) x y = runEnvM (ordWPermanent x y) == EQ

instance Eq (ZoneObject zone ot) where
  (==) x y = runEnvM (ordZoneObject x y) == EQ

----------------------------------------

instance Ord (Ability ot) where
  compare x y = runEnvM (ordAbility x y)

instance Ord AnyCard where
  compare x y = runEnvM (ordAnyCard x y)

instance Ord AnyToken where
  compare x y = runEnvM (ordAnyToken x y)

instance Ord (Card ot) where
  compare x y = runEnvM (ordCard x y)

instance Ord (CardFacet ot) where
  compare x y = runEnvM (ordCardFacet x y)

instance Ord Condition where
  compare x y = runEnvM (ordCondition x y)

instance IndexOT ot => Ord (Cost ot) where
  compare x y = runEnvM (ordCost x y)

instance Typeable ef => Ord (Effect ef) where
  compare x y = runEnvM (ordEffect x y)

instance (Typeable el, Typeable p, IsOT ot) => Ord (Elect p el ot) where
  compare x y = runEnvM (ordElectEl x y)

instance Ord EventListener where
  compare x y = runEnvM (ordEventListener x y)

instance Ord (ObjectN ot) where
  compare x y = runEnvM (ordObjectN x y)

instance IndexOT ot => Ord (Requirement zone ot) where
  compare x y = runEnvM (ordRequirement x y)

instance Ord (SetCard ot) where
  compare x y = runEnvM (ordSetCard x y)

instance Ord (SetToken ot) where
  compare x y = runEnvM (ordSetToken x y)

instance Ord (StaticAbility zone ot) where
  compare x y = runEnvM (ordStaticAbility x y)

instance Ord (TimePoint p) where
  compare x y = runEnvM (ordTimePoint x y)

instance Ord (Token ot) where
  compare x y = runEnvM (ordToken x y)

instance IndexOT ot => Ord (TriggeredAbility zone ot) where
  compare x y = runEnvM (ordTriggeredAbility x y)

instance (Typeable el, Typeable p, IsZO zone ot) => Ord (WithMaskedObject zone (Elect p el ot)) where
  compare x y = runEnvM (ordWithMaskedObjectElectEl x y)

instance IsZO zone ot => Ord (WithThis zone Ability ot) where
  compare x y = runEnvM (ordWithThis ordAbility x y)

instance IsZO zone ot => Ord (WithThisActivated zone ot) where
  compare x y = runEnvM (ordWithThisActivated x y)

instance IsZO zone ot => Ord (WithThisTriggered zone ot) where
  compare x y = runEnvM (ordWithThisTriggered x y)

instance Ord (WAny ot) where
  compare x y = runEnvM (ordWAny x y)

instance Ord (WNonCreatureCard ot) where
  compare x y = runEnvM (ordWNonCreatureCard x y)

instance Ord (WPermanent ot) where
  compare x y = runEnvM (ordWPermanent x y)

instance Ord (ZoneObject zone ot) where
  compare x y = runEnvM (ordZoneObject x y)

----------------------------------------

newtype EnvM a = EnvM {unEnvM :: State.State Env a}
  deriving (Functor)

instance Applicative EnvM where
  pure = EnvM . pure
  EnvM f <*> EnvM a = EnvM $ f <*> a

instance Monad EnvM where
  EnvM a >>= f = EnvM $ a >>= unEnvM . f

-- TODO: Smarts need to be added to handle Card loops.
-- For cards, rely on names being unique. (A linter tool can validate unique naming.)
-- For tokens prolly use some recursion limit or encode unique names/tags into tokens.
-- Add a toggle config to switch between unique naming technique and loop limit technique.
-- Both are useful. Unique naming is faster. The loop limiter can be used to implement linter.
-- Default Ord instances will rely on unique naming? Regardless this lower level `runEnvM` API
-- can be exported too for finely controlled behavior.
newtype Env = Env
  { nextRawId :: Int
  }

mkEnv :: Env
mkEnv =
  Env
    { nextRawId = 0
    }

runEnvM :: EnvM a -> a
runEnvM (EnvM m) = State.evalState m mkEnv

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

newVariableId :: EnvM VariableId
newVariableId = EnvM do
  raw <- State.gets nextRawId
  State.modify' \st -> st{nextRawId = nextRawId st + 1}
  pure $ VariableId raw

newObject :: forall a. IsObjectType a => EnvM (Object a)
newObject = EnvM do
  raw <- State.gets nextRawId
  let i = ObjectId raw
      obj = idToObject @a $ UntypedObject DefaultObjectDiscriminant i
  State.modify' \st -> st{nextRawId = nextRawId st + 1}
  pure obj

-- NOTE: The `Typeable (ObjectN ot)` not needed, but keep it for now to prove it's possible
newObjectN ::
  forall a ot.
  (Typeable ot, IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  EnvM (ObjectN ot)
newObjectN make = do
  obj <- newObject @a
  let objN = make obj
  pure objN

withObjectCont ::
  forall (a :: ObjectType) ot' x.
  (IsObjectType a, IsOT ot') =>
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

compareOT ::
  forall ot1 ot2.
  (IndexOT ot1, IndexOT ot2) =>
  EnvM Ordering
compareOT = pure $ compare (indexOT @ot1) (indexOT @ot2)

compareZone ::
  forall zone1 zone2.
  (IsZone zone1, IsZone zone2) =>
  EnvM Ordering
compareZone = pure $ compare (litZone @zone1) (litZone @zone2)

compareZoneOT ::
  forall zone1 zone2 ot1 ot2.
  (IsZone zone1, IsZone zone2) =>
  (IndexOT ot1, IndexOT ot2) =>
  EnvM Ordering
compareZoneOT = pure $ compare (litZone @zone1, indexOT @ot1) (litZone @zone2, indexOT @ot2)

lenseList :: List x -> x
lenseList = \case
  List [x] -> x
  _ -> error "logic error: should not happen by construction"

----------------------------------------

ordAbility :: Ability ot -> Ability ot -> EnvM Ordering
ordAbility x = case x of
  Activated ability1 -> \case
    Activated ability2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithThisActivated zone1 ot1 ->
            WithThisActivated zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast ability2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just ability2 -> ordWithThisActivated ability1 ability2
       in go ability1 ability2
    y -> compareIndexM x y
  Static ability1 -> \case
    Static ability2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZone zone1 =>
            IsZone zone2 =>
            IndexOT ot1 =>
            IndexOT ot2 =>
            StaticAbility zone1 ot1 ->
            StaticAbility zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast ability2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just ability2 -> ordStaticAbility ability1 ability2
       in go ability1 ability2
    y -> compareIndexM x y
  Triggered ability1 -> \case
    Triggered ability2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithThisTriggered zone1 ot1 ->
            WithThisTriggered zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast ability2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just ability2 -> ordWithThisTriggered ability1 ability2
       in go ability1 ability2
    y -> compareIndexM x y

ordAbilities :: IsOT ot => [Ability ot] -> [Ability ot] -> EnvM Ordering
ordAbilities = listM ordAbility

ordActivatedAbility :: ActivatedAbility zone ot -> ActivatedAbility zone ot -> EnvM Ordering
ordActivatedAbility x = case x of
  Ability cost1 effect1 -> \case
    Ability cost2 effect2 -> seqM [ordCost cost1 cost2, ordElectEl effect1 effect2]

ordAnyCard :: AnyCard -> AnyCard -> EnvM Ordering
ordAnyCard x = case x of
  AnyCard1 card1 -> \case
    AnyCard1 card2 ->
      let go :: forall ot1 ot2. (IsOT ot1, IsOT ot2) => Card ot1 -> Card ot2 -> EnvM Ordering
          go _ _ = case cast card2 of
            Nothing -> compareOT @ot1 @ot2
            Just card2 -> ordCard card1 card2
       in go card1 card2
    y -> compareIndexM x y
  AnyCard2 card1 -> \case
    AnyCard2 card2 ->
      let go ::
            forall ot1a ot1b ot2a ot2b.
            (IsOT ot1a, IsOT ot1b, IsOT ot2a, IsOT ot2b) =>
            Card (ot1a, ot1b) ->
            Card (ot2a, ot2b) ->
            EnvM Ordering
          go _ _ = case cast card2 of
            Nothing -> seqM [compareOT @ot1a @ot2a, compareOT @ot1b @ot2b]
            Just card2 -> ordCard card1 card2
       in go card1 card2
    y -> compareIndexM x y

ordAnyToken :: AnyToken -> AnyToken -> EnvM Ordering
ordAnyToken = \case
  AnyToken token1 -> \case
    AnyToken token2 ->
      let go :: forall ot1 ot2. (IsOT ot1, IsOT ot2) => Token ot1 -> Token ot2 -> EnvM Ordering
          go _ _ = case cast token2 of
            Nothing -> compareOT @ot1 @ot2
            Just token2 -> ordToken token1 token2
       in go token1 token2

ordCard :: Card ot -> Card ot -> EnvM Ordering
ordCard x = case x of
  Card name1 yourCard1 -> \case
    Card name2 yourCard2 -> do
      seqM
        [ pure $ compare name1 name2
        , ordYourCard yourCard1 yourCard2
        ]
  DoubleSidedCard card1a card1b -> \case
    DoubleSidedCard card2a card2b ->
      seqM
        [ ordCard card1a card2a
        , ordCard card1b card2b
        ]
    y -> compareIndexM x y
  SplitCard card1a card1b fuse1 -> \case
    SplitCard card2a card2b fuse2 ->
      seqM
        [ ordCard card1a card2a
        , ordCard card1b card2b
        , pure $ compare fuse1 fuse2
        ]
    y -> compareIndexM x y

ordCardFacet :: CardFacet ot -> CardFacet ot -> EnvM Ordering
ordCardFacet = \case
  ArtifactFacet colors1 cost1 artTypes1 creatTypes1 abilities1 -> \case
    ArtifactFacet colors2 cost2 artTypes2 creatTypes2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare artTypes1 artTypes2
        , pure $ compare creatTypes1 creatTypes2
        , ordAbilities abilities1 abilities2
        ]
  ArtifactCreatureFacet colors1 cost1 artTypes1 creatTypes1 power1 toughness1 artAbils1 creatAbils1 bothAbils1 ->
    \case
      ArtifactCreatureFacet colors2 cost2 artTypes2 creatTypes2 power2 toughness2 artAbils2 creatAbils2 bothAbils2 ->
        seqM
          [ ordColors colors1 colors2
          , ordCost cost1 cost2
          , pure $ compare artTypes1 artTypes2
          , pure $ compare creatTypes1 creatTypes2
          , pure $ compare power1 power2
          , pure $ compare toughness1 toughness2
          , ordAbilities artAbils1 artAbils2
          , ordAbilities creatAbils1 creatAbils2
          , ordAbilities bothAbils1 bothAbils2
          ]
  ArtifactLandFacet artTypes1 creatTypes1 landTypes1 artAbils1 landAbils1 bothAbils1 ->
    \case
      ArtifactLandFacet artTypes2 creatTypes2 landTypes2 artAbils2 landAbils2 bothAbils2 ->
        seqM
          [ pure $ compare artTypes1 artTypes2
          , pure $ compare creatTypes1 creatTypes2
          , pure $ compare landTypes1 landTypes2
          , ordAbilities artAbils1 artAbils2
          , ordAbilities landAbils1 landAbils2
          , ordAbilities bothAbils1 bothAbils2
          ]
  CreatureFacet colors1 cost1 creatTypes1 power1 toughness1 abilities1 -> \case
    CreatureFacet colors2 cost2 creatTypes2 power2 toughness2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare creatTypes1 creatTypes2
        , pure $ compare power1 power2
        , pure $ compare toughness1 toughness2
        , ordAbilities abilities1 abilities2
        ]
  EnchantmentFacet colors1 cost1 creatTypes1 enchantTypes1 abilities1 -> \case
    EnchantmentFacet colors2 cost2 creatTypes2 enchantTypes2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare creatTypes1 creatTypes2
        , ordEnchantmentTypes enchantTypes1 enchantTypes2
        , ordAbilities abilities1 abilities2
        ]
  EnchantmentCreatureFacet colors1 cost1 creatTypes1 enchantTypes1 power1 toughness1 creatAbils1 enchAbils1 bothAbils1 ->
    \case
      EnchantmentCreatureFacet colors2 cost2 creatTypes2 enchantTypes2 power2 toughness2 creatAbils2 enchAbils2 bothAbils2 ->
        seqM
          [ ordColors colors1 colors2
          , ordCost cost1 cost2
          , pure $ compare creatTypes1 creatTypes2
          , ordEnchantmentTypes enchantTypes1 enchantTypes2
          , pure $ compare power1 power2
          , pure $ compare toughness1 toughness2
          , ordAbilities creatAbils1 creatAbils2
          , ordAbilities enchAbils1 enchAbils2
          , ordAbilities bothAbils1 bothAbils2
          ]
  InstantFacet colors1 cost1 creatTypes1 abilities1 effect1 -> \case
    InstantFacet colors2 cost2 creatTypes2 abilities2 effect2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare creatTypes1 creatTypes2
        , ordAbilities abilities1 abilities2
        , ordWithThis ordElectEl effect1 effect2
        ]
  LandFacet creatTypes1 landTypes1 abilities1 -> \case
    LandFacet creatTypes2 landTypes2 abilities2 ->
      seqM
        [ pure $ compare creatTypes1 creatTypes2
        , pure $ compare landTypes1 landTypes2
        , ordAbilities abilities1 abilities2
        ]
  PlaneswalkerFacet colors1 cost1 loyalty1 abilities1 -> \case
    PlaneswalkerFacet colors2 cost2 loyalty2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare loyalty1 loyalty2
        , ordAbilities abilities1 abilities2
        ]
  SorceryFacet colors1 cost1 creatTypes1 abilities1 effect1 -> \case
    SorceryFacet colors2 cost2 creatTypes2 abilities2 effect2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare creatTypes1 creatTypes2
        , ordAbilities abilities1 abilities2
        , ordWithThis ordElectEl effect1 effect2
        ]

ordCase :: Typeable x => (x -> x -> EnvM Ordering) -> Case x -> Case x -> EnvM Ordering
ordCase ordX x = case x of
  CaseFin varFin1 natList1 -> \case
    CaseFin varFin2 natList2 ->
      let go ::
            forall u1 u2 n1 n2.
            IsUser u1 =>
            IsUser u2 =>
            (IsNat n1, IsNat n2) =>
            Variable (Fin u1 n1) ->
            Variable (Fin u2 n2) ->
            EnvM Ordering
          go _ _ = case cast (varFin2, natList2) of
            Nothing ->
              seqM
                [ pure $ compare (litNat @n1) (litNat @n2)
                , pure $ compare (typeRep (Proxy @u1)) (typeRep (Proxy @u2))
                ]
            Just (varFin2, natList2) ->
              seqM
                [ ordVariable varFin1 varFin2
                , ordNatList ordX natList1 natList2
                ]
       in go varFin1 varFin2

ordColor :: Color -> Color -> EnvM Ordering
ordColor c1 c2 = pure $ compare c1 c2

class OrdColors colors where
  ordColors :: colors -> colors -> EnvM Ordering

instance OrdColors [Color] where
  ordColors = listM ordColor

instance OrdColors Colors where
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
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (any2, obj2, reqs2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (any2, obj2, reqs2) ->
              seqM
                [ ordWAny any1 any2
                , ordZoneObject obj1 obj2
                , ordRequirements reqs1 reqs2
                ]
       in go obj1 obj2
    y -> compareIndexM x y

ordConditions :: [Condition] -> [Condition] -> EnvM Ordering
ordConditions = listM ordCondition

ordCost :: IndexOT ot => Cost ot -> Cost ot -> EnvM Ordering
ordCost x = case x of
  AndCosts costs1 -> \case
    AndCosts costs2 -> ordCosts costs1 costs2
    y -> compareIndexM x y
  CostCase case1 -> \case
    CostCase case2 -> ordCase ordCost case1 case2
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
    SacrificeCost perm2 reqs2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            [Requirement zone1 ot1] ->
            [Requirement zone2 ot2] ->
            EnvM Ordering
          go _ _ = case cast (perm2, reqs2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (perm2, reqs2) -> seqM [ordWPermanent perm1 perm2, ordRequirements reqs1 reqs2]
       in go reqs1 reqs2
    y -> compareIndexM x y
  TapCost reqs1 -> \case
    TapCost reqs2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            [Requirement zone1 ot1] ->
            [Requirement zone2 ot2] ->
            EnvM Ordering
          go _ _ = case cast reqs2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just reqs2 -> ordRequirements reqs1 reqs2
       in go reqs1 reqs2
    y -> compareIndexM x y

ordCosts :: IndexOT ot => [Cost ot] -> [Cost ot] -> EnvM Ordering
ordCosts = listM ordCost

ordDamage :: Damage var -> Damage var -> EnvM Ordering
ordDamage x y = pure $ compare x y

ordEffect :: forall ef. Typeable ef => Effect ef -> Effect ef -> EnvM Ordering
ordEffect x = case x of
  AddMana player1 mana1 -> \case
    AddMana player2 mana2 ->
      seqM [ordZoneObject player1 player2, ordManaPool mana1 mana2]
    y -> compareIndexM x y
  AddToBattlefield perm1 player1 token1 -> \case
    AddToBattlefield perm2 player2 token2 ->
      let go ::
            forall ot1 ot2.
            (IsOT ot1, IsOT ot2) =>
            WPermanent ot1 ->
            WPermanent ot2 ->
            EnvM Ordering
          go _ _ = case cast (perm2, token2) of
            Nothing -> compareOT @ot1 @ot2
            Just (perm2, token2) ->
              seqM
                [ ordWPermanent perm1 perm2
                , ordZoneObject player1 player2
                , ordToken token1 token2
                ]
       in go perm1 perm2
    y -> compareIndexM x y
  CantBeRegenerated creature1 -> \case
    CantBeRegenerated creature2 -> ordZoneObject creature1 creature2
    y -> compareIndexM x y
  ChangeTo perm1 obj1 card1 -> \case
    ChangeTo perm2 obj2 card2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (perm2, card2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (perm2, card2) ->
              seqM
                [ ordWPermanent perm1 perm2
                , ordZoneObject obj1 obj2
                , ordCard card1 card2
                ]
       in go obj1 obj2
    y -> compareIndexM x y
  CounterAbility ability1 -> \case
    CounterAbility ability2 ->
      ordZoneObject ability1 ability2
    y -> compareIndexM x y
  CounterSpell spell1 -> \case
    CounterSpell spell2 -> ordZoneObject spell1 spell2
    y -> compareIndexM x y
  DealDamage source1 victim1 damage1 -> \case
    DealDamage source2 victim2 damage2 ->
      let go ::
            forall zone1 zone2.
            IsZO zone1 OTNDamageSource =>
            IsZO zone2 OTNDamageSource =>
            ZO zone1 OTNDamageSource ->
            ZO zone2 OTNDamageSource ->
            EnvM Ordering
          go _ _ = case cast source2 of
            Nothing -> compareZone @zone1 @zone2
            Just source2 ->
              seqM
                [ ordZoneObject source1 source2
                , ordZoneObject victim1 victim2
                , ordDamage damage1 damage2
                ]
       in go source1 source2
    y -> compareIndexM x y
  Destroy victim1 -> \case
    Destroy victim2 -> ordZoneObject victim1 victim2
    y -> compareIndexM x y
  DrawCards player1 amount1 -> \case
    DrawCards player2 amount2 ->
      seqM [pure $ compare amount1 amount2, ordZoneObject player1 player2]
    y -> compareIndexM x y
  EffectContinuous effect1 -> \case
    EffectContinuous effect2 -> ordEffect effect1 effect2
    y -> compareIndexM x y
  EffectCase case1 -> \case
    EffectCase case2 -> ordCase ordEffect case1 case2
    y -> compareIndexM x y
  GainAbility any1 obj1 ability1 -> \case
    GainAbility any2 obj2 ability2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (any2, obj2, ability2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (any2, obj2, ability2) ->
              seqM
                [ ordWAny any1 any2
                , ordZoneObject obj1 obj2
                , ordAbility ability1 ability2
                ]
       in go obj1 obj2
    y -> compareIndexM x y
  LoseAbility any1 obj1 ability1 -> \case
    LoseAbility any2 obj2 ability2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (any2, obj2, ability2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (any2, obj2, ability2) ->
              seqM
                [ ordWAny any1 any2
                , ordZoneObject obj1 obj2
                , ordAbility ability1 ability2
                ]
       in go obj1 obj2
    y -> compareIndexM x y
  PutOntoBattlefield wPerm1 player1 card1 -> \case
    PutOntoBattlefield wPerm2 player2 card2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (wPerm2, card2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (wPerm2, card2) ->
              seqM
                [ ordWPermanent wPerm1 wPerm2
                , ordZoneObject player1 player2
                , ordZoneObject card1 card2
                ]
       in go card1 card2
    y -> compareIndexM x y
  Sacrifice perm1 player1 reqs1 -> \case
    Sacrifice perm2 player2 reqs2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            [Requirement zone1 ot1] ->
            [Requirement zone2 ot2] ->
            EnvM Ordering
          go _ _ = case cast (perm2, reqs2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (perm2, reqs2) ->
              seqM
                [ ordWPermanent perm1 perm2
                , ordZoneObject player1 player2
                , ordRequirements reqs1 reqs2
                ]
       in go reqs1 reqs2
    y -> compareIndexM x y
  SearchLibrary wCard1 player1 card1 -> \case
    SearchLibrary wCard2 player2 card2 ->
      let go ::
            forall ot1 ot2.
            IsOT ot1 =>
            IsOT ot2 =>
            WCard ot1 ->
            WCard ot2 ->
            EnvM Ordering
          go _ _ = case cast (wCard2, card2) of
            Nothing -> compareOT @ot1 @ot2
            Just (wCard2, card2) ->
              seqM
                [ ordWCard wCard1 wCard2
                , ordZoneObject player1 player2
                , ordWithLinkedObject ordElectEl card1 card2
                ]
       in go wCard1 wCard2
    y -> compareIndexM x y
  Sequence effects1 -> \case
    Sequence effects2 -> listM ordEffect effects1 effects2
    y -> compareIndexM x y
  ShuffleLibrary player1 -> \case
    ShuffleLibrary player2 -> ordZoneObject player1 player2
    y -> compareIndexM x y
  StatDelta creature1 power1 toughness1 -> \case
    StatDelta creature2 power2 toughness2 ->
      seqM
        [ ordZoneObject creature1 creature2
        , pure $ compare power1 power2
        , pure $ compare toughness1 toughness2
        ]
    y -> compareIndexM x y
  Tap victim1 -> \case
    Tap victim2 -> ordZoneObject victim1 victim2
    y -> compareIndexM x y
  Untap victim1 -> \case
    Untap victim2 -> ordZoneObject victim1 victim2
    y -> compareIndexM x y
  Until event1 effect1 -> \case
    Until event2 effect2 ->
      seqM [ordElectEl event1 event2, ordEffect effect1 effect2]
    y -> compareIndexM x y
  WithList withList1 -> \case
    WithList withList2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithList (Effect ef) zone1 ot1 ->
            WithList (Effect ef) zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast withList2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just withList2 -> ordWithList ordEffect withList1 withList2
       in go withList1 withList2
    y -> compareIndexM x y

ordElectEl ::
  forall p el ot.
  (IndexOT ot, Typeable el, Typeable p) =>
  Elect p el ot ->
  Elect p el ot ->
  EnvM Ordering
ordElectEl x = case x of
  ActivePlayer playerToElect1 -> \case
    ActivePlayer playerToElect2 -> do
      player' <- newObjectN @ 'OTPlayer toObject1'
      let player = toZone player'
          elect1 = playerToElect1 player
          elect2 = playerToElect2 player
      ordElectEl elect1 elect2
    y -> compareIndexM x y
  All with1 -> \case
    All with2 -> ordWithMaskedObjectsElectEl with1 with2
    y -> compareIndexM x y
  Choose player1 with1 -> \case
    Choose player2 with2 ->
      let go ::
            forall zone1 zone2.
            IsPrePost p =>
            Typeable el =>
            IsZO zone1 ot =>
            IsZO zone2 ot =>
            WithMaskedObject zone1 (Elect p el ot) ->
            WithMaskedObject zone2 (Elect p el ot) ->
            EnvM Ordering
          go _ _ = case cast with2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot @ot
            Just with2 ->
              seqM
                [ ordZoneObject player1 player2
                , ordWithMaskedObjectElectEl with1 with2
                ]
       in go with1 with2
    y -> compareIndexM x y
  ChooseOption player1 natList1 varToElect1 -> \case
    ChooseOption player2 natList2 varToElect2 ->
      let go ::
            forall u1 u2 n1 n2.
            IsUser u1 =>
            IsUser u2 =>
            IsNat n1 =>
            IsNat n2 =>
            NatList u1 n1 Condition ->
            NatList u2 n2 Condition ->
            EnvM Ordering
          go _ _ = case cast (natList2, varToElect2) of
            Nothing ->
              seqM
                [ pure $ compare (litNat @n1) (litNat @n2)
                , pure $ compare (typeRep (Proxy @u1)) (typeRep (Proxy @u2))
                ]
            Just (natList2, varToElect2) -> do
              discr <- newVariableId
              let var = ReifiedVariable discr FZ
                  elect1 = varToElect1 var
                  elect2 = varToElect2 var
              seqM
                [ ordZoneObject player1 player2
                , ordNatList ordCondition natList1 natList2
                , ordElectEl elect1 elect2
                ]
       in go natList1 natList2
    y -> compareIndexM x y
  Condition cond1 -> \case
    Condition cond2 -> ordCondition cond1 cond2
    y -> compareIndexM x y
  ControllerOf obj1 playerToElect1 -> \case
    ControllerOf obj2 playerToElect2 ->
      let go ::
            forall zone1 zone2 ot'.
            ot' ~ OTNAny =>
            IsZO zone1 ot' =>
            IsZO zone2 ot' =>
            ZO zone1 ot' ->
            ZO zone2 ot' ->
            EnvM Ordering
          go _ _ = case cast obj2 of
            Nothing -> compareZone @zone1 @zone2
            Just obj2 -> do
              player' <- newObjectN @ 'OTPlayer toObject1'
              let player = toZone player'
                  elect1 = playerToElect1 player
                  elect2 = playerToElect2 player
              seqM [ordZoneObject obj1 obj2, ordElectEl elect1 elect2]
       in go obj1 obj2
    y -> compareIndexM x y
  Cost cost1 -> \case
    Cost cost2 -> ordCost cost1 cost2
    y -> compareIndexM x y
  Effect effects1 -> \case
    Effect effects2 -> listM ordEffect effects1 effects2
    y -> compareIndexM x y
  ElectActivated ability1 -> \case
    ElectActivated ability2 ->
      let go ::
            forall zone1 zone2.
            IsZO zone1 ot =>
            IsZO zone2 ot =>
            ActivatedAbility zone1 ot ->
            ActivatedAbility zone2 ot ->
            EnvM Ordering
          go _ _ = case cast ability2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot @ot
            Just ability2 -> ordActivatedAbility ability1 ability2
       in go ability1 ability2
    y -> compareIndexM x y
  ElectCard card1 -> \case
    ElectCard card2 -> ordCardFacet card1 card2
    y -> compareIndexM x y
  ElectCase case1 -> \case
    ElectCase case2 -> ordCase ordElectEl case1 case2
    y -> compareIndexM x y
  Elect elect1 -> \case
    Elect elect2 -> ordElectEl elect1 elect2
    y -> compareIndexM x y
  Event event1 -> \case
    Event event2 -> ordEvent event1 event2
    y -> compareIndexM x y
  If cond1 then1 else1 -> \case
    If cond2 then2 else2 ->
      seqM
        [ordCondition cond1 cond2, ordElectEl then1 then2, ordElseE else1 else2]
    y -> compareIndexM x y
  Listen listener1 -> \case
    Listen listener2 -> ordEventListener listener1 listener2
    y -> compareIndexM x y
  Random with1 -> \case
    Random with2 -> ordWithMaskedObjectElectEl with1 with2
    y -> compareIndexM x y
  Target player1 with1 -> \case
    Target player2 with2 ->
      let go ::
            forall el zone1 zone2.
            Typeable el =>
            IsZO zone1 ot =>
            IsZO zone2 ot =>
            WithMaskedObject zone1 (Elect 'Pre el ot) ->
            WithMaskedObject zone2 (Elect 'Pre el ot) ->
            EnvM Ordering
          go _ _ = case cast with2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot @ot
            Just with2 ->
              seqM
                [ ordZoneObject player1 player2
                , ordWithMaskedObjectElectEl with1 with2
                ]
       in go with1 with2
    y -> compareIndexM x y
  VariableFromPower obj1 varToElect1 -> \case
    VariableFromPower obj2 varToElect2 -> do
      discr <- newVariableId
      let var = ReifiedVariable discr 0
          elect1 = varToElect1 var
          elect2 = varToElect2 var
      seqM [ordZoneObject obj1 obj2, ordElectEl elect1 elect2]
    y -> compareIndexM x y
  VariableInt varToRet1 -> \case
    VariableInt varToRet2 -> do
      discr <- newVariableId
      let var = ReifiedVariable discr 0
          ret1 = varToRet1 var
          ret2 = varToRet2 var
      ordElectEl ret1 ret2
    y -> compareIndexM x y

ordElectPostEl :: forall el ot. (Typeable el, IndexOT ot) => Elect 'Post el ot -> Elect 'Post el ot -> EnvM Ordering
ordElectPostEl = ordElectEl

ordElseE :: IndexOT ot => Else e ot -> Else e ot -> EnvM Ordering
ordElseE = \case
  ElseCost elect1 -> \case
    ElseCost elect2 -> ordElectEl elect1 elect2
  ElseEffect elect1 -> \case
    ElseEffect elect2 -> ordElectEl elect1 elect2
  ElseEvent -> \case
    ElseEvent -> pure EQ

ordEnchant :: IsZO zone ot => Enchant zone ot -> Enchant zone ot -> EnvM Ordering
ordEnchant = \case
  Enchant withObj1 -> \case
    Enchant withObj2 -> ordWithLinkedObject ordElectEl withObj1 withObj2

ordEnchantmentType :: EnchantmentType ot -> EnchantmentType ot -> EnvM Ordering
ordEnchantmentType = \case
  Aura enchant1 -> \case
    Aura enchant2 ->
      let go ::
            forall zone1 ot1 zone2 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            Enchant zone1 ot1 ->
            Enchant zone2 ot2 ->
            EnvM Ordering
          go enchant1 enchant2 = case cast enchant2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just enchant2 -> ordEnchant enchant1 enchant2
       in go enchant1 enchant2

ordEnchantmentTypes :: [EnchantmentType ot] -> [EnchantmentType ot] -> EnvM Ordering
ordEnchantmentTypes = listM ordEnchantmentType

ordEventListener' ::
  forall liftOT.
  Typeable liftOT =>
  (forall ot. IsOT ot => liftOT ot -> liftOT ot -> EnvM Ordering) ->
  EventListener' liftOT ->
  EventListener' liftOT ->
  EnvM Ordering
ordEventListener' ordM x = case x of
  BecomesTapped perm1 with1 -> \case
    BecomesTapped perm2 with2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithLinkedObject zone1 liftOT ot1 ->
            WithLinkedObject zone2 liftOT ot2 ->
            EnvM Ordering
          go _ _ = case cast (perm2, with2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (perm2, with2) ->
              seqM
                [ordWPermanent perm1 perm2, ordWithLinkedObject ordM with1 with2]
       in go with1 with2
    y -> compareIndexM x y
  Events listeners1 -> \case
    Events listeners2 -> listM (ordEventListener' ordM) listeners1 listeners2
    y -> compareIndexM x y
  SpellIsCast spell1 with1 -> \case
    SpellIsCast spell2 with2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithLinkedObject zone1 liftOT ot1 ->
            WithLinkedObject zone2 liftOT ot2 ->
            EnvM Ordering
          go _ _ = case cast (spell2, with2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (spell2, with2) ->
              seqM [ordWSpell spell1 spell2, ordWithLinkedObject ordM with1 with2]
       in go with1 with2
    y -> compareIndexM x y
  TimePoint time1 elect1 -> \case
    TimePoint time2 elect2 -> case cast (time2, elect2) of
      Nothing -> pure $ compare (consIndex time1) (consIndex time2)
      Just (time2, elect2) ->
        seqM [ordTimePoint time1 time2, ordM elect1 elect2]
    y -> compareIndexM x y

ordEvent :: Event -> Event -> EnvM Ordering
ordEvent = ordEventListener' \Proxy -> pure $ pure EQ

ordEventListener :: EventListener -> EventListener -> EnvM Ordering
ordEventListener = ordEventListener' ordElectEl

ordManaCost :: ManaCost var -> ManaCost var -> EnvM Ordering
ordManaCost x y = pure $ compare x y

ordManaPool :: ManaPool snow -> ManaPool snow -> EnvM Ordering
ordManaPool x y = pure $ compare x y

ordNatList :: IsUser u => (x -> x -> EnvM Ordering) -> NatList u n x -> NatList u n x -> EnvM Ordering
ordNatList ordX = \case
  LZ x1 -> \case
    LZ x2 -> ordX x1 x2
  LS x1 xs1 -> \case
    LS x2 xs2 ->
      seqM
        [ ordX x1 x2
        , ordNatList ordX xs1 xs2
        ]

ordO1 ::
  forall zone a x ot.
  (Typeable x, IsZO zone ot, Inst1 IsObjectType a, IsZO zone (OT1 a)) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement zone (OT1 a)] ->
  [Requirement zone ot] ->
  (ZO zone (OT1 a) -> x) ->
  (ZO zone ot -> x) ->
  EnvM Ordering
ordO1 ordM reqs1 reqs2 cont1 cont2 = case cast' (reqs2, cont2) of
  Nothing -> compareOT @(OT1 a) @ot
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2
      , withObjectCont @a ordM O1 (cont1 . toZone) (cont2 . toZone)
      ]
 where
  cast' ::
    ([Requirement zone ot], ZO zone ot -> x) ->
    Maybe ([Requirement zone (OT1 a)], ZO zone (OT1 a) -> x)
  cast' = cast

ordO2 ::
  forall zone a b x ot.
  (Typeable x, IsZO zone ot, Inst2 IsObjectType a b, IsZO zone (OT2 a b)) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement zone (OT2 a b)] ->
  [Requirement zone ot] ->
  (ZO zone (OT2 a b) -> x) ->
  (ZO zone ot -> x) ->
  EnvM Ordering
ordO2 ordM reqs1 reqs2 cont1 cont2 = case cast' (reqs2, cont2) of
  Nothing -> compareOT @(OT2 a b) @ot
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2
      , withObjectCont @a ordM O2a (cont1 . toZone) (cont2 . toZone)
      ]
 where
  cast' ::
    ([Requirement zone ot], ZO zone ot -> x) ->
    Maybe ([Requirement zone (OT2 a b)], ZO zone (OT2 a b) -> x)
  cast' = cast

ordO3 ::
  forall zone a b c x ot.
  ( Typeable x
  , IsZO zone ot
  , Inst3 IsObjectType a b c
  , IsZO zone (OT3 a b c)
  ) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement zone (OT3 a b c)] ->
  [Requirement zone ot] ->
  (ZO zone (OT3 a b c) -> x) ->
  (ZO zone ot -> x) ->
  EnvM Ordering
ordO3 ordM reqs1 reqs2 cont1 cont2 = case cast' (reqs2, cont2) of
  Nothing -> compareOT @(OT3 a b c) @ot
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2
      , withObjectCont @a ordM O3a (cont1 . toZone) (cont2 . toZone)
      ]
 where
  cast' ::
    ([Requirement zone ot], ZO zone ot -> x) ->
    Maybe ([Requirement zone (OT3 a b c)], ZO zone (OT3 a b c) -> x)
  cast' = cast

ordO4 ::
  forall zone a b c d x ot.
  ( Typeable x
  , IsZO zone ot
  , Inst4 IsObjectType a b c d
  , IsZO zone (OT4 a b c d)
  ) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement zone (OT4 a b c d)] ->
  [Requirement zone ot] ->
  (ZO zone (OT4 a b c d) -> x) ->
  (ZO zone ot -> x) ->
  EnvM Ordering
ordO4 ordM reqs1 reqs2 cont1 cont2 = case cast' (reqs2, cont2) of
  Nothing -> compareOT @(OT4 a b c d) @ot
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2
      , withObjectCont @a ordM O4a (cont1 . toZone) (cont2 . toZone)
      ]
 where
  cast' ::
    ([Requirement zone ot], ZO zone ot -> x) ->
    Maybe ([Requirement zone (OT4 a b c d)], ZO zone (OT4 a b c d) -> x)
  cast' = cast

ordO5 ::
  forall zone a b c d e x ot.
  ( Typeable x
  , IsZO zone ot
  , Inst5 IsObjectType a b c d e
  , IsZO zone (OT5 a b c d e)
  ) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement zone (OT5 a b c d e)] ->
  [Requirement zone ot] ->
  (ZO zone (OT5 a b c d e) -> x) ->
  (ZO zone ot -> x) ->
  EnvM Ordering
ordO5 ordM reqs1 reqs2 cont1 cont2 = case cast' (reqs2, cont2) of
  Nothing -> compareOT @(OT5 a b c d e) @ot
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2
      , withObjectCont @a ordM O5a (cont1 . toZone) (cont2 . toZone)
      ]
 where
  cast' ::
    ([Requirement zone ot], ZO zone ot -> x) ->
    Maybe
      ([Requirement zone (OT5 a b c d e)], ZO zone (OT5 a b c d e) -> x)
  cast' = cast

ordO6 ::
  forall zone a b c d e f x ot.
  ( Typeable x
  , IsZO zone ot
  , Inst6 IsObjectType a b c d e f
  , IsZO zone (OT6 a b c d e f)
  ) =>
  (x -> x -> EnvM Ordering) ->
  [Requirement zone (OT6 a b c d e f)] ->
  [Requirement zone ot] ->
  (ZO zone (OT6 a b c d e f) -> x) ->
  (ZO zone ot -> x) ->
  EnvM Ordering
ordO6 ordM reqs1 reqs2 cont1 cont2 = case cast' (reqs2, cont2) of
  Nothing -> compareOT @(OT6 a b c d e f) @ot
  Just (reqs2, cont2) ->
    seqM
      [ ordRequirements reqs1 reqs2
      , withObjectCont @a ordM O6a (cont1 . toZone) (cont2 . toZone)
      ]
 where
  cast' ::
    ([Requirement zone ot], ZO zone ot -> x) ->
    Maybe
      ([Requirement zone (OT6 a b c d e f)], ZO zone (OT6 a b c d e f) -> x)
  cast' = cast

ordObject0 :: ObjectN OT0 -> ObjectN OT0 -> EnvM Ordering
ordObject0 objN1 objN2 = do
  let i1 = getObjectId objN1
      i2 = getObjectId objN2
  pure $ compare i1 i2

ordObjectN' :: VisitObjectN ot => ObjectN ot -> ObjectN ot -> EnvM Ordering
ordObjectN' objN1 objN2 = do
  let i1 = visitObjectN' objectToId objN1
      i2 = visitObjectN' objectToId objN2
  pure $ compare i1 i2

ordObjectN :: ObjectN ot -> ObjectN ot -> EnvM Ordering
ordObjectN objN1 objN2 = case objN1 of
  O0{} -> ordObject0 objN1 objN2
  O1{} -> ordObjectN' objN1 objN2
  O2a{} -> ordObjectN' objN1 objN2
  O2b{} -> ordObjectN' objN1 objN2
  ON2b{} -> ordObjectN' objN1 objN2
  ON2a{} -> ordObjectN' objN1 objN2
  O3b{} -> ordObjectN' objN1 objN2
  O3a{} -> ordObjectN' objN1 objN2
  O3c{} -> ordObjectN' objN1 objN2
  ON3a{} -> ordObjectN' objN1 objN2
  ON3b{} -> ordObjectN' objN1 objN2
  ON3c{} -> ordObjectN' objN1 objN2
  O4a{} -> ordObjectN' objN1 objN2
  O4b{} -> ordObjectN' objN1 objN2
  O4c{} -> ordObjectN' objN1 objN2
  O4d{} -> ordObjectN' objN1 objN2
  ON4a{} -> ordObjectN' objN1 objN2
  ON4b{} -> ordObjectN' objN1 objN2
  ON4c{} -> ordObjectN' objN1 objN2
  ON4d{} -> ordObjectN' objN1 objN2
  O5b{} -> ordObjectN' objN1 objN2
  O5a{} -> ordObjectN' objN1 objN2
  O5c{} -> ordObjectN' objN1 objN2
  O5d{} -> ordObjectN' objN1 objN2
  O5e{} -> ordObjectN' objN1 objN2
  ON5a{} -> ordObjectN' objN1 objN2
  ON5b{} -> ordObjectN' objN1 objN2
  ON5c{} -> ordObjectN' objN1 objN2
  ON5d{} -> ordObjectN' objN1 objN2
  ON5e{} -> ordObjectN' objN1 objN2
  O6a{} -> ordObjectN' objN1 objN2
  O6b{} -> ordObjectN' objN1 objN2
  O6c{} -> ordObjectN' objN1 objN2
  O6d{} -> ordObjectN' objN1 objN2
  O6e{} -> ordObjectN' objN1 objN2
  O6f{} -> ordObjectN' objN1 objN2
  ON6a{} -> ordObjectN' objN1 objN2
  ON6b{} -> ordObjectN' objN1 objN2
  ON6c{} -> ordObjectN' objN1 objN2
  ON6d{} -> ordObjectN' objN1 objN2
  ON6e{} -> ordObjectN' objN1 objN2
  ON6f{} -> ordObjectN' objN1 objN2
  O7a{} -> ordObjectN' objN1 objN2
  O7b{} -> ordObjectN' objN1 objN2
  O7c{} -> ordObjectN' objN1 objN2
  O7d{} -> ordObjectN' objN1 objN2
  O7e{} -> ordObjectN' objN1 objN2
  O7f{} -> ordObjectN' objN1 objN2
  O7g{} -> ordObjectN' objN1 objN2
  ON7a{} -> ordObjectN' objN1 objN2
  ON7b{} -> ordObjectN' objN1 objN2
  ON7c{} -> ordObjectN' objN1 objN2
  ON7d{} -> ordObjectN' objN1 objN2
  ON7e{} -> ordObjectN' objN1 objN2
  ON7f{} -> ordObjectN' objN1 objN2
  ON7g{} -> ordObjectN' objN1 objN2
  O8a{} -> ordObjectN' objN1 objN2
  O8b{} -> ordObjectN' objN1 objN2
  O8c{} -> ordObjectN' objN1 objN2
  O8d{} -> ordObjectN' objN1 objN2
  O8e{} -> ordObjectN' objN1 objN2
  O8f{} -> ordObjectN' objN1 objN2
  O8g{} -> ordObjectN' objN1 objN2
  O8h{} -> ordObjectN' objN1 objN2
  ON8a{} -> ordObjectN' objN1 objN2
  ON8b{} -> ordObjectN' objN1 objN2
  ON8c{} -> ordObjectN' objN1 objN2
  ON8d{} -> ordObjectN' objN1 objN2
  ON8e{} -> ordObjectN' objN1 objN2
  ON8f{} -> ordObjectN' objN1 objN2
  ON8g{} -> ordObjectN' objN1 objN2
  ON8h{} -> ordObjectN' objN1 objN2
  O9a{} -> ordObjectN' objN1 objN2
  O9b{} -> ordObjectN' objN1 objN2
  O9c{} -> ordObjectN' objN1 objN2
  O9d{} -> ordObjectN' objN1 objN2
  O9e{} -> ordObjectN' objN1 objN2
  O9f{} -> ordObjectN' objN1 objN2
  O9g{} -> ordObjectN' objN1 objN2
  O9h{} -> ordObjectN' objN1 objN2
  O9i{} -> ordObjectN' objN1 objN2
  ON9a{} -> ordObjectN' objN1 objN2
  ON9b{} -> ordObjectN' objN1 objN2
  ON9c{} -> ordObjectN' objN1 objN2
  ON9d{} -> ordObjectN' objN1 objN2
  ON9e{} -> ordObjectN' objN1 objN2
  ON9f{} -> ordObjectN' objN1 objN2
  ON9g{} -> ordObjectN' objN1 objN2
  ON9h{} -> ordObjectN' objN1 objN2
  ON9i{} -> ordObjectN' objN1 objN2
  O10b{} -> ordObjectN' objN1 objN2
  O10a{} -> ordObjectN' objN1 objN2
  O10c{} -> ordObjectN' objN1 objN2
  O10d{} -> ordObjectN' objN1 objN2
  O10e{} -> ordObjectN' objN1 objN2
  O10f{} -> ordObjectN' objN1 objN2
  O10g{} -> ordObjectN' objN1 objN2
  O10h{} -> ordObjectN' objN1 objN2
  O10i{} -> ordObjectN' objN1 objN2
  O10j{} -> ordObjectN' objN1 objN2
  ON10a{} -> ordObjectN' objN1 objN2
  ON10b{} -> ordObjectN' objN1 objN2
  ON10c{} -> ordObjectN' objN1 objN2
  ON10d{} -> ordObjectN' objN1 objN2
  ON10e{} -> ordObjectN' objN1 objN2
  ON10f{} -> ordObjectN' objN1 objN2
  ON10g{} -> ordObjectN' objN1 objN2
  ON10h{} -> ordObjectN' objN1 objN2
  ON10i{} -> ordObjectN' objN1 objN2
  ON10j{} -> ordObjectN' objN1 objN2
  O11b{} -> ordObjectN' objN1 objN2
  O11a{} -> ordObjectN' objN1 objN2
  O11c{} -> ordObjectN' objN1 objN2
  O11d{} -> ordObjectN' objN1 objN2
  O11e{} -> ordObjectN' objN1 objN2
  O11f{} -> ordObjectN' objN1 objN2
  O11g{} -> ordObjectN' objN1 objN2
  O11h{} -> ordObjectN' objN1 objN2
  O11i{} -> ordObjectN' objN1 objN2
  O11j{} -> ordObjectN' objN1 objN2
  O11k{} -> ordObjectN' objN1 objN2
  ON11a{} -> ordObjectN' objN1 objN2
  ON11b{} -> ordObjectN' objN1 objN2
  ON11c{} -> ordObjectN' objN1 objN2
  ON11d{} -> ordObjectN' objN1 objN2
  ON11e{} -> ordObjectN' objN1 objN2
  ON11f{} -> ordObjectN' objN1 objN2
  ON11g{} -> ordObjectN' objN1 objN2
  ON11h{} -> ordObjectN' objN1 objN2
  ON11i{} -> ordObjectN' objN1 objN2
  ON11j{} -> ordObjectN' objN1 objN2
  ON11k{} -> ordObjectN' objN1 objN2
  O12b{} -> ordObjectN' objN1 objN2
  O12a{} -> ordObjectN' objN1 objN2
  O12c{} -> ordObjectN' objN1 objN2
  O12d{} -> ordObjectN' objN1 objN2
  O12e{} -> ordObjectN' objN1 objN2
  O12f{} -> ordObjectN' objN1 objN2
  O12g{} -> ordObjectN' objN1 objN2
  O12h{} -> ordObjectN' objN1 objN2
  O12i{} -> ordObjectN' objN1 objN2
  O12j{} -> ordObjectN' objN1 objN2
  O12k{} -> ordObjectN' objN1 objN2
  O12l{} -> ordObjectN' objN1 objN2
  ON12a{} -> ordObjectN' objN1 objN2
  ON12b{} -> ordObjectN' objN1 objN2
  ON12c{} -> ordObjectN' objN1 objN2
  ON12d{} -> ordObjectN' objN1 objN2
  ON12e{} -> ordObjectN' objN1 objN2
  ON12f{} -> ordObjectN' objN1 objN2
  ON12g{} -> ordObjectN' objN1 objN2
  ON12h{} -> ordObjectN' objN1 objN2
  ON12i{} -> ordObjectN' objN1 objN2
  ON12j{} -> ordObjectN' objN1 objN2
  ON12k{} -> ordObjectN' objN1 objN2
  ON12l{} -> ordObjectN' objN1 objN2

ordRequirement :: IndexOT ot => Requirement zone ot -> Requirement zone ot -> EnvM Ordering
ordRequirement x = case x of
  ControlledBy player1 -> \case
    ControlledBy player2 -> ordZoneObject player1 player2
    y -> compareIndexM x y
  ControlsA req1 -> \case
    ControlsA req2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            Requirement zone1 ot1 ->
            Requirement zone2 ot2 ->
            EnvM Ordering
          go req1 req2 = case cast req2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just req2 -> ordRequirement req1 req2
       in go req1 req2
    y -> compareIndexM x y
  HasAbility ability1 -> \case
    HasAbility ability2 -> ordWithThis ordAbility ability1 ability2
    y -> compareIndexM x y
  HasLandType type1 -> \case
    HasLandType type2 -> pure $ compare type1 type2
    y -> compareIndexM x y
  Is any1 obj1 -> \case
    Is any2 obj2 -> seqM [ordWAny any1 any2, ordZoneObject obj1 obj2]
    y -> compareIndexM x y
  IsTapped perm1 -> \case
    IsTapped perm2 -> ordWPermanent perm1 perm2
    y -> compareIndexM x y
  Not req1 -> \case
    Not req2 -> ordRequirement req1 req2
    y -> compareIndexM x y
  OfColors colors1 -> \case
    OfColors colors2 -> ordColors colors1 colors2
    y -> compareIndexM x y
  OwnedBy player1 -> \case
    OwnedBy player2 -> ordZoneObject player1 player2
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
  R2 reqsA1 reqsB1 -> \case
    R2 reqsA2 reqsB2 ->
      seqM [ordRequirements reqsA1 reqsA2, ordRequirements reqsB1 reqsB2]
    y -> compareIndexM x y
  R3 reqsA1 reqsB1 reqsC1 -> \case
    R3 reqsA2 reqsB2 reqsC2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2
        , ordRequirements reqsB1 reqsB2
        , ordRequirements reqsC1 reqsC2
        ]
    y -> compareIndexM x y
  R4 reqsA1 reqsB1 reqsC1 reqsD1 -> \case
    R4 reqsA2 reqsB2 reqsC2 reqsD2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2
        , ordRequirements reqsB1 reqsB2
        , ordRequirements reqsC1 reqsC2
        , ordRequirements reqsD1 reqsD2
        ]
    y -> compareIndexM x y
  R5 reqsA1 reqsB1 reqsC1 reqsD1 reqsE1 -> \case
    R5 reqsA2 reqsB2 reqsC2 reqsD2 reqsE2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2
        , ordRequirements reqsB1 reqsB2
        , ordRequirements reqsC1 reqsC2
        , ordRequirements reqsD1 reqsD2
        , ordRequirements reqsE1 reqsE2
        ]
    y -> compareIndexM x y

ordRequirements :: IsZO zone ot => [Requirement zone ot] -> [Requirement zone ot] -> EnvM Ordering
ordRequirements = listM ordRequirement

ordSetCard :: SetCard ot -> SetCard ot -> EnvM Ordering
ordSetCard = \case
  SetCard set1 rarity1 card1 -> \case
    SetCard set2 rarity2 card2 ->
      seqM
        [ pure $ compare set1 set2
        , pure $ compare rarity1 rarity2
        , ordCard card1 card2
        ]

ordSetToken :: SetToken ot -> SetToken ot -> EnvM Ordering
ordSetToken = \case
  SetToken set1 rarity1 token1 -> \case
    SetToken set2 rarity2 token2 ->
      seqM
        [ pure $ compare set1 set2
        , pure $ compare rarity1 rarity2
        , ordToken token1 token2
        ]

ordStaticAbility :: StaticAbility zone ot -> StaticAbility zone ot -> EnvM Ordering
ordStaticAbility x = case x of
  As electListener1 -> \case
    As electListener2 -> ordElectEl electListener1 electListener2
    y -> compareIndexM x y
  Bestow elect1 enchant1 -> \case
    Bestow elect2 enchant2 -> seqM [ordElectEl elect1 elect2, ordEnchant enchant1 enchant2]
    y -> compareIndexM x y
  FirstStrike -> \case
    FirstStrike -> pure EQ
    y -> compareIndexM x y
  Flying -> \case
    Flying -> pure EQ
    y -> compareIndexM x y
  Fuse -> \case
    Fuse -> pure EQ
  Haste -> \case
    Haste -> pure EQ
    y -> compareIndexM x y
  StaticContinuous elect1 -> \case
    StaticContinuous elect2 -> ordElectEl elect1 elect2
    y -> compareIndexM x y
  Suspend duration1 elect1 -> \case
    Suspend duration2 elect2 ->
      seqM [pure $ compare duration1 duration2, ordElectEl elect1 elect2]
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
ordToken x = case x of
  Token wPerm1 card1 -> \case
    Token wPerm2 card2 ->
      seqM [ordWPermanent wPerm1 wPerm2, ordCard card1 card2]

ordTriggeredAbility ::
  forall zone ot.
  IndexOT ot =>
  TriggeredAbility zone ot ->
  TriggeredAbility zone ot ->
  EnvM Ordering
ordTriggeredAbility = \case
  When listener1 -> \case
    When listener2 -> ordElectPostEl @EventListener @ot listener1 listener2

ordVariable :: Variable x -> Variable x -> EnvM Ordering
ordVariable = \case
  ReifiedVariable vid1 _ -> \case
    ReifiedVariable vid2 _ -> pure $ compare vid1 vid2

ordWithLinkedObject ::
  (Typeable x, IsZO zone ot) =>
  (x ot -> x ot -> EnvM Ordering) ->
  WithLinkedObject zone x ot ->
  WithLinkedObject zone x ot ->
  EnvM Ordering
ordWithLinkedObject ordM x = case x of
  LProxy reqs1 -> \case
    LProxy reqs2 -> ordRequirements reqs1 reqs2
  --
  L1 NonProxyElectEffect reqs1 cont1 -> \case
    L1 NonProxyElectEffect reqs2 cont2 ->
      ordO1 ordM reqs1 reqs2 cont1 cont2
  L2 NonProxyElectEffect reqs1 cont1 -> \case
    L2 NonProxyElectEffect reqs2 cont2 ->
      ordO2 ordM reqs1 reqs2 cont1 cont2
  L3 NonProxyElectEffect reqs1 cont1 -> \case
    L3 NonProxyElectEffect reqs2 cont2 ->
      ordO3 ordM reqs1 reqs2 cont1 cont2
  L4 NonProxyElectEffect reqs1 cont1 -> \case
    L4 NonProxyElectEffect reqs2 cont2 ->
      ordO4 ordM reqs1 reqs2 cont1 cont2
  L5 NonProxyElectEffect reqs1 cont1 -> \case
    L5 NonProxyElectEffect reqs2 cont2 ->
      ordO5 ordM reqs1 reqs2 cont1 cont2
  --
  L1 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    L1 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO1 ordM reqs1 reqs2 cont1 cont2
  L2 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    L2 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO2 ordM reqs1 reqs2 cont1 cont2
  L3 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    L3 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO3 ordM reqs1 reqs2 cont1 cont2
  L4 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    L4 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO4 ordM reqs1 reqs2 cont1 cont2
  L5 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    L5 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO5 ordM reqs1 reqs2 cont1 cont2

ordWithList :: (ret -> ret -> EnvM Ordering) -> WithList ret zone ot -> WithList ret zone ot -> EnvM Ordering
ordWithList ordRet x = case x of
  CountOf (lenseList -> z1) cont1 -> \case
    CountOf (lenseList -> z2) cont2 -> do
      discr <- newVariableId
      let var = ReifiedVariable discr 0
      seqM
        [ ordZoneObject z1 z2
        , ordRet (cont1 var) (cont2 var)
        ]
    y -> compareIndexM x y
  Each (lenseList -> z1) cont1 -> \case
    Each (lenseList -> z2) cont2 ->
      seqM
        [ ordZoneObject z1 z2
        , ordRet (cont1 z1) (cont2 z2)
        ]
    y -> compareIndexM x y
  SuchThat reqs1 withList1 -> \case
    SuchThat reqs2 withList2 ->
      seqM
        [ ordRequirements reqs1 reqs2
        , ordWithList ordRet withList1 withList2
        ]
    y -> compareIndexM x y

ordWithMaskedObjectElectEl ::
  Typeable p =>
  Typeable el =>
  IsZO zone ot =>
  WithMaskedObject zone (Elect p el ot) ->
  WithMaskedObject zone (Elect p el ot) ->
  EnvM Ordering
ordWithMaskedObjectElectEl x = case x of
  Masked1 reqs1 cont1 -> \case
    Masked1 reqs2 cont2 -> ordO1 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  Masked2 reqs1 cont1 -> \case
    Masked2 reqs2 cont2 -> ordO2 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  Masked3 reqs1 cont1 -> \case
    Masked3 reqs2 cont2 -> ordO3 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  Masked4 reqs1 cont1 -> \case
    Masked4 reqs2 cont2 -> ordO4 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  Masked5 reqs1 cont1 -> \case
    Masked5 reqs2 cont2 -> ordO5 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
  Masked6 reqs1 cont1 -> \case
    Masked6 reqs2 cont2 -> ordO6 ordM reqs1 reqs2 cont1 cont2
    y -> pure $ compare (consIndex x) (consIndex y)
 where
  ordM = ordElectEl

ordWithMaskedObjectsElectEl ::
  Typeable p =>
  Typeable el =>
  IsZO zone ot =>
  WithMaskedObjects zone (Elect p el ot) ->
  WithMaskedObjects zone (Elect p el ot) ->
  EnvM Ordering
ordWithMaskedObjectsElectEl x = case x of
  Maskeds1 reqs1 cont1 -> \case
    Maskeds1 reqs2 cont2 -> ordO1 ordM reqs1 reqs2 (cont1 . pure) (cont2 . pure)
    y -> pure $ compare (consIndex x) (consIndex y)
  Maskeds2 reqs1 cont1 -> \case
    Maskeds2 reqs2 cont2 -> ordO2 ordM reqs1 reqs2 (cont1 . pure) (cont2 . pure)
    y -> pure $ compare (consIndex x) (consIndex y)
  Maskeds3 reqs1 cont1 -> \case
    Maskeds3 reqs2 cont2 -> ordO3 ordM reqs1 reqs2 (cont1 . pure) (cont2 . pure)
    y -> pure $ compare (consIndex x) (consIndex y)
  Maskeds4 reqs1 cont1 -> \case
    Maskeds4 reqs2 cont2 -> ordO4 ordM reqs1 reqs2 (cont1 . pure) (cont2 . pure)
    y -> pure $ compare (consIndex x) (consIndex y)
  Maskeds5 reqs1 cont1 -> \case
    Maskeds5 reqs2 cont2 -> ordO5 ordM reqs1 reqs2 (cont1 . pure) (cont2 . pure)
    y -> pure $ compare (consIndex x) (consIndex y)
  Maskeds6 reqs1 cont1 -> \case
    Maskeds6 reqs2 cont2 -> ordO6 ordM reqs1 reqs2 (cont1 . pure) (cont2 . pure)
    y -> pure $ compare (consIndex x) (consIndex y)
 where
  ordM = ordElectEl

ordWithThis ::
  forall zone liftOT ot.
  Typeable liftOT =>
  IsZO zone ot =>
  (liftOT ot -> liftOT ot -> EnvM Ordering) ->
  WithThis zone liftOT ot ->
  WithThis zone liftOT ot ->
  EnvM Ordering
ordWithThis ordM = \case
  This1 cont1 -> \case
    This1 cont2 -> ordO1 ordM reqs1 reqs2 cont1 cont2
  This2 cont1 -> \case
    This2 cont2 ->
      let go ::
            forall a b ota otb.
            ota ~ OT1 a =>
            otb ~ OT1 b =>
            Inst2 IsObjectType a b =>
            ((ZO zone ota, ZO zone otb) -> liftOT ot) ->
            ((ZO zone ota, ZO zone otb) -> liftOT ot) ->
            EnvM Ordering
          go cont1 cont2 = do
            objNa' <- newObjectN @a O1
            objNb' <- newObjectN @b O1
            let objNa = toZone objNa'
                objNb = toZone objNb'
                lifted1 = cont1 (objNa, objNb)
                lifted2 = cont2 (objNa, objNb)
            ordM lifted1 lifted2
       in go cont1 cont2
 where
  reqs1 = []
  reqs2 = []

ordWithThisActivated :: IsZO zone ot => WithThisActivated zone ot -> WithThisActivated zone ot -> EnvM Ordering
ordWithThisActivated = ordWithThis ordElectEl

ordWithThisTriggered :: IsZO zone ot => WithThisTriggered zone ot -> WithThisTriggered zone ot -> EnvM Ordering
ordWithThisTriggered = ordWithThis ordTriggeredAbility

ordW2 ::
  forall witness ot1 a1 b1 ot2 a2 b2.
  ( Typeable witness
  , ot1 ~ OT2 a1 b1
  , ot2 ~ OT2 a2 b2
  , IsOT ot1
  , IsOT ot2
  , Inst2 IsObjectType a1 b1
  , Inst2 IsObjectType a2 b2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW2 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 @ot2
  Just{} -> pure EQ

ordW3 ::
  forall witness ot1 a1 b1 c1 ot2 a2 b2 c2.
  ( Typeable witness
  , ot1 ~ OT3 a1 b1 c1
  , ot2 ~ OT3 a2 b2 c2
  , IsOT ot1
  , IsOT ot2
  , Inst3 IsObjectType a1 b1 c1
  , Inst3 IsObjectType a2 b2 c2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW3 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 @ot2
  Just{} -> pure EQ

ordW4 ::
  forall witness ot1 a1 b1 c1 d1 ot2 a2 b2 c2 d2.
  ( Typeable witness
  , ot1 ~ OT4 a1 b1 c1 d1
  , ot2 ~ OT4 a2 b2 c2 d2
  , IsOT ot1
  , IsOT ot2
  , Inst4 IsObjectType a1 b1 c1 d1
  , Inst4 IsObjectType a2 b2 c2 d2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW4 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 @ot2
  Just{} -> pure EQ

ordW5 ::
  forall witness ot1 a1 b1 c1 d1 e1 ot2 a2 b2 c2 d2 e2.
  ( Typeable witness
  , ot1 ~ OT5 a1 b1 c1 d1 e1
  , ot2 ~ OT5 a2 b2 c2 d2 e2
  , IsOT ot1
  , IsOT ot2
  , Inst5 IsObjectType a1 b1 c1 d1 e1
  , Inst5 IsObjectType a2 b2 c2 d2 e2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW5 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 @ot2
  Just{} -> pure EQ

ordW6 ::
  forall witness ot1 a1 b1 c1 d1 e1 f1 ot2 a2 b2 c2 d2 e2 f2.
  ( Typeable witness
  , ot1 ~ OT6 a1 b1 c1 d1 e1 f1
  , ot2 ~ OT6 a2 b2 c2 d2 e2 f2
  , IsOT ot1
  , IsOT ot2
  , Inst6 IsObjectType a1 b1 c1 d1 e1 f1
  , Inst6 IsObjectType a2 b2 c2 d2 e2 f2
  ) =>
  witness ot1 ->
  witness ot2 ->
  EnvM Ordering
ordW6 _wit1 wit2 = case (cast wit2 :: Maybe (witness ot1)) of
  Nothing -> compareOT @ot1 @ot2
  Just{} -> pure EQ

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

ordWNonCreatureCard ::
  WNonCreatureCard ot -> WNonCreatureCard ot -> EnvM Ordering
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

ordYourCard :: YourCardFacet ot -> YourCardFacet ot -> EnvM Ordering
ordYourCard = \case
  YourArtifact cont1 -> \case
    YourArtifact cont2 -> goPerm cont1 cont2
  YourArtifactCreature cont1 -> \case
    YourArtifactCreature cont2 -> goPerm cont1 cont2
  YourArtifactLand cont1 -> \case
    YourArtifactLand cont2 -> goPerm cont1 cont2
  YourCreature cont1 -> \case
    YourCreature cont2 -> goPerm cont1 cont2
  YourEnchantment cont1 -> \case
    YourEnchantment cont2 -> goPerm cont1 cont2
  YourEnchantmentCreature cont1 -> \case
    YourEnchantmentCreature cont2 -> goPerm cont1 cont2
  YourLand cont1 -> \case
    YourLand cont2 -> goPerm cont1 cont2
  YourPlaneswalker cont1 -> \case
    YourPlaneswalker cont2 -> goPerm cont1 cont2
  --
  YourInstant cont1 -> \case
    YourInstant cont2 -> goSpell cont1 cont2
  YourSorcery cont1 -> \case
    YourSorcery cont2 -> goSpell cont1 cont2
 where
  withYou action = do
    you' <- newObjectN @ 'OTPlayer toObject1'
    let you = toZone you'
    action you
  goPerm cont1 cont2 = withYou \you -> do
    ordCardFacet (cont1 you) (cont2 you)
  goSpell cont1 cont2 = withYou \you -> do
    ordElectEl (cont1 you) (cont2 you)

ordZoneObject :: ZO zone ot -> ZO zone ot -> EnvM Ordering
ordZoneObject x = case x of
  ZO _ objN1 -> \case
    ZO _ objN2 -> ordObjectN objN1 objN2

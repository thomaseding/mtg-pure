{-# LANGUAGE Safe #-}
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
  ordWithThisStatic,
  ordWithThisTriggered,
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
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Mana.ManaCost (ManaCost)
import safe MtgPure.Model.Mana.ManaPool (ManaPool)
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
import safe MtgPure.Model.Object.ObjectN (ObjectN)
import safe MtgPure.Model.Object.ObjectN_ (ObjectN' (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN.Classes (ToObject1' (toObject1'))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Object.VisitObjectN (visitObjectN')
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
  EntersStatic (..),
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
  WithThisStatic,
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
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZoneObject (..), toZone)

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

instance (Typeable el, Typeable p, IsOTN ot) => Eq (Elect p el ot) where
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

instance (Typeable el, Typeable p, IsZO zone ot) => Eq (WithMaskedObject zone (Elect p el) ot) where
  (==) x y = runEnvM (ordWithMaskedObjectElectEl x y) == EQ

instance IsZO zone ot => Eq (WithThis zone Ability ot) where
  (==) x y = runEnvM (ordWithThis ordAbility x y) == EQ

instance IsZO zone ot => Eq (WithThisActivated zone ot) where
  (==) x y = runEnvM (ordWithThisActivated x y) == EQ

instance IsZO zone ot => Eq (WithThisStatic zone ot) where
  (==) x y = runEnvM (ordWithThisStatic x y) == EQ

instance IsZO zone ot => Eq (WithThisTriggered zone ot) where
  (==) x y = runEnvM (ordWithThisTriggered x y) == EQ

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

instance (Typeable el, Typeable p, IsOTN ot) => Ord (Elect p el ot) where
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

instance (Typeable el, Typeable p, IsZO zone ot) => Ord (WithMaskedObject zone (Elect p el) ot) where
  compare x y = runEnvM (ordWithMaskedObjectElectEl x y)

instance IsZO zone ot => Ord (WithThis zone Ability ot) where
  compare x y = runEnvM (ordWithThis ordAbility x y)

instance IsZO zone ot => Ord (WithThisActivated zone ot) where
  compare x y = runEnvM (ordWithThisActivated x y)

instance IsZO zone ot => Ord (WithThisStatic zone ot) where
  compare x y = runEnvM (ordWithThisStatic x y)

instance IsZO zone ot => Ord (WithThisTriggered zone ot) where
  compare x y = runEnvM (ordWithThisTriggered x y)

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
  (IsObjectType a, IsOTN ot') =>
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
            WithThisStatic zone1 ot1 ->
            WithThisStatic zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast ability2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just ability2 -> ordWithThisStatic ability1 ability2
       in go ability1 ability2
    y -> compareIndexM x y
  StaticWithoutThis ability1 -> \case
    StaticWithoutThis ability2 ->
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

ordAbilities :: IsOTN ot => [Ability ot] -> [Ability ot] -> EnvM Ordering
ordAbilities = listM ordAbility

ordActivatedAbility :: ActivatedAbility zone ot -> ActivatedAbility zone ot -> EnvM Ordering
ordActivatedAbility x = case x of
  Ability cost1 effect1 -> \case
    Ability cost2 effect2 -> seqM [ordCost cost1 cost2, ordElectEl effect1 effect2]
    y -> compareIndexM x y
  Cycling cost1 -> \case
    Cycling cost2 -> ordCost cost1 cost2
    y -> compareIndexM x y

ordAnyCard :: AnyCard -> AnyCard -> EnvM Ordering
ordAnyCard x = case x of
  AnyCard1 card1 -> \case
    AnyCard1 card2 ->
      let go :: forall ot1 ot2. (IsOTN ot1, IsOTN ot2) => Card ot1 -> Card ot2 -> EnvM Ordering
          go _ _ = case cast card2 of
            Nothing -> compareOT @ot1 @ot2
            Just card2 -> ordCard card1 card2
       in go card1 card2
    y -> compareIndexM x y
  AnyCard2 card1 -> \case
    AnyCard2 card2 ->
      let go ::
            forall ot1a ot1b ot2a ot2b.
            (IsOTN ot1a, IsOTN ot1b, IsOTN ot2a, IsOTN ot2b) =>
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
      let go :: forall ot1 ot2. (IsOTN ot1, IsOTN ot2) => Token ot1 -> Token ot2 -> EnvM Ordering
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
  ArtifactFacet colors1 cost1 sups1 artTypes1 abilities1 -> \case
    ArtifactFacet colors2 cost2 sups2 artTypes2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare sups1 sups2
        , pure $ compare artTypes1 artTypes2
        , ordAbilities abilities1 abilities2
        ]
  ArtifactCreatureFacet colors1 cost1 sups1 artTypes1 creatTypes1 power1 toughness1 artAbils1 creatAbils1 bothAbils1 ->
    \case
      ArtifactCreatureFacet colors2 cost2 sups2 artTypes2 creatTypes2 power2 toughness2 artAbils2 creatAbils2 bothAbils2 ->
        seqM
          [ ordColors colors1 colors2
          , ordCost cost1 cost2
          , pure $ compare sups1 sups2
          , pure $ compare artTypes1 artTypes2
          , pure $ compare creatTypes1 creatTypes2
          , pure $ compare power1 power2
          , pure $ compare toughness1 toughness2
          , ordAbilities artAbils1 artAbils2
          , ordAbilities creatAbils1 creatAbils2
          , ordAbilities bothAbils1 bothAbils2
          ]
  ArtifactLandFacet sups1 artTypes1 landTypes1 artAbils1 landAbils1 bothAbils1 ->
    \case
      ArtifactLandFacet sups2 artTypes2 landTypes2 artAbils2 landAbils2 bothAbils2 ->
        seqM
          [ pure $ compare sups1 sups2
          , pure $ compare artTypes1 artTypes2
          , pure $ compare landTypes1 landTypes2
          , ordAbilities artAbils1 artAbils2
          , ordAbilities landAbils1 landAbils2
          , ordAbilities bothAbils1 bothAbils2
          ]
  CreatureFacet colors1 cost1 sups1 creatTypes1 power1 toughness1 abilities1 -> \case
    CreatureFacet colors2 cost2 sups2 creatTypes2 power2 toughness2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare sups1 sups2
        , pure $ compare creatTypes1 creatTypes2
        , pure $ compare power1 power2
        , pure $ compare toughness1 toughness2
        , ordAbilities abilities1 abilities2
        ]
  EnchantmentFacet colors1 cost1 sups1 enchantTypes1 abilities1 -> \case
    EnchantmentFacet colors2 cost2 sups2 enchantTypes2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare sups1 sups2
        , ordEnchantmentTypes enchantTypes1 enchantTypes2
        , ordAbilities abilities1 abilities2
        ]
  EnchantmentCreatureFacet colors1 cost1 sups1 creatTypes1 enchantTypes1 power1 toughness1 creatAbils1 enchAbils1 bothAbils1 ->
    \case
      EnchantmentCreatureFacet colors2 cost2 sups2 creatTypes2 enchantTypes2 power2 toughness2 creatAbils2 enchAbils2 bothAbils2 ->
        seqM
          [ ordColors colors1 colors2
          , ordCost cost1 cost2
          , pure $ compare sups1 sups2
          , pure $ compare creatTypes1 creatTypes2
          , ordEnchantmentTypes enchantTypes1 enchantTypes2
          , pure $ compare power1 power2
          , pure $ compare toughness1 toughness2
          , ordAbilities creatAbils1 creatAbils2
          , ordAbilities enchAbils1 enchAbils2
          , ordAbilities bothAbils1 bothAbils2
          ]
  InstantFacet colors1 cost1 sups1 abilities1 effect1 -> \case
    InstantFacet colors2 cost2 sups2 abilities2 effect2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare sups1 sups2
        , ordAbilities abilities1 abilities2
        , ordWithThis ordElectEl effect1 effect2
        ]
  LandFacet sups1 landTypes1 abilities1 -> \case
    LandFacet sups2 landTypes2 abilities2 ->
      seqM
        [ pure $ compare sups1 sups2
        , pure $ compare landTypes1 landTypes2
        , ordAbilities abilities1 abilities2
        ]
  PlaneswalkerFacet colors1 cost1 sup1 loyalty1 abilities1 -> \case
    PlaneswalkerFacet colors2 cost2 sup2 loyalty2 abilities2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare sup1 sup2
        , pure $ compare loyalty1 loyalty2
        , ordAbilities abilities1 abilities2
        ]
  SorceryFacet colors1 cost1 sup1 abilities1 effect1 -> \case
    SorceryFacet colors2 cost2 sup2 abilities2 effect2 ->
      seqM
        [ ordColors colors1 colors2
        , ordCost cost1 cost2
        , pure $ compare sup1 sup2
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
  Satisfies obj1 reqs1 -> \case
    Satisfies obj2 reqs2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (obj2, reqs2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (obj2, reqs2) ->
              seqM
                [ ordZoneObject obj1 obj2
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
  ExileCost reqs1 -> \case
    ExileCost reqs2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            [Requirement zone1 ot1] ->
            [Requirement zone2 ot2] ->
            EnvM Ordering
          go _ _ = case cast reqs2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just reqs2 -> seqM [ordRequirements reqs1 reqs2]
       in go reqs1 reqs2
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
  SacrificeCost reqs1 -> \case
    SacrificeCost reqs2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            [Requirement zone1 ot1] ->
            [Requirement zone2 ot2] ->
            EnvM Ordering
          go _ _ = case cast reqs2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just reqs2 -> seqM [ordRequirements reqs1 reqs2]
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
  AddToBattlefield player1 token1 -> \case
    AddToBattlefield player2 token2 ->
      let go ::
            forall ot1 ot2.
            (IsOTN ot1, IsOTN ot2) =>
            Token ot1 ->
            Token ot2 ->
            EnvM Ordering
          go _ _ = case cast token2 of
            Nothing -> compareOT @ot1 @ot2
            Just token2 ->
              seqM
                [ ordZoneObject player1 player2
                , ordToken token1 token2
                ]
       in go token1 token2
    y -> compareIndexM x y
  CantBeRegenerated creature1 -> \case
    CantBeRegenerated creature2 -> ordZoneObject creature1 creature2
    y -> compareIndexM x y
  ChangeTo obj1 card1 -> \case
    ChangeTo obj2 card2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast card2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just card2 ->
              seqM
                [ ordZoneObject obj1 obj2
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
  EndTheTurn -> \case
    EndTheTurn -> pure EQ
    y -> compareIndexM x y
  Exile obj1 -> \case
    Exile obj2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast obj2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just obj2 -> ordZoneObject obj1 obj2
       in go obj1 obj2
    y -> compareIndexM x y
  GainAbility obj1 ability1 -> \case
    GainAbility obj2 ability2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (obj2, ability2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (obj2, ability2) ->
              seqM
                [ ordZoneObject obj1 obj2
                , ordAbility ability1 ability2
                ]
       in go obj1 obj2
    y -> compareIndexM x y
  GainControl player1 obj1 -> \case
    GainControl player2 obj2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (player2, obj2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (player2, obj2) ->
              seqM
                [ ordZoneObject player1 player2
                , ordZoneObject obj1 obj2
                ]
       in go obj1 obj2
    y -> compareIndexM x y
  GainLife player1 amount1 -> \case
    GainLife player2 amount2 ->
      seqM [pure $ compare amount1 amount2, ordZoneObject player1 player2]
    y -> compareIndexM x y
  LoseAbility obj1 ability1 -> \case
    LoseAbility obj2 ability2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast (obj2, ability2) of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just (obj2, ability2) ->
              seqM
                [ ordZoneObject obj1 obj2
                , ordAbility ability1 ability2
                ]
       in go obj1 obj2
    y -> compareIndexM x y
  LoseLife player1 amount1 -> \case
    LoseLife player2 amount2 ->
      seqM [pure $ compare amount1 amount2, ordZoneObject player1 player2]
    y -> compareIndexM x y
  PutOntoBattlefield player1 card1 -> \case
    PutOntoBattlefield player2 card2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            ZO zone1 ot1 ->
            ZO zone2 ot2 ->
            EnvM Ordering
          go _ _ = case cast card2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just card2 ->
              seqM
                [ ordZoneObject player1 player2
                , ordZoneObject card1 card2
                ]
       in go card1 card2
    y -> compareIndexM x y
  Sacrifice player1 reqs1 -> \case
    Sacrifice player2 reqs2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            [Requirement zone1 ot1] ->
            [Requirement zone2 ot2] ->
            EnvM Ordering
          go _ _ = case cast reqs2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just reqs2 ->
              seqM
                [ ordZoneObject player1 player2
                , ordRequirements reqs1 reqs2
                ]
       in go reqs1 reqs2
    y -> compareIndexM x y
  SearchLibrary searcher1 searchee1 withCard1 -> \case
    SearchLibrary searcher2 searchee2 withCard2 ->
      let go ::
            forall ot1 ot2.
            IsOTN ot1 =>
            IsOTN ot2 =>
            WithLinkedObject 'ZLibrary (Elect 'Post (Effect 'OneShot)) ot1 ->
            WithLinkedObject 'ZLibrary (Elect 'Post (Effect 'OneShot)) ot2 ->
            EnvM Ordering
          go _ _ = case cast withCard2 of
            Nothing -> compareOT @ot1 @ot2
            Just withCard2 ->
              seqM
                [ ordZoneObject searcher1 searcher2
                , ordZoneObject searchee1 searchee2
                , ordWithLinkedObject ordElectEl withCard1 withCard2
                ]
       in go withCard1 withCard2
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
    Tap victim2 ->
      let go ::
            forall ot1 ot2.
            IsZO 'ZBattlefield ot1 =>
            IsZO 'ZBattlefield ot2 =>
            ZO 'ZBattlefield ot1 ->
            ZO 'ZBattlefield ot2 ->
            EnvM Ordering
          go _ _ = case cast victim2 of
            Nothing -> compareOT @ot1 @ot2
            Just victim2 -> ordZoneObject victim1 victim2
       in go victim1 victim2
    y -> compareIndexM x y
  Untap victim1 -> \case
    Untap victim2 ->
      let go ::
            forall ot1 ot2.
            IsZO 'ZBattlefield ot1 =>
            IsZO 'ZBattlefield ot2 =>
            ZO 'ZBattlefield ot1 ->
            ZO 'ZBattlefield ot2 ->
            EnvM Ordering
          go _ _ = case cast victim2 of
            Nothing -> compareOT @ot1 @ot2
            Just victim2 -> ordZoneObject victim1 victim2
       in go victim1 victim2
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
            WithMaskedObject zone1 (Elect p el) ot ->
            WithMaskedObject zone2 (Elect p el) ot ->
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
  OwnerOf obj1 playerToElect1 -> \case
    OwnerOf obj2 playerToElect2 ->
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
            WithMaskedObject zone1 (Elect 'Pre el) ot ->
            WithMaskedObject zone2 (Elect 'Pre el) ot ->
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

ordEntersStatic :: EntersStatic zone ot -> EntersStatic zone ot -> EnvM Ordering
ordEntersStatic = \case
  EntersTapped -> \case
    EntersTapped -> pure EQ

ordEventListener' ::
  forall liftOT.
  Typeable liftOT =>
  (forall ot. IsOTN ot => liftOT ot -> liftOT ot -> EnvM Ordering) ->
  EventListener' liftOT ->
  EventListener' liftOT ->
  EnvM Ordering
ordEventListener' ordM x = case x of
  BecomesTapped with1 -> \case
    BecomesTapped with2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithLinkedObject zone1 liftOT ot1 ->
            WithLinkedObject zone2 liftOT ot2 ->
            EnvM Ordering
          go _ _ = case cast with2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just with2 -> ordWithLinkedObject ordM with1 with2
       in go with1 with2
    y -> compareIndexM x y
  EntersBattlefield with1 -> \case
    EntersBattlefield with2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithLinkedObject zone1 liftOT ot1 ->
            WithLinkedObject zone2 liftOT ot2 ->
            EnvM Ordering
          go _ _ = case cast with2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just with2 -> ordWithLinkedObject ordM with1 with2
       in go with1 with2
    y -> compareIndexM x y
  EntersNonBattlefield with1 -> \case
    EntersNonBattlefield with2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithLinkedObject zone1 liftOT ot1 ->
            WithLinkedObject zone2 liftOT ot2 ->
            EnvM Ordering
          go _ _ = case cast with2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just with2 -> ordWithLinkedObject ordM with1 with2
       in go with1 with2
    y -> compareIndexM x y
  Events listeners1 -> \case
    Events listeners2 -> listM (ordEventListener' ordM) listeners1 listeners2
    y -> compareIndexM x y
  SpellIsCast with1 -> \case
    SpellIsCast with2 ->
      let go ::
            forall zone1 zone2 ot1 ot2.
            zone1 ~ 'ZBattlefield =>
            zone2 ~ 'ZBattlefield =>
            IsZO zone1 ot1 =>
            IsZO zone2 ot2 =>
            WithLinkedObject zone1 liftOT ot1 ->
            WithLinkedObject zone2 liftOT ot2 ->
            EnvM Ordering
          go _ _ = case cast with2 of
            Nothing -> compareZoneOT @zone1 @zone2 @ot1 @ot2
            Just with2 -> ordWithLinkedObject ordM with1 with2
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

ordObjectN' :: ObjectN ot -> ObjectN ot -> EnvM Ordering
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
    HasAbility ability2 -> ordAbility ability1 ability2
    y -> compareIndexM x y
  HasLandType type1 -> \case
    HasLandType type2 -> pure $ compare type1 type2
    y -> compareIndexM x y
  Is obj1 -> \case
    Is obj2 -> ordZoneObject obj1 obj2
    y -> compareIndexM x y
  IsOpponentOf player1 -> \case
    IsOpponentOf player2 -> ordZoneObject player1 player2
    y -> compareIndexM x y
  IsTapped -> \case
    IsTapped -> pure EQ
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
  Req2 reqsA1 reqsB1 -> \case
    Req2 reqsA2 reqsB2 ->
      seqM [ordRequirements reqsA1 reqsA2, ordRequirements reqsB1 reqsB2]
    y -> compareIndexM x y
  Req3 reqsA1 reqsB1 reqsC1 -> \case
    Req3 reqsA2 reqsB2 reqsC2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2
        , ordRequirements reqsB1 reqsB2
        , ordRequirements reqsC1 reqsC2
        ]
    y -> compareIndexM x y
  Req4 reqsA1 reqsB1 reqsC1 reqsD1 -> \case
    Req4 reqsA2 reqsB2 reqsC2 reqsD2 ->
      seqM
        [ ordRequirements reqsA1 reqsA2
        , ordRequirements reqsB1 reqsB2
        , ordRequirements reqsC1 reqsC2
        , ordRequirements reqsD1 reqsD2
        ]
    y -> compareIndexM x y
  Req5 reqsA1 reqsB1 reqsC1 reqsD1 reqsE1 -> \case
    Req5 reqsA2 reqsB2 reqsC2 reqsD2 reqsE2 ->
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
  CantBlock -> \case
    CantBlock -> pure EQ
    y -> compareIndexM x y
  Defender -> \case
    Defender -> pure EQ
    y -> compareIndexM x y
  Enters entersStatic1 -> \case
    Enters entersStatic2 -> ordEntersStatic entersStatic1 entersStatic2
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
  Landwalk reqs1 -> \case
    Landwalk reqs2 -> ordRequirements reqs1 reqs2
    y -> compareIndexM x y
  Phasing -> \case
    Phasing -> pure EQ
    y -> compareIndexM x y
  StaticContinuous elect1 -> \case
    StaticContinuous elect2 -> ordElectEl elect1 elect2
    y -> compareIndexM x y
  Suspend duration1 elect1 -> \case
    Suspend duration2 elect2 ->
      seqM [pure $ compare duration1 duration2, ordElectEl elect1 elect2]
    y -> compareIndexM x y
  Trample -> \case
    Trample -> pure EQ
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
  Token card1 -> \case
    Token card2 -> ordCard card1 card2

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
  Linked1 NonProxyElectEffect reqs1 cont1 -> \case
    Linked1 NonProxyElectEffect reqs2 cont2 ->
      ordO1 ordM reqs1 reqs2 cont1 cont2
  Linked2 NonProxyElectEffect reqs1 cont1 -> \case
    Linked2 NonProxyElectEffect reqs2 cont2 ->
      ordO2 ordM reqs1 reqs2 cont1 cont2
  Linked3 NonProxyElectEffect reqs1 cont1 -> \case
    Linked3 NonProxyElectEffect reqs2 cont2 ->
      ordO3 ordM reqs1 reqs2 cont1 cont2
  Linked4 NonProxyElectEffect reqs1 cont1 -> \case
    Linked4 NonProxyElectEffect reqs2 cont2 ->
      ordO4 ordM reqs1 reqs2 cont1 cont2
  Linked5 NonProxyElectEffect reqs1 cont1 -> \case
    Linked5 NonProxyElectEffect reqs2 cont2 ->
      ordO5 ordM reqs1 reqs2 cont1 cont2
  --
  Linked1 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    Linked1 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO1 ordM reqs1 reqs2 cont1 cont2
  Linked2 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    Linked2 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO2 ordM reqs1 reqs2 cont1 cont2
  Linked3 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    Linked3 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO3 ordM reqs1 reqs2 cont1 cont2
  Linked4 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    Linked4 NonProxyElectPrePostEffect reqs2 cont2 ->
      ordO4 ordM reqs1 reqs2 cont1 cont2
  Linked5 NonProxyElectPrePostEffect reqs1 cont1 -> \case
    Linked5 NonProxyElectPrePostEffect reqs2 cont2 ->
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
  WithMaskedObject zone (Elect p el) ot ->
  WithMaskedObject zone (Elect p el) ot ->
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
  WithMaskedObjects zone (Elect p el) ot ->
  WithMaskedObjects zone (Elect p el) ot ->
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
  This3 cont1 -> \case
    This3 cont2 ->
      let go ::
            forall a b c ota otb otc.
            ota ~ OT1 a =>
            otb ~ OT1 b =>
            otc ~ OT1 c =>
            Inst3 IsObjectType a b c =>
            ((ZO zone ota, ZO zone otb, ZO zone otc) -> liftOT ot) ->
            ((ZO zone ota, ZO zone otb, ZO zone otc) -> liftOT ot) ->
            EnvM Ordering
          go cont1 cont2 = do
            objNa' <- newObjectN @a O1
            objNb' <- newObjectN @b O1
            objNc' <- newObjectN @c O1
            let objNa = toZone objNa'
                objNb = toZone objNb'
                objNc = toZone objNc'
                lifted1 = cont1 (objNa, objNb, objNc)
                lifted2 = cont2 (objNa, objNb, objNc)
            ordM lifted1 lifted2
       in go cont1 cont2
  This4 cont1 -> \case
    This4 cont2 ->
      let go ::
            forall a b c d ota otb otc otd.
            ota ~ OT1 a =>
            otb ~ OT1 b =>
            otc ~ OT1 c =>
            otd ~ OT1 d =>
            Inst4 IsObjectType a b c d =>
            ((ZO zone ota, ZO zone otb, ZO zone otc, ZO zone otd) -> liftOT ot) ->
            ((ZO zone ota, ZO zone otb, ZO zone otc, ZO zone otd) -> liftOT ot) ->
            EnvM Ordering
          go cont1 cont2 = do
            objNa' <- newObjectN @a O1
            objNb' <- newObjectN @b O1
            objNc' <- newObjectN @c O1
            objNd' <- newObjectN @d O1
            let objNa = toZone objNa'
                objNb = toZone objNb'
                objNc = toZone objNc'
                objNd = toZone objNd'
                lifted1 = cont1 (objNa, objNb, objNc, objNd)
                lifted2 = cont2 (objNa, objNb, objNc, objNd)
            ordM lifted1 lifted2
       in go cont1 cont2
  This5 cont1 -> \case
    This5 cont2 ->
      let go ::
            forall a b c d e ota otb otc otd ote.
            ota ~ OT1 a =>
            otb ~ OT1 b =>
            otc ~ OT1 c =>
            otd ~ OT1 d =>
            ote ~ OT1 e =>
            Inst5 IsObjectType a b c d e =>
            ((ZO zone ota, ZO zone otb, ZO zone otc, ZO zone otd, ZO zone ote) -> liftOT ot) ->
            ((ZO zone ota, ZO zone otb, ZO zone otc, ZO zone otd, ZO zone ote) -> liftOT ot) ->
            EnvM Ordering
          go cont1 cont2 = do
            objNa' <- newObjectN @a O1
            objNb' <- newObjectN @b O1
            objNc' <- newObjectN @c O1
            objNd' <- newObjectN @d O1
            objNe' <- newObjectN @e O1
            let objNa = toZone objNa'
                objNb = toZone objNb'
                objNc = toZone objNc'
                objNd = toZone objNd'
                objNe = toZone objNe'
                lifted1 = cont1 (objNa, objNb, objNc, objNd, objNe)
                lifted2 = cont2 (objNa, objNb, objNc, objNd, objNe)
            ordM lifted1 lifted2
       in go cont1 cont2
 where
  reqs1 = []
  reqs2 = []

ordWithThisActivated :: IsZO zone ot => WithThisActivated zone ot -> WithThisActivated zone ot -> EnvM Ordering
ordWithThisActivated = ordWithThis ordElectEl

ordWithThisStatic :: IsZO zone ot => WithThisStatic zone ot -> WithThisStatic zone ot -> EnvM Ordering
ordWithThisStatic = ordWithThis ordStaticAbility

ordWithThisTriggered :: IsZO zone ot => WithThisTriggered zone ot -> WithThisTriggered zone ot -> EnvM Ordering
ordWithThisTriggered = ordWithThis ordTriggeredAbility

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

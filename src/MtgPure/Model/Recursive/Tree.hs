{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

-- WIP
module MtgPure.Model.Recursive.Tree (
  TreeM,
  TreeConfig (..),
  runTreeM,
  BuildTree (..),
  buildTree,
  Tree (..),
) where

import safe qualified Control.Monad.State.Strict as State
import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5, Inst6, Inst7)
import safe Data.Kind (Type)
import safe Data.Nat (Fin (..), IsNat, Nat (..), NatList (..))
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ArtifactType (ArtifactType)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.CardSet (CardSet)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage)
import safe MtgPure.Model.Defense (Defense)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana.ManaCost (ManaCost)
import safe MtgPure.Model.Mana.ManaPool (ManaPool)
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType, idToObject)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OTN,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNActivatedOrTriggeredAbility,
  OTNAny,
  OTNArtifact,
  OTNArtifactCreature,
  OTNArtifactLand,
  OTNBattle,
  OTNCreature,
  OTNCreaturePlayerPlaneswalker,
  OTNDamageSource,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNInstant,
  OTNLand,
  OTNPermanent,
  OTNPlaneswalker,
  OTNPlayer,
  OTNSorcery,
  OTNSpell,
 )
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (ObjectId),
  UntypedObject (..),
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (
  ObjectN (O1, O2a, O3a, O4a, O5a, O6a, O7a),
 )
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.Rarity (Rarity)
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  BattleType (..),
  Card (..),
  CardCharacteristic (..),
  CardSpec (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectOT (..),
  Else (..),
  Enchant (..),
  EnchantmentType (..),
  EntersStatic (..),
  Event,
  EventListener,
  EventListener' (..),
  FinPayment,
  IsSpecificCard,
  IsUser,
  List (..),
  Requirement (..),
  SetCard (..),
  SetToken (..),
  SomeZone (..),
  StaticAbility (..),
  Token (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisAbility (..),
  WithThisActivated,
  WithThisOneShot,
  WithThisStatic,
  WithThisTriggered,
  WithThisZ (..),
 )
import safe MtgPure.Model.Supertype (Supertype)
import safe MtgPure.Model.TimePoint (TimePoint)
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (
  Var (..),
  Variable (..),
  VariableId,
  VariableId' (..),
  getVariableId,
 )
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  toZone,
 )

--------------------------------------------------------------------------------

data TreeConfig = TreeConfig
  { treeConfig_ :: ()
  , treeConfig_maxCardDepth :: Maybe Int
  -- ^ This is needed because cards can be mutually recursive with other cards and/or tokens.
  -- In the general case, recursion can only be determined by card name, since comparing
  -- infinite value types is not possible.
  }

-- Don't export this.
data TreeState = TreeState
  { treeState_ :: ()
  , treeState_config :: TreeConfig
  , treeState_cardDepth :: Int
  , nextBoundObjectId :: ObjectId -- e.g. `\target ->`
  , nextBoundVariableId :: VariableId -- e.g. `\x ->`
  }

newtype TreeM a = TreeM {unTreeM :: State.State TreeState a}
  deriving (Functor)

instance Applicative TreeM where
  pure :: a -> TreeM a
  pure = TreeM . pure

  (<*>) :: TreeM (a -> b) -> TreeM a -> TreeM b
  (<*>) f x = TreeM $ unTreeM f <*> unTreeM x

instance Monad TreeM where
  (>>=) :: TreeM a -> (a -> TreeM b) -> TreeM b
  (>>=) m f = TreeM $ unTreeM m >>= unTreeM . f

instance State.MonadState TreeState TreeM where
  get :: TreeM TreeState
  get = TreeM State.get

  put :: TreeState -> TreeM ()
  put = TreeM . State.put

runTreeM :: TreeConfig -> TreeM a -> a
runTreeM config m = State.evalState (unTreeM m) st
 where
  st =
    TreeState
      { treeState_ = ()
      , treeState_config = config
      , treeState_cardDepth = 0
      , -- Start at 1 to match the object numbering of @Recursive.Show@ (whose
        -- first bound object renders as e.g. @you1@), so a Tree-based renderer
        -- can reproduce the same names.
        nextBoundObjectId = ObjectId 1
      , nextBoundVariableId = VariableId 0
      }

class BuildTree a where
  buildTreeM :: a -> TreeM (Tree a)

buildTree :: (BuildTree a) => TreeConfig -> a -> Tree a
buildTree config = runTreeM config . buildTreeM

--------------------------------------------------------------------------------

-- Binder generation. The DSL is continuation-based: constructors like
-- @ActivePlayer :: (ZOPlayer -> Elect s el ot) -> Elect s el ot@ carry
-- functions. To reify them as first-order @Tree@s we mint a fresh object (or
-- variable), feed it to the continuation, recurse on the resulting body, and
-- store both the reified binder and the body. This mirrors @Recursive.Show@.

-- | Mint a fresh 'ObjectId', bumping the counter.
newObjectId :: TreeM ObjectId
newObjectId = State.state \st ->
  let ObjectId n = nextBoundObjectId st
   in (ObjectId n, st{nextBoundObjectId = ObjectId $ n + 1})

-- | Mint a fresh object of the given type and lift it through @make@ (e.g. 'O1').
newObjectN ::
  forall a ot. (IsObjectType a) => (Object a -> ObjectN ot) -> TreeM (ObjectN ot)
newObjectN make = do
  i <- newObjectId
  let obj = idToObject @a $ UntypedObject DefaultObjectDiscriminant i
  pure $ make obj

-- | Mint a fresh zone object to feed into a continuation.
newZO ::
  forall zone a ot.
  (IsZone zone, IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  TreeM (ZO zone ot)
newZO make = toZone <$> newObjectN make

-- | Mint a fresh 'VariableId', bumping the counter.
newVariableId :: TreeM VariableId
newVariableId = State.state \st ->
  let vid = nextBoundVariableId st
   in (vid, st{nextBoundVariableId = (1 +) <$> vid})

--------------------------------------------------------------------------------

-- Notably this doesn't use continuations. It is just a pure data without embedded functions.
--
-- This is useful to have so it's easy to walk the tree in various ways with straightforward recursion.
-- No need for users to handle continuations or generate variables.
--
-- Example uses:
--  * Show record style
--  * Show functional style
--  * Simple linting (prolly better to write using the real data types instead of this tree type.)
--  * Lets printers name variables nicer since it has the full tree.
--  * Generate JSON
--  * Pretty-printing
--  * Easier control of use of parens vs dollar sign
--  * Control on whether or not combinators/smart-constructors are used vs raw constructors.
--  * Control on whether or not OTNAliases are used vs explicit OTN types.
--
-- XXX: Prolly want to keep the `ot` types since IndexOT exists and the strong types are useful.
-- In the future, if there is need, there can be a `UntypedTree` variant that doesn't have the `ot` types.
-- To create an `UntypedTree`, build one of these `Tree ot`s and then convert that to an `UntypedTree`.
-- Each UntypedTree constructor can have a lazy field for the `ot` object types as `[OT]`.
data family Tree (a :: Type) :: Type

data instance Tree (ActivatedAbility zone ot) where
  TreeAbility ::
    (IsZO zone ot) =>
    { treeActivated_cost :: Tree Cost
    , treeActivated_effect :: Tree (Elect 'ResolveStage (Effect 'OneShot) ot)
    } ->
    Tree (ActivatedAbility zone ot)
  TreeCycling :: (ot ~ OTN x, IsOTN ot) => Tree Cost -> Tree (ActivatedAbility 'ZHand ot)

data instance Tree AnyCard where
  TreeAnyCard1 :: (ot ~ OTN x, IsSpecificCard ot) => Tree (Card ot) -> Tree AnyCard
  TreeAnyCard2 :: (IsSpecificCard ot1, IsSpecificCard ot2) => Tree (Card (ot1, ot2)) -> Tree AnyCard

data instance Tree AnyToken where
  TreeAnyToken :: (IsSpecificCard ot) => Tree (Token ot) -> Tree AnyToken

data instance Tree (Case x) where
  TreeCaseFin ::
    (IsUser u, IsNat n) =>
    { treeCaseFin :: Tree (Variable (Fin u n))
    , treeOfFin :: Tree (NatList () n x)
    } ->
    Tree (Case x)

data instance Tree Condition where
  TreeCAnd :: Tree [Condition] -> Tree Condition
  TreeCNot :: Tree Condition -> Tree Condition
  TreeCOr :: Tree [Condition] -> Tree Condition
  TreeSatisfies ::
    (IsZO zone ot) =>
    Tree (ZO zone ot) ->
    Tree [Requirement zone ot] ->
    Tree Condition

data instance Tree (Variable a) where
  TreeVariable ::
    { treeVariableBaseName :: String
    , treeVariableId :: VariableId
    } ->
    Tree (Variable a)

-- | Generic list mirror: a `Tree` of a list is the list of element `Tree`s.
data instance Tree [a] where
  TreeList :: [Tree a] -> Tree [a]

-- | Zone objects are already first-order data, so their `Tree` just wraps them.
-- (The wrapper still lets printers give them nicer names via the surrounding tree.)
-- Deliberately unconstrained so binder objects of any @ot@ (e.g. @OT1 a@) reify.
data instance Tree (ZO zone ot) where
  TreeZO :: ZO zone ot -> Tree (ZO zone ot)

--------------------------------------------------------------------------------

instance BuildTree (Variable a) where
  buildTreeM :: Variable a -> TreeM (Tree (Variable a))
  buildTreeM var =
    pure
      TreeVariable
        { treeVariableBaseName = "x"
        , treeVariableId = getVariableId var
        }

instance (BuildTree a) => BuildTree [a] where
  buildTreeM :: [a] -> TreeM (Tree [a])
  buildTreeM xs = TreeList <$> mapM buildTreeM xs

instance BuildTree (ZO zone ot) where
  buildTreeM :: ZO zone ot -> TreeM (Tree (ZO zone ot))
  buildTreeM = pure . TreeZO

instance BuildTree Condition where
  buildTreeM :: Condition -> TreeM (Tree Condition)
  buildTreeM = \case
    CAnd conds -> TreeCAnd <$> buildTreeM conds
    CNot cond -> TreeCNot <$> buildTreeM cond
    COr conds -> TreeCOr <$> buildTreeM conds
    Satisfies zo reqs -> TreeSatisfies <$> buildTreeM zo <*> buildTreeM reqs

--------------------------------------------------------------------------------
-- NatList

data instance Tree (NatList u n x) where
  TreeLZ :: (Show u, Typeable u) => u -> Tree x -> Tree (NatList u 'Z x)
  TreeLS ::
    (Show u, Typeable u, IsNat n) =>
    u ->
    Tree x ->
    Tree (NatList u n x) ->
    Tree (NatList u ('S n) x)

instance (BuildTree x) => BuildTree (NatList u n x) where
  buildTreeM :: NatList u n x -> TreeM (Tree (NatList u n x))
  buildTreeM = \case
    LZ u x -> TreeLZ u <$> buildTreeM x
    LS u x xs -> (\tx txs -> TreeLS u tx txs) <$> buildTreeM x <*> buildTreeM xs

--------------------------------------------------------------------------------
-- Case

instance (BuildTree x) => BuildTree (Case x) where
  buildTreeM :: Case x -> TreeM (Tree (Case x))
  buildTreeM = \case
    CaseFin var natList -> TreeCaseFin <$> buildTreeM var <*> buildTreeM natList

--------------------------------------------------------------------------------
-- Requirement

data instance Tree (Requirement zone ot) where
  TreeControlledBy :: Tree (ZO 'ZBattlefield OTNPlayer) -> Tree (Requirement 'ZBattlefield ot)
  TreeControlsA :: Tree (Requirement 'ZBattlefield ot) -> Tree (Requirement zone OTNPlayer)
  TreeHasAbility :: Tree (SomeZone WithThisAbility ot) -> Tree (Requirement zone ot)
  TreeHasLandType :: LandType -> Tree (Requirement zone OTNLand)
  TreeIs :: (IsZO zone ot) => Tree (ZO zone ot) -> Tree (Requirement zone ot)
  TreeIsOpponentOf :: Tree (ZO 'ZBattlefield OTNPlayer) -> Tree (Requirement zone OTNPlayer)
  TreeIsTapped :: Tree (Requirement 'ZBattlefield ot)
  TreeNot :: Tree (Requirement zone ot) -> Tree (Requirement zone ot)
  TreeOfColors :: Colors -> Tree (Requirement zone ot)
  TreeOwnedBy :: Tree (ZO 'ZBattlefield OTNPlayer) -> Tree (Requirement zone ot)
  TreeRAnd :: Tree [Requirement zone ot] -> Tree (Requirement zone ot)
  TreeROr :: Tree [Requirement zone ot] -> Tree (Requirement zone ot)
  TreeReq2 ::
    (Inst2 IsObjectType a b) =>
    Tree [Requirement zone (OT1 a)] ->
    Tree [Requirement zone (OT1 b)] ->
    Tree (Requirement zone (OT2 a b))
  TreeReq3 ::
    (Inst3 IsObjectType a b c) =>
    Tree [Requirement zone (OT1 a)] ->
    Tree [Requirement zone (OT1 b)] ->
    Tree [Requirement zone (OT1 c)] ->
    Tree (Requirement zone (OT3 a b c))
  TreeReq4 ::
    (Inst4 IsObjectType a b c d) =>
    Tree [Requirement zone (OT1 a)] ->
    Tree [Requirement zone (OT1 b)] ->
    Tree [Requirement zone (OT1 c)] ->
    Tree [Requirement zone (OT1 d)] ->
    Tree (Requirement zone (OT4 a b c d))
  TreeReq5 ::
    (Inst5 IsObjectType a b c d e) =>
    Tree [Requirement zone (OT1 a)] ->
    Tree [Requirement zone (OT1 b)] ->
    Tree [Requirement zone (OT1 c)] ->
    Tree [Requirement zone (OT1 d)] ->
    Tree [Requirement zone (OT1 e)] ->
    Tree (Requirement zone (OT5 a b c d e))

instance BuildTree (Requirement zone ot) where
  buildTreeM :: Requirement zone ot -> TreeM (Tree (Requirement zone ot))
  buildTreeM = \case
    ControlledBy p -> TreeControlledBy <$> buildTreeM p
    ControlsA r -> TreeControlsA <$> buildTreeM r
    HasAbility someZone -> TreeHasAbility <$> buildTreeM someZone
    HasLandType lt -> pure $ TreeHasLandType lt
    Is zo -> TreeIs <$> buildTreeM zo
    IsOpponentOf p -> TreeIsOpponentOf <$> buildTreeM p
    IsTapped -> pure TreeIsTapped
    Not r -> TreeNot <$> buildTreeM r
    OfColors c -> pure $ TreeOfColors c
    OwnedBy p -> TreeOwnedBy <$> buildTreeM p
    RAnd rs -> TreeRAnd <$> buildTreeM rs
    ROr rs -> TreeROr <$> buildTreeM rs
    Req2 a b -> TreeReq2 <$> buildTreeM a <*> buildTreeM b
    Req3 a b c -> TreeReq3 <$> buildTreeM a <*> buildTreeM b <*> buildTreeM c
    Req4 a b c d -> TreeReq4 <$> buildTreeM a <*> buildTreeM b <*> buildTreeM c <*> buildTreeM d
    Req5 a b c d e ->
      TreeReq5 <$> buildTreeM a <*> buildTreeM b <*> buildTreeM c <*> buildTreeM d <*> buildTreeM e

--------------------------------------------------------------------------------
-- Cost

data instance Tree Cost where
  TreeAndCosts :: Tree [Cost] -> Tree Cost
  TreeCostCase :: Tree (Case Cost) -> Tree Cost
  TreeDiscardRandomCost :: Int -> Tree Cost
  TreeExileCost :: Tree [Requirement zone ot'] -> Tree Cost
  TreeLoyaltyCost :: Tree (ZO 'ZBattlefield OTNPlaneswalker) -> Loyalty -> Tree Cost
  TreeManaCost :: ManaCost 'Var -> Tree Cost
  TreeOrCosts :: Tree [Cost] -> Tree Cost
  TreePayLife :: Int -> Tree Cost
  TreeSacrificeCost :: (IsOTN ot') => Tree [Requirement 'ZBattlefield ot'] -> Tree Cost
  TreeTapCost :: Tree [Requirement 'ZBattlefield ot'] -> Tree Cost

instance BuildTree Cost where
  buildTreeM :: Cost -> TreeM (Tree Cost)
  buildTreeM = \case
    AndCosts cs -> TreeAndCosts <$> buildTreeM cs
    CostCase c -> TreeCostCase <$> buildTreeM c
    DiscardRandomCost n -> pure $ TreeDiscardRandomCost n
    ExileCost rs -> TreeExileCost <$> buildTreeM rs
    LoyaltyCost pw l -> (\t -> TreeLoyaltyCost t l) <$> buildTreeM pw
    ManaCost mc -> pure $ TreeManaCost mc
    OrCosts cs -> TreeOrCosts <$> buildTreeM cs
    PayLife n -> pure $ TreePayLife n
    SacrificeCost rs -> TreeSacrificeCost <$> buildTreeM rs
    TapCost rs -> TreeTapCost <$> buildTreeM rs

--------------------------------------------------------------------------------
-- Proxy (used as the @liftOT@ of `Event`)

data instance Tree (Proxy ot) where
  TreeProxy :: Tree (Proxy ot)

instance BuildTree (Proxy ot) where
  buildTreeM :: Proxy ot -> TreeM (Tree (Proxy ot))
  buildTreeM _ = pure TreeProxy

--------------------------------------------------------------------------------
-- ElectOT (thin newtype wrapper; @BuildTree@ deferred until `Elect` exists)

data instance Tree (ElectOT s liftOT ot) where
  TreeElectOT :: Tree (Elect s (liftOT ot) ot) -> Tree (ElectOT s liftOT ot)

--------------------------------------------------------------------------------
-- WithLinkedObject: the linked object shares the carrier's @ot@.

data instance Tree (WithLinkedObject liftOT zone ot) where
  TreeLinked1 ::
    (ot ~ OT1 a, Inst1 IsObjectType a) =>
    Tree [Requirement zone ot] ->
    Tree (ZO zone ot) ->
    Tree (liftOT ot) ->
    Tree (WithLinkedObject liftOT zone ot)
  TreeLinked2 ::
    (ot ~ OT2 a b, Inst2 IsObjectType a b) =>
    Tree [Requirement zone ot] ->
    Tree (ZO zone ot) ->
    Tree (liftOT ot) ->
    Tree (WithLinkedObject liftOT zone ot)
  TreeLinked3 ::
    (ot ~ OT3 a b c, Inst3 IsObjectType a b c) =>
    Tree [Requirement zone ot] ->
    Tree (ZO zone ot) ->
    Tree (liftOT ot) ->
    Tree (WithLinkedObject liftOT zone ot)
  TreeLinked4 ::
    (ot ~ OT4 a b c d, Inst4 IsObjectType a b c d) =>
    Tree [Requirement zone ot] ->
    Tree (ZO zone ot) ->
    Tree (liftOT ot) ->
    Tree (WithLinkedObject liftOT zone ot)
  TreeLinked5 ::
    (ot ~ OT5 a b c d e, Inst5 IsObjectType a b c d e) =>
    Tree [Requirement zone ot] ->
    Tree (ZO zone ot) ->
    Tree (liftOT ot) ->
    Tree (WithLinkedObject liftOT zone ot)

instance
  (IsZone zone, BuildTree (liftOT ot)) =>
  BuildTree (WithLinkedObject liftOT zone ot)
  where
  buildTreeM ::
    WithLinkedObject liftOT zone ot ->
    TreeM (Tree (WithLinkedObject liftOT zone ot))
  buildTreeM = \case
    Linked1 reqs cont -> goLinked TreeLinked1 reqs cont (newZO @zone O1)
    Linked2 reqs cont -> goLinked TreeLinked2 reqs cont (newZO @zone O2a)
    Linked3 reqs cont -> goLinked TreeLinked3 reqs cont (newZO @zone O3a)
    Linked4 reqs cont -> goLinked TreeLinked4 reqs cont (newZO @zone O4a)
    Linked5 reqs cont -> goLinked TreeLinked5 reqs cont (newZO @zone O5a)
   where
    goLinked make reqs cont mkZo = do
      tReqs <- buildTreeM reqs
      zo <- mkZo
      tZo <- buildTreeM zo
      tBody <- buildTreeM (cont zo)
      pure $ make tReqs tZo tBody

--------------------------------------------------------------------------------
-- WithMaskedObject: the masked object's @ot'@ is independent of the result @ot@.

data instance Tree (WithMaskedObject liftOT zone ot) where
  TreeMasked1 ::
    (Inst1 IsObjectType a, IsOTN (OT1 a)) =>
    Tree [Requirement zone (OT1 a)] ->
    Tree (ZO zone (OT1 a)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObject liftOT zone ot)
  TreeMasked2 ::
    (Inst2 IsObjectType a b, IsOTN (OT2 a b)) =>
    Tree [Requirement zone (OT2 a b)] ->
    Tree (ZO zone (OT2 a b)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObject liftOT zone ot)
  TreeMasked3 ::
    (Inst3 IsObjectType a b c, IsOTN (OT3 a b c)) =>
    Tree [Requirement zone (OT3 a b c)] ->
    Tree (ZO zone (OT3 a b c)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObject liftOT zone ot)
  TreeMasked4 ::
    (Inst4 IsObjectType a b c d, IsOTN (OT4 a b c d)) =>
    Tree [Requirement zone (OT4 a b c d)] ->
    Tree (ZO zone (OT4 a b c d)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObject liftOT zone ot)
  TreeMasked5 ::
    (Inst5 IsObjectType a b c d e, IsOTN (OT5 a b c d e)) =>
    Tree [Requirement zone (OT5 a b c d e)] ->
    Tree (ZO zone (OT5 a b c d e)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObject liftOT zone ot)
  TreeMasked6 ::
    (Inst6 IsObjectType a b c d e f, IsOTN (OT6 a b c d e f)) =>
    Tree [Requirement zone (OT6 a b c d e f)] ->
    Tree (ZO zone (OT6 a b c d e f)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObject liftOT zone ot)
  TreeMasked7 ::
    (Inst7 IsObjectType a b c d e f g, IsOTN (OT7 a b c d e f g)) =>
    Tree [Requirement zone (OT7 a b c d e f g)] ->
    Tree (ZO zone (OT7 a b c d e f g)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObject liftOT zone ot)

instance
  (IsZone zone, BuildTree (liftOT ot)) =>
  BuildTree (WithMaskedObject liftOT zone ot)
  where
  buildTreeM ::
    WithMaskedObject liftOT zone ot ->
    TreeM (Tree (WithMaskedObject liftOT zone ot))
  buildTreeM = \case
    Masked1 reqs cont -> goMasked TreeMasked1 reqs cont (newZO @zone O1)
    Masked2 reqs cont -> goMasked TreeMasked2 reqs cont (newZO @zone O2a)
    Masked3 reqs cont -> goMasked TreeMasked3 reqs cont (newZO @zone O3a)
    Masked4 reqs cont -> goMasked TreeMasked4 reqs cont (newZO @zone O4a)
    Masked5 reqs cont -> goMasked TreeMasked5 reqs cont (newZO @zone O5a)
    Masked6 reqs cont -> goMasked TreeMasked6 reqs cont (newZO @zone O6a)
    Masked7 reqs cont -> goMasked TreeMasked7 reqs cont (newZO @zone O7a)
   where
    goMasked make reqs cont mkZo = do
      tReqs <- buildTreeM reqs
      zo <- mkZo
      tZo <- buildTreeM zo
      tBody <- buildTreeM (cont zo)
      pure $ make tReqs tZo tBody

--------------------------------------------------------------------------------
-- WithMaskedObjects: like WithMaskedObject but the continuation takes a list.
-- We reify a single representative object (mirroring `Recursive.Show`).

data instance Tree (WithMaskedObjects liftOT zone ot) where
  TreeMaskeds1 ::
    (Inst1 IsObjectType a, IsOTN (OT1 a)) =>
    Tree [Requirement zone (OT1 a)] ->
    Tree (ZO zone (OT1 a)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObjects liftOT zone ot)
  TreeMaskeds2 ::
    (Inst2 IsObjectType a b, IsOTN (OT2 a b)) =>
    Tree [Requirement zone (OT2 a b)] ->
    Tree (ZO zone (OT2 a b)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObjects liftOT zone ot)
  TreeMaskeds3 ::
    (Inst3 IsObjectType a b c, IsOTN (OT3 a b c)) =>
    Tree [Requirement zone (OT3 a b c)] ->
    Tree (ZO zone (OT3 a b c)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObjects liftOT zone ot)
  TreeMaskeds4 ::
    (Inst4 IsObjectType a b c d, IsOTN (OT4 a b c d)) =>
    Tree [Requirement zone (OT4 a b c d)] ->
    Tree (ZO zone (OT4 a b c d)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObjects liftOT zone ot)
  TreeMaskeds5 ::
    (Inst5 IsObjectType a b c d e, IsOTN (OT5 a b c d e)) =>
    Tree [Requirement zone (OT5 a b c d e)] ->
    Tree (ZO zone (OT5 a b c d e)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObjects liftOT zone ot)
  TreeMaskeds6 ::
    (Inst6 IsObjectType a b c d e f, IsOTN (OT6 a b c d e f)) =>
    Tree [Requirement zone (OT6 a b c d e f)] ->
    Tree (ZO zone (OT6 a b c d e f)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObjects liftOT zone ot)
  TreeMaskeds7 ::
    (Inst7 IsObjectType a b c d e f g, IsOTN (OT7 a b c d e f g)) =>
    Tree [Requirement zone (OT7 a b c d e f g)] ->
    Tree (ZO zone (OT7 a b c d e f g)) ->
    Tree (liftOT ot) ->
    Tree (WithMaskedObjects liftOT zone ot)

instance
  (IsZone zone, BuildTree (liftOT ot)) =>
  BuildTree (WithMaskedObjects liftOT zone ot)
  where
  buildTreeM ::
    WithMaskedObjects liftOT zone ot ->
    TreeM (Tree (WithMaskedObjects liftOT zone ot))
  buildTreeM = \case
    Maskeds1 reqs cont -> goMaskeds TreeMaskeds1 reqs cont (newZO @zone O1)
    Maskeds2 reqs cont -> goMaskeds TreeMaskeds2 reqs cont (newZO @zone O2a)
    Maskeds3 reqs cont -> goMaskeds TreeMaskeds3 reqs cont (newZO @zone O3a)
    Maskeds4 reqs cont -> goMaskeds TreeMaskeds4 reqs cont (newZO @zone O4a)
    Maskeds5 reqs cont -> goMaskeds TreeMaskeds5 reqs cont (newZO @zone O5a)
    Maskeds6 reqs cont -> goMaskeds TreeMaskeds6 reqs cont (newZO @zone O6a)
    Maskeds7 reqs cont -> goMaskeds TreeMaskeds7 reqs cont (newZO @zone O7a)
   where
    goMaskeds make reqs cont mkZo = do
      tReqs <- buildTreeM reqs
      zo <- mkZo
      tZo <- buildTreeM zo
      tBody <- buildTreeM (cont (pure zo))
      pure $ make tReqs tZo tBody

--------------------------------------------------------------------------------
-- WithThis: reifies the (possibly multi-part) self object.

data instance Tree (WithThis liftOT zone ot) where
  TreeThis1 ::
    (Inst1 IsObjectType a) =>
    Tree (ZO zone (OT1 a)) ->
    Tree (liftOT (OT1 a)) ->
    Tree (WithThis liftOT zone (OT1 a))
  TreeThis2 ::
    (Inst2 IsObjectType a b) =>
    Tree (ZO zone (OT1 a)) ->
    Tree (ZO zone (OT1 b)) ->
    Tree (liftOT (OT2 a b)) ->
    Tree (WithThis liftOT zone (OT2 a b))
  TreeThis3 ::
    (Inst3 IsObjectType a b c) =>
    Tree (ZO zone (OT1 a)) ->
    Tree (ZO zone (OT1 b)) ->
    Tree (ZO zone (OT1 c)) ->
    Tree (liftOT (OT3 a b c)) ->
    Tree (WithThis liftOT zone (OT3 a b c))
  TreeThis4 ::
    (Inst4 IsObjectType a b c d) =>
    Tree (ZO zone (OT1 a)) ->
    Tree (ZO zone (OT1 b)) ->
    Tree (ZO zone (OT1 c)) ->
    Tree (ZO zone (OT1 d)) ->
    Tree (liftOT (OT4 a b c d)) ->
    Tree (WithThis liftOT zone (OT4 a b c d))
  TreeThis5 ::
    (Inst5 IsObjectType a b c d e) =>
    Tree (ZO zone (OT1 a)) ->
    Tree (ZO zone (OT1 b)) ->
    Tree (ZO zone (OT1 c)) ->
    Tree (ZO zone (OT1 d)) ->
    Tree (ZO zone (OT1 e)) ->
    Tree (liftOT (OT5 a b c d e)) ->
    Tree (WithThis liftOT zone (OT5 a b c d e))
  TreeThis6 ::
    (Inst6 IsObjectType a b c d e f) =>
    Tree (ZO zone (OT1 a)) ->
    Tree (ZO zone (OT1 b)) ->
    Tree (ZO zone (OT1 c)) ->
    Tree (ZO zone (OT1 d)) ->
    Tree (ZO zone (OT1 e)) ->
    Tree (ZO zone (OT1 f)) ->
    Tree (liftOT (OT6 a b c d e f)) ->
    Tree (WithThis liftOT zone (OT6 a b c d e f))

instance
  (IsZone zone, BuildTree (liftOT ot)) =>
  BuildTree (WithThis liftOT zone ot)
  where
  buildTreeM :: WithThis liftOT zone ot -> TreeM (Tree (WithThis liftOT zone ot))
  buildTreeM = \case
    This1 cont -> do
      a <- newZO @zone O1
      TreeThis1 <$> buildTreeM a <*> buildTreeM (cont a)
    This2 cont -> do
      a <- newZO @zone O1
      b <- newZO @zone O1
      TreeThis2 <$> buildTreeM a <*> buildTreeM b <*> buildTreeM (cont (a, b))
    This3 cont -> do
      a <- newZO @zone O1
      b <- newZO @zone O1
      c <- newZO @zone O1
      TreeThis3 <$> buildTreeM a <*> buildTreeM b <*> buildTreeM c <*> buildTreeM (cont (a, b, c))
    This4 cont -> do
      a <- newZO @zone O1
      b <- newZO @zone O1
      c <- newZO @zone O1
      d <- newZO @zone O1
      TreeThis4
        <$> buildTreeM a
        <*> buildTreeM b
        <*> buildTreeM c
        <*> buildTreeM d
        <*> buildTreeM (cont (a, b, c, d))
    This5 cont -> do
      a <- newZO @zone O1
      b <- newZO @zone O1
      c <- newZO @zone O1
      d <- newZO @zone O1
      e <- newZO @zone O1
      TreeThis5
        <$> buildTreeM a
        <*> buildTreeM b
        <*> buildTreeM c
        <*> buildTreeM d
        <*> buildTreeM e
        <*> buildTreeM (cont (a, b, c, d, e))
    This6 cont -> do
      a <- newZO @zone O1
      b <- newZO @zone O1
      c <- newZO @zone O1
      d <- newZO @zone O1
      e <- newZO @zone O1
      f <- newZO @zone O1
      TreeThis6
        <$> buildTreeM a
        <*> buildTreeM b
        <*> buildTreeM c
        <*> buildTreeM d
        <*> buildTreeM e
        <*> buildTreeM f
        <*> buildTreeM (cont (a, b, c, d, e, f))

--------------------------------------------------------------------------------
-- More binder helpers used by `Elect`.

-- | Mint a fresh battlefield player object (the most common bound object).
newPlayer :: TreeM (ZO 'ZBattlefield OTNPlayer)
newPlayer = newZO @'ZBattlefield @'OTPlayer O1

-- | Mint a fresh reified variable holding the given placeholder value.
newVar :: a -> TreeM (Variable a)
newVar x = do
  vid <- newVariableId
  pure $ ReifiedVariable vid x

--------------------------------------------------------------------------------
-- ElectOT (build; deferred from above now that `Elect` has an instance)

instance BuildTree (ElectOT s liftOT ot) where
  buildTreeM :: ElectOT s liftOT ot -> TreeM (Tree (ElectOT s liftOT ot))
  buildTreeM (ElectOT e) = TreeElectOT <$> buildTreeM e

--------------------------------------------------------------------------------
-- Elect

data instance Tree (Elect s el ot) where
  TreeActivePlayer ::
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (Elect s el ot) ->
    Tree (Elect s el ot)
  TreeAll ::
    Tree (WithMaskedObjects (Elect s el) 'ZBattlefield ot) ->
    Tree (Elect s el ot)
  TreeChoose ::
    (IsZone zone) =>
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (WithMaskedObject (Elect s el) zone ot) ->
    Tree (Elect s el ot)
  TreeChooseOption ::
    (IsUser u, IsNat n) =>
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (NatList u n Condition) ->
    Tree (Variable (Fin u n)) ->
    Tree (Elect s el ot) ->
    Tree (Elect s el ot)
  TreeElectCondition :: Tree Condition -> Tree (Elect s Condition ot)
  TreeControllerOf ::
    (IsZO zone OTNAny) =>
    Tree (ZO zone OTNAny) ->
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (Elect s el ot) ->
    Tree (Elect s el ot)
  TreeElectCost :: Tree Cost -> Tree (Elect 'IntrinsicStage Cost ot)
  TreeElectEffect ::
    (Typeable ef) => Tree [Effect ef] -> Tree (Elect 'ResolveStage (Effect ef) ot)
  TreeElectActivated ::
    Tree (ActivatedAbility zone ot) ->
    Tree (Elect 'TargetStage (ActivatedAbility zone ot) ot)
  TreeElectCardFacet ::
    Tree (CardCharacteristic ot) ->
    Tree (Elect 'IntrinsicStage (CardCharacteristic ot) ot)
  TreeElectCardSpec ::
    Tree (CardSpec ot) -> Tree (Elect 'TargetStage (CardSpec ot) ot)
  TreeElectCase :: Tree (Case (Elect s el ot)) -> Tree (Elect s el ot)
  TreeEndTargets ::
    (Typeable el) =>
    Tree (Elect 'ResolveStage el ot) ->
    Tree (Elect 'TargetStage (Elect 'ResolveStage el ot) ot)
  TreeElectEvent :: Tree Event -> Tree (Elect 'ResolveStage Event ot)
  TreeIf ::
    Tree Condition ->
    Tree (Elect s el ot) ->
    Tree (Else s el ot) ->
    Tree (Elect s el ot)
  TreeListen :: Tree EventListener -> Tree (Elect 'IntrinsicStage EventListener ot)
  TreeOwnerOf ::
    (IsZO zone OTNAny) =>
    Tree (ZO zone OTNAny) ->
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (Elect s el ot) ->
    Tree (Elect s el ot)
  TreePlayerPays ::
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree Cost ->
    Tree (Variable FinPayment) ->
    Tree (Elect 'ResolveStage el ot) ->
    Tree (Elect 'ResolveStage el ot)
  TreeRandom ::
    Tree (WithMaskedObject (Elect 'ResolveStage el) 'ZBattlefield ot) ->
    Tree (Elect 'ResolveStage el ot)
  TreeTarget ::
    (IsZone zone) =>
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (WithMaskedObject (Elect 'TargetStage el) zone ot) ->
    Tree (Elect 'TargetStage el ot)
  TreeVariableFromPower ::
    Tree (ZO 'ZBattlefield OTNCreature) ->
    Tree (Variable Int) ->
    Tree (Elect 'ResolveStage el ot) ->
    Tree (Elect 'ResolveStage el ot)
  TreeVariableInt ::
    Tree (Variable Int) ->
    Tree (Elect 'TargetStage el ot) ->
    Tree (Elect 'TargetStage el ot)
  TreeYour ::
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (Elect 'IntrinsicStage el ot) ->
    Tree (Elect 'IntrinsicStage el ot)

instance BuildTree (Elect s el ot) where
  buildTreeM :: Elect s el ot -> TreeM (Tree (Elect s el ot))
  buildTreeM = \case
    ActivePlayer cont -> do
      p <- newPlayer
      TreeActivePlayer <$> buildTreeM p <*> buildTreeM (cont p)
    All wmos -> TreeAll <$> buildTreeM wmos
    Choose player wmo -> TreeChoose <$> buildTreeM player <*> buildTreeM wmo
    ChooseOption player natList cont -> do
      var <- newVar FZ
      TreeChooseOption
        <$> buildTreeM player
        <*> buildTreeM natList
        <*> buildTreeM var
        <*> buildTreeM (cont var)
    Condition cond -> TreeElectCondition <$> buildTreeM cond
    ControllerOf zo cont -> do
      p <- newPlayer
      TreeControllerOf <$> buildTreeM zo <*> buildTreeM p <*> buildTreeM (cont p)
    Cost cost -> TreeElectCost <$> buildTreeM cost
    Effect effects -> TreeElectEffect <$> buildTreeM effects
    ElectActivated activated -> TreeElectActivated <$> buildTreeM activated
    ElectCardFacet facet -> TreeElectCardFacet <$> buildTreeM facet
    ElectCardSpec spec -> TreeElectCardSpec <$> buildTreeM spec
    ElectCase case_ -> TreeElectCase <$> buildTreeM case_
    EndTargets elect -> TreeEndTargets <$> buildTreeM elect
    Event event -> TreeElectEvent <$> buildTreeM event
    If cond then_ else_ ->
      TreeIf <$> buildTreeM cond <*> buildTreeM then_ <*> buildTreeM else_
    Listen listener -> TreeListen <$> buildTreeM listener
    OwnerOf zo cont -> do
      p <- newPlayer
      TreeOwnerOf <$> buildTreeM zo <*> buildTreeM p <*> buildTreeM (cont p)
    PlayerPays player cost cont -> do
      var <- newVar FZ
      TreePlayerPays
        <$> buildTreeM player
        <*> buildTreeM cost
        <*> buildTreeM var
        <*> buildTreeM (cont var)
    Random wmo -> TreeRandom <$> buildTreeM wmo
    Target player wmo -> TreeTarget <$> buildTreeM player <*> buildTreeM wmo
    VariableFromPower creature cont -> do
      var <- newVar 0
      TreeVariableFromPower
        <$> buildTreeM creature
        <*> buildTreeM var
        <*> buildTreeM (cont var)
    VariableInt cont -> do
      var <- newVar 0
      TreeVariableInt <$> buildTreeM var <*> buildTreeM (cont var)
    Your cont -> do
      p <- newPlayer
      TreeYour <$> buildTreeM p <*> buildTreeM (cont p)

--------------------------------------------------------------------------------
-- Else

data instance Tree (Else s el ot) where
  TreeElseCost :: (el ~ Cost) => Tree (Elect s el ot) -> Tree (Else s el ot)
  TreeElseEffect :: (el ~ Effect 'OneShot) => Tree (Elect s el ot) -> Tree (Else s el ot)
  TreeElseEvent :: (el ~ EventListener' liftOT) => Tree (Else s el ot)

instance BuildTree (Else s el ot) where
  buildTreeM :: Else s el ot -> TreeM (Tree (Else s el ot))
  buildTreeM = \case
    ElseCost elect -> TreeElseCost <$> buildTreeM elect
    ElseEffect elect -> TreeElseEffect <$> buildTreeM elect
    ElseEvent -> pure TreeElseEvent

--------------------------------------------------------------------------------
-- EventListener' (and thus Event / EventListener)

data instance Tree (EventListener' liftOT) where
  TreeBecomesTapped ::
    (IsOTN ot) =>
    Tree (WithLinkedObject liftOT 'ZBattlefield ot) ->
    Tree (EventListener' liftOT)
  TreeEntersBattlefield ::
    (IsOTN ot) =>
    Tree (WithLinkedObject liftOT 'ZBattlefield ot) ->
    Tree (EventListener' liftOT)
  TreeEntersNonBattlefield ::
    (IsZO zone ot) =>
    Tree (WithLinkedObject liftOT zone ot) ->
    Tree (EventListener' liftOT)
  TreeEvents :: Tree [EventListener' liftOT] -> Tree (EventListener' liftOT)
  TreeSpellIsCast ::
    (IsOTN ot) =>
    Tree (WithLinkedObject liftOT 'ZBattlefield ot) ->
    Tree (EventListener' liftOT)
  TreeTimePoint ::
    (Typeable p) =>
    TimePoint p ->
    Tree (liftOT OTNPlayer) ->
    Tree (EventListener' liftOT)

-- The quantified constraint lives on this helper (a function, so it is not
-- subject to the instance-termination check) rather than on a generic
-- @EventListener'@ instance; concrete instances below discharge it against the
-- blanket @Proxy@/@Elect@ instances. This avoids needing @UndecidableInstances@.
buildEventListenerM ::
  (forall ot. BuildTree (liftOT ot)) =>
  EventListener' liftOT ->
  TreeM (Tree (EventListener' liftOT))
buildEventListenerM = \case
  BecomesTapped w -> TreeBecomesTapped <$> buildTreeM w
  EntersBattlefield w -> TreeEntersBattlefield <$> buildTreeM w
  EntersNonBattlefield w -> TreeEntersNonBattlefield <$> buildTreeM w
  -- Recurse via the helper directly (there is no generic instance to call).
  Events ws -> TreeEvents . TreeList <$> mapM buildEventListenerM ws
  SpellIsCast w -> TreeSpellIsCast <$> buildTreeM w
  TimePoint tp x -> (\t -> TreeTimePoint tp t) <$> buildTreeM x

-- | @Event = EventListener' Proxy@
instance BuildTree (EventListener' Proxy) where
  buildTreeM :: EventListener' Proxy -> TreeM (Tree (EventListener' Proxy))
  buildTreeM = buildEventListenerM

-- | @EventListener = EventListener' (Elect 'ResolveStage (Effect 'OneShot))@
instance BuildTree (EventListener' (Elect 'ResolveStage (Effect 'OneShot))) where
  buildTreeM ::
    EventListener' (Elect 'ResolveStage (Effect 'OneShot)) ->
    TreeM (Tree (EventListener' (Elect 'ResolveStage (Effect 'OneShot))))
  buildTreeM = buildEventListenerM

--------------------------------------------------------------------------------
-- Effect

data instance Tree (Effect ef) where
  TreeAddMana ::
    Tree (ZO 'ZBattlefield OTNPlayer) -> ManaPool 'NonSnow -> Tree (Effect 'OneShot)
  TreeAddToBattlefield ::
    Tree (ZO 'ZBattlefield OTNPlayer) -> Tree (Token ot) -> Tree (Effect 'OneShot)
  TreeCantBeRegenerated ::
    Tree (ZO 'ZBattlefield OTNCreature) -> Tree (Effect 'Continuous)
  TreeChangeTo ::
    Tree (ZO 'ZBattlefield OTNPermanent) -> Tree (Card ot) -> Tree (Effect 'Continuous)
  TreeCounterAbility ::
    Tree (ZO 'ZStack OTNActivatedOrTriggeredAbility) -> Tree (Effect 'OneShot)
  TreeCounterSpell :: Tree (ZO 'ZStack OTNSpell) -> Tree (Effect 'OneShot)
  TreeDealDamage ::
    (IsZO zone OTNDamageSource) =>
    Tree (ZO zone OTNDamageSource) ->
    Tree (ZO 'ZBattlefield OTNCreaturePlayerPlaneswalker) ->
    Damage 'Var ->
    Tree (Effect 'OneShot)
  TreeDestroy :: Tree (ZO 'ZBattlefield OTNPermanent) -> Tree (Effect 'OneShot)
  TreeDrawCards :: Tree (ZO 'ZBattlefield OTNPlayer) -> Int -> Tree (Effect 'OneShot)
  TreeEffectCase :: Tree (Case (Effect ef)) -> Tree (Effect ef)
  TreeEffectContinuous :: Tree (Effect 'Continuous) -> Tree (Effect 'OneShot)
  TreeEndTheTurn :: Tree (Effect 'OneShot)
  TreeExile :: (IsZO zone ot) => Tree (ZO zone ot) -> Tree (Effect 'OneShot)
  TreeGainAbility ::
    (IsOTN ot) =>
    Tree (ZO 'ZBattlefield ot) ->
    Tree (WithThisAbility 'ZBattlefield ot) ->
    Tree (Effect 'Continuous)
  TreeGainControl ::
    (IsOTN ot) =>
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (ZO 'ZBattlefield ot) ->
    Tree (Effect 'Continuous)
  TreeGainLife :: Tree (ZO 'ZBattlefield OTNPlayer) -> Int -> Tree (Effect 'OneShot)
  TreeLoseAbility ::
    (IsOTN ot) =>
    Tree (ZO 'ZBattlefield ot) ->
    Tree (WithThisAbility 'ZBattlefield ot) ->
    Tree (Effect 'Continuous)
  TreeLoseLife :: Tree (ZO 'ZBattlefield OTNPlayer) -> Int -> Tree (Effect 'OneShot)
  TreePutOntoBattlefield ::
    (IsZO zone ot) =>
    Tree (ZO 'ZBattlefield OTNPlayer) -> Tree (ZO zone ot) -> Tree (Effect 'OneShot)
  TreeSacrifice ::
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree [Requirement 'ZBattlefield ot] ->
    Tree (Effect 'OneShot)
  TreeSearchLibrary ::
    (IsOTN ot) =>
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (WithLinkedObject (Elect 'ResolveStage (Effect 'OneShot)) 'ZLibrary ot) ->
    Tree (Effect 'OneShot)
  TreeSequence :: Tree [Effect ef] -> Tree (Effect ef)
  TreeShuffleLibrary :: Tree (ZO 'ZBattlefield OTNPlayer) -> Tree (Effect 'OneShot)
  TreeStatDelta ::
    Tree (ZO 'ZBattlefield OTNCreature) -> Power -> Toughness -> Tree (Effect 'Continuous)
  TreeTap :: (IsOTN ot) => Tree (ZO 'ZBattlefield ot) -> Tree (Effect 'OneShot)
  TreeUntap :: (IsOTN ot) => Tree (ZO 'ZBattlefield ot) -> Tree (Effect 'OneShot)
  TreeUntil ::
    Tree (Elect 'ResolveStage Event OTNPlayer) ->
    Tree (Effect 'Continuous) ->
    Tree (Effect 'Continuous)
  TreeWithList :: Tree (WithList (Effect ef) zone ot) -> Tree (Effect ef)

instance BuildTree (Effect ef) where
  buildTreeM :: Effect ef -> TreeM (Tree (Effect ef))
  buildTreeM = \case
    AddMana player mana -> (\t -> TreeAddMana t mana) <$> buildTreeM player
    AddToBattlefield player token ->
      TreeAddToBattlefield <$> buildTreeM player <*> buildTreeM token
    CantBeRegenerated creature -> TreeCantBeRegenerated <$> buildTreeM creature
    ChangeTo before after -> TreeChangeTo <$> buildTreeM before <*> buildTreeM after
    CounterAbility obj -> TreeCounterAbility <$> buildTreeM obj
    CounterSpell obj -> TreeCounterSpell <$> buildTreeM obj
    DealDamage source victim damage ->
      (\s v -> TreeDealDamage s v damage) <$> buildTreeM source <*> buildTreeM victim
    Destroy obj -> TreeDestroy <$> buildTreeM obj
    DrawCards player n -> (\t -> TreeDrawCards t n) <$> buildTreeM player
    EffectCase case_ -> TreeEffectCase <$> buildTreeM case_
    EffectContinuous effect -> TreeEffectContinuous <$> buildTreeM effect
    EndTheTurn -> pure TreeEndTheTurn
    Exile obj -> TreeExile <$> buildTreeM obj
    GainAbility obj ability -> TreeGainAbility <$> buildTreeM obj <*> buildTreeM ability
    GainControl player obj -> TreeGainControl <$> buildTreeM player <*> buildTreeM obj
    GainLife player n -> (\t -> TreeGainLife t n) <$> buildTreeM player
    LoseAbility obj ability -> TreeLoseAbility <$> buildTreeM obj <*> buildTreeM ability
    LoseLife player n -> (\t -> TreeLoseLife t n) <$> buildTreeM player
    PutOntoBattlefield player obj ->
      TreePutOntoBattlefield <$> buildTreeM player <*> buildTreeM obj
    Sacrifice player reqs -> TreeSacrifice <$> buildTreeM player <*> buildTreeM reqs
    SearchLibrary searcher searchee withCard ->
      TreeSearchLibrary <$> buildTreeM searcher <*> buildTreeM searchee <*> buildTreeM withCard
    Sequence effects -> TreeSequence <$> buildTreeM effects
    ShuffleLibrary player -> TreeShuffleLibrary <$> buildTreeM player
    StatDelta creature power toughness ->
      (\t -> TreeStatDelta t power toughness) <$> buildTreeM creature
    Tap obj -> TreeTap <$> buildTreeM obj
    Untap obj -> TreeUntap <$> buildTreeM obj
    Until electEvent effect -> TreeUntil <$> buildTreeM electEvent <*> buildTreeM effect
    WithList withList -> TreeWithList <$> buildTreeM withList

--------------------------------------------------------------------------------
-- WithList

lenseList :: List x -> x
lenseList = \case
  List [x] -> x
  _ -> error "logic error: WithList should hold a singleton by construction"

data instance Tree (WithList ret zone ot) where
  TreeCountOf ::
    (IsZO zone ot) =>
    List (ZO zone ot) ->
    Tree (Variable Int) ->
    Tree ret ->
    Tree (WithList ret zone ot)
  TreeEach ::
    (IsZO zone ot) =>
    List (ZO zone ot) ->
    Tree (ZO zone ot) ->
    Tree ret ->
    Tree (WithList ret zone ot)
  TreeSuchThat ::
    Tree [Requirement zone ot] ->
    Tree (WithList ret zone ot) ->
    Tree (WithList ret zone ot)

instance (BuildTree ret) => BuildTree (WithList ret zone ot) where
  buildTreeM :: WithList ret zone ot -> TreeM (Tree (WithList ret zone ot))
  buildTreeM = \case
    CountOf zos cont -> do
      var <- newVar 0
      (\tv tret -> TreeCountOf zos tv tret) <$> buildTreeM var <*> buildTreeM (cont var)
    Each zos cont -> do
      let zo = lenseList zos
      (\tzo tret -> TreeEach zos tzo tret) <$> buildTreeM zo <*> buildTreeM (cont zo)
    SuchThat reqs withList ->
      TreeSuchThat <$> buildTreeM reqs <*> buildTreeM withList

--------------------------------------------------------------------------------
-- EntersStatic

data instance Tree (EntersStatic zone ot) where
  TreeEntersTapped :: Tree (EntersStatic 'ZBattlefield ot)

instance BuildTree (EntersStatic zone ot) where
  buildTreeM :: EntersStatic zone ot -> TreeM (Tree (EntersStatic zone ot))
  buildTreeM = \case
    EntersTapped -> pure TreeEntersTapped

--------------------------------------------------------------------------------
-- Enchant

data instance Tree (Enchant zone ot) where
  TreeEnchant ::
    (IsZO zone ot) =>
    Tree (WithLinkedObject (Elect 'ResolveStage (Effect 'Continuous)) zone ot) ->
    Tree (Enchant zone ot)

instance BuildTree (Enchant zone ot) where
  buildTreeM :: Enchant zone ot -> TreeM (Tree (Enchant zone ot))
  buildTreeM = \case
    Enchant withObj -> TreeEnchant <$> buildTreeM withObj

--------------------------------------------------------------------------------
-- EnchantmentType

data instance Tree (EnchantmentType ot) where
  TreeAura ::
    (ot ~ OTNEnchantment) => Tree (Enchant zone ot') -> Tree (EnchantmentType ot)

instance BuildTree (EnchantmentType ot) where
  buildTreeM :: EnchantmentType ot -> TreeM (Tree (EnchantmentType ot))
  buildTreeM = \case
    Aura enchant -> TreeAura <$> buildTreeM enchant

--------------------------------------------------------------------------------
-- ActivatedAbility (Tree data instance predefined near the top)

instance BuildTree (ActivatedAbility zone ot) where
  buildTreeM :: ActivatedAbility zone ot -> TreeM (Tree (ActivatedAbility zone ot))
  buildTreeM = \case
    Ability cost effect -> TreeAbility <$> buildTreeM cost <*> buildTreeM effect
    Cycling cost -> TreeCycling <$> buildTreeM cost

--------------------------------------------------------------------------------
-- TriggeredAbility

data instance Tree (TriggeredAbility zone ot) where
  TreeWhen ::
    Tree (Elect 'IntrinsicStage EventListener ot) ->
    Tree (TriggeredAbility 'ZBattlefield ot)

instance BuildTree (TriggeredAbility zone ot) where
  buildTreeM :: TriggeredAbility zone ot -> TreeM (Tree (TriggeredAbility zone ot))
  buildTreeM = \case
    When listener -> TreeWhen <$> buildTreeM listener

--------------------------------------------------------------------------------
-- StaticAbility

data instance Tree (StaticAbility zone ot) where
  TreeAs ::
    Tree (Elect 'ResolveStage EventListener ot) -> Tree (StaticAbility 'ZBattlefield ot)
  TreeBestow ::
    (ot ~ OTNEnchantmentCreature) =>
    Tree (Elect 'IntrinsicStage Cost ot) ->
    Tree (Enchant 'ZBattlefield OTNCreature) ->
    Tree (StaticAbility 'ZBattlefield ot)
  TreeCantBlock :: (ot ~ OTNCreature) => Tree (StaticAbility 'ZBattlefield ot)
  TreeDefender :: (ot ~ OTNCreature) => Tree (StaticAbility 'ZBattlefield ot)
  TreeEnters :: Tree (EntersStatic zone ot) -> Tree (StaticAbility zone ot)
  TreeFirstStrike :: (ot ~ OTNCreature) => Tree (StaticAbility 'ZBattlefield ot)
  TreeFlying :: (ot ~ OTNCreature) => Tree (StaticAbility 'ZBattlefield ot)
  TreeFuse :: Tree (StaticAbility 'ZHand (ot, ot))
  TreeHaste :: (ot ~ OTNCreature) => Tree (StaticAbility 'ZBattlefield ot)
  TreeLandwalk ::
    (ot ~ OTNCreature) =>
    Tree [Requirement 'ZBattlefield OTNLand] ->
    Tree (StaticAbility 'ZBattlefield ot)
  TreePhasing :: Tree (StaticAbility 'ZBattlefield ot)
  TreeStaticContinuous ::
    Tree (Elect 'ResolveStage (Effect 'Continuous) ot) ->
    Tree (StaticAbility 'ZBattlefield ot)
  TreeSuspend ::
    Int -> Tree (Elect 'IntrinsicStage Cost ot) -> Tree (StaticAbility 'ZBattlefield ot)
  TreeTrample :: (ot ~ OTNCreature) => Tree (StaticAbility 'ZBattlefield ot)

instance BuildTree (StaticAbility zone ot) where
  buildTreeM :: StaticAbility zone ot -> TreeM (Tree (StaticAbility zone ot))
  buildTreeM = \case
    As electListener -> TreeAs <$> buildTreeM electListener
    Bestow cost enchant -> TreeBestow <$> buildTreeM cost <*> buildTreeM enchant
    CantBlock -> pure TreeCantBlock
    Defender -> pure TreeDefender
    Enters entersStatic -> TreeEnters <$> buildTreeM entersStatic
    FirstStrike -> pure TreeFirstStrike
    Flying -> pure TreeFlying
    Fuse -> pure TreeFuse
    Haste -> pure TreeHaste
    Landwalk reqs -> TreeLandwalk <$> buildTreeM reqs
    Phasing -> pure TreePhasing
    StaticContinuous continuous -> TreeStaticContinuous <$> buildTreeM continuous
    Suspend time cost -> (\t -> TreeSuspend time t) <$> buildTreeM cost
    Trample -> pure TreeTrample

--------------------------------------------------------------------------------
-- Ability

data instance Tree (Ability zone ot) where
  TreeActivated ::
    Tree (Elect 'IntrinsicStage (ActivatedAbility zone ot) ot) -> Tree (Ability zone ot)
  TreeStatic :: Tree (StaticAbility zone ot) -> Tree (Ability zone ot)
  TreeTriggered :: Tree (TriggeredAbility zone ot) -> Tree (Ability zone ot)

instance BuildTree (Ability zone ot) where
  buildTreeM :: Ability zone ot -> TreeM (Tree (Ability zone ot))
  buildTreeM = \case
    Activated ability -> TreeActivated <$> buildTreeM ability
    Static ability -> TreeStatic <$> buildTreeM ability
    Triggered ability -> TreeTriggered <$> buildTreeM ability

--------------------------------------------------------------------------------
-- WithThisAbility

data instance Tree (WithThisAbility zone ot) where
  TreeWithThisActivated ::
    (IsZO zone ot) => Tree (WithThisActivated zone ot) -> Tree (WithThisAbility zone ot)
  TreeWithThisStatic ::
    (IsZO zone ot) => Tree (WithThisStatic zone ot) -> Tree (WithThisAbility zone ot)
  TreeWithThisTriggered ::
    (IsZO zone ot) => Tree (WithThisTriggered zone ot) -> Tree (WithThisAbility zone ot)

instance BuildTree (WithThisAbility zone ot) where
  buildTreeM :: WithThisAbility zone ot -> TreeM (Tree (WithThisAbility zone ot))
  buildTreeM = \case
    WithThisActivated withThis -> TreeWithThisActivated <$> buildTreeM withThis
    WithThisStatic withThis -> TreeWithThisStatic <$> buildTreeM withThis
    WithThisTriggered withThis -> TreeWithThisTriggered <$> buildTreeM withThis

--------------------------------------------------------------------------------
-- WithThisZ

data instance Tree (WithThisZ liftZOT zone ot) where
  TreeWithThisZ ::
    (IsZO zone ot) =>
    Tree (WithThis (liftZOT zone) zone ot) ->
    Tree (WithThisZ liftZOT zone ot)

instance
  (BuildTree (liftZOT zone ot)) =>
  BuildTree (WithThisZ liftZOT zone ot)
  where
  buildTreeM :: WithThisZ liftZOT zone ot -> TreeM (Tree (WithThisZ liftZOT zone ot))
  buildTreeM = \case
    WithThisZ withThis -> TreeWithThisZ <$> buildTreeM withThis

--------------------------------------------------------------------------------
-- SomeZone

data instance Tree (SomeZone liftZOT ot) where
  TreeSomeZone ::
    (ot ~ OTN x, IsZO zone ot) =>
    Tree (liftZOT zone ot) ->
    Tree (SomeZone liftZOT ot)
  TreeSomeZone2 ::
    (IsZO zone ot1, IsZO zone ot2) =>
    Tree (liftZOT zone (ot1, ot2)) ->
    Tree (SomeZone liftZOT (ot1, ot2))

-- As with `EventListener'`, the quantified constraint lives on this helper
-- (avoiding @UndecidableInstances@); concrete instances below cover the
-- @liftZOT@s that actually occur in card data.
buildSomeZoneM ::
  (forall zone. BuildTree (liftZOT zone ot)) =>
  SomeZone liftZOT ot ->
  TreeM (Tree (SomeZone liftZOT ot))
buildSomeZoneM = \case
  SomeZone x -> TreeSomeZone <$> buildTreeM x
  SomeZone2 x -> TreeSomeZone2 <$> buildTreeM x

-- | @[SomeZone Ability (ot1, ot2)]@ in split cards.
instance BuildTree (SomeZone Ability ot) where
  buildTreeM :: SomeZone Ability ot -> TreeM (Tree (SomeZone Ability ot))
  buildTreeM = buildSomeZoneM

-- | @[SomeZone WithThisAbility ot]@ in card specs and @Requirement.HasAbility@.
instance BuildTree (SomeZone WithThisAbility ot) where
  buildTreeM ::
    SomeZone WithThisAbility ot -> TreeM (Tree (SomeZone WithThisAbility ot))
  buildTreeM = buildSomeZoneM

-- | The @SomeZone (WithThisZ …)@ flavors exist for parity with the standalone
-- @Show@ instances (not reachable from card data, but supported).
instance BuildTree (SomeZone (WithThisZ ActivatedAbility) ot) where
  buildTreeM ::
    SomeZone (WithThisZ ActivatedAbility) ot ->
    TreeM (Tree (SomeZone (WithThisZ ActivatedAbility) ot))
  buildTreeM = buildSomeZoneM

instance BuildTree (SomeZone (WithThisZ StaticAbility) ot) where
  buildTreeM ::
    SomeZone (WithThisZ StaticAbility) ot ->
    TreeM (Tree (SomeZone (WithThisZ StaticAbility) ot))
  buildTreeM = buildSomeZoneM

instance BuildTree (SomeZone (WithThisZ TriggeredAbility) ot) where
  buildTreeM ::
    SomeZone (WithThisZ TriggeredAbility) ot ->
    TreeM (Tree (SomeZone (WithThisZ TriggeredAbility) ot))
  buildTreeM = buildSomeZoneM

--------------------------------------------------------------------------------
-- Token

data instance Tree (Token ot) where
  TreeToken :: Tree (Card ot) -> Tree (Token ot)

instance BuildTree (Token ot) where
  buildTreeM :: Token ot -> TreeM (Tree (Token ot))
  buildTreeM = \case
    Token card -> TreeToken <$> buildTreeM card

--------------------------------------------------------------------------------
-- Card

data instance Tree (Card ot) where
  TreeCard ::
    (ot ~ OTN x, IsSpecificCard ot) =>
    CardName ->
    Tree (Elect 'IntrinsicStage (CardCharacteristic ot) ot) ->
    Tree (Card ot)
  TreeDoubleSidedCard ::
    (ot1 ~ OTN x, ot2 ~ OTN y, Inst2 IsSpecificCard ot1 ot2) =>
    Tree (Card ot1) ->
    Tree (Card ot2) ->
    Tree (Card (ot1, ot2))
  TreeSplitCard ::
    (ot1 ~ OTN x, ot2 ~ OTN y, Inst2 IsSpecificCard ot1 ot2) =>
    Tree (Card ot1) ->
    Tree (Card ot2) ->
    Tree [SomeZone Ability (ot1, ot2)] ->
    Tree (Card (ot1, ot2))

instance BuildTree (Card ot) where
  buildTreeM :: Card ot -> TreeM (Tree (Card ot))
  buildTreeM = \case
    Card name elect -> (\t -> TreeCard name t) <$> buildTreeM elect
    DoubleSidedCard card1 card2 ->
      TreeDoubleSidedCard <$> buildTreeM card1 <*> buildTreeM card2
    SplitCard card1 card2 abilities ->
      TreeSplitCard <$> buildTreeM card1 <*> buildTreeM card2 <*> buildTreeM abilities

--------------------------------------------------------------------------------
-- AnyCard / AnyToken

instance BuildTree AnyCard where
  buildTreeM :: AnyCard -> TreeM (Tree AnyCard)
  buildTreeM = \case
    AnyCard1 card -> TreeAnyCard1 <$> buildTreeM card
    AnyCard2 card -> TreeAnyCard2 <$> buildTreeM card

instance BuildTree AnyToken where
  buildTreeM :: AnyToken -> TreeM (Tree AnyToken)
  buildTreeM = \case
    AnyToken token -> TreeAnyToken <$> buildTreeM token

--------------------------------------------------------------------------------
-- CardCharacteristic

data instance Tree (CardCharacteristic ot) where
  TreeArtifactCharacteristic ::
    Colors ->
    [Supertype OTNArtifact] ->
    [ArtifactType] ->
    Tree (CardSpec OTNArtifact) ->
    Tree (CardCharacteristic OTNArtifact)
  TreeArtifactCreatureCharacteristic ::
    Colors ->
    [Supertype OTNArtifactCreature] ->
    [ArtifactType] ->
    [CreatureType] ->
    Power ->
    Toughness ->
    Tree (CardSpec OTNArtifactCreature) ->
    Tree (CardCharacteristic OTNArtifactCreature)
  TreeArtifactLandCharacteristic ::
    [Supertype OTNArtifactLand] ->
    [ArtifactType] ->
    [LandType] ->
    Tree (CardSpec OTNArtifactLand) ->
    Tree (CardCharacteristic OTNArtifactLand)
  TreeBattleCharacteristic ::
    Colors ->
    [Supertype OTNBattle] ->
    [BattleType] ->
    Defense ->
    Tree (CardSpec OTNBattle) ->
    Tree (CardCharacteristic OTNBattle)
  TreeCreatureCharacteristic ::
    Colors ->
    [Supertype OTNCreature] ->
    [CreatureType] ->
    Power ->
    Toughness ->
    Tree (CardSpec OTNCreature) ->
    Tree (CardCharacteristic OTNCreature)
  TreeEnchantmentCharacteristic ::
    Colors ->
    [Supertype OTNEnchantment] ->
    Tree [EnchantmentType OTNEnchantment] ->
    Tree (CardSpec OTNEnchantment) ->
    Tree (CardCharacteristic OTNEnchantment)
  TreeEnchantmentCreatureCharacteristic ::
    Colors ->
    [Supertype OTNEnchantmentCreature] ->
    [CreatureType] ->
    Tree [EnchantmentType OTNEnchantmentCreature] ->
    Power ->
    Toughness ->
    Tree (CardSpec OTNEnchantmentCreature) ->
    Tree (CardCharacteristic OTNEnchantmentCreature)
  TreeInstantCharacteristic ::
    Colors ->
    [Supertype OTNInstant] ->
    Tree (Elect 'TargetStage (CardSpec OTNInstant) OTNInstant) ->
    Tree (CardCharacteristic OTNInstant)
  TreeLandCharacteristic ::
    [Supertype OTNLand] ->
    [LandType] ->
    Tree (CardSpec OTNLand) ->
    Tree (CardCharacteristic OTNLand)
  TreePlaneswalkerCharacteristic ::
    Colors ->
    [Supertype OTNPlaneswalker] ->
    Tree (CardSpec OTNPlaneswalker) ->
    Tree (CardCharacteristic OTNPlaneswalker)
  TreeSorceryCharacteristic ::
    Colors ->
    [Supertype OTNSorcery] ->
    Tree (Elect 'TargetStage (CardSpec OTNSorcery) OTNSorcery) ->
    Tree (CardCharacteristic OTNSorcery)

instance BuildTree (CardCharacteristic ot) where
  buildTreeM :: CardCharacteristic ot -> TreeM (Tree (CardCharacteristic ot))
  buildTreeM = \case
    ArtifactCharacteristic colors sups artTypes spec ->
      TreeArtifactCharacteristic colors sups artTypes <$> buildTreeM spec
    ArtifactCreatureCharacteristic colors sups artTypes creatTypes power toughness spec ->
      TreeArtifactCreatureCharacteristic colors sups artTypes creatTypes power toughness
        <$> buildTreeM spec
    ArtifactLandCharacteristic sups artTypes landTypes spec ->
      TreeArtifactLandCharacteristic sups artTypes landTypes <$> buildTreeM spec
    BattleCharacteristic colors sups battleTypes defense spec ->
      TreeBattleCharacteristic colors sups battleTypes defense <$> buildTreeM spec
    CreatureCharacteristic colors sups creatureTypes power toughness spec ->
      TreeCreatureCharacteristic colors sups creatureTypes power toughness <$> buildTreeM spec
    EnchantmentCharacteristic colors sups enchTypes spec ->
      TreeEnchantmentCharacteristic colors sups <$> buildTreeM enchTypes <*> buildTreeM spec
    EnchantmentCreatureCharacteristic colors sups creatTypes enchTypes power toughness spec ->
      (\te ts -> TreeEnchantmentCreatureCharacteristic colors sups creatTypes te power toughness ts)
        <$> buildTreeM enchTypes
        <*> buildTreeM spec
    InstantCharacteristic colors sups spec ->
      TreeInstantCharacteristic colors sups <$> buildTreeM spec
    LandCharacteristic sups landTypes spec ->
      TreeLandCharacteristic sups landTypes <$> buildTreeM spec
    PlaneswalkerCharacteristic colors sups spec ->
      TreePlaneswalkerCharacteristic colors sups <$> buildTreeM spec
    SorceryCharacteristic colors sups spec ->
      TreeSorceryCharacteristic colors sups <$> buildTreeM spec

--------------------------------------------------------------------------------
-- CardSpec

data instance Tree (CardSpec ot) where
  TreeArtifactSpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNArtifact] ->
    Tree (CardSpec OTNArtifact)
  TreeArtifactCreatureSpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNArtifact] ->
    Tree [SomeZone WithThisAbility OTNCreature] ->
    Tree [SomeZone WithThisAbility OTNArtifactCreature] ->
    Tree (CardSpec OTNArtifactCreature)
  TreeArtifactLandSpec ::
    Tree [SomeZone WithThisAbility OTNArtifact] ->
    Tree [SomeZone WithThisAbility OTNLand] ->
    Tree [SomeZone WithThisAbility OTNArtifactLand] ->
    Tree (CardSpec OTNArtifactLand)
  TreeBattleSpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNBattle] ->
    Tree (CardSpec OTNBattle)
  TreeCreatureSpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNCreature] ->
    Tree (CardSpec OTNCreature)
  TreeEnchantmentSpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNEnchantment] ->
    Tree (CardSpec OTNEnchantment)
  TreeEnchantmentCreatureSpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNCreature] ->
    Tree [SomeZone WithThisAbility OTNEnchantment] ->
    Tree [SomeZone WithThisAbility OTNEnchantmentCreature] ->
    Tree (CardSpec OTNEnchantmentCreature)
  TreeInstantSpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNInstant] ->
    Tree (WithThisOneShot OTNInstant) ->
    Tree (CardSpec OTNInstant)
  TreeLandSpec ::
    Tree [SomeZone WithThisAbility OTNLand] ->
    Tree (CardSpec OTNLand)
  TreePlaneswalkerSpec ::
    Tree Cost ->
    Loyalty ->
    Tree [SomeZone WithThisAbility OTNPlaneswalker] ->
    Tree (CardSpec OTNPlaneswalker)
  TreeSorcerySpec ::
    Tree Cost ->
    Tree [SomeZone WithThisAbility OTNSorcery] ->
    Tree (WithThisOneShot OTNSorcery) ->
    Tree (CardSpec OTNSorcery)

instance BuildTree (CardSpec ot) where
  buildTreeM :: CardSpec ot -> TreeM (Tree (CardSpec ot))
  buildTreeM = \case
    ArtifactSpec cost abilities ->
      TreeArtifactSpec <$> buildTreeM cost <*> buildTreeM abilities
    ArtifactCreatureSpec cost artAbils creatAbils bothAbils ->
      TreeArtifactCreatureSpec
        <$> buildTreeM cost
        <*> buildTreeM artAbils
        <*> buildTreeM creatAbils
        <*> buildTreeM bothAbils
    ArtifactLandSpec artAbils landAbils bothAbils ->
      TreeArtifactLandSpec
        <$> buildTreeM artAbils
        <*> buildTreeM landAbils
        <*> buildTreeM bothAbils
    BattleSpec cost abilities ->
      TreeBattleSpec <$> buildTreeM cost <*> buildTreeM abilities
    CreatureSpec cost abilities ->
      TreeCreatureSpec <$> buildTreeM cost <*> buildTreeM abilities
    EnchantmentSpec cost abilities ->
      TreeEnchantmentSpec <$> buildTreeM cost <*> buildTreeM abilities
    EnchantmentCreatureSpec cost creatAbils enchAbils bothAbils ->
      TreeEnchantmentCreatureSpec
        <$> buildTreeM cost
        <*> buildTreeM creatAbils
        <*> buildTreeM enchAbils
        <*> buildTreeM bothAbils
    InstantSpec cost abilities effect ->
      TreeInstantSpec <$> buildTreeM cost <*> buildTreeM abilities <*> buildTreeM effect
    LandSpec abilities ->
      TreeLandSpec <$> buildTreeM abilities
    PlaneswalkerSpec cost loyalty abilities ->
      (\tc ta -> TreePlaneswalkerSpec tc loyalty ta) <$> buildTreeM cost <*> buildTreeM abilities
    SorcerySpec cost abilities effect ->
      TreeSorcerySpec <$> buildTreeM cost <*> buildTreeM abilities <*> buildTreeM effect

--------------------------------------------------------------------------------
-- SetCard / SetToken

data instance Tree (SetCard ot) where
  TreeSetCard :: CardSet -> Rarity -> Tree (Card ot) -> Tree (SetCard ot)

instance BuildTree (SetCard ot) where
  buildTreeM :: SetCard ot -> TreeM (Tree (SetCard ot))
  buildTreeM = \case
    SetCard set rarity card -> (\t -> TreeSetCard set rarity t) <$> buildTreeM card

data instance Tree (SetToken ot) where
  TreeSetToken :: CardSet -> Rarity -> Tree (Token ot) -> Tree (SetToken ot)

instance BuildTree (SetToken ot) where
  buildTreeM :: SetToken ot -> TreeM (Tree (SetToken ot))
  buildTreeM = \case
    SetToken set rarity token -> (\t -> TreeSetToken set rarity t) <$> buildTreeM token

--------------------------------------------------------------------------------
-- BattleType

data instance Tree BattleType where
  TreeSeige :: Tree BattleType

instance BuildTree BattleType where
  buildTreeM :: BattleType -> TreeM (Tree BattleType)
  buildTreeM = \case
    Seige -> pure TreeSeige

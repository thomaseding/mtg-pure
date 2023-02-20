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
import safe Data.Kind (Type)
import safe Data.Nat (Fin (..), IsNat, NatList (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.Object.OTN (
  OTN,
 )
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (ObjectId),
 )
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  IsSpecificCard,
  IsUser,
  Token,
 )
import safe MtgPure.Model.Variable (
  Variable (..),
  VariableId,
  VariableId' (..),
 )
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
 )

--------------------------------------------------------------------------------

data TreeConfig = TreeConfig
  { treeConfig_ :: ()
  , -- | This is needed because cards can be mutually recursive with other cards and/or tokens.
    -- In the general case, recursion can only be determined by card name, since comparing
    -- infinite value types is not possible.
    treeConfig_maxCardDepth :: Maybe Int
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
  pure = TreeM . pure
  (<*>) f x = TreeM $ unTreeM f <*> unTreeM x

instance Monad TreeM where
  (>>=) m f = TreeM $ unTreeM m >>= unTreeM . f

instance State.MonadState TreeState TreeM where
  get = TreeM State.get
  put = TreeM . State.put

runTreeM :: TreeConfig -> TreeM a -> a
runTreeM config m = State.evalState (unTreeM m) st
 where
  st =
    TreeState
      { treeState_ = ()
      , treeState_config = config
      , treeState_cardDepth = 0
      , nextBoundObjectId = ObjectId 0
      , nextBoundVariableId = VariableId 0
      }

class BuildTree a where
  buildTreeM :: a -> TreeM (Tree a)

buildTree :: BuildTree a => TreeConfig -> a -> Tree a
buildTree config = runTreeM config . buildTreeM

--------------------------------------------------------------------------------

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
--
-- XXX: Prolly want to keep the `ot` types since IndexOT exists and the strong types are useful.
-- In the future, if there is need, there can be a `UntypedTree` variant that doesn't have the `ot` types.
-- To create an `UntypedTree`, build one of these `Tree ot`s and then convert that to an `UntypedTree`.
-- Each UntypedTree constructor can have a lazy field for the `ot` object types as `[ObjectType]`.
data family Tree (a :: Type) :: Type

data instance Tree (ActivatedAbility zone ot) where
  TreeAbility ::
    IsZO zone ot =>
    { treeActivated_cost :: Tree (Cost ot)
    , treeActivated_effect :: Tree (Elect 'TargetStage (Effect 'OneShot) ot)
    } ->
    Tree (ActivatedAbility zone ot)
  TreeCycling :: (ot ~ OTN x, IsOTN ot) => Tree (Cost ot) -> Tree (ActivatedAbility 'ZHand ot)

data instance Tree AnyCard where
  TreeAnyCard1 :: (ot ~ OTN x, IsSpecificCard ot) => Tree (Card ot) -> Tree AnyCard
  TreeAnyCard2 :: (ot1 ~ OTN x, IsSpecificCard ot1, ot2 ~ OTN y, IsSpecificCard ot2) => Tree (Card ot1) -> Tree (Card ot2) -> Tree AnyCard

data instance Tree AnyToken where
  TreeAnyToken :: IsSpecificCard ot => Tree (Token ot) -> Tree AnyToken

data instance Tree (Case x) where
  TreeCaseFin ::
    (IsUser u, IsNat n) =>
    { treeCaseFin :: Tree (Variable (Fin u n))
    , treeOfFin :: Tree (NatList u n x)
    } ->
    Tree (Case x)

data instance Tree Condition where
  TreeCAnd :: Tree [Condition] -> Tree Condition
  TreeCNot :: Tree Condition -> Tree Condition
  TreeCOr :: Tree [Condition] -> Tree Condition
  TreeSatisfies :: IsZO zone ot => Tree (ActivatedAbility zone ot) -> Tree Condition

data instance Tree (Variable a) where
  TreeVariable ::
    { treeVariableBaseName :: String
    , treeVariableId :: VariableId
    } ->
    Tree (Variable a)

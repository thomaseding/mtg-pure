{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.State (
  Fwd,
  Magic,
  MagicEx,
  MagicCont,
  queryMagic,
  --
  mkGameState,
  --
  mkOpaqueGameState,
  OpaqueGameState,
  GameState (..),
  GameInput (..),
  GameResult (..),
  GameFormat (..),
  Prompt,
  TargetId,
  AnyRequirement (..),
  PendingReady (..),
  Pending,
  Ready,
  Elected (..),
  electedObject_controller,
  electedObject_cost,
  electedObject_effect,
  AnyElected (..),
  StackEntry (..),
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd (Fwd')
import safe MtgPure.Engine.Monad (Magic', MagicCont', MagicEx', runMagicRO)
import safe MtgPure.Engine.Prompt (PlayerIndex, Prompt')
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mulligan (Mulligan)
import safe MtgPure.Model.Object (
  IsObjectType (..),
  OT0,
  Object,
  ObjectDiscriminant,
  ObjectType (..),
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.ObjectId (ObjectId (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTInstant,
  OTSorcery,
 )
import safe MtgPure.Model.Permanent (Permanent)
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (Card, CardTypeDef, Cost, Effect, Elect, Requirement)
import safe MtgPure.Model.Sideboard (Sideboard)
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Tribal (IsTribal, Tribal (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO)

type Fwd m = Fwd' (GameResult m) (GameState m) m

type Prompt = Prompt' OpaqueGameState

type TargetId = ObjectId

data AnyRequirement :: Type where
  AnyRequirement :: IsZO zone ot => Requirement zone ot -> AnyRequirement

data PendingReady (p :: PrePost) (el :: Type) (ot :: Type) where
  Pending :: {unPending :: Elect 'Post el ot} -> Pending el ot
  Ready :: {unReady :: el} -> Ready el ot

type Pending = PendingReady 'Pre

type Ready = PendingReady 'Post

data Elected (pCost :: PrePost) (pEffect :: PrePost) (mTribal :: Maybe Tribal) (ot :: Type) :: Type where
  ElectedActivatedAbility ::
    IsZO zone ot =>
    { electedActivatedAbility_controller :: Object 'OTPlayer
    , electedActivatedAbility_this :: ZO zone ot
    , electedActivatedAbility_cost :: PendingReady pCost (Cost ot) ot
    , electedActivatedAbility_effect :: PendingReady pEffect (Effect 'OneShot) ot
    } ->
    Elected pCost pEffect 'Nothing ot
  ElectedInstant ::
    IsTribal tribal =>
    { electedInstant_controller :: Object 'OTPlayer
    , electedInstant_card :: Card OTInstant
    , electedInstant_def :: CardTypeDef tribal OTInstant
    , electedInstant_cost :: PendingReady pCost (Cost OTInstant) OTInstant
    , electedInstant_effect :: PendingReady pEffect (Effect 'OneShot) OTInstant
    } ->
    Elected pCost pEffect ( 'Just tribal) OTInstant
  ElectedSorcery ::
    IsTribal tribal =>
    { electedSorcery_controller :: Object 'OTPlayer
    , electedSorcery_card :: Card OTSorcery
    , electedSorcery_def :: CardTypeDef tribal OTSorcery
    , electedSorcery_cost :: PendingReady pCost (Cost OTSorcery) OTSorcery
    , electedSorcery_effect :: PendingReady pEffect (Effect 'OneShot) OTSorcery
    } ->
    Elected pCost pEffect ( 'Just tribal) OTSorcery
  deriving (Typeable)

electedObject_controller :: Elected pCost pEffect tribal ot -> Object 'OTPlayer
electedObject_controller elected = ($ elected) $ case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_controller
  ElectedInstant{} -> electedInstant_controller
  ElectedSorcery{} -> electedSorcery_controller

electedObject_cost :: Elected pCost pEffect tribal ot -> PendingReady pCost (Cost ot) ot
electedObject_cost elected = ($ elected) $ case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_cost
  ElectedInstant{} -> electedInstant_cost
  ElectedSorcery{} -> electedSorcery_cost

electedObject_effect :: Elected pCost pEffect mTribal ot -> PendingReady pEffect (Effect 'OneShot) ot
electedObject_effect elected = ($ elected) $ case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_effect
  ElectedInstant{} -> electedInstant_effect
  ElectedSorcery{} -> electedSorcery_effect

data AnyElected (pCost :: PrePost) (pEffect :: PrePost) (mTribal :: Maybe Tribal) :: Type where
  AnyElected :: Elected pCost pEffect mTribal ot -> AnyElected pCost pEffect mTribal
  deriving (Typeable)

data StackEntry (mTribal :: Maybe Tribal) = StackEntry
  { stackEntryTargets :: [TargetId]
  , stackEntryElected :: AnyElected 'Post 'Pre mTribal
  }

data GameState (m :: Type -> Type) where
  GameState ::
    { magicCurrentTurn :: Int
    , magicFwd :: Fwd m
    , magicGraveyardCards :: Map.Map (ZO 'ZGraveyard OT0) (Card ())
    , magicHandCards :: Map.Map (ZO 'ZHand OT0) (Card ())
    , magicManaBurn :: Bool
    , magicNextObjectDiscriminant :: ObjectDiscriminant
    , magicNextObjectId :: ObjectId
    , magicPermanents :: Map.Map (ZO 'ZBattlefield OT0) Permanent
    , magicPhaseStep :: PhaseStep
    , magicPlayers :: Map.Map (Object 'OTPlayer) Player -- contains all players
    , magicPlayerOrderAPNAP :: Stream.Stream (Object 'OTPlayer) -- does not contain losers
    , magicPlayerOrderPriority :: [Object 'OTPlayer] -- does not contain losers
    , magicPlayerOrderTurn :: Stream.Stream (Object 'OTPlayer) -- does not contain losers
    , magicPrompt :: Prompt m
    , magicStack :: Stack
    , magicStackEntryAbilityMap :: Map.Map (ZO 'ZStack OT0) (StackEntry 'Nothing)
    , magicStackEntryNonTribalMap :: Map.Map (ZO 'ZStack OT0) (StackEntry ( 'Just 'NonTribal))
    , magicStackEntryTribalMap :: Map.Map (ZO 'ZStack OT0) (StackEntry ( 'Just 'Tribal))
    , magicStartingPlayer :: Object 'OTPlayer
    , magicTargetProperties :: Map.Map TargetId AnyRequirement
    } ->
    GameState m
  deriving (Typeable)

newtype OpaqueGameState m = OpaqueGameState (GameState m)

data GameFormat
  = Vintage
  deriving (Eq, Ord, Show, Typeable)

data GameInput m = GameInput
  { gameInput_decks :: [(Deck, Sideboard)]
  , gameInput_gameFormat :: GameFormat
  , gameInput_mulligan :: Mulligan
  , gameInput_prompt :: Prompt m
  }

data GameResult m = GameResult
  { gameEndState :: GameState m
  , gameWinners :: [PlayerIndex] -- 🏆🥇🏆
  , gameLosers :: [PlayerIndex] -- 👎👎👎
  }
  deriving (Typeable)

type Magic v rw m = Magic' (GameResult m) (GameState m) v rw m

type MagicEx ex v rw m = MagicEx' (GameResult m) (GameState m) ex v rw m

type MagicCont v rw m a = MagicCont' (GameResult m) (GameState m) v rw m a

mkOpaqueGameState :: GameState m -> OpaqueGameState m
mkOpaqueGameState = OpaqueGameState

queryMagic :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic (OpaqueGameState st) = runMagicRO st

mkPlayer :: GameFormat -> Deck -> Sideboard -> Player
mkPlayer format deck sideboard =
  Player
    { playerDrewFromEmptyLibrary = False
    , playerGraveyard = Graveyard []
    , playerHand = Hand []
    , playerLandsPlayedThisTurn = 0
    , playerLibrary = mkLibrary deck
    , playerLife = life
    , playerLost = False
    , playerMana = mempty
    , playerStartingDeck = deck
    , playerStartingHandSize = 7
    , playerStartingLife = life
    , playerStartingSideboard = sideboard
    }
 where
  -- 103.3
  life = Life $ case format of
    Vintage -> 20

mkLibrary :: Deck -> Library
mkLibrary (Deck cards) = Library cards

mkGameState :: Fwd m -> GameInput m -> Maybe (GameState m)
mkGameState fwd input = case playerObjects of
  [] -> Nothing
  oPlayer : _ ->
    Just
      GameState
        { magicCurrentTurn = 0
        , magicFwd = fwd
        , magicGraveyardCards = mempty
        , magicHandCards = mempty
        , magicManaBurn = False
        , magicNextObjectDiscriminant = (1 +) <$> DefaultObjectDiscriminant
        , magicNextObjectId = ObjectId playerCount
        , magicPermanents = mempty
        , magicPhaseStep = PSBeginningPhase UntapStep
        , magicPlayers = playerMap
        , magicPlayerOrderAPNAP = Stream.cycle playerObjects
        , magicPlayerOrderPriority = []
        , magicPlayerOrderTurn = Stream.cycle playerObjects
        , magicPrompt = gameInput_prompt input
        , magicStack = Stack []
        , magicStackEntryAbilityMap = mempty
        , magicStackEntryNonTribalMap = mempty
        , magicStackEntryTribalMap = mempty
        , magicStartingPlayer = oPlayer
        , magicTargetProperties = mempty
        }
 where
  format = gameInput_gameFormat input
  players = map (uncurry $ mkPlayer format) $ gameInput_decks input
  playerCount = length players
  playerObjects = map (idToObject . ObjectId) [0 .. playerCount - 1]
  playerMap = Map.fromList $ zip playerObjects players

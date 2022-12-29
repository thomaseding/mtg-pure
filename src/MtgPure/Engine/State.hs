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
  runMagicEx,
  runMagicCont,
  logCall,
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
  --
  logCallPop,
  logCallPush,
  logCallUnwind,
) where

import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe Data.Typeable (Typeable)
import safe Language.Haskell.TH.Syntax (Name)
import safe MtgPure.Engine.Fwd.Type (Fwd')
import safe MtgPure.Engine.Monad (
  EnvLogCall (..),
  Magic',
  MagicCont',
  MagicEx',
  fromRO,
  get,
  internalFromPrivate,
  logCallPop',
  logCallPush',
  logCallUnwind',
  runMagicCont',
  runMagicEx',
  runMagicRO,
 )
import safe MtgPure.Engine.Prompt (
  CallFrameId,
  InternalLogicError (..),
  PlayerIndex,
  Prompt' (..),
 )
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Mulligan (Mulligan)
import safe MtgPure.Model.Object.OTKind (
  OTInstant,
  OTSorcery,
 )
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (ObjectDiscriminant, ObjectId (..))
import safe MtgPure.Model.Object.ObjectType (
  ObjectType (..),
 )
import safe MtgPure.Model.Permanent (Permanent)
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  AnyCard,
  Card,
  CardFacet,
  Cost,
  Effect,
  Elect,
  Requirement,
 )
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard)
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)

type Fwd m = Fwd' (GameResult m) (GameState m) m

instance Show (Fwd m) where
  show _ = show ''Fwd

type Prompt = Prompt' OpaqueGameState

instance Show (Prompt m) where
  show _ = show ''Prompt

type TargetId = ObjectId

data AnyRequirement :: Type where
  AnyRequirement :: IsZO zone ot => Requirement zone ot -> AnyRequirement

deriving instance Show AnyRequirement

data PendingReady (p :: PrePost) (el :: Type) (ot :: Type) where
  Pending :: {unPending :: Elect 'Post el ot} -> Pending el ot
  Ready :: {unReady :: el} -> Ready el ot

deriving instance Show el => Show (PendingReady p el ot)

type Pending = PendingReady 'Pre

type Ready = PendingReady 'Post

data Elected (pEffect :: PrePost) (ot :: Type) :: Type where
  ElectedActivatedAbility ::
    IsZO zone ot =>
    { electedActivatedAbility_controller :: Object 'OTPlayer
    , electedActivatedAbility_this :: ZO zone ot
    , electedActivatedAbility_cost :: Cost ot
    , electedActivatedAbility_effect :: PendingReady pEffect (Effect 'OneShot) ot
    } ->
    Elected pEffect ot
  ElectedInstant ::
    { electedInstant_controller :: Object 'OTPlayer
    , electedInstant_card :: Card OTInstant
    , electedInstant_facet :: CardFacet OTInstant
    , electedInstant_cost :: Cost OTInstant
    , electedInstant_effect :: PendingReady pEffect (Effect 'OneShot) OTInstant
    } ->
    Elected pEffect OTInstant
  ElectedSorcery ::
    { electedSorcery_controller :: Object 'OTPlayer
    , electedSorcery_card :: Card OTSorcery
    , electedSorcery_facet :: CardFacet OTSorcery
    , electedSorcery_cost :: Cost OTSorcery
    , electedSorcery_effect :: PendingReady pEffect (Effect 'OneShot) OTSorcery
    } ->
    Elected pEffect OTSorcery
  deriving (Typeable)

electedObject_controller :: Elected pEffect ot -> Object 'OTPlayer
electedObject_controller elected = ($ elected) case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_controller
  ElectedInstant{} -> electedInstant_controller
  ElectedSorcery{} -> electedSorcery_controller

electedObject_cost :: Elected pEffect ot -> Cost ot
electedObject_cost elected = ($ elected) case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_cost
  ElectedInstant{} -> electedInstant_cost
  ElectedSorcery{} -> electedSorcery_cost

electedObject_effect :: Elected pEffect ot -> PendingReady pEffect (Effect 'OneShot) ot
electedObject_effect elected = ($ elected) case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_effect
  ElectedInstant{} -> electedInstant_effect
  ElectedSorcery{} -> electedSorcery_effect

data AnyElected (pEffect :: PrePost) :: Type where
  AnyElected :: Elected pEffect ot -> AnyElected pEffect
  deriving (Typeable)

data StackEntry = StackEntry
  { stackEntryTargets :: [TargetId]
  , stackEntryElected :: AnyElected 'Pre
  }

data GameState (m :: Type -> Type) where
  GameState ::
    { magicCurrentTurn :: Int
    , magicFwd :: Fwd m
    , magicGraveyardCards :: Map.Map (ZO 'ZGraveyard OT0) AnyCard
    , magicHandCards :: Map.Map (ZO 'ZHand OT0) AnyCard
    , magicLibraryCards :: Map.Map (ZO 'ZLibrary OT0) AnyCard
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
    , magicStackEntryMap :: Map.Map (ZO 'ZStack OT0) StackEntry
    , magicStartingPlayer :: Object 'OTPlayer
    , magicTargetProperties :: Map.Map TargetId AnyRequirement
    } ->
    GameState m
  deriving (Typeable)

newtype OpaqueGameState m = OpaqueGameState (GameState m)

instance Show (OpaqueGameState m) where
  show _ = show ''OpaqueGameState

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

queryMagic' :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic' (OpaqueGameState st) = runMagicRO st

queryMagic :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic opaque = queryMagic' opaque . logCall 'queryMagic

runMagicEx ::
  (IsReadWrite rw, Monad m) =>
  (Either ex a -> b) ->
  MagicEx ex v rw m a ->
  Magic v rw m b
runMagicEx = runMagicEx' envLogCall

runMagicCont ::
  (IsReadWrite rw, Monad m) =>
  (Either a b -> c) ->
  MagicCont v rw m a b ->
  Magic v rw m c
runMagicCont = runMagicCont' envLogCall

envLogCall :: (IsReadWrite rw, Monad m) => EnvLogCall (GameResult m) (GameState m) v rw m
envLogCall =
  EnvLogCall
    { envLogCallCorruptCallStackLogging = error $ show CorruptCallStackLogging
    , envLogCallPromptPush = \frame -> do
        st <- internalFromPrivate $ fromRO get
        let prompt = magicPrompt st
        lift $ promptLogCallPush prompt (OpaqueGameState st) frame
    , envLogCallPromptPop = \frame -> do
        st <- internalFromPrivate $ fromRO get
        let prompt = magicPrompt st
        lift $ promptLogCallPop prompt (OpaqueGameState st) frame
    }

logCallUnwind :: (IsReadWrite rw, Monad m) => Maybe CallFrameId -> Magic v rw m ()
logCallUnwind = logCallUnwind' envLogCall

logCallPush :: (IsReadWrite rw, Monad m) => String -> Magic v rw m CallFrameId
logCallPush = logCallPush' envLogCall

logCallPop :: (IsReadWrite rw, Monad m) => Magic v rw m Bool
logCallPop = logCallPop' envLogCall

newtype Named :: Type where
  Named :: String -> Named

class IsNamed name where
  toNamed :: name -> Named

instance IsNamed Named where
  toNamed = id

instance IsNamed Name where
  toNamed = Named . show

instance IsNamed (Name, [String]) where
  toNamed (name, parts) = Named $ show name ++ "." ++ List.intercalate "." parts

instance IsNamed (Name, String) where
  toNamed (name, part) = toNamed (name, [part])

showNamed :: IsNamed name => name -> String
showNamed name = case toNamed name of
  Named s -> s

class LogCall x where
  logCall :: IsNamed name => name -> x -> x

instance (IsReadWrite rw, Monad m) => LogCall (Magic p rw m z) where
  logCall name action = do
    _ <- logCallPush $ showNamed name
    result <- action
    _ <- logCallPop
    pure result

instance (IsReadWrite rw, Monad m) => LogCall (a -> Magic p rw m z) where
  logCall name action a = do
    _ <- logCallPush $ showNamed name
    result <- action a
    _ <- logCallPop
    pure result

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> Magic p rw m z) where
  logCall name action a b = do
    _ <- logCallPush $ showNamed name
    result <- action a b
    _ <- logCallPop
    pure result

instance (IsReadWrite rw, Monad m) => LogCall (MagicCont p rw m y z) where
  logCall name action = do
    _ <- lift $ logCallPush $ showNamed name
    result <- action
    _ <- lift logCallPop
    pure result

instance (IsReadWrite rw, Monad m) => LogCall (a -> MagicCont p rw m y z) where
  logCall name action a = do
    _ <- lift $ logCallPush $ showNamed name
    result <- action a
    _ <- lift logCallPop
    pure result

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> MagicCont p rw m y z) where
  logCall name action a b = do
    _ <- lift $ logCallPush $ showNamed name
    result <- action a b
    _ <- lift logCallPop
    pure result

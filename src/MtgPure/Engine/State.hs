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
  MagicCont,
  queryMagic,
  runMagicCont,
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
  AnyElected (..),
  --
  withHeadlessPrompt,
  --
  logCall,
  logCallRec,
  logCallPop,
  logCallPush,
  logCallUnwind,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Data.Functor ((<&>))
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe Data.Typeable (Typeable)
import safe Language.Haskell.TH.Syntax (Name)
import safe MtgPure.Engine.Fwd.Type (Fwd')
import safe MtgPure.Engine.Monad (
  EnvLogCall (..),
  Magic',
  MagicCont',
  fromRO,
  get,
  internalFromPrivate,
  liftCont,
  logCallPop',
  logCallPush',
  logCallTop,
  logCallUnwind',
  runMagicCont',
  runMagicRO,
 )
import safe MtgPure.Engine.Prompt (
  CallFrameId,
  CallFrameInfo (callFrameName),
  CardCount (..),
  CardIndex (..),
  InternalLogicError (..),
  PlayerIndex (..),
  PriorityAction (..),
  Prompt' (..),
 )
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Mulligan (Mulligan)
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
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO)

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
  ElectedSpell ::
    { electedSpell_controller :: Object 'OTPlayer
    , electedSpell_card :: AnyCard -- Card ot
    , electedSpell_facet :: CardFacet ot
    , electedSpell_cost :: Cost ot
    , electedSpell_effect :: Maybe (PendingReady pEffect (Effect 'OneShot) ot)
    } ->
    Elected pEffect ot
  deriving (Typeable)

electedObject_controller :: Elected pEffect ot -> Object 'OTPlayer
electedObject_controller elected = ($ elected) case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_controller
  ElectedSpell{} -> electedSpell_controller

electedObject_cost :: Elected pEffect ot -> Cost ot
electedObject_cost elected = ($ elected) case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_cost
  ElectedSpell{} -> electedSpell_cost

data AnyElected (pEffect :: PrePost) :: Type where
  AnyElected :: IsOTN ot => Elected pEffect ot -> AnyElected pEffect
  deriving (Typeable)

data GameState (m :: Type -> Type) where
  -- TODO: there should be a field that tracks which ObjectIds have been known to each player.
  -- This would allow the Public API to query game state a given player knows about without leaking hidden information.
  -- Simply knowing that a non-visible ID exists allow players to cheat (clients could spam it and then glean zone information and whatnot).
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
    , -- This is not bundled with the AnyElected map because the Cost hasn't necessarily been determined when targets are being built
      magicStackEntryTargetsMap :: Map.Map (ZO 'ZStack OT0) [TargetId]
    , magicStackEntryElectedMap :: Map.Map (ZO 'ZStack OT0) (AnyElected 'Pre)
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
  , gameWinners :: [PlayerIndex] -- ????????????
  , gameLosers :: [PlayerIndex] -- ????????????
  }
  deriving (Typeable)

type Magic v rw m = Magic' (GameResult m) (GameState m) v rw m

type MagicCont v rw m a = MagicCont' (GameResult m) (GameState m) v rw m a

mkOpaqueGameState :: GameState m -> OpaqueGameState m
mkOpaqueGameState = OpaqueGameState

queryMagic' :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic' (OpaqueGameState st) = runMagicRO st

queryMagic :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic opaque = queryMagic' opaque . logCall 'queryMagic

runMagicCont ::
  (IsReadWrite rw, Monad m) =>
  (Either a b -> c) ->
  MagicCont v rw m a b ->
  Magic v rw m c
runMagicCont = runMagicCont' envLogCall

headlessPrompt :: Monad m => Prompt m
headlessPrompt =
  Prompt
    { exceptionCantBeginGameWithoutPlayers = pure ()
    , exceptionInvalidCastSpell = \_ _ _ -> pure ()
    , exceptionInvalidGenericManaPayment = \_ _ -> pure ()
    , exceptionInvalidPlayLand = \_ _ _ -> pure ()
    , exceptionInvalidShuffle = \_ _ -> pure ()
    , exceptionInvalidStartingPlayer = \_ _ -> pure ()
    , exceptionZoneObjectDoesNotExist = \_ -> pure ()
    , promptDebugMessage = \_ -> pure ()
    , promptGetStartingPlayer = \_ _ -> pure $ PlayerIndex 0
    , promptLogCallPop = \_ _ -> pure ()
    , promptLogCallPush = \_ _ -> pure ()
    , promptPayGeneric = \_ _ _ _ -> undefined -- TODO: take first N manas from input pool to satisfy
    , promptPerformMulligan = \_ _ _ -> pure False
    , promptPickZO = \_ _ _ -> pure . NonEmpty.head
    , promptPriorityAction = \_ _ _ -> pure PassPriority
    , promptShuffle = \_ (CardCount count) _ -> pure $ map CardIndex [0 .. count - 1]
    }

withHeadlessPrompt :: Monad m => GameState m -> GameState m
withHeadlessPrompt st =
  let prompt = magicPrompt st
   in st
        { magicPrompt =
            headlessPrompt
              { promptDebugMessage = promptDebugMessage prompt
              , promptLogCallPop = promptLogCallPop prompt
              , promptLogCallPush = promptLogCallPush prompt
              }
        }

envLogCall :: (IsReadWrite rw, Monad m) => EnvLogCall (GameResult m) (GameState m) v rw m
envLogCall =
  EnvLogCall
    { envLogCallCorruptCallStackLogging = do
        pure () -- this introduces enough laziness to not crash strict data field
        error $ show CorruptCallStackLogging
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

logCallPush :: (IsReadWrite rw, Monad m) => String -> Magic v rw m CallFrameInfo
logCallPush = logCallPush' envLogCall

logCallPop :: (IsReadWrite rw, Monad m) => Magic v rw m (Maybe CallFrameInfo)
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

type IsRec = Bool

class LogCall x where
  logCallImpl :: IsNamed name => IsRec -> name -> x -> x

logCall :: (LogCall x, IsNamed name) => name -> x -> x
logCall = logCallImpl False

-- | Like `logCall` but doesn't log if the top frame has the same name.
logCallRec :: (LogCall x, IsNamed name) => name -> x -> x
logCallRec = logCallImpl True

logCallImpl' ::
  (IsNamed name, IsReadWrite rw, Monad m, Monad m') =>
  (forall x. Magic v rw m' x -> m x) ->
  Bool ->
  name ->
  m a ->
  m a
logCallImpl' lift' isRec name action = do
  let name' = showNamed name
  cond <- case isRec of
    False -> pure True
    True ->
      lift' logCallTop <&> \case
        Nothing -> True
        Just frame -> callFrameName frame /= name'
  mFrame <- case cond of
    False -> pure Nothing
    True -> fmap Just $ lift' $ logCallPush name'
  result <- action
  M.when cond do
    mFrame' <- lift' logCallPop
    case mFrame == mFrame' of
      False -> error $ show CorruptCallStackLogging
      True -> pure ()
  pure result

instance (IsReadWrite rw, Monad m) => LogCall (Magic p rw m z) where
  logCallImpl = logCallImpl' id

instance (IsReadWrite rw, Monad m) => LogCall (MagicCont p rw m y z) where
  logCallImpl = logCallImpl' liftCont

instance (IsReadWrite rw, Monad m) => LogCall (a -> Magic p rw m z) where
  logCallImpl isRec name action a = logCallImpl isRec name $ action a

instance (IsReadWrite rw, Monad m) => LogCall (a -> MagicCont p rw m y z) where
  logCallImpl isRec name action a = logCallImpl isRec name $ action a

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> Magic p rw m z) where
  logCallImpl isRec name action a b = logCallImpl isRec name $ action a b

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> MagicCont p rw m y z) where
  logCallImpl isRec name action a b = logCallImpl isRec name $ action a b

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> c -> Magic p rw m z) where
  logCallImpl isRec name action a b c = logCallImpl isRec name $ action a b c

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> c -> MagicCont p rw m y z) where
  logCallImpl isRec name action a b c = logCallImpl isRec name $ action a b c

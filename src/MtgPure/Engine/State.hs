{-# LANGUAGE Safe #-}
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
  Continuation,
  queryMagic,
  runMagicCont,
  ToPriorityEnd (..),
  --
  mkOpaqueGameState,
  getOpaqueGameState,
  OpaqueGameState,
  GameState (..),
  GameInput (..),
  GameResult (..),
  GameFormat (..),
  Prompt,
  TargetId,
  AnyRequirement (..),
  --
  withHeadlessPrompt,
  --
  logCall,
  logCallRec,
  logCallPop,
  logCallPush,
  logCallUnwind,
  --
  EvListener,
  EvListenerId (..),
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Data.Functor ((<&>))
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe Data.Nat (Fin (..))
import safe qualified Data.Stream as Stream
import safe Data.Typeable (Typeable)
import safe Data.Void (Void)
import safe Language.Haskell.TH.Syntax (Name)
import safe MtgPure.Engine.Fwd.Type (Fwd')
import safe MtgPure.Engine.Monad (
  CallFrameId,
  CallFrameInfo (callFrameName),
  EnvLogCall (..),
  HasEnvLogCall (..),
  Magic',
  MagicCont',
  PriorityEnd,
  fromPublic,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  liftCont,
  logCallPop',
  logCallPush',
  logCallTop,
  logCallUnwind',
  magicMapBail,
  modify,
  runMagicCont',
  runMagicRO,
 )
import safe MtgPure.Engine.Prompt (
  AnyElected,
  CardCount (..),
  CardIndex (..),
  Ev,
  InternalLogicError (..),
  PlayerIndex (..),
  PriorityAction (..),
  Prompt' (..),
  TriggerTime,
 )
import safe MtgPure.Model.Deck (Deck (..))
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
  Requirement,
 )
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard)
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Variable (VariableId)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)

type Fwd m = Fwd' (GameResult m) (GameState m) m

instance Show (Fwd m) where
  show _ = show ''Fwd

type Prompt m = Prompt' OpaqueGameState m (Magic 'Public 'RO m)

instance Show (Prompt m) where
  show _ = show ''Prompt

type TargetId = ObjectId

data AnyRequirement :: Type where
  AnyRequirement :: IsZO zone ot => Requirement zone ot -> AnyRequirement

deriving instance Show AnyRequirement

type EvListener v rw m = TriggerTime -> Ev -> Magic v rw m ()

newtype EvListenerId = EvListenerId Int
  deriving (Eq, Ord, Show)

data GameState (m :: Type -> Type) where
  -- TODO: there should be a field that tracks which ObjectIds have been known to each player.
  -- This would allow the Public API to query game state a given player knows about without leaking hidden information.
  -- Simply knowing that a non-visible ID exists allow players to cheat (clients could spam it and then glean zone information and whatnot).
  GameState ::
    { magicCurrentTurn :: Int
    , magicFwd :: Fwd m
    , magicExiledCards :: Map.Map (ZO 'ZExile OT0) AnyCard
    , magicGraveyardCards :: Map.Map (ZO 'ZGraveyard OT0) AnyCard
    , magicHandCards :: Map.Map (ZO 'ZHand OT0) AnyCard
    , magicLibraryCards :: Map.Map (ZO 'ZLibrary OT0) AnyCard
    , magicListenerNextId :: EvListenerId
    , magicListeners :: Map.Map EvListenerId (EvListener 'Private 'RW m)
    , magicManaBurn :: Bool
    , magicNextObjectDiscriminant :: ObjectDiscriminant
    , magicNextObjectId :: ObjectId
    , magicNextVariableId :: VariableId
    , magicOwnershipMap :: Map.Map ObjectId (Object 'OTPlayer)
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

class RegisterEventListener (v :: Visibility) (rw :: ReadWrite) where
  registerListener :: Monad m => EvListener v rw m -> Magic 'Private 'RW m EvListenerId
  listenLocally :: Monad m => [EvListener v rw m] -> Magic 'Private 'RW m a -> Magic 'Private 'RW m a

instance RegisterEventListener 'Private 'RW where
  registerListener listener = do
    evId@(EvListenerId i) <- fromRO $ gets magicListenerNextId
    modify \st ->
      st
        { magicListenerNextId = EvListenerId $ i + 1
        , magicListeners = Map.insert evId listener $ magicListeners st
        }
    pure evId
  listenLocally listeners action = do
    listenerIds <- mapM registerListener listeners
    result <- action
    mapM_ unregisterListener listenerIds
    pure result
   where
    unregisterListener (EvListenerId i) = modify \st ->
      st{magicListeners = Map.delete (EvListenerId i) $ magicListeners st}

instance RegisterEventListener 'Public 'RO where
  registerListener listener = registerListener @ 'Private @ 'RW $ \time -> fromPublic . fromRO . listener time
  listenLocally listeners = listenLocally @ 'Private @ 'RW $ map f listeners
   where
    f listener time = fromPublic . fromRO . listener time

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

type MagicCont v rw bail m a = MagicCont' (GameResult m) (GameState m) v rw bail m a

mkOpaqueGameState :: GameState m -> OpaqueGameState m
mkOpaqueGameState = OpaqueGameState

getOpaqueGameState :: Monad m => Magic 'Public 'RO m (OpaqueGameState m)
getOpaqueGameState = internalFromPrivate $ gets mkOpaqueGameState

queryMagic' :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic' (OpaqueGameState st) = runMagicRO st

queryMagic :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic opaque = queryMagic' opaque . logCall 'queryMagic

runMagicCont ::
  (IsReadWrite rw, Monad m) =>
  MagicCont v rw bail m a ->
  Magic v rw m (Either bail a)
runMagicCont = runMagicCont' envLogCall

type Continuation v rw bail m = MagicCont v rw bail m bail

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
    , promptChooseAttackers = \_ _ _ -> pure []
    , promptChooseBlockers = \_ _ _ _ -> pure []
    , promptChooseOption = \_ _ _ -> pure FZ
    , promptDebugMessage = \_ -> pure ()
    , promptGetStartingPlayer = \_ _ -> pure $ PlayerIndex 0
    , promptLogCallPop = \_ _ -> pure ()
    , promptLogCallPush = \_ _ -> pure ()
    , promptPayDynamicMana = \_ _ _ _ -> undefined -- TODO: take first N manas from input pool to satisfy
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

instance (IsReadWrite rw, Monad m) => HasEnvLogCall (GameResult m) (GameState m) rw m where
  theEnvLogCall = envLogCall

envLogCall :: (IsReadWrite rw, Monad m) => EnvLogCall (GameResult m) (GameState m) v rw m
envLogCall =
  EnvLogCall
    { envLogCallCorruptCallStackLogging = do
        pure () -- this introduces enough laziness to not immediately crash strict data field
        error $ show CorruptCallStackLogging
    , envLogCallPromptPush = \frame -> do
        st <- internalFromPrivate $ fromRO get
        let prompt = magicPrompt st
        M.lift $ promptLogCallPush prompt (OpaqueGameState st) frame
    , envLogCallPromptPop = \frame -> do
        st <- internalFromPrivate $ fromRO get
        let prompt = magicPrompt st
        M.lift $ promptLogCallPop prompt (OpaqueGameState st) frame
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

instance (IsReadWrite rw, Monad m) => LogCall (MagicCont p rw bail m z) where
  logCallImpl = logCallImpl' liftCont

instance (IsReadWrite rw, Monad m) => LogCall (a -> Magic p rw m z) where
  logCallImpl isRec name action a = logCallImpl isRec name $ action a

instance (IsReadWrite rw, Monad m) => LogCall (a -> MagicCont p rw bail m z) where
  logCallImpl isRec name action a = logCallImpl isRec name $ action a

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> Magic p rw m z) where
  logCallImpl isRec name action a b = logCallImpl isRec name $ action a b

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> MagicCont p rw bail m z) where
  logCallImpl isRec name action a b = logCallImpl isRec name $ action a b

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> c -> Magic p rw m z) where
  logCallImpl isRec name action a b c = logCallImpl isRec name $ action a b c

instance (IsReadWrite rw, Monad m) => LogCall (a -> b -> c -> MagicCont p rw bail m z) where
  logCallImpl isRec name action a b c = logCallImpl isRec name $ action a b c

class ToPriorityEnd a b where
  toPriorityEnd :: Monad m => MagicCont 'Private 'RW a m b -> MagicCont 'Private 'RW PriorityEnd m PriorityEnd

instance ToPriorityEnd Void Void where
  toPriorityEnd = mapVoidToEnd . fmap Left

instance ToPriorityEnd Void () where
  toPriorityEnd = mapVoidToEnd . fmap Right

instance ToPriorityEnd Void PriorityEnd where
  toPriorityEnd = mapVoidToEnd

instance ToPriorityEnd () () where
  toPriorityEnd = mapUnitToEnd . fmap Right

instance ToPriorityEnd () Void where
  toPriorityEnd = mapUnitToEnd . fmap Left

instance ToPriorityEnd () PriorityEnd where
  toPriorityEnd = mapUnitToEnd

mapVoidToEnd :: Monad m => MagicCont 'Private 'RW Void m PriorityEnd -> MagicCont 'Private 'RW PriorityEnd m PriorityEnd
mapVoidToEnd = mapToEndImpl Left

mapUnitToEnd :: Monad m => MagicCont 'Private 'RW () m PriorityEnd -> MagicCont 'Private 'RW PriorityEnd m PriorityEnd
mapUnitToEnd = mapToEndImpl Right

mapToEndImpl :: Monad m => (a -> PriorityEnd) -> MagicCont 'Private 'RW a m PriorityEnd -> MagicCont 'Private 'RW PriorityEnd m PriorityEnd
mapToEndImpl f = magicMapBail $ mapBail . mapRet
 where
  mapRet = fmap f
  mapBail = magicMapBail \m -> liftCont do
    either f f <$> runMagicCont m

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Engine.Monad (
  Magic',
  MagicCont',
  Continuation',
  PriorityEnd,
  CallFrameId,
  CallFrameInfo (..),
  HasEnvLogCall (..),
  EnvLogCall (..),
  runMagicRO,
  runMagicRW,
  runMagicCont',
  magicThrow,
  magicCatch,
  magicContBail,
  magicMapBail,
  modify,
  put,
  get,
  gets,
  local,
  liftCont,
  toPrivate,
  toRW,
  fromPublicRO,
  fromPublic,
  fromRO,
  internalFromPrivate,
  internalFromRW,
  logCallPop',
  logCallPush',
  logCallTop,
  logCallUnwind',
) where

import safe Control.Monad.Access (
  AccessM (runAccessM),
  IsReadWrite (..),
  ReadWrite (..),
  SReadWrite (..),
  Visibility (..),
 )
import safe qualified Control.Monad.Access as Access
import safe qualified Control.Monad.State.Strict as State
import safe Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import safe Control.Monad.Trans.Except (ExceptT (ExceptT), catchE, runExceptT, throwE, withExceptT)
import safe Control.Monad.Util (untilJust)
import safe Data.Function (on)
import safe Data.Kind (Type)
import safe Data.Maybe (listToMaybe)
import safe Data.Typeable (Typeable)
import safe Data.Void (Void)

type CallFrameId = Int

data CallFrameInfo = CallFrameInfo
  { callFrameId :: CallFrameId
  , callFrameName :: String
  }
  deriving (Eq, Ord, Show)

data EnvLogCall ex st v rw m = EnvLogCall
  { envLogCallCorruptCallStackLogging :: Magic' ex st v rw m ()
  , envLogCallPromptPush :: CallFrameInfo -> Magic' ex st v rw m ()
  , envLogCallPromptPop :: CallFrameInfo -> Magic' ex st v rw m ()
  }

data LogCallState = LogCallState
  { logCallDepth :: !Int
  , logCallFrames :: ![CallFrameInfo]
  }
  deriving (Eq, Ord, Show)

emptyLogCallState :: LogCallState
emptyLogCallState =
  LogCallState
    { logCallDepth = 0
    , logCallFrames = []
    }

type LogCallT = State.StateT LogCallState

type Inner st v rw m = AccessM v rw (State.StateT st (LogCallT m))

data
  Magic'
    (ex :: Type)
    (st :: Type)
    (v :: Visibility)
    (rw :: ReadWrite)
    (m :: Type -> Type)
    (a :: Type) :: Type
  where
  MagicRO ::
    { unMagicRO :: Inner st v 'RO m a
    } ->
    Magic' ex st v 'RO m a
  MagicRW ::
    { unMagicRW :: ExceptT ex (Inner st v 'RW m) a
    } ->
    Magic' ex st v 'RW m a
  deriving (Typeable)

instance Functor m => Functor (Magic' ex st v rw m) where
  fmap f = \case
    MagicRO a -> MagicRO $ fmap f a
    MagicRW a -> MagicRW $ fmap f a

instance (IsReadWrite rw, Monad m) => Applicative (Magic' ex st v rw m) where
  pure = case singReadWrite @rw of
    SRO -> MagicRO . pure
    SRW -> MagicRW . pure
  magicF <*> magicA = case (magicF, magicA) of
    (MagicRO f, MagicRO a) -> MagicRO $ f <*> a
    (MagicRW f, MagicRW a) -> MagicRW $ f <*> a

instance (IsReadWrite rw, Monad m) => Monad (Magic' ex st v rw m) where
  magicA >>= f = case magicA of
    MagicRO a -> MagicRO $ a >>= unMagicRO . f
    MagicRW a -> MagicRW $ a >>= unMagicRW . f

instance (IsReadWrite rw) => MonadTrans (Magic' ex st v rw) where
  lift = case singReadWrite @rw of
    SRO -> MagicRO . lift . lift . lift
    SRW -> MagicRW . lift . lift . lift . lift

instance (IsReadWrite rw, MonadIO m) => MonadIO (Magic' ex st v rw m) where
  liftIO = case singReadWrite @rw of
    SRO -> MagicRO . liftIO
    SRW -> MagicRW . liftIO

type MagicEsc ex st esc v rw m = ExceptT esc (Magic' ex st v rw m)

type PriorityEnd = Either Void ()

type Continuation' ex st v rw bail m = MagicCont' ex st v rw bail m bail

type MagicContInner ex st v rw bail m a = MagicEsc ex st (Continuation' ex st v rw bail m) v rw m a

newtype MagicCont' ex st v rw bail m a = MagicCont
  { unMagicCont :: MagicContInner ex st v rw bail m a
  }

class (IsReadWrite rw, Monad m) => HasEnvLogCall ex st rw m where
  theEnvLogCall :: EnvLogCall ex st v rw m

instance HasEnvLogCall ex st rw m => Functor (MagicCont' ex st v rw bail m) where
  fmap f = MagicCont . fmap f . unMagicCont

instance HasEnvLogCall ex st rw m => Applicative (MagicCont' ex st v rw bail m) where
  pure = MagicCont . pure
  MagicCont f <*> MagicCont a = MagicCont $ f <*> a

instance HasEnvLogCall ex st rw m => Monad (MagicCont' ex st v rw bail m) where
  MagicCont a >>= f = MagicCont $ a >>= unMagicCont . f

instance (HasEnvLogCall ex st rw m, MonadIO m) => MonadIO (MagicCont' ex st v rw bail m) where
  liftIO = liftCont . liftIO

magicThrow :: Monad m => ex -> Magic' ex st 'Private 'RW m b
magicThrow = MagicRW . throwE

magicCatch ::
  Monad m =>
  Magic' ex st 'Private 'RW m a ->
  (ex -> Magic' ex st 'Private 'RW m a) ->
  Magic' ex st 'Private 'RW m a
magicCatch (MagicRW m) f = MagicRW $ catchE m $ unMagicRW . f

-- | NOTE: This hijacks the current continuation.
-- Use `liftCont` instead of this if you need to preserve the current continuation.
magicContBail :: HasEnvLogCall ex st rw m => MagicCont' ex st v rw bail m bail -> MagicCont' ex st v rw bail m a
magicContBail = MagicCont . throwE

magicMapBail ::
  Monad m =>
  (Continuation' ex st v rw bail m -> Continuation' ex st v rw bail' m) ->
  MagicCont' ex st v rw bail m a ->
  MagicCont' ex st v rw bail' m a
magicMapBail f (MagicCont m) = MagicCont $ withExceptT f m

runMagicRO :: Monad m => st -> Magic' ex st v 'RO m a -> m a
runMagicRO st (MagicRO accessM) =
  let stateM = runAccessM accessM
      callM = State.evalStateT stateM st
      m = State.runStateT callM emptyLogCallState
   in sanityCheckCallStackState <$> m

runMagicRW :: Monad m => st -> Magic' ex st v 'RW m a -> m (Either ex a)
runMagicRW st (MagicRW exceptM) =
  let accessM = runExceptT exceptM
      stateM = runAccessM accessM
      callM = State.evalStateT stateM st
      m = State.runStateT callM emptyLogCallState
   in sanityCheckCallStackState <$> m

sanityCheckCallStackState :: (a, LogCallState) -> a
sanityCheckCallStackState (a, st) = case st == emptyLogCallState of
  True -> a
  False -> error "corrupt call stack"

runMagicEsc ::
  (IsReadWrite rw, Monad m) =>
  EnvLogCall ex st v rw m ->
  (Either esc a -> b) ->
  MagicEsc ex st esc v rw m a ->
  Magic' ex st v rw m b
runMagicEsc _env f action = do
  f <$> runExceptT action

runMagicContBail ::
  (IsReadWrite rw, Monad m) =>
  Maybe CallFrameInfo ->
  EnvLogCall ex st v rw m ->
  MagicCont' ex st v rw bail m bail ->
  Magic' ex st v rw m bail
runMagicContBail top env (MagicCont m) = do
  logCallUnwind' env $ callFrameId <$> top
  runMagicEsc env id m >>= \case
    Left m' -> runMagicContBail top env m'
    Right x -> pure x

runMagicCont' ::
  (IsReadWrite rw, Monad m) =>
  EnvLogCall ex st v rw m ->
  MagicCont' ex st v rw bail m a ->
  Magic' ex st v rw m (Either bail a)
runMagicCont' env (MagicCont m) = do
  top <- logCallTop
  result <-
    runMagicEsc env id m >>= \case
      Left m' -> Left <$> runMagicContBail top env m'
      Right x -> pure $ Right x
  top' <- logCallTop
  case on (==) (fmap callFrameId) top top' of
    True -> pure ()
    False -> envLogCallCorruptCallStackLogging env
  pure result

liftCont :: (IsReadWrite rw, Monad m) => Magic' ex st v rw m a -> MagicCont' ex st v rw bail m a
liftCont = MagicCont . lift

internalLiftCallStackState ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  LogCallT m a ->
  Magic' ex st v rw m a
internalLiftCallStackState = case singReadWrite @rw of
  SRO -> MagicRO . lift . lift
  SRW -> MagicRW . lift . lift . lift

internalLiftState ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  State.StateT st (LogCallT m) a ->
  Magic' ex st v rw m a
internalLiftState = case singReadWrite @rw of
  SRO -> MagicRO . lift
  SRW -> MagicRW . lift . lift

modify :: Monad m => (st -> st) -> Magic' ex st 'Private 'RW m ()
modify = internalLiftState . State.modify'

put :: Monad m => st -> Magic' ex st 'Private 'RW m ()
put = internalLiftState . State.put

get :: Monad m => Magic' ex st 'Private 'RO m st
get = internalLiftState State.get

gets :: Monad m => (st -> a) -> Magic' ex st 'Private 'RO m a
gets = internalLiftState . State.gets

local ::
  (IsReadWrite rw, Monad m) =>
  (st -> st) ->
  Magic' ex st v rw m a ->
  Magic' ex st v rw m a
local f m = do
  st <- internalLiftState State.get
  internalLiftState $ State.modify f
  x <- m
  internalLiftState $ State.put st
  pure x

toPrivate ::
  forall ex st v rw m a.
  Monad m =>
  Magic' ex st v rw m a ->
  Magic' ex st 'Private rw m a
toPrivate = \case
  MagicRO m -> MagicRO $ Access.safeToPrivate m
  MagicRW (ExceptT m) -> MagicRW $ ExceptT $ Access.safeToPrivate m

toRW ::
  forall ex st v rw m a.
  Monad m =>
  Magic' ex st v rw m a ->
  Magic' ex st v 'RW m a
toRW = \case
  MagicRO m -> MagicRW $ ExceptT $ Right <$> Access.safeToRW m
  magic@MagicRW{} -> magic

fromPublicRO ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st 'Public 'RO m a ->
  Magic' ex st v rw m a
fromPublicRO = fromPublic . fromRO

fromPublic ::
  forall ex st v rw m a.
  Monad m =>
  Magic' ex st 'Public rw m a ->
  Magic' ex st v rw m a
fromPublic = \case
  MagicRO m -> MagicRO $ Access.safeFromPublic m
  MagicRW (ExceptT m) -> MagicRW $ ExceptT $ Access.safeFromPublic m

fromRO ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st v 'RO m a ->
  Magic' ex st v rw m a
fromRO (MagicRO m) = case singReadWrite @rw of
  SRO -> MagicRO $ Access.safeFromRO m
  SRW -> MagicRW $ ExceptT $ Right <$> Access.safeFromRO m

internalFromPrivate ::
  forall ex st v rw m a.
  Monad m =>
  Magic' ex st 'Private rw m a ->
  Magic' ex st v rw m a
internalFromPrivate = \case
  MagicRO m -> MagicRO $ Access.unsafeFromPrivate m
  MagicRW (ExceptT m) -> MagicRW $ ExceptT $ Access.unsafeFromPrivate m

internalFromRW ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  (ex -> Magic' ex st v 'RO m a) ->
  Magic' ex st v 'RW m a ->
  Magic' ex st v rw m a
internalFromRW f magic@(MagicRW m) = case singReadWrite @rw of
  SRO ->
    MagicRO (Access.unsafeFromRW $ runExceptT m) >>= \case
      Left ex -> f ex
      Right a -> pure a
  SRW -> magic

logCallUnwind' :: (IsReadWrite rw, Monad m) => EnvLogCall ex st v rw m -> Maybe CallFrameId -> Magic' ex st v rw m ()
logCallUnwind' env top =
  untilJust \_attempt -> do
    top' <- logCallTop
    case top == fmap callFrameId top' of
      True -> pure $ Just ()
      False -> do
        success <- logCallPop' env
        case success of
          Nothing -> envLogCallCorruptCallStackLogging env
          Just _ -> pure ()
        pure Nothing

logCallPush' :: (IsReadWrite rw, Monad m) => EnvLogCall ex st v rw m -> String -> Magic' ex st v rw m CallFrameInfo
logCallPush' env name = do
  i <- internalLiftCallStackState $ State.gets logCallDepth
  let frame =
        CallFrameInfo
          { callFrameId = i
          , callFrameName = name
          }
  internalLiftCallStackState $
    State.modify' \st ->
      st
        { logCallDepth = i + 1
        , logCallFrames = frame : logCallFrames st
        }
  envLogCallPromptPush env frame
  pure frame

logCallPop' :: (IsReadWrite rw, Monad m) => EnvLogCall ex st v rw m -> Magic' ex st v rw m (Maybe CallFrameInfo)
logCallPop' env = do
  frames <- internalLiftCallStackState $ State.gets logCallFrames
  case frames of
    [] -> pure Nothing
    frame : frames' -> do
      internalLiftCallStackState $
        State.modify' \st ->
          st
            { logCallDepth = logCallDepth st - 1
            , logCallFrames = frames'
            }
      n <- internalLiftCallStackState $ State.gets logCallDepth
      case n == callFrameId frame of
        True -> pure ()
        False -> error "CorruptCallStackLogging"
      envLogCallPromptPop env frame
      pure $ Just frame

logCallTop :: (IsReadWrite rw, Monad m) => Magic' ex st v rw m (Maybe CallFrameInfo)
logCallTop = internalLiftCallStackState $ State.gets $ listToMaybe . logCallFrames

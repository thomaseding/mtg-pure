{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Engine.Monad (
  Magic',
  MagicCont',
  EnvLogCall (..),
  runMagicRO,
  runMagicRW,
  runMagicCont',
  magicThrow,
  magicCatch,
  magicCont,
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

import safe qualified Control.Monad as M
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
import safe Control.Monad.Trans.Except (ExceptT (ExceptT), catchE, runExceptT, throwE)
import safe Control.Monad.Util (untilJust)
import safe Data.Function (on)
import safe Data.Functor ((<&>))
import safe Data.Kind (Type)
import safe Data.Maybe (listToMaybe)
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Prompt (
  CallFrameId,
  CallFrameInfo (..),
  InternalLogicError (CorruptCallStackLogging),
 )

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

type MagicEx' ex st ex' v rw m = ExceptT ex' (Magic' ex st v rw m)

newtype MagicCont' ex st v rw m a b = MagicCont'
  { unMagicCont' :: MagicEx' ex st (Magic' ex st v rw m a) v rw m b
  }
  deriving (Functor)

instance (IsReadWrite rw, Monad m) => Applicative (MagicCont' ex st v rw m a) where
  pure = MagicCont' . pure
  MagicCont' f <*> MagicCont' a = MagicCont' $ f <*> a

instance (IsReadWrite rw, Monad m) => Monad (MagicCont' ex st v rw m a) where
  MagicCont' a >>= f = MagicCont' $ a >>= unMagicCont' . f

magicThrow :: Monad m => ex -> Magic' ex st 'Private 'RW m b
magicThrow = MagicRW . throwE

magicCatch ::
  Monad m =>
  Magic' ex st 'Private 'RW m a ->
  (ex -> Magic' ex st 'Private 'RW m a) ->
  Magic' ex st 'Private 'RW m a
magicCatch (MagicRW m) f = MagicRW $ catchE m $ unMagicRW . f

-- NOTE:
-- This hijacks the current continuation.
-- Use `liftCont` instead of this if you need to preseve the current continuation.
magicCont :: Monad m => Magic' ex st 'Private 'RW m a -> MagicCont' ex st 'Private 'RW m a b
magicCont = MagicCont' . throwE

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

runMagicEx' ::
  (IsReadWrite rw, Monad m) =>
  EnvLogCall ex st v rw m ->
  (Either ex' a -> b) ->
  MagicEx' ex st ex' v rw m a ->
  Magic' ex st v rw m b
runMagicEx' env f action = do
  top <- logCallTop
  eitherR <- runExceptT action
  case eitherR of
    Left{} -> logCallUnwind' env $ fmap callFrameId top
    Right{} -> do
      top' <- logCallTop
      case on (==) (fmap callFrameId) top top' of
        True -> pure ()
        False -> envLogCallCorruptCallStackLogging env
  pure $ f eitherR

runMagicCont' ::
  (IsReadWrite rw, Monad m) =>
  EnvLogCall ex st v rw m ->
  (Either a b -> c) ->
  MagicCont' ex st v rw m a b ->
  Magic' ex st v rw m c
runMagicCont' env f = M.join . runMagicEx' env g . unMagicCont'
 where
  g = \case
    Left cont -> f . Left <$> cont
    Right b -> pure $ f $ Right b

liftCont :: (IsReadWrite rw, Monad m) => Magic' ex st v rw m b -> MagicCont' ex st v rw m a b
liftCont = MagicCont' . lift

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
  (ex -> a) ->
  Magic' ex st v 'RW m a ->
  Magic' ex st v rw m a
internalFromRW f magic@(MagicRW m) = case singReadWrite @rw of
  SRO ->
    MagicRO $
      Access.unsafeFromRW $
        runExceptT m <&> \case
          Left ex -> f ex
          Right a -> a
  SRW -> magic

logCallUnwind' :: (IsReadWrite rw, Monad m) => EnvLogCall ex st v rw m -> Maybe CallFrameId -> Magic' ex st v rw m ()
logCallUnwind' env top =
  untilJust \_ -> do
    top' <- logCallTop
    case top == fmap callFrameId top' of
      True -> pure $ Just ()
      False -> do
        success <- logCallPop' env
        case success of
          False -> error $ show CorruptCallStackLogging
          True -> pure Nothing

logCallPush' :: (IsReadWrite rw, Monad m) => EnvLogCall ex st v rw m -> String -> Magic' ex st v rw m CallFrameId
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
  pure i

logCallPop' :: (IsReadWrite rw, Monad m) => EnvLogCall ex st v rw m -> Magic' ex st v rw m Bool
logCallPop' env = do
  frames <- internalLiftCallStackState $ State.gets logCallFrames
  case frames of
    [] -> pure False
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
        False -> error $ show CorruptCallStackLogging
      envLogCallPromptPop env frame
      pure True

logCallTop :: (IsReadWrite rw, Monad m) => Magic' ex st v rw m (Maybe CallFrameInfo)
logCallTop = internalLiftCallStackState $ State.gets $ listToMaybe . logCallFrames

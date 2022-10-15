{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Engine.Monad (
  Magic',
  MagicEx',
  MagicCont',
  EnvLogCall (..),
  runMagicRO,
  runMagicRW,
  runMagicEx',
  runMagicCont',
  magicThrow,
  magicCatch,
  modify,
  put,
  get,
  gets,
  local,
  toPrivate,
  toRW,
  fromPublicRO,
  fromPublic,
  fromRO,
  internalFromPrivate,
  internalFromRW,
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
import safe Data.Function (on)
import safe Data.Functor ((<&>))
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Prompt (CallFrameId, CallFrameInfo (callFrameId))

type Inner st v rw m = AccessM v rw (State.StateT st m)

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
    SRO -> MagicRO . lift . lift
    SRW -> MagicRW . lift . lift . lift

instance (IsReadWrite rw, MonadIO m) => MonadIO (Magic' ex st v rw m) where
  liftIO = case singReadWrite @rw of
    SRO -> MagicRO . lift . liftIO
    SRW -> MagicRW . lift . lift . liftIO

type MagicEx' ex st ex' v rw m = ExceptT ex' (Magic' ex st v rw m)

type MagicCont' ex st v rw m a = MagicEx' ex st (Magic' ex st v rw m a) v rw m

magicThrow :: Monad m => ex -> Magic' ex st 'Private 'RW m b
magicThrow ex = MagicRW $ throwE ex

magicCatch ::
  Monad m =>
  Magic' ex st 'Private 'RW m a ->
  (ex -> Magic' ex st 'Private 'RW m a) ->
  Magic' ex st 'Private 'RW m a
magicCatch (MagicRW m) f = MagicRW $ catchE m $ unMagicRW . f

runMagicRO :: Monad m => st -> Magic' ex st v 'RO m a -> m a
runMagicRO st (MagicRO accessM) =
  let stateM = runAccessM accessM
      m = State.evalStateT stateM st
   in m

runMagicRW :: Monad m => st -> Magic' ex st v 'RW m a -> m (Either ex a)
runMagicRW st (MagicRW exceptM) =
  let accessM = runExceptT exceptM
      stateM = runAccessM accessM
      m = State.evalStateT stateM st
   in m

data EnvLogCall ex st v rw m = EnvLogCall
  { envLogCallTop :: Magic' ex st v rw m (Maybe CallFrameInfo)
  , envLogCallUnwind :: Maybe CallFrameId -> Magic' ex st v rw m ()
  , envLogCallCorruptCallStackLogging :: Magic' ex st v rw m ()
  }

runMagicEx' ::
  (IsReadWrite rw, Monad m) =>
  EnvLogCall ex st v rw m ->
  (Either ex' a -> b) ->
  MagicEx' ex st ex' v rw m a ->
  Magic' ex st v rw m b
runMagicEx' env f action = do
  top <- envLogCallTop env
  eitherR <- runExceptT action
  case eitherR of
    Left{} -> do
      envLogCallUnwind env $ fmap callFrameId top
    Right{} -> do
      top' <- envLogCallTop env
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
runMagicCont' env f = M.join . runMagicEx' env g
 where
  g = \case
    Left cont -> f . Left <$> cont
    Right b -> pure $ f $ Right b

-- XXX: Don't export this to keep Visbility and ReadWrite honest.
liftState :: forall ex st v rw m a. (IsReadWrite rw, Monad m) => State.StateT st m a -> Magic' ex st v rw m a
liftState = case singReadWrite @rw of
  SRO -> MagicRO . lift
  SRW -> MagicRW . lift . lift

modify :: Monad m => (st -> st) -> Magic' ex st 'Private 'RW m ()
modify = liftState . State.modify'

put :: Monad m => st -> Magic' ex st 'Private 'RW m ()
put = liftState . State.put

get :: Monad m => Magic' ex st 'Private 'RO m st
get = liftState State.get

gets :: Monad m => (st -> a) -> Magic' ex st 'Private 'RO m a
gets = liftState . State.gets

local ::
  (IsReadWrite rw, Monad m) =>
  (st -> st) ->
  Magic' ex st v rw m a ->
  Magic' ex st v rw m a
local f m = do
  st <- liftState State.get
  liftState $ State.modify f
  x <- m
  liftState $ State.put st
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

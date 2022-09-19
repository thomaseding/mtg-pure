{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module MtgPure.Engine.Monad (
  Magic',
  runMagicRO,
  runMagicRW,
  magicThrow,
  modify,
  put,
  get,
  gets,
  local,
  safeToPrivate,
  safeToRW,
  safeFromPublicRO,
  safeFromPublic,
  safeFromRO,
  unsafeFromPrivate,
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
import safe Control.Monad.Trans (MonadTrans (..))
import safe Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

type Inner st v rw m = AccessM v rw (State.StateT st m)

data family
  Magic'
    (ex :: Type)
    (st :: Type)
    (v :: Visibility)
    (rw :: ReadWrite)
    (m :: Type -> Type)
    (a :: Type) ::
    Type

newtype instance Magic' ex st v 'RO m a = MagicRO
  { unMagicRO :: Inner st v 'RO m a
  }
  deriving (Typeable)

newtype instance Magic' ex st v 'RW m a = MagicRW
  { unMagicRW :: ExceptT ex (Inner st v 'RW m) a
  }
  deriving (Typeable)

instance (IsReadWrite rw, Functor m) => Functor (Magic' ex st v rw m) where
  fmap f magic = case (singReadWrite @rw, magic) of
    (SRO, MagicRO a) -> MagicRO $ fmap f a
    (SRW, MagicRW a) -> MagicRW $ fmap f a

instance (IsReadWrite rw, Monad m) => Applicative (Magic' ex st v rw m) where
  pure = case singReadWrite @rw of
    SRO -> MagicRO . pure
    SRW -> MagicRW . pure
  magicF <*> magicA = case (singReadWrite @rw, magicF, magicA) of
    (SRO, MagicRO f, MagicRO a) -> MagicRO $ f <*> a
    (SRW, MagicRW f, MagicRW a) -> MagicRW $ f <*> a

instance (IsReadWrite rw, Monad m) => Monad (Magic' ex st v rw m) where
  magicA >>= f = case (singReadWrite @rw, magicA) of
    (SRO, MagicRO a) -> MagicRO $ a >>= unMagicRO . f
    (SRW, MagicRW a) -> MagicRW $ a >>= unMagicRW . f

instance (IsReadWrite rw) => MonadTrans (Magic' ex st v rw) where
  lift = case singReadWrite @rw of
    SRO -> MagicRO . lift . lift
    SRW -> MagicRW . lift . lift . lift

magicThrow :: Monad m => ex -> Magic' ex st v 'RW m b
magicThrow ex = MagicRW $ throwE ex

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

safeToPrivate ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st v rw m a ->
  Magic' ex st 'Private rw m a
safeToPrivate magic = case (singReadWrite @rw, magic) of
  (SRO, MagicRO m) -> MagicRO $ Access.safeToPrivate m
  (SRW, MagicRW (ExceptT m)) -> MagicRW $ ExceptT $ Access.safeToPrivate m

safeToRW ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st v rw m a ->
  Magic' ex st v 'RW m a
safeToRW magic = case (singReadWrite @rw, magic) of
  (SRO, MagicRO m) -> MagicRW $ ExceptT $ Right <$> Access.safeToRW m
  (SRW, _) -> magic

safeFromPublicRO ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st 'Public 'RO m a ->
  Magic' ex st v rw m a
safeFromPublicRO = safeFromPublic . safeFromRO

safeFromPublic ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st 'Public rw m a ->
  Magic' ex st v rw m a
safeFromPublic magic = case (singReadWrite @rw, magic) of
  (SRO, MagicRO m) -> MagicRO $ Access.safeFromPublic m
  (SRW, MagicRW (ExceptT m)) -> MagicRW $ ExceptT $ Access.safeFromPublic m

safeFromRO ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st v 'RO m a ->
  Magic' ex st v rw m a
safeFromRO magic = case (singReadWrite @rw, magic) of
  (SRO, MagicRO m) -> MagicRO $ Access.safeFromRO m
  (SRW, MagicRO m) -> MagicRW $ ExceptT $ Right <$> Access.safeFromRO m

unsafeFromPrivate ::
  forall ex st v rw m a.
  (IsReadWrite rw, Monad m) =>
  Magic' ex st 'Private rw m a ->
  Magic' ex st v rw m a
unsafeFromPrivate magic = case (singReadWrite @rw, magic) of
  (SRO, MagicRO m) -> MagicRO $ Access.unsafeFromPrivate m
  (SRW, MagicRW (ExceptT m)) -> MagicRW $ ExceptT $ Access.unsafeFromPrivate m

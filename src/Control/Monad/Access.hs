{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Control.Monad.Access (
  Visibility (..),
  ReadWrite (..),
  SReadWrite (..),
  IsReadWrite (..),
  AccessM,
  runAccessM,
  safeToPrivate,
  safeToRW,
  safeFromPublic,
  safeFromRO,
  unsafeFromPrivate,
  unsafeFromRW,
  unsafeToPublic,
  unsafeToRO,
) where

import safe Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Visibility
  = Public
  | Private
  deriving (Eq, Ord, Show, Typeable)

data ReadWrite
  = RO -- ReadOnly
  | RW -- ReadWrite
  deriving (Eq, Ord, Show, Typeable)

data SReadWrite (rw :: ReadWrite) :: Type where
  SRO :: SReadWrite 'RO
  SRW :: SReadWrite 'RW
  deriving (Typeable)

class IsReadWrite (rw :: ReadWrite) where
  singReadWrite :: SReadWrite rw

instance IsReadWrite 'RO where
  singReadWrite = SRO

instance IsReadWrite 'RW where
  singReadWrite = SRW

newtype
  AccessM
    (v :: Visibility)
    (rw :: ReadWrite)
    (m :: Type -> Type)
    (a :: Type) ::
    Type
  where
  AccessM ::
    { runAccessM :: m a
    } ->
    AccessM v rw m a
  deriving (Typeable)

instance (Functor m) => Functor (AccessM v rw m) where
  fmap f (AccessM a) = AccessM $ fmap f a

instance (Applicative m) => Applicative (AccessM v rw m) where
  pure = AccessM . pure
  AccessM f <*> AccessM a = AccessM $ f <*> a

instance (Monad m) => Monad (AccessM v rw m) where
  AccessM a >>= f = AccessM $ a >>= runAccessM . f

instance MonadTrans (AccessM v rw) where
  lift = AccessM

instance (MonadIO m) => MonadIO (AccessM v rw m) where
  liftIO = AccessM . liftIO

safeToPrivate :: AccessM v rw m a -> AccessM 'Private rw m a
safeToPrivate (AccessM a) = AccessM a

safeToRW :: AccessM v rw m a -> AccessM v 'RW m a
safeToRW (AccessM a) = AccessM a

safeFromPublic :: AccessM 'Public rw m a -> AccessM v rw m a
safeFromPublic (AccessM a) = AccessM a

safeFromRO :: AccessM v 'RO m a -> AccessM v rw m a
safeFromRO (AccessM a) = AccessM a

unsafeFromPrivate :: AccessM 'Private rw m a -> AccessM v rw m a
unsafeFromPrivate (AccessM a) = AccessM a

unsafeFromRW :: AccessM v 'RW m a -> AccessM v rw m a
unsafeFromRW (AccessM a) = AccessM a

unsafeToPublic :: AccessM v rw m a -> AccessM 'Public rw m a
unsafeToPublic (AccessM a) = AccessM a

unsafeToRO :: AccessM v rw m a -> AccessM v 'RO m a
unsafeToRO (AccessM a) = AccessM a

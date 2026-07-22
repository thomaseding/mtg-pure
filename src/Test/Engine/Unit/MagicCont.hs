{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module Test.Engine.Unit.MagicCont (
  main,
  mainUnitMagicCont,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadIO (liftIO))
import safe Data.IORef (newIORef, readIORef, writeIORef)
import safe GHC.Stack (HasCallStack)
import safe MtgPure.Engine.Monad (
  EnvLogCall (..),
  HasEnvLogCall (..),
  MagicCont',
  liftCont,
  magicContBail,
  magicThrow,
  runMagicCont',
  runMagicRW,
 )

main :: (HasCallStack) => IO ()
main = mainUnitMagicCont

data Ex = Ex
  deriving (Eq, Show)

data St = St
  { st_ :: ()
  , stInt :: Int
  , stMsgs :: [String]
  }
  deriving (Show)

type UnitCont = MagicCont' Ex St 'Private 'RW

instance (Monad m) => HasEnvLogCall Ex St 'RW m where
  theEnvLogCall :: (Monad m) => EnvLogCall Ex St v 'RW m
  theEnvLogCall = envLogCall

envLogCall :: (Monad m) => EnvLogCall Ex St v 'RW m
envLogCall =
  EnvLogCall
    { envLogCallCorruptCallStackLogging = error "envLogCallCorruptCallStackLogging"
    , envLogCallPromptPush = \_ -> pure ()
    , envLogCallPromptPop = \_ -> pure ()
    }

runUnitCont :: (Monad m) => (Either a b -> c) -> UnitCont a m b -> m (Either Ex c)
runUnitCont f action = runMagicRW st $ f <$> runMagicCont' envLogCall action
 where
  st =
    St
      { st_ = ()
      , stInt = 0
      , stMsgs = []
      }

mainUnitMagicCont :: (HasCallStack) => IO ()
mainUnitMagicCont = do
  unitCont1
  unitCont2
  unitCont3

expectEq :: (HasCallStack, Eq a, Show a) => String -> a -> a -> IO ()
expectEq name expected actual =
  case actual == expected of
    True -> pure ()
    False -> error $ name <> ": expected " <> show expected <> " but got " <> show actual

unitCont1 :: (HasCallStack) => IO ()
unitCont1 = do
  resumedRef <- newIORef False
  result <- runUnitCont id do
    _ <- magicContBail $ pure 666
    liftIO $ writeIORef resumedRef True
    pure "failure"
  expectEq "unitCont1 result" (Right $ Left 666) (result :: Either Ex (Either Int String))
  resumed <- readIORef resumedRef
  expectEq "unitCont1 resumed after bail" False resumed

unitCont2 :: (HasCallStack) => IO ()
unitCont2 = do
  resumedRef <- newIORef False
  result <- runUnitCont id do
    () <- liftCont $ magicThrow Ex
    liftIO $ writeIORef resumedRef True
    pure "failure"
  expectEq "unitCont2 result" (Left Ex) (result :: Either Ex (Either Int String))
  resumed <- readIORef resumedRef
  expectEq "unitCont2 resumed after throw" False resumed

unitCont3 :: (HasCallStack) => IO ()
unitCont3 = do
  result <- runUnitCont id do
    pure "success"
  expectEq "unitCont3 result" (Right $ Right "success") (result :: Either Ex (Either Int String))

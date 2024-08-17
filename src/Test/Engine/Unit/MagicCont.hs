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

main :: IO ()
main = mainUnitMagicCont

data Ex = Ex
  deriving (Show)

data St = St
  { st_ :: ()
  , stInt :: Int
  , stMsgs :: [String]
  }
  deriving (Show)

type UnitCont = MagicCont' Ex St 'Private 'RW

instance (Monad m) => HasEnvLogCall Ex St 'RW m where
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

mainUnitMagicCont :: IO ()
mainUnitMagicCont = do
  putStrLn "\nunitCont1"
  unitCont1
  putStrLn "\nunitCont2"
  unitCont2
  putStrLn "\nunitCont3"
  unitCont3
  putStrLn ""

unitCont1 :: IO ()
unitCont1 = do
  result <- runUnitCont id do
    _ <- magicContBail $ pure 666
    liftIO $ putStrLn "if this prints, then the test failed"
    pure "failure"
  print (result :: Either Ex (Either Int String))

unitCont2 :: IO ()
unitCont2 = do
  result <- runUnitCont id do
    () <- liftCont $ magicThrow Ex
    liftIO $ putStrLn "if this prints, then the test failed"
    pure "failure"
  print (result :: Either Ex (Either Int String))

unitCont3 :: IO ()
unitCont3 = do
  result <- runUnitCont id do
    pure "success"
  print (result :: Either Ex (Either Int String))

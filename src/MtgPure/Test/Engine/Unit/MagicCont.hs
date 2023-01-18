{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Test.Engine.Unit.MagicCont (
  main,
  mainUnitMagicCont,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadIO (liftIO))
import safe MtgPure.Engine.Monad (
  EnvLogCall (..),
  MagicCont',
  liftCont,
  magicCont,
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

type UnitCont m a = MagicCont' Ex St 'Private 'RW m a

envLogCall :: Monad m => EnvLogCall Ex St 'Private 'RW m
envLogCall =
  EnvLogCall
    { envLogCallCorruptCallStackLogging = pure ()
    , envLogCallPromptPush = \_ -> pure ()
    , envLogCallPromptPop = \_ -> pure ()
    }

runUnitCont :: Monad m => (Either a b -> c) -> UnitCont m a b -> m (Either Ex c)
runUnitCont f action = runMagicRW st $ runMagicCont' envLogCall f action
 where
  st =
    St
      { st_ = ()
      , stInt = 0
      , stMsgs = []
      }

mainUnitMagicCont :: IO ()
mainUnitMagicCont = do
  unitCont1
  unitCont2
  unitCont3

unitCont1 :: IO ()
unitCont1 = do
  result <- runUnitCont id do
    _ <- magicCont $ pure 666
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

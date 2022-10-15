{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Test.MountainShock (
  main,
  mainMountainShock,
) where

import safe Control.Monad.Trans (MonadIO (..))
import safe qualified Control.Monad.Trans.State.Strict as State
import Data.Maybe (listToMaybe)
import safe qualified Data.Set as Set
import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  CallFrameId,
  CallFrameInfo (..),
  CardCount (..),
  CardIndex (..),
  InternalLogicError (CorruptCallStackLogging),
  PlayerIndex (PlayerIndex),
  Prompt' (..),
 )
import safe MtgPure.Engine.State (GameFormat (..), GameInput (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainMountainShock

-- NOTE: Still a WIP
mainMountainShock :: IO ()
mainMountainShock = runDemo (playGame input) >>= print

deck :: Deck
deck =
  Deck $
    concat
      [ replicate 30 $ AnyCard mountain
      , replicate 30 $ AnyCard shock
      ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate 7 $ AnyCard mountain
      , replicate 8 $ AnyCard shock
      ]

data DemoState = DemoState
  { demo_logDepth :: !Int
  , demo_logFrames :: ![CallFrameInfo]
  , demo_logIgnore :: !(Set.Set String)
  }

type Demo = State.StateT DemoState IO

runDemo :: Demo a -> IO a
runDemo demo = do
  (x, st) <-
    State.runStateT
      demo
      DemoState
        { demo_logDepth = 0
        , demo_logFrames = []
        , demo_logIgnore = demoLogIgnore
        }
  case demo_logDepth st of
    0 -> pure ()
    _ -> error $ show CorruptCallStackLogging
  case demo_logFrames st of
    [] -> pure ()
    _ -> error $ show CorruptCallStackLogging
  pure x

input :: GameInput Demo
input =
  GameInput
    { gameInput_decks = replicate 2 (deck, side)
    , gameInput_gameFormat = Vintage
    , gameInput_mulligan = DisableMulligan
    , gameInput_prompt =
        Prompt
          { exceptionCantBeginGameWithoutPlayers = liftIO $ putStrLn "exceptionCantBeginGameWithoutPlayers"
          , exceptionInvalidCastSpell = \_ _ _ -> liftIO $ putStrLn "exceptionInvalidCastSpell"
          , exceptionInvalidPlayLand = \_ _ _ -> liftIO $ putStrLn "exceptionInvalidPlayLand"
          , exceptionInvalidShuffle = \_ _ -> liftIO $ putStrLn "exceptionInvalidShuffle"
          , exceptionInvalidStartingPlayer = \_ _ -> liftIO $ putStrLn "exceptionInvalidStartingPlayer"
          , promptActivateAbility = \_ _ -> pure Nothing
          , promptCastSpell = \_ _ -> pure Nothing
          , promptDebugMessage = \msg -> liftIO $ putStrLn $ "DEBUG: " ++ msg
          , promptGetStartingPlayer = \_count -> pure $ PlayerIndex 0
          , promptLogCallPop = demoLogCallPop
          , promptLogCallPush = demoLogCallPush
          , promptLogCallTop = demoLogCallTop
          , promptPerformMulligan = \_p _hand -> pure False
          , promptPickZO = \_p zos -> pure $ case zos of
              [] -> error "should be non-empty"
              zo : _ -> zo
          , promptPlayLand = \_st _p -> pure Nothing
          , promptShuffle = \(CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

demoIgnoreCall :: String -> Demo Bool
demoIgnoreCall name = State.gets $ Set.member name . demo_logIgnore

demoLogCallPush :: String -> Demo CallFrameId
demoLogCallPush name = do
  i <- State.gets demo_logDepth
  let frame =
        CallFrameInfo
          { callFrameId = i
          , callFrameName = name
          }
  State.modify' $ \st ->
    st
      { demo_logDepth = i + 1
      , demo_logFrames = frame : demo_logFrames st
      }
  demoIgnoreCall name >>= \case
    True -> pure ()
    False -> do
      let indent = replicate i ' '
      liftIO $ putStrLn $ indent ++ "+" ++ name ++ ": " ++ show i
  pure i

demoLogCallPop :: Demo Bool
demoLogCallPop = do
  frames <- State.gets demo_logFrames
  case frames of
    [] -> pure False
    frame : frames' -> do
      State.modify' $ \st ->
        st
          { demo_logDepth = demo_logDepth st - 1
          , demo_logFrames = frames'
          }
      n <- State.gets demo_logDepth
      case n == callFrameId frame of
        True -> pure ()
        False -> error $ show CorruptCallStackLogging
      let indent = replicate n ' '
      liftIO $ putStrLn $ indent ++ "-" ++ callFrameName frame ++ ": " ++ show n
      pure True

demoLogCallTop :: Demo (Maybe CallFrameInfo)
demoLogCallTop = State.gets $ listToMaybe . demo_logFrames

demoLogIgnore :: Set.Set String
demoLogIgnore =
  Set.fromList
    [ ""
    , "MtgPure.Engine.Core.findHandCardImpl"
    , "MtgPure.Engine.Core.findLibraryCardImpl"
    , "MtgPure.Engine.Core.findPlayerImpl"
    , "MtgPure.Engine.Core.getActivePlayerImpl"
    , "MtgPure.Engine.Core.getAlivePlayerCountImpl"
    , "MtgPure.Engine.Core.getAPNAPImpl"
    , "MtgPure.Engine.Core.getPlayerImpl"
    , "MtgPure.Engine.Core.getPlayersImpl"
    , "MtgPure.Engine.Core.newObjectIdImpl"
    , "MtgPure.Engine.Core.pushHandCardImpl"
    , "MtgPure.Engine.Core.pushLibraryCardImpl"
    , "MtgPure.Engine.Core.removeHandCardImpl"
    , "MtgPure.Engine.Core.removeLibraryCardImpl"
    , "MtgPure.Engine.Core.setPlayerImpl"
    ]

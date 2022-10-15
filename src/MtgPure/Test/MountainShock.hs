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

import Control.Exception (assert)
import safe Control.Monad.Trans (MonadIO (..))
import safe qualified Control.Monad.Trans.State.Strict as State
import safe qualified Data.Set as Set
import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Engine.Fwd.Wrap (getPlayer, queryMagic, withEachPlayer_)
import safe MtgPure.Engine.Monad (internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  CallFrameInfo (..),
  CardCount (..),
  CardIndex (..),
  InternalLogicError (CorruptCallStackLogging),
  PlayerIndex (PlayerIndex),
  Prompt' (..),
  ShowZO (ShowZO),
 )
import safe MtgPure.Engine.State (GameFormat (..), GameInput (..), OpaqueGameState)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Object (Object (..), ObjectType (..))
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (ObjectId))
import safe MtgPure.Model.Player (Player (..))
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
  { demo_ :: ()
  , demo_logDisabled :: !Bool
  }

type Demo = State.StateT DemoState IO

runDemo :: Demo a -> IO a
runDemo demo = do
  (x, st) <-
    State.runStateT
      demo
      DemoState
        { demo_ = ()
        , demo_logDisabled = False
        }
  case demo_logDisabled st of
    False -> pure ()
    True -> error $ show CorruptCallStackLogging
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
          , promptPerformMulligan = \_p _hand -> pure False
          , promptPickZO = \_p zos -> pure $ case zos of
              [] -> error "should be non-empty"
              zo : _ -> zo
          , promptPlayLand = \_st _p -> pure Nothing
          , promptShuffle = \(CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

demoLogCallPush :: OpaqueGameState Demo -> CallFrameInfo -> Demo ()
demoLogCallPush opaque frame = case name == "MtgPure.Engine.Fwd.Wrap.queryMagic" of
  True -> State.modify' $ \st -> assert (not $ demo_logDisabled st) st{demo_logDisabled = True}
  False ->
    State.gets demo_logDisabled >>= \case
      True -> pure ()
      False -> case Set.member name demoLogIgnore of
        True -> pure ()
        False -> do
          let indent = replicate i ' '
          liftIO $ putStrLn $ indent ++ "+" ++ name ++ ": " ++ show i
          case Set.member name demoLogDetailed of
            False -> pure ()
            True -> demoPrintGameState opaque
 where
  name = callFrameName frame
  i = callFrameId frame

demoLogCallPop :: OpaqueGameState Demo -> CallFrameInfo -> Demo ()
demoLogCallPop _opaque frame = case name == "MtgPure.Engine.Fwd.Wrap.queryMagic" of
  True -> State.modify' $ \st -> assert (demo_logDisabled st) st{demo_logDisabled = False}
  False ->
    State.gets demo_logDisabled >>= \case
      True -> pure ()
      False -> case Set.member name demoLogIgnore of
        True -> pure ()
        False -> do
          let indent = replicate i ' '
          liftIO $ putStrLn $ indent ++ "-" ++ name ++ ": " ++ show i
 where
  name = callFrameName frame
  i = callFrameId frame

getPlayerName :: Object 'OTPlayer -> String
getPlayerName o = case getObjectId o of
  ObjectId i -> case i of
    0 -> "Alice"
    1 -> "Bob"
    _ -> "UnknownPlayer" ++ show i

horizLine :: IO ()
horizLine = do
  putStrLn $ replicate 20 '-'

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPrintGameState :: OpaqueGameState Demo -> Demo ()
demoPrintGameState opaque = queryMagic opaque $ do
  liftIO $ do
    horizLine
    print "GAME STATE BEGIN"
  withEachPlayer_ $ \oPlayer -> do
    let name = getPlayerName oPlayer
    liftIO $ do
      horizLine
      putStrLn $ name ++ ":"
    player <- internalFromPrivate $ getPlayer oPlayer
    liftIO $ do
      print ("library", length $ unLibrary $ playerLibrary player)
      print ("hand", map ShowZO $ unHand $ playerHand player)
  liftIO $ do
    print "GAME STATE END"
    horizLine

demoLogDetailed :: Set.Set String
demoLogDetailed =
  Set.fromList
    [ ""
    , "MtgPure.Engine.Turn.untapStep"
    ]

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

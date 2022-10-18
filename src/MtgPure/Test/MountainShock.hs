{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
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

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadIO (..))
import safe qualified Control.Monad.Trans.State.Strict as State
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Engine.Fwd.Wrap (
  getAllActivatedAbilities,
  getPlayer,
  queryMagic,
  withEachPlayer_,
 )
import safe MtgPure.Engine.Monad (gets, internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  ActivateAbility (ActivateAbility),
  CallFrameInfo (..),
  CardCount (..),
  CardIndex (..),
  InternalLogicError (CorruptCallStackLogging),
  PlayLand (..),
  PlayerIndex (PlayerIndex),
  Prompt' (..),
  ShowZO (ShowZO),
  SomeActivatedAbility (someActivatedZO),
 )
import safe MtgPure.Engine.State (
  GameFormat (..),
  GameInput (..),
  GameState (..),
  Magic,
  OpaqueGameState,
 )
import safe MtgPure.Model.CardName (CardName (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Object (Object (..), ObjectType (..))
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (ObjectId))
import safe MtgPure.Model.ObjectType.Kind (OTCard, OTPermanent)
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (AnyCard (..), Card (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (ZO)
import safe MtgPure.Model.ZoneObject.Convert (toZO0, toZO1)
import safe System.Random (randomRIO)

main :: IO ()
main = mainMountainShock

-- NOTE: Still a WIP
mainMountainShock :: IO ()
mainMountainShock = runDemo (playGame input) >>= print

deck :: Deck
deck =
  Deck $
    concat $
      replicate
        30
        [ AnyCard mountain
        , AnyCard shock
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
          , promptActivateAbility = demoActivateAbility
          , promptCastSpell = \_ _ -> pure Nothing
          , promptDebugMessage = \msg -> liftIO $ putStrLn $ "DEBUG: " ++ msg
          , promptGetStartingPlayer = \_count -> pure $ PlayerIndex 0
          , promptLogCallPop = demoLogCallPop
          , promptLogCallPush = demoLogCallPush
          , promptPerformMulligan = \_p _hand -> pure False
          , promptPickZO = \_p zos -> pure case zos of
              [] -> error "should be non-empty"
              zo : _ -> zo
          , promptPlayLand = demoPlayLand
          , promptShuffle = \(CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

tabWidth :: Int
tabWidth = 2

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPlayLand :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe PlayLand)
demoPlayLand opaque oPlayer = queryMagic opaque do
  player <- internalFromPrivate $ getPlayer oPlayer
  let Hand zos = playerHand player
  case zos of
    [] -> pure Nothing
    _ -> do
      let n = length zos
      idx <- randomRIO (-1, n - 1)
      let zo = zos !! idx
          zo0 = toZO0 @ 'ZHand zo
      case idx of
        -1 -> pure Nothing
        _ -> do
          liftIO $ print ("playing land", ShowZO zo)
          pure $ Just $ PlayLand $ toZO1 zo0

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoActivateAbility :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe ActivateAbility)
demoActivateAbility opaque oPlayer = queryMagic opaque do
  -- TODO: Filter based on player
  _player <- internalFromPrivate $ getPlayer oPlayer
  abilities <- internalFromPrivate (getAllActivatedAbilities @ 'ZBattlefield @OTPermanent)
  liftIO $ print ("len all abils", length abilities)
  case abilities of
    [] -> pure Nothing
    _ -> do
      let n = length abilities
      idx <- randomRIO (-1, n - 1)
      let ability = abilities !! idx
      case idx of
        -1 -> pure Nothing
        _ -> do
          liftIO $ print ("activating ability", ShowZO $ someActivatedZO ability)
          pure $ Just $ ActivateAbility ability

demoLogCallPush :: OpaqueGameState Demo -> CallFrameInfo -> Demo ()
demoLogCallPush opaque frame = case name == "MtgPure.Engine.Fwd.Wrap.queryMagic" of
  True -> State.modify' \st -> assert (not $ demo_logDisabled st) st{demo_logDisabled = True}
  False ->
    State.gets demo_logDisabled >>= \case
      True -> pure ()
      False -> case Set.member name logIgnore of
        True -> pure ()
        False -> do
          let indent = replicate (tabWidth * i) ' '
          liftIO $ putStrLn $ indent ++ "+" ++ name ++ ": " ++ show i
          case Set.member name logDetailed of
            False -> pure ()
            True -> printGameState opaque
 where
  name = callFrameName frame
  i = callFrameId frame

demoLogCallPop :: OpaqueGameState Demo -> CallFrameInfo -> Demo ()
demoLogCallPop _opaque frame = case name == "MtgPure.Engine.Fwd.Wrap.queryMagic" of
  True -> State.modify' \st -> assert (demo_logDisabled st) st{demo_logDisabled = False}
  False ->
    State.gets demo_logDisabled >>= \case
      True -> pure ()
      False -> case Set.member name logIgnore of
        True -> pure ()
        False -> do
          let indent = replicate (tabWidth * i) ' '
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
printGameState :: OpaqueGameState Demo -> Demo ()
printGameState opaque = queryMagic opaque do
  liftIO do
    horizLine
    print "GAME STATE BEGIN"
  withEachPlayer_ \oPlayer -> do
    let name = getPlayerName oPlayer
    liftIO do
      horizLine
      putStrLn $ name ++ ":"
    player <- internalFromPrivate $ getPlayer oPlayer
    liftIO do
      print ("life", playerLife player)
      print ("library", length $ unLibrary $ playerLibrary player)
      print ("mana", show $ playerMana player)
    printHand $ playerHand player
  liftIO do
    print "GAME STATE END"
    horizLine
    M.void getLine

printHand :: Hand -> Magic 'Public 'RO Demo ()
printHand (Hand zos) = do
  names <- T.for zos getHandCardName
  liftIO $ print ("hand", names)

getHandCardName :: ZO 'ZHand OTCard -> Magic 'Public 'RO Demo String
getHandCardName zo = do
  let zo0 = toZO0 zo
  handCards <- internalFromPrivate $ gets magicHandCards
  case Map.lookup zo0 handCards of
    Nothing -> pure $ "IllegalHandCard@" ++ show (ShowZO zo)
    Just (AnyCard card) -> case card of
      Card (CardName name) _ -> pure name

logDetailed :: Set.Set String
logDetailed =
  Set.fromList
    [ ""
    , "MtgPure.Engine.Turn.untapStep"
    ]

logIgnore :: Set.Set String
logIgnore =
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

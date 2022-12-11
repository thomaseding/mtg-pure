{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}

module MtgPure.Test.MountainShock (
  main,
  mainMountainShock,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadIO (..))
import safe qualified Control.Monad.Trans as M
import safe qualified Control.Monad.Trans.State.Strict as State
import safe Control.Monad.Util (UntilJust (..), untilJust)
import safe qualified Data.List as List
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Engine.Fwd.Api (
  allPlayers,
  allZOActivatedAbilities,
  controllerOf,
  eachLogged_,
  getPlayer,
  satisfies,
 )
import safe MtgPure.Engine.Monad (get, gets, internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  CallFrameInfo (..),
  CardCount (..),
  CardIndex (..),
  InternalLogicError (CorruptCallStackLogging),
  Play (..),
  PlayerIndex (PlayerIndex),
  Prompt' (..),
  SomeActivatedAbility (someActivatedZO),
 )
import safe MtgPure.Engine.State (
  GameFormat (..),
  GameInput (..),
  GameState (..),
  Magic,
  OpaqueGameState,
  queryMagic,
 )
import safe MtgPure.Model.CardName (CardName (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectId (ObjectId (ObjectId), getObjectId)
import safe MtgPure.Model.ObjectType (ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTActivatedAbility,
  OTCard,
  OTLand,
  OTPermanent,
  OTSpell,
 )
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (AnyCard (..), Card (..), Requirement (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.VisitObjectN (VisitObjectN (promoteIdToObjectN))
import safe MtgPure.Model.Zone (SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (ZO, ZoneObject (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, toZO1, zo0ToCard, zo0ToPermanent)
import safe MtgPure.ModelCombinators (isTapped)
import safe qualified System.IO as IO
import safe System.Random (randomRIO)
import safe Text.Read (readMaybe)

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
        (if True then 1 else 30)
        [ AnyCard mountain
        , AnyCard shock
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate (if True then 1 else 7) $ AnyCard mountain
      , replicate (if True then 1 else 8) $ AnyCard shock
      ]

data DemoState = DemoState
  { demo_ :: ()
  , demo_logDisabled :: !Int
  }

type Demo = State.StateT DemoState IO

runDemo :: Demo a -> IO a
runDemo demo = do
  (x, st) <-
    State.runStateT
      demo
      DemoState
        { demo_ = ()
        , demo_logDisabled = 0
        }
  case demo_logDisabled st of
    0 -> pure ()
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
          , exceptionInvalidPlayLand = \_ player msg -> liftIO $ print (player, msg)
          , exceptionInvalidShuffle = \_ _ -> liftIO $ putStrLn "exceptionInvalidShuffle"
          , exceptionInvalidStartingPlayer = \_ _ -> liftIO $ putStrLn "exceptionInvalidStartingPlayer"
          , promptActivateAbility = if True then demoActivateAbilityUser else demoActivateAbilityRandom
          , promptCastSpell = if True then demoCastSpellUser else demoCastSpellRandom
          , promptDebugMessage = \msg -> liftIO $ putStrLn $ "DEBUG: " ++ msg
          , promptGetStartingPlayer = \_count -> pure $ PlayerIndex 0
          , promptLogCallPop = demoLogCallPop
          , promptLogCallPush = demoLogCallPush
          , promptPerformMulligan = \_p _hand -> pure False
          , promptPickZO = \_p zos -> pure case zos of
              zo :| _ -> zo
          , promptPlayLand = if True then demoPlayLandUser else demoPlayLandRandom
          , promptShuffle = \(CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

tabWidth :: Int
tabWidth = 2

pause :: MonadIO m => m ()
pause = M.void $ liftIO getLine

prompt :: MonadIO m => String -> m ()
prompt s = liftIO do
  putStr s
  IO.hFlush IO.stdout

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPlayLandUser :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTLand))
demoPlayLandUser opaque oPlayer = queryMagic opaque do
  player <- internalFromPrivate $ getPlayer oPlayer
  let Hand zos = playerHand player
  case zos of
    [] -> pure Nothing
    _ -> do
      M.lift $ printGameState opaque
      mZoZo0 <- liftIO $ untilJust \uj -> do
        case uj of
          FirstTry -> pure ()
          Retried -> putStrLn "Invalid ID. Retrying..."
        prompt "PlayLand: "
        text <- getLine
        case readMaybe @Int text of
          Nothing -> pure Nothing
          Just (-1) -> pure $ Just Nothing
          Just i -> do
            let zo0 = toZO0 @ 'ZHand $ ObjectId i
                zo = zo0ToCard zo0
            case zo `elem` zos of
              False -> pure Nothing
              True -> pure $ Just $ Just (zo, zo0)
      case mZoZo0 of
        Nothing -> pure Nothing
        Just (zo, zo0) -> do
          liftIO $ print ("playing land", zo)
          pure $ Just $ PlayLand $ toZO1 zo0

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPlayLandRandom :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTLand))
demoPlayLandRandom opaque oPlayer = queryMagic opaque do
  player <- internalFromPrivate $ getPlayer oPlayer
  let Hand zos = playerHand player
  case zos of
    [] -> pure Nothing
    _ -> do
      idx <- randomRIO (-1, length zos - 1)
      case idx of
        -1 -> pure Nothing
        _ -> do
          let zo = zos !! idx
              zo0 = toZO0 @ 'ZHand zo
          liftIO $ print ("playing land", zo)
          pure $ Just $ PlayLand $ toZO1 zo0

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoCastSpellUser :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTSpell))
demoCastSpellUser opaque oPlayer = queryMagic opaque do
  player <- internalFromPrivate $ getPlayer oPlayer
  let noMana = playerMana player == mempty
      Hand zos = playerHand player
  case (noMana, zos) of
    (True, _) -> pure Nothing
    (_, []) -> pure Nothing
    _ -> do
      M.lift $ printGameState opaque
      prompt "CastSpell: "
      text <- liftIO getLine
      case readMaybe @Int text of
        Nothing -> pure Nothing
        Just i -> do
          let zo0 = toZO0 @ 'ZHand $ ObjectId i
              zo = zo0ToCard zo0
          case zo `elem` zos of
            False -> pure Nothing
            True -> do
              liftIO $ print ("casting spell", zo)
              pause
              pure $ Just $ CastSpell $ ZO SZHand $ promoteIdToObjectN $ getObjectId zo0

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoCastSpellRandom :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTSpell))
demoCastSpellRandom opaque oPlayer = queryMagic opaque do
  player <- internalFromPrivate $ getPlayer oPlayer
  let noMana = playerMana player == mempty
      Hand zos = playerHand player
  case (noMana, zos) of
    (True, _) -> pure Nothing
    (_, []) -> pure Nothing
    _ -> do
      idx <- randomRIO (-1, length zos - 1)
      let zo = zos !! idx
          zo0 = toZO0 @ 'ZHand zo
      case idx of
        -1 -> pure Nothing
        _ -> do
          liftIO $ print ("casting spell", zo)
          pause
          pure $ Just $ CastSpell $ ZO SZHand $ promoteIdToObjectN $ getObjectId zo0

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoActivateAbilityUser :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTActivatedAbility))
demoActivateAbilityUser opaque oPlayer = queryMagic opaque do
  allAbilities <- internalFromPrivate (allZOActivatedAbilities @ 'ZBattlefield @OTPermanent)
  abilities <- internalFromPrivate $ flip M.filterM allAbilities \ability -> do
    let zo = someActivatedZO ability
    controller <- controllerOf zo
    notTapped <- satisfies zo $ Not isTapped -- XXX: Non-general temp hack specifically for Mountain
    liftIO $ print (getObjectId oPlayer, getObjectId controller, notTapped)
    pure $ controller == oPlayer && notTapped
  liftIO $ print (getObjectId oPlayer, "len all abils", length allAbilities)
  liftIO $ print (getObjectId oPlayer, "len controlled abils", length abilities)
  case abilities of
    [] -> pure Nothing
    _ -> do
      M.lift $ printGameState opaque
      prompt "ActivateAbility: "
      text <- liftIO getLine
      case readMaybe @Int text of
        Nothing -> pure Nothing
        Just i -> do
          let zo0 = toZO0 $ ObjectId i
              zo = zo0ToPermanent zo0
              isZO = (zo ==) . someActivatedZO
          case List.find isZO abilities of
            Nothing -> pure Nothing
            Just ability -> do
              liftIO $ print ("activating ability", someActivatedZO ability)
              pure $ Just $ ActivateAbility ability

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoActivateAbilityRandom :: OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTActivatedAbility))
demoActivateAbilityRandom opaque oPlayer = queryMagic opaque do
  allAbilities <- internalFromPrivate (allZOActivatedAbilities @ 'ZBattlefield @OTPermanent)
  abilities <- internalFromPrivate $ flip M.filterM allAbilities \ability -> do
    let zo = someActivatedZO ability
    controller <- controllerOf zo
    notTapped <- satisfies zo $ Not isTapped
    liftIO $ print (getObjectId oPlayer, getObjectId controller, notTapped)
    pure $ controller == oPlayer && notTapped
  liftIO $ print (getObjectId oPlayer, "len all abils", length allAbilities)
  liftIO $ print (getObjectId oPlayer, "len controlled abils", length abilities)
  case abilities of
    [] -> pure Nothing
    _ -> do
      pause
      idx <- randomRIO (-1, length abilities - 1)
      case idx of
        -1 -> pure Nothing
        _ -> do
          let ability = abilities !! idx
          liftIO $ print ("activating ability", someActivatedZO ability)
          pure $ Just $ ActivateAbility ability

demoLogCallPush :: OpaqueGameState Demo -> CallFrameInfo -> Demo ()
demoLogCallPush opaque frame = case name == show 'queryMagic of
  True -> State.modify' \st -> st{demo_logDisabled = demo_logDisabled st + 1}
  False ->
    State.gets demo_logDisabled >>= \case
      0 -> case Set.member name logIgnore of
        True -> pure ()
        False -> do
          let indent = replicate (tabWidth * i) ' '
          liftIO $ putStrLn $ indent ++ "+" ++ name ++ ": " ++ show i
          case Set.member name logDetailed of
            False -> pure ()
            True -> printGameState opaque
      _ -> pure ()
 where
  name = callFrameName frame
  i = callFrameId frame

demoLogCallPop :: OpaqueGameState Demo -> CallFrameInfo -> Demo ()
demoLogCallPop _opaque frame = case name == show 'queryMagic of
  True -> State.modify' \st -> assert (demo_logDisabled st > 0) st{demo_logDisabled = demo_logDisabled st - 1}
  False ->
    State.gets demo_logDisabled >>= \case
      0 -> case Set.member name logIgnore of
        True -> pure ()
        False -> do
          let indent = replicate (tabWidth * i) ' '
          liftIO $ putStrLn $ indent ++ "-" ++ name ++ ": " ++ show i
      _ -> pure ()
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
printGameState opaque = queryMagic opaque case dumpEverything of
  True -> do
    st <- internalFromPrivate get
    liftIO $ print st
  False -> do
    liftIO do
      horizLine
      print "GAME STATE BEGIN"
    oPlayers <- allPlayers
    eachLogged_ oPlayers \oPlayer -> do
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
    pure () -- print priority
    pure () -- print stack
    liftIO do
      print "GAME STATE END"
      horizLine
      pause
 where
  dumpEverything = True

printHand :: Hand -> Magic 'Public 'RO Demo ()
printHand (Hand zos) = do
  names <- T.for zos getHandCardName
  liftIO $ print ("hand", names)

getHandCardName :: ZO 'ZHand OTCard -> Magic 'Public 'RO Demo String
getHandCardName zo = do
  let zo0 = toZO0 zo
  handCards <- internalFromPrivate $ gets magicHandCards
  case Map.lookup zo0 handCards of
    Nothing -> pure $ "IllegalHandCard@" ++ show zo
    Just (AnyCard card) -> case card of
      Card (CardName name) _ -> pure $ name ++ "@" ++ show zo

logDetailed :: Set.Set String
logDetailed =
  Set.fromList
    [ ""
    , "MtgPure.Engine.Turn.cleanupStep"
    , "MtgPure.Engine.Turn.precombatMainPhase"
    ]

logIgnore :: Set.Set String
logIgnore =
  Set.fromList
    [ ""
    , "MtgPure.Engine.Core.findHandCard"
    , "MtgPure.Engine.Core.findLibraryCard"
    , "MtgPure.Engine.Core.findPlayer"
    , "MtgPure.Engine.Core.getActivePlayer"
    , "MtgPure.Engine.Core.getAlivePlayerCount"
    , "MtgPure.Engine.Core.getAPNAP"
    , "MtgPure.Engine.Core.getPlayer"
    , "MtgPure.Engine.Core.getPlayers"
    , "MtgPure.Engine.Core.newObjectId"
    , "MtgPure.Engine.Core.pushHandCard"
    , "MtgPure.Engine.Core.pushLibraryCard"
    , "MtgPure.Engine.Core.removeHandCard"
    , "MtgPure.Engine.Core.removeLibraryCard"
    , "MtgPure.Engine.Core.setPlayer"
    ]

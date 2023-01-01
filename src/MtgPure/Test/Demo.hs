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

module MtgPure.Test.Demo (
  runDemo,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadIO (..))
import safe qualified Control.Monad.Trans as M
import safe qualified Control.Monad.Trans.State.Strict as State
import safe Control.Monad.Util (Attempt, Attempt' (..))
import safe Data.Char (isSpace)
import safe Data.Functor ((<&>))
import safe Data.List (stripPrefix)
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import safe MtgPure.Engine.Fwd.Api (
  allPlayers,
  eachLogged_,
  getPlayer,
  indexToActivated,
 )
import safe MtgPure.Engine.Monad (get, gets, internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex (AbsoluteActivatedAbilityIndex),
  CallFrameInfo (..),
  CardCount (..),
  CardIndex (..),
  InternalLogicError (CorruptCallStackLogging),
  PlayerIndex (PlayerIndex),
  PriorityAction (..),
  Prompt' (..),
  RelativeAbilityIndex (RelativeAbilityIndex),
  SomeActivatedAbility (..),
  SpecialAction (..),
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
import safe MtgPure.Model.Object.OTKind (
  OTAny,
  OTCard,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (ObjectId (ObjectId), getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  AnyCard (..),
  Card (..),
 )
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, toZO1, zo0ToSpell)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)
import safe qualified System.IO as IO
import safe Text.Read (readMaybe)

data DemoState = DemoState
  { demo_ :: ()
  , demo_logDisabled :: Int
  , demo_replayInputs :: [String]
  , demo_replayLog :: Maybe FilePath
  }

-- TODO: rename this to Console and move outside of Test and into Client
type Demo = State.StateT DemoState IO

runDemo :: Maybe FilePath -> [String] -> [(Deck, Sideboard)] -> IO ()
runDemo replayLog replayInputs decks = do
  case replayLog of
    Nothing -> pure ()
    Just file -> appendFile file "---------------------\n"
  (result, st') <- State.runStateT action st
  case demo_logDisabled st' of
    0 -> pure ()
    _ -> error $ show CorruptCallStackLogging
  print result
 where
  action = playGame $ gameInput decks
  st =
    DemoState
      { demo_ = ()
      , demo_logDisabled = 0
      , demo_replayInputs = replayInputs
      , demo_replayLog = replayLog
      }

gameInput :: [(Deck, Sideboard)] -> GameInput Demo
gameInput decks =
  GameInput
    { gameInput_decks = decks
    , gameInput_gameFormat = Vintage
    , gameInput_mulligan = DisableMulligan
    , gameInput_prompt =
        Prompt
          { exceptionCantBeginGameWithoutPlayers = liftIO $ putStrLn "exceptionCantBeginGameWithoutPlayers"
          , exceptionInvalidCastSpell = \_ _ _ -> liftIO $ putStrLn "exceptionInvalidCastSpell"
          , exceptionInvalidPlayLand = \_ player msg -> liftIO $ print (player, msg)
          , exceptionInvalidShuffle = \_ _ -> liftIO $ putStrLn "exceptionInvalidShuffle"
          , exceptionInvalidStartingPlayer = \_ _ -> liftIO $ putStrLn "exceptionInvalidStartingPlayer"
          , exceptionZoneObjectDoesNotExist = \zo -> liftIO $ print ("exceptionZoneObjectDoesNotExist", zo)
          , promptDebugMessage = \msg -> liftIO $ putStrLn $ "DEBUG: " ++ msg
          , promptGetStartingPlayer = \_attempt _count -> pure $ PlayerIndex 0
          , promptLogCallPop = demoLogCallPop
          , promptLogCallPush = demoLogCallPush
          , promptPerformMulligan = \_attempt _p _hand -> pure False
          , promptPickZO = demoPickZo
          , promptPriorityAction = demoPriorityAction
          , promptShuffle = \_attempt (CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

tabWidth :: Int
tabWidth = 2

pause :: MonadIO m => m ()
pause = M.void $ liftIO getLine

prompt :: String -> Demo String
prompt msg = do
  liftIO do
    putStr msg
    IO.hFlush IO.stdout
  State.gets demo_replayInputs >>= \case
    [] -> liftIO getLine
    s : ss -> do
      State.modify' \st' -> st'{demo_replayInputs = ss}
      liftIO $ putStrLn s
      pure s

demoPickZo ::
  (IsZO zone ot, Monad m) =>
  Attempt ->
  OpaqueGameState m ->
  Object 'OTPlayer ->
  NonEmpty (ZO zone ot) ->
  Demo (ZO zone ot)
demoPickZo _attempt _opaque _p zos = case zos of
  zo :| _ -> do
    liftIO $ print ("picked", zo, "from", NonEmpty.toList zos)
    pure zo

newtype CommandInput = CommandInput [Int]

instance Show CommandInput where
  show (CommandInput raw) = case raw of
    [0] -> "Pass"
    1 : xs -> "ActivateAbility " ++ go xs
    2 : xs -> "CastSpell " ++ go xs
    3 : xs -> "PlayLand " ++ go xs
    xs -> go xs
   where
    go = unwords . map show

instance Read CommandInput where
  readsPrec _ = readsCommandInput

readsCommandInput :: String -> [(CommandInput, String)]
readsCommandInput = readsCommandInput' True . map dotToSpace
 where
  dotToSpace = \case
    '.' -> ' '
    c -> c

readsCommandInput' :: Bool -> String -> [(CommandInput, String)]
readsCommandInput' isHead s0 = case reads' s0 of
  [(x, s1)] -> case span isSpace s1 of
    (_, "") -> [(CommandInput [x], "")]
    ("", _) -> []
    (_, s2) -> prepend x $ readsCommandInput' False s2
  _ -> []
 where
  prepend x res = res <&> \(CommandInput xs, s) -> (CommandInput $ x : xs, s)
  reads' s = case isHead of
    False -> reads s
    True -> case dropWhile isSpace s of
      (stripPrefix "Pass" -> Just s') -> namedRead 0 s'
      (stripPrefix "ActivateAbility " -> Just s') -> namedRead 1 s'
      (stripPrefix "CastSpell " -> Just s') -> namedRead 2 s'
      (stripPrefix "PlayLand " -> Just s') -> namedRead 3 s'
      _ -> reads s
  namedRead n s = [(n, ' ' : s)]

parseCommandInput :: Monad m => CommandInput -> Magic 'Public 'RO m (PriorityAction ())
parseCommandInput (CommandInput raw) = case raw of
  [0] -> pure PassPriority
  [1, objId, abilityIndex] -> do
    let index = AbsoluteActivatedAbilityIndex (ObjectId objId) $ RelativeAbilityIndex abilityIndex
    -- XXX: This can be generalized by scanning across various zones.(with OTAny each time).
    mAbility <- internalFromPrivate $ indexToActivated index
    pure case mAbility of
      Nothing -> AskPriorityActionAgain
      Just ability -> PriorityAction $ ActivateAbility (ability :: SomeActivatedAbility 'ZBattlefield OTAny)
  [2, spellId] -> do
    let zo0 = toZO0 @ 'ZHand $ ObjectId spellId -- XXX: Use `getZoneOf` + `singZone` to generalize.
        zo = zo0ToSpell zo0
    pure $ PriorityAction $ CastSpell zo
  [3, landId] -> do
    let zo0 = toZO0 @ 'ZHand $ ObjectId landId -- XXX: Use `getZoneOf` + `singZone` to generalize.
        zo = toZO1 zo0
    pure $ PriorityAction $ SpecialAction $ PlayLand zo
  _ -> pure AskPriorityActionAgain

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPriorityAction :: Attempt -> OpaqueGameState Demo -> Object 'OTPlayer -> Demo (PriorityAction ())
demoPriorityAction attempt opaque oPlayer = do
  (action, commandInput) <- queryMagic opaque do
    M.lift $ printGameState opaque
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- M.lift $ prompt $ "PriorityAction: " ++ show oPlayer ++ ": "
    let commandInput = case readMaybe text of
          Nothing -> CommandInput []
          Just x -> x
    action <- parseCommandInput commandInput
    pure (action, commandInput)
  State.gets demo_replayLog >>= \case
    Nothing -> pure ()
    Just file -> liftIO $ appendFile file $ show commandInput ++ "\n"
  pure action

demoLogCallPush :: OpaqueGameState Demo -> CallFrameInfo -> Demo ()
demoLogCallPush opaque frame = case name == show 'queryMagic of
  True -> State.modify' \st -> st{demo_logDisabled = demo_logDisabled st + 1}
  False ->
    State.gets demo_logDisabled >>= \case
      0 -> case Map.lookup name logIgnore of
        Just _ -> pure ()
        Nothing -> do
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
      0 -> case Map.lookup name logIgnore of
        Just _ -> pure ()
        _ -> do
          let indent = replicate (tabWidth * i) ' '
          liftIO $ putStrLn $ indent ++ "-" ++ name ++ ": " ++ show i
      _ -> pure ()
 where
  name = callFrameName frame
  i = callFrameId frame

getPlayerName :: Object 'OTPlayer -> String
getPlayerName o = case getObjectId o of
  ObjectId i -> case i of
    1 -> "Alice"
    2 -> "Bob"
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

-- TODO: Make these actually do something
data IgnoreBehavior = IgnoreAll -- IgnoreNested

logIgnore :: Map.Map String IgnoreBehavior
logIgnore =
  Map.fromList
    [ ("", IgnoreAll)
    , ("MtgPure.Engine.Core.findHandCard", IgnoreAll)
    , ("MtgPure.Engine.Core.findLibraryCard", IgnoreAll)
    , ("MtgPure.Engine.Core.findPlayer", IgnoreAll)
    , ("MtgPure.Engine.Core.getActivePlayer", IgnoreAll)
    , ("MtgPure.Engine.Core.getAlivePlayerCount", IgnoreAll)
    , ("MtgPure.Engine.Core.getAPNAP", IgnoreAll)
    , ("MtgPure.Engine.Core.getPlayer", IgnoreAll)
    , ("MtgPure.Engine.Core.getPlayers", IgnoreAll)
    , ("MtgPure.Engine.Core.newObjectId", IgnoreAll)
    , ("MtgPure.Engine.Core.pushHandCard", IgnoreAll)
    , ("MtgPure.Engine.Core.pushLibraryCard", IgnoreAll)
    , ("MtgPure.Engine.Core.removeHandCard", IgnoreAll)
    , ("MtgPure.Engine.Core.removeLibraryCard", IgnoreAll)
    , ("MtgPure.Engine.Core.setPlayer", IgnoreAll)
    ]

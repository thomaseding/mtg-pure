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
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import safe MtgPure.Engine.Fwd.Api (
  allPlayers,
  allZOActivatedAbilities,
  controllerOf,
  eachLogged_,
  getPlayer,
  indexToActivated,
  satisfies,
 )
import safe MtgPure.Engine.Monad (get, gets, internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex (AbsoluteActivatedAbilityIndex),
  CallFrameInfo (..),
  CardCount (..),
  CardIndex (..),
  InternalLogicError (CorruptCallStackLogging),
  Play (..),
  PlayerIndex (PlayerIndex),
  Prompt' (..),
  RelativeAbilityIndex (RelativeAbilityIndex),
  SomeActivatedAbility (..),
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
  OTActivatedAbility,
  OTAny,
  OTCard,
  OTLand,
  OTPermanent,
  OTPlayer,
  OTSpell,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (GetObjectId, ObjectId (ObjectId), getObjectId)
import safe MtgPure.Model.Object.ObjectN (ObjectN)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Object.VisitObjectN (VisitObjectN (promoteIdToObjectN))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  AnyCard (..),
  Card (..),
  Cost (AndCosts),
  Elect (Effect, ElectActivated),
  Requirement (..),
  WithThisActivated,
 )
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Zone (IsZone, SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, toZO1, zo0ToAny, zo0ToSpell)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO, ZoneObject (..))
import safe MtgPure.ModelCombinators (AsWithThis (thisObject), isTapped)
import safe qualified System.IO as IO
import safe System.Random (randomRIO)
import safe Text.Read (readMaybe)

data DemoState = DemoState
  { demo_ :: ()
  , demo_logDisabled :: !Int
  , demo_replayInputs :: ![String]
  }

type Demo = State.StateT DemoState IO

runDemo :: [String] -> [(Deck, Sideboard)] -> IO ()
runDemo replayInputs decks = do
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
          , promptActivateAbility = if True then demoActivateAbilityUser else demoActivateAbilityRandom
          , promptCastSpell = if True then demoCastSpellUser else demoCastSpellRandom
          , promptDebugMessage = \msg -> liftIO $ putStrLn $ "DEBUG: " ++ msg
          , promptGetStartingPlayer = \_attempt _count -> pure $ PlayerIndex 0
          , promptLogCallPop = demoLogCallPop
          , promptLogCallPush = demoLogCallPush
          , promptPerformMulligan = \_attempt _p _hand -> pure False
          , promptPickZO = demoPickZo
          , promptPlayLand = if True then demoPlayLandUser else demoPlayLandRandom
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
      State.modify' \st -> st{demo_replayInputs = ss}
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

data ReadActivated = ReadActivated Int Int

instance Read ReadActivated where
  readsPrec _ s = case reads s of
    [(0, rest)] -> [(ReadActivated 0 0, rest)]
    [(i, ' ' : rest)] -> case reads $ dropWhile isSpace rest of
      [(j, rest')] -> [(ReadActivated i j, rest')]
      _ -> []
    _ -> []

data PlayK ot where
  PlayLandK :: PlayK OTLand
  CastSpellK :: PlayK OTSpell

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPlayThingUser ::
  forall ot.
  GetObjectId (ObjectN ot) =>
  PlayK ot ->
  Attempt ->
  OpaqueGameState Demo ->
  Object 'OTPlayer ->
  Demo (Maybe (Play ot))
demoPlayThingUser k attempt opaque oPlayer = queryMagic opaque do
  M.lift $ printGameState opaque
  liftIO case attempt of
    Attempt 0 -> pure ()
    Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
  zo <- do
    let s = case k of
          PlayLandK -> "PlayLand"
          CastSpellK -> "CastSpell"
    text <- M.lift $ prompt $ s ++ " " ++ show oPlayer ++ ": "
    let i = case readMaybe @Int text of
          Nothing -> -1
          Just x -> x
    let zo0 = toZO0 @ 'ZHand $ ObjectId i -- XXX: Use `getZoneOf` + `singZone` to generalize.
        zo :: ZO 'ZHand ot
        zo = case k of
          PlayLandK -> toZO1 zo0
          CastSpellK -> zo0ToSpell zo0
    pure zo
  case getObjectId zo of
    ObjectId 0 -> pure Nothing
    _ -> do
      let playing = case k of
            PlayLandK -> "playing land: "
            CastSpellK -> "casting spell: "
          play = case k of
            PlayLandK -> PlayLand
            CastSpellK -> CastSpell
      liftIO $ print (playing, zo)
      pure $ Just $ play zo

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPlayLandUser :: Attempt -> OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTLand))
demoPlayLandUser = demoPlayThingUser PlayLandK

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoCastSpellUser :: Attempt -> OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTSpell))
demoCastSpellUser = demoPlayThingUser CastSpellK

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoActivateAbilityUser :: Attempt -> OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTActivatedAbility))
demoActivateAbilityUser attempt opaque oPlayer = queryMagic opaque do
  M.lift $ printGameState opaque
  liftIO case attempt of
    Attempt 0 -> pure ()
    Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
  index <- do
    text <- M.lift $ prompt $ "ActivateAbility " ++ show oPlayer ++ ": "
    let ReadActivated i j = case readMaybe @ReadActivated text of
          Nothing -> ReadActivated (-1) 0
          Just x -> x
    pure $ AbsoluteActivatedAbilityIndex (ObjectId i) $ RelativeAbilityIndex j
  case getObjectId index of
    ObjectId 0 -> pure Nothing
    _ -> do
      mAbility <- internalFromPrivate $ indexToActivated index
      let ability = case mAbility of
            Nothing -> SomeActivatedAbility dummyZo dummyActivatedAbility
            Just x -> x
          zo :: ZO 'ZBattlefield OTAny -- XXX: Use `getZoneOf` + `singZone` to generalize.
          zo = someActivatedZO ability
      liftIO $ print ("activating ability", zo)
      pure $ Just $ ActivateAbility ability

dummyZo :: IsZone zone => ZO zone OTAny
dummyZo = zo0ToAny $ toZO0 $ ObjectId (-1)

dummyActivatedAbility :: WithThisActivated 'ZBattlefield OTPlayer
dummyActivatedAbility = thisObject \_this ->
  ElectActivated $
    Ability
      { activated_cost = AndCosts []
      , activated_effect = Effect []
      }

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
demoPlayLandRandom :: Attempt -> OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTLand))
demoPlayLandRandom _attempt opaque oPlayer = queryMagic opaque do
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
demoCastSpellRandom :: Attempt -> OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTSpell))
demoCastSpellRandom _attempt opaque oPlayer = queryMagic opaque do
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
demoActivateAbilityRandom :: Attempt -> OpaqueGameState Demo -> Object 'OTPlayer -> Demo (Maybe (Play OTActivatedAbility))
demoActivateAbilityRandom _attempt opaque oPlayer = queryMagic opaque do
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

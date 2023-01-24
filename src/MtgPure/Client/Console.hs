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
{-# HLINT ignore "Use fromRight" #-}

module MtgPure.Client.Console (
  ConsoleInput (..),
  Console,
  runConsole,
  playConsoleGame,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.State.Strict as State
import safe Control.Monad.Trans (MonadIO (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import safe Control.Monad.Util (Attempt, Attempt' (..))
import safe qualified Data.Char as Char
import safe Data.Kind (Type)
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe Data.Monoid (First (..))
import safe Data.Read.Sep (Tuple (..), TupleList (..))
import safe Data.Read.Symbol (CI, CS, Many1, Or)
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import safe MtgPure.Engine.Fwd.Api (
  eachLogged_,
  getAlivePlayers,
  getBasicLandTypes,
  getPlayer,
  indexToActivated,
  toZO,
 )
import safe MtgPure.Engine.Monad (CallFrameInfo (..), get, gets, internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex (AbsoluteActivatedAbilityIndex),
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
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.CardName (CardName (..), HasCardName (..))
import safe MtgPure.Model.Combinators (basicManaAbility)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Mana.ManaCost (DynamicManaCost (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool, ManaPayment (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ToManaPool (toCompleteManaPool)
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNCard,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (ObjectId (..), getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.PhaseStep (prettyPhaseStep)
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Variable (Var (..))
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, toZO1, zo0ToSpell)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)
import safe qualified System.IO as IO
import safe Text.Read (readMaybe)

data Quit = Quit

data ConsoleInput = ConsoleInput
  { consoleInput_ :: ()
  , consoleInput_replayInputs :: [String]
  , consoleInput_replayLog :: Maybe FilePath
  }

data ConsoleState = ConsoleState
  { console_ :: ()
  , console_logDisabled :: Int
  , console_replayInputs :: [String]
  , console_replayLog :: Maybe FilePath
  }

newtype Console a = Console
  { unConsole :: ExceptT Quit (State.StateT ConsoleState IO) a
  }
  deriving (Functor)

instance Applicative Console where
  pure = Console . pure
  Console f <*> Console a = Console $ f <*> a

instance Monad Console where
  Console a >>= f = Console $ a >>= unConsole . f

instance MonadIO Console where
  liftIO = Console . liftIO

instance State.MonadState ConsoleState Console where
  get = Console State.get
  put = Console . State.put

runConsole :: ConsoleInput -> Console () -> IO ()
runConsole input m = either (const ()) id <$> runConsole' input m

runConsole' :: ConsoleInput -> Console a -> IO (Either Quit a)
runConsole' input action = do
  State.runStateT (runExceptT (unConsole action)) st >>= \case
    (Left Quit, _) -> pure $ Left Quit
    (Right result, st') -> case console_logDisabled st' of
      0 -> pure $ Right result
      _ -> error $ show CorruptCallStackLogging
 where
  st =
    ConsoleState
      { console_ = ()
      , console_logDisabled = 0
      , console_replayInputs = consoleInput_replayInputs input
      , console_replayLog = consoleInput_replayLog input
      }

playConsoleGame :: [(Deck, Sideboard)] -> Console ()
playConsoleGame decks = do
  Console (State.gets console_replayLog) >>= \case
    Nothing -> pure ()
    Just file -> liftIO $ appendFile file "---------------------\n"
  result <- playGame $ gameInput decks
  liftIO $ print result

gameInput :: [(Deck, Sideboard)] -> GameInput Console
gameInput decks =
  GameInput
    { gameInput_decks = decks
    , gameInput_gameFormat = Vintage
    , gameInput_mulligan = DisableMulligan
    , gameInput_prompt =
        Prompt
          { exceptionCantBeginGameWithoutPlayers = liftIO $ putStrLn "exceptionCantBeginGameWithoutPlayers"
          , exceptionInvalidCastSpell = \_ _ _ -> liftIO $ putStrLn "exceptionInvalidCastSpell"
          , exceptionInvalidGenericManaPayment = \_ _ -> liftIO $ putStrLn "exceptionInvalidGenericManaPayment"
          , exceptionInvalidPlayLand = \_ player msg -> liftIO $ print (player, msg)
          , exceptionInvalidShuffle = \_ _ -> liftIO $ putStrLn "exceptionInvalidShuffle"
          , exceptionInvalidStartingPlayer = \_ _ -> liftIO $ putStrLn "exceptionInvalidStartingPlayer"
          , exceptionZoneObjectDoesNotExist = \zo -> liftIO $ print ("exceptionZoneObjectDoesNotExist", zo)
          , promptChooseAttackers = \_ _ _ -> pure []
          , promptChooseBlockers = \_ _ _ _ -> pure []
          , promptDebugMessage = \msg -> liftIO $ putStrLn $ "DEBUG: " ++ msg
          , promptGetStartingPlayer = \_attempt _count -> pure $ PlayerIndex 0
          , promptLogCallPop = consoleLogCallPop
          , promptLogCallPush = consoleLogCallPush
          , promptPayDynamicMana = consolePromptPayDynamicMana
          , promptPerformMulligan = \_attempt _p _hand -> pure False
          , promptPickZO = consolePickZo
          , promptPriorityAction = consolePriorityAction
          , promptShuffle = \_attempt (CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

tabWidth :: Int
tabWidth = 2

pause :: MonadIO m => m ()
pause = M.void $ liftIO getLine

prompt :: String -> Console String
prompt msg = do
  liftIO do
    putStr msg
    IO.hFlush IO.stdout
  Console (State.gets console_replayInputs) >>= \case
    [] -> liftIO getLine
    s : ss -> do
      Console $ State.modify' \st' -> st'{console_replayInputs = ss}
      liftIO $ putStrLn s
      pure s

consolePickZo ::
  IsZO zone ot =>
  Attempt ->
  OpaqueGameState Console ->
  Object 'OTPlayer ->
  NonEmpty (ZO zone ot) ->
  Magic 'Public 'RO Console (ZO zone ot)
consolePickZo _attempt _opaque _p zos = case zos of
  zo :| _ -> do
    liftIO $ print ("picked", zo, "from", NonEmpty.toList zos)
    pure zo

data CommandAbilityIndex :: Type where
  CIAbilityIndex :: Int -> CommandAbilityIndex
  CIImplicitAbility :: BasicLandType -> CommandAbilityIndex
  CIInferImplicitAbility :: CommandAbilityIndex

instance Show CommandAbilityIndex where
  show = \case
    CIAbilityIndex n -> show n
    CIImplicitAbility landType -> case landType of
      Plains -> "W"
      Island -> "U"
      Swamp -> "B"
      Mountain -> "R"
      Forest -> "G"
    CIInferImplicitAbility -> "*"

type AsW = Tuple Sep (Or (CI "W") (CS "-1"), ())

type AsU = Tuple Sep (Or (CI "U") (CS "-2"), ())

type AsB = Tuple Sep (Or (CI "B") (CS "-3"), ())

type AsR = Tuple Sep (Or (CI "R") (CS "-4"), ())

type AsG = Tuple Sep (Or (CI "G") (CS "-5"), ())

type AsI = Tuple Sep (Or (CI "*") (CS "-6"), ())

instance Read CommandAbilityIndex where
  readsPrec _ s = case s of
    (reads @AsW -> [(Tuple1 _, rest)]) -> [(CIImplicitAbility Plains, rest)]
    (reads @AsU -> [(Tuple1 _, rest)]) -> [(CIImplicitAbility Island, rest)]
    (reads @AsB -> [(Tuple1 _, rest)]) -> [(CIImplicitAbility Swamp, rest)]
    (reads @AsR -> [(Tuple1 _, rest)]) -> [(CIImplicitAbility Mountain, rest)]
    (reads @AsG -> [(Tuple1 _, rest)]) -> [(CIImplicitAbility Forest, rest)]
    (reads @AsI -> [(Tuple1 _, rest)]) -> [(CIInferImplicitAbility, rest)]
    _ -> case reads s of
      [(n, rest)] -> case n >= 0 of
        True -> [(CIAbilityIndex n, rest)]
        False -> []
      _ -> []

data CommandInput :: Type where
  CIQuit :: CommandInput
  CIConcede :: CommandInput
  CIAskAgain :: CommandInput
  CIHelp :: Maybe Int -> CommandInput
  CIPass :: CommandInput
  CIActivateAbility :: ObjectId -> CommandAbilityIndex -> [ObjectId] -> CommandInput
  CICastSpell :: ObjectId -> [ObjectId] -> CommandInput
  CIPlayLand :: ObjectId -> [ObjectId] -> CommandInput

instance Show CommandInput where
  show = \case
    CIQuit -> "Quit"
    CIConcede -> "Concede"
    CIAskAgain -> "AskAgain"
    CIHelp Nothing -> "Help"
    CIHelp (Just n) -> "Help " ++ show n
    CIPass -> "Pass"
    CIActivateAbility objectId abilityIndex extras -> "ActivateAbility " ++ showId objectId ++ " " ++ show abilityIndex ++ showExtras extras
    CICastSpell spellId extras -> "CastSpell " ++ showId spellId ++ showExtras extras
    CIPlayLand landId extras -> "PlayLand " ++ showId landId ++ showExtras extras
   where
    showId = show . unObjectId
    showExtras = \case
      [] -> ""
      xs -> " " ++ unwords (map showId xs)

type Sep = Many1 (CS " ")

type AsQuit = Tuple Sep (CI "quit", ())

type AsConcede = Tuple Sep (CI "concede", ())

type AsHelp0 = Tuple Sep (Or (CI "help") (CS "?"), ())

type AsHelp1 = Tuple Sep (Or (CI "help") (CS "?"), Int, ())

type AsPass = Tuple Sep (Or (CI "pass") (CS "0"), ())

type AsActivateAbility = TupleList Sep (Or (CI "activateAbility") (CS "1"), Int, CommandAbilityIndex, ()) Int

type AsCastSpell = TupleList Sep (Or (CI "castSpell") (CS "2"), Int, ()) Int

type AsPlayLand = TupleList Sep (Or (CI "playLand") (CS "3"), Int, ()) Int

instance Read CommandInput where
  readsPrec _ str = case massageString str of
    (reads @AsQuit -> [(Tuple1 _, rest)]) -> [(CIQuit, rest)]
    (reads @AsConcede -> [(Tuple1 _, rest)]) -> [(CIConcede, rest)]
    (reads @AsHelp0 -> [(Tuple1 _, rest)]) -> [(CIHelp Nothing, rest)]
    (reads @AsHelp1 -> [(Tuple2 _ n, rest)]) -> [(CIHelp $ Just n, rest)]
    (reads @AsPass -> [(Tuple1 _, rest)]) -> [(CIPass, rest)]
    (reads @AsActivateAbility -> [(TupleList3 _ objectId abilityIndex extras, rest)]) ->
      [(CIActivateAbility (ObjectId objectId) abilityIndex $ map ObjectId extras, rest)]
    (reads @AsCastSpell -> [(TupleList2 _ spellId extras, rest)]) ->
      [(CICastSpell (ObjectId spellId) $ map ObjectId extras, rest)]
    (reads @AsPlayLand -> [(TupleList2 _ landId extras, rest)]) ->
      [(CIPlayLand (ObjectId landId) $ map ObjectId extras, rest)]
    _ -> []
   where
    massageString = map massageChar . takeWhile (not . isComment)
    massageChar = \case
      '.' -> ' '
      c -> Char.toLower c
    isComment = \case
      ';' -> True
      _ -> False

parseCommandInput :: CommandInput -> Magic 'Public 'RO Console (PriorityAction ())
parseCommandInput = \case
  CIQuit -> quit
  CIConcede -> concede
  CIPass -> passPriority
  CIActivateAbility objId abilityIndex extraIds -> activateAbility objId abilityIndex extraIds
  CICastSpell spellId extraIds -> castSpell spellId extraIds
  CIPlayLand landId extraIds -> playLand landId extraIds
  CIAskAgain -> askAgain
  CIHelp _ -> help

askAgain :: Magic 'Public 'RO Console (PriorityAction ())
askAgain = pure $ AskPriorityActionAgain Nothing

help :: Magic 'Public 'RO Console (PriorityAction ())
help = error "Help is not implemented yet"

quit :: Magic 'Public 'RO Console (PriorityAction ())
quit = M.lift $ Console $ throwE Quit

concede :: Magic 'Public 'RO Console (PriorityAction ())
concede = pure Concede

passPriority :: Magic 'Public 'RO Console (PriorityAction ())
passPriority = pure PassPriority

activateAbility :: ObjectId -> CommandAbilityIndex -> [ObjectId] -> Magic 'Public 'RO Console (PriorityAction ())
activateAbility objId abilityIndex _extraIds = do
  mZo <- internalFromPrivate $ toZO @ 'ZBattlefield @OTNAny objId
  case mZo of
    Nothing -> tryAgain
    Just zo -> case abilityIndex of
      CIAbilityIndex relIndex -> do
        let index = AbsoluteActivatedAbilityIndex objId $ RelativeAbilityIndex relIndex
        mAction <-
          mconcat . map First
            <$> sequence
              [ goIndex @ 'ZBattlefield index
              -- , goIndex @ 'ZExile index
              -- , goIndex @ 'ZGraveyard index
              -- , goIndex @ 'ZHand index
              -- , goIndex @ 'ZLibrary index
              -- , goIndex @ 'ZStack index
              ]
        case getFirst mAction of
          Just action -> pure action
          Nothing -> tryAgain
      CIImplicitAbility ty -> do
        pure $ PriorityAction $ ActivateAbility $ SomeActivatedAbility zo $ basicManaAbility ty
      CIInferImplicitAbility -> do
        tys <- internalFromPrivate $ getBasicLandTypes zo
        case tys of
          [ty] -> pure $ PriorityAction $ ActivateAbility $ SomeActivatedAbility zo $ basicManaAbility ty
          _ -> tryAgain
 where
  tryAgain :: Magic 'Public 'RO Console (PriorityAction ())
  tryAgain = pure $ AskPriorityActionAgain Nothing

  goIndex :: forall zone. IsZone zone => AbsoluteActivatedAbilityIndex -> Magic 'Public 'RO Console (Maybe (PriorityAction ()))
  goIndex index = do
    mAbility <- internalFromPrivate $ indexToActivated index
    case mAbility of
      Just ability -> pure $ Just $ PriorityAction $ ActivateAbility (ability :: SomeActivatedAbility zone OTNAny)
      Nothing -> pure Nothing

castSpell :: ObjectId -> [ObjectId] -> Magic 'Public 'RO Console (PriorityAction ())
castSpell spellId _extraIds = do
  let zo0 = toZO0 @ 'ZHand spellId
      zo = zo0ToSpell zo0
  pure $ PriorityAction $ CastSpell zo

playLand :: ObjectId -> [ObjectId] -> Magic 'Public 'RO Console (PriorityAction ())
playLand landId _extraIds = do
  let zo0 = toZO0 @ 'ZHand landId
      zo = toZO1 zo0
  pure $ PriorityAction $ SpecialAction $ PlayLand zo

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
consolePriorityAction ::
  Attempt ->
  OpaqueGameState Console ->
  Object 'OTPlayer ->
  Magic 'Public 'RO Console (PriorityAction ())
consolePriorityAction attempt opaque oPlayer = M.lift do
  (action, commandInput) <- queryMagic opaque do
    M.lift $ printGameState opaque
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    M.liftIO do
      putStrLn ""
      putStrLn "---- LEGEND ----"
      putStrLn ""
      putStrLn "<abilityIndex> is the 0-based index of the ability in the list of abilities of an object."
      putStrLn "The <abilityIndex> of implicit mana abilities are -1 to -6, respectively: W, U, B, R, G, *"
      putStrLn "  where * infers the mana ability when it is unambiguous."
      putStrLn ""
      putStrLn " quit"
      putStrLn " help"
      putStrLn " pass"
      putStrLn " activateAbility <objectId> <abilityIndex>"
      putStrLn " castSpell <spellId>"
      putStrLn " playLand <landId>"
      putStrLn ""
    text <- M.lift $ prompt $ "PriorityAction: " ++ show oPlayer ++ ": "
    let commandInput = case readMaybe text of
          Nothing -> CIAskAgain
          Just x -> x
    action <- parseCommandInput commandInput
    pure (action, commandInput)
  Console (State.gets console_replayLog) >>= \case
    Nothing -> pure ()
    Just file -> do
      info <- getPriorityInfo opaque oPlayer
      liftIO $ appendFile file $ show commandInput ++ " ; " ++ info ++ "\n"
  pure action

getPriorityInfo :: OpaqueGameState Console -> Object 'OTPlayer -> Console String
getPriorityInfo opaque oPlayer = queryMagic opaque do
  phaseStep <- internalFromPrivate $ gets magicPhaseStep
  turnNumber <- internalFromPrivate $ gets magicCurrentTurn
  pure $ show oPlayer ++ " " ++ prettyPhaseStep phaseStep ++ " Turn" ++ show turnNumber

consolePromptPayDynamicMana ::
  Attempt ->
  OpaqueGameState Console ->
  Object 'OTPlayer ->
  DynamicManaCost 'NoVar ->
  Magic 'Public 'RO Console ManaPayment
consolePromptPayDynamicMana attempt opaque oPlayer dyn = M.lift do
  (pool, text) <- queryMagic opaque do
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    let sPlayer = "player=" ++ show (unObjectId $ getObjectId oPlayer)
    text <- M.lift $ prompt $ "PayDynamicMana " ++ sPlayer ++ " cost=" ++ show dyn ++ ": "
    let pool = case parseManaPool text of
          Nothing -> mempty
          Just p -> p
    pure (pool, text)
  let _ = text -- TODO: log the choice
  pure mempty{paymentMana = pool}

parseManaPool :: String -> Maybe CompleteManaPool
parseManaPool = parseManaPool' . map Char.toUpper

parseManaPool' :: String -> Maybe CompleteManaPool
parseManaPool' = \case
  [] -> Just mempty
  'W' : s -> (toCompleteManaPool W <>) <$> parseManaPool' s
  'U' : s -> (toCompleteManaPool U <>) <$> parseManaPool' s
  'B' : s -> (toCompleteManaPool B <>) <$> parseManaPool' s
  'R' : s -> (toCompleteManaPool R <>) <$> parseManaPool' s
  'G' : s -> (toCompleteManaPool G <>) <$> parseManaPool' s
  'C' : s -> (toCompleteManaPool C <>) <$> parseManaPool' s
  _ -> Nothing

consoleLogCallPush :: OpaqueGameState Console -> CallFrameInfo -> Console ()
consoleLogCallPush opaque frame = case name == show 'queryMagic of
  True -> Console $ State.modify' \st -> st{console_logDisabled = console_logDisabled st + 1}
  False ->
    Console (State.gets console_logDisabled) >>= \case
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

consoleLogCallPop :: OpaqueGameState Console -> CallFrameInfo -> Console ()
consoleLogCallPop _opaque frame = case name == show 'queryMagic of
  True -> Console $ State.modify' \st ->
    assert (console_logDisabled st > 0) st{console_logDisabled = console_logDisabled st - 1}
  False ->
    Console (State.gets console_logDisabled) >>= \case
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

horizontalLine :: IO ()
horizontalLine = do
  putStrLn $ replicate 20 '-'

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
printGameState :: OpaqueGameState Console -> Console ()
printGameState opaque = queryMagic opaque case dumpEverything of
  True -> do
    st <- internalFromPrivate get
    liftIO $ print st
  False -> do
    liftIO do
      horizontalLine
      print "GAME STATE BEGIN"
    oPlayers <- getAlivePlayers
    eachLogged_ oPlayers \oPlayer -> do
      let name = getPlayerName oPlayer
      liftIO do
        horizontalLine
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
      horizontalLine
      pause
 where
  dumpEverything = True

printHand :: Hand -> Magic 'Public 'RO Console ()
printHand (Hand zos) = do
  names <- T.for zos getHandCardName
  liftIO $ print ("hand", names)

getHandCardName :: ZO 'ZHand OTNCard -> Magic 'Public 'RO Console String
getHandCardName zo = do
  let zo0 = toZO0 zo
  handCards <- internalFromPrivate $ gets magicHandCards
  case Map.lookup zo0 handCards of
    Nothing -> pure $ "IllegalHandCard@" ++ show zo
    Just anyCard -> do
      let CardName name = getCardName anyCard
      pure $ name ++ "@" ++ show zo

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

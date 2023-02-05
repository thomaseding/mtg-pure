{-# LANGUAGE Safe #-}
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

module MtgPure.Client.Terminal (
  TerminalInput (..),
  Terminal,
  runTerminal,
  playTerminalGame,
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
import safe qualified Data.List as List
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe Data.Monoid (First (..))
import safe Data.Nat (Fin, NatList)
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import safe MtgPure.Client.Terminal.CommandInput (
  CommandAbilityIndex (..),
  CommandInput (..),
  defaultCommandAliases,
  runParseCommandInput,
 )
import safe MtgPure.Engine.Fwd.Api (
  eachLogged_,
  getAlivePlayers,
  getIntrinsicManaAbilities,
  getPlayer,
  getTrivialManaAbilities,
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
import safe MtgPure.Model.CardName (CardName (..), HasCardName (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.IsManaAbility (isTrivialManaAbility)
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Mana.ManaCost (DynamicManaCost (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool, ManaPayment (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ToManaPool (toCompleteManaPool)
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNCard,
  OTNPermanent,
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

data Quit = Quit

data TerminalInput = TerminalInput
  { terminalInput_ :: ()
  , terminalInput_replayInputs :: [String]
  , terminalInput_replayLog :: Maybe FilePath
  }

data TerminalState = TerminalState
  { terminal_ :: ()
  , terminal_logDisabled :: Int
  , terminal_replayInputs :: [String]
  , terminal_replayLog :: Maybe FilePath
  }

newtype Terminal a = Terminal
  { unTerminal :: ExceptT Quit (State.StateT TerminalState IO) a
  }
  deriving (Functor)

instance Applicative Terminal where
  pure = Terminal . pure
  Terminal f <*> Terminal a = Terminal $ f <*> a

instance Monad Terminal where
  Terminal a >>= f = Terminal $ a >>= unTerminal . f

instance MonadIO Terminal where
  liftIO = Terminal . liftIO

instance State.MonadState TerminalState Terminal where
  get = Terminal State.get
  put = Terminal . State.put

runTerminal :: TerminalInput -> Terminal () -> IO ()
runTerminal input m = either (const ()) id <$> runTerminal' input m

runTerminal' :: TerminalInput -> Terminal a -> IO (Either Quit a)
runTerminal' input action = do
  State.runStateT (runExceptT (unTerminal action)) st >>= \case
    (Left Quit, _) -> pure $ Left Quit
    (Right result, st') -> case terminal_logDisabled st' of
      0 -> pure $ Right result
      _ -> error $ show CorruptCallStackLogging
 where
  st =
    TerminalState
      { terminal_ = ()
      , terminal_logDisabled = 0
      , terminal_replayInputs = terminalInput_replayInputs input
      , terminal_replayLog = terminalInput_replayLog input
      }

playTerminalGame :: [(Deck, Sideboard)] -> Terminal ()
playTerminalGame decks = do
  Terminal (State.gets terminal_replayLog) >>= \case
    Nothing -> pure ()
    Just file -> liftIO $ appendFile file "---------------------\n"
  result <- playGame $ gameInput decks
  liftIO $ print result

gameInput :: [(Deck, Sideboard)] -> GameInput Terminal
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
          , promptChooseOption = terminalChooseOption
          , promptDebugMessage = \msg -> liftIO $ putStrLn $ "DEBUG: " ++ msg
          , promptGetStartingPlayer = \_attempt _count -> pure $ PlayerIndex 0
          , promptLogCallPop = terminalLogCallPop
          , promptLogCallPush = terminalLogCallPush
          , promptPayDynamicMana = terminalPromptPayDynamicMana
          , promptPerformMulligan = \_attempt _p _hand -> pure False
          , promptPickZO = terminalPickZo
          , promptPriorityAction = terminalPriorityAction
          , promptShuffle = \_attempt (CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

tabWidth :: Int
tabWidth = 2

pause :: MonadIO m => m ()
pause = M.void $ liftIO getLine

prompt :: String -> Terminal String
prompt msg = do
  liftIO do
    putStr msg
    IO.hFlush IO.stdout
  Terminal (State.gets terminal_replayInputs) >>= \case
    [] -> liftIO getLine
    s : ss -> do
      Terminal $ State.modify' \st' -> st'{terminal_replayInputs = ss}
      liftIO $ putStrLn s
      pure s

terminalPickZo ::
  IsZO zone ot =>
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  NonEmpty (ZO zone ot) ->
  Magic 'Public 'RO Terminal (ZO zone ot)
terminalPickZo _attempt _opaque _p zos = case zos of
  zo :| _ -> do
    liftIO $ print ("picked", zo, "from", NonEmpty.toList zos)
    pure zo

parseCommandInput :: CommandInput -> Magic 'Public 'RO Terminal (PriorityAction ())
parseCommandInput = \case
  CIQuit -> quit
  CIConcede -> concede
  CIPass -> passPriority
  CIActivateAbility objId abilityIndex extraIds -> activateAbility objId abilityIndex extraIds
  CICastSpell spellId extraIds -> castSpell spellId extraIds
  CIPlayLand landId extraIds -> playLand landId extraIds
  CIAskAgain -> askAgain
  CIHelp _ -> help
  CIExamineAbility objId abilityIndex -> undefined objId abilityIndex
  CIExamineObject objId -> undefined objId

askAgain :: Magic 'Public 'RO Terminal (PriorityAction ())
askAgain = pure $ AskPriorityActionAgain Nothing

help :: Magic 'Public 'RO Terminal (PriorityAction ())
help = M.liftIO do
  clearScreen
  putStrLn "Help for REPL Commands:"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  help [<command>] - Display information about the specified command or all commands if no command is provided."
  putStrLn "  quit - Quit the game."
  putStrLn "  examine <object_id> [<ability_index>] - Display detailed information of an object or ability."
  putStrLn "  pass - Pass priority."
  putStrLn "  activateAbility <object_id> <ability_index> <target_id>* - Activate an ability of an object."
  putStrLn "  castSpell <card_id> <target_id>* - Cast a spell."
  putStrLn "  playLand <card_id> - Play a land."
  putStrLn ""
  putStrLn "Commands are case-insensitive and can be entered using dots \".\" in place of spaces."
  putStrLn ""
  putStrLn "Some command have a single character aliases. They are as follows:"
  putStrLn "  ? help"
  putStrLn "  + examine"
  putStrLn "  0 pass"
  putStrLn "  1 activateAbility"
  putStrLn "  2 castSpell"
  putStrLn "  3 playLand"
  putStrLn ""
  putStrLn "IDs:"
  putStrLn "  Positive decimal numbers used to uniquely identify objects. This generalizes an genuine MTG \"object\" (CR 109.1)"
  putStrLn "  to include things like players. These are displayed on screen as `[<id>]` next to their corresponding object."
  putStrLn ""
  putStrLn "Indices:"
  putStrLn "  0-based non-negative decimal numbers used to identify an ability of a card."
  putStrLn "  Specific negative indices with symbol aliases can also be used to activate mana abilities of permanents."
  putStrLn ""
  putStrLn "Mana Ability Indices:"
  putStrLn "  -1 W - Activates the \"T: Add W\" ability of a permanent."
  putStrLn "  -2 U - Activates the \"T: Add U\" ability of a permanent."
  putStrLn "  -3 B - Activates the \"T: Add B\" ability of a permanent."
  putStrLn "  -4 G - Activates the \"T: Add G\" ability of a permanent."
  putStrLn "  -5 G - Activates the \"T: Add G\" ability of a permanent."
  putStrLn "  -6 C - Activates the \"T: Add C\" ability of a permanent."
  putStrLn "  -7 * - Infers one of the above mana abilities when unambiguous."
  putStrLn ""
  putStrLn "\"#\" can be used to comment out the rest of the line. Useful in replay files."
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "> examine 4 # Displays detailed information of the object with ID 4."
  putStrLn "> examine 4 0 # Displays detailed information of the first ability of the object with ID 4."
  putStrLn "> activateAbility 7 * # Activates the unique simple mana ability of the permanent with ID 7."
  putStrLn "> activateAbility 7 -1 # Activates the \"T: Add W\" ability of the permanent with ID 7."
  putStrLn "> activateAbility 7 W # Activates the \"T: Add W\" ability of the permanent with ID 7."
  putStrLn "> activateAbility 7 0 # Activates the first ability of the permanent with ID 7."
  putStrLn "> activateAbility 7 0 8 # Activates the first ability of the permanent with ID 7 targeting the object with ID 8."
  putStrLn "> activateAbility 7 0 8 9 # Activates the first ability of the permanent with ID 7 targeting the objects with IDs 8 and 9."
  putStrLn "> castSpell 3 5 # Casts the spell with ID 3 targeting the object with ID 5."
  putStrLn "> castSpell 3 5 11 # Casts the spell with ID 3 targeting the objects with IDs 5 and 11."
  putStrLn ""
  putStrLn "Commands are designed to be entered efficiently on a numeric keypad. Examples:"
  putStrLn "> +.7 # Displays detailed information of the object with ID 7."
  putStrLn "> +.7.0 # Displays detailed information of the first ability of the object with ID 7."
  putStrLn "> 1.7.* # Activates the unique simple mana ability of the permanent with ID 7."
  putStrLn "> 1.7.-1 # Activates the \"T: Add W\" ability of the permanent with ID 7."
  putStrLn "> 1.7.0 # Activates the first ability of the permanent with ID 7."
  putStrLn ""
  putStrLn "Spaces (or dots) are sometimes optional between command arguments. Examples:"
  putStrLn "> +7 # Displays detailed information of the object with ID 7."
  putStrLn "> +7.0 # Displays detailed information of the first ability of the object with ID 7."
  putStrLn "> 1.7* # Activates the unique simple mana ability of the permanent with ID 7."
  putStrLn "> 1.7-1 # Activates the \"T: Add W\" ability of the permanent with ID 7."
  putStrLn "> 1.7w # Activates the \"T: Add W\" ability of the permanent with ID 7."
  M.void getLine
  pure $ AskPriorityActionAgain Nothing

quit :: Magic 'Public 'RO Terminal (PriorityAction ())
quit = M.lift $ Terminal $ throwE Quit

concede :: Magic 'Public 'RO Terminal (PriorityAction ())
concede = pure Concede

passPriority :: Magic 'Public 'RO Terminal (PriorityAction ())
passPriority = pure PassPriority

activateAbility :: ObjectId -> CommandAbilityIndex -> [ObjectId] -> Magic 'Public 'RO Terminal (PriorityAction ())
activateAbility objId abilityIndex _extraIds = do
  mZoAny <- internalFromPrivate $ toZO @ 'ZBattlefield @OTNAny objId
  mZoPerm <- internalFromPrivate $ toZO @ 'ZBattlefield @OTNPermanent objId
  case abilityIndex of
    CIAbilityIndex relIndex -> do
      case mZoAny of
        Nothing -> askAgain
        Just _zoAny -> do
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
            Nothing -> askAgain
    CIManaAbility ty -> case mZoPerm of
      Nothing -> askAgain
      Just zoPerm -> do
        -- TODO: This code should just assume the ability exists since the engine
        -- should eventually be able to vet if the ability exists or not.
        abilities <- internalFromPrivate $ getIntrinsicManaAbilities zoPerm
        let cond (SomeActivatedAbility _zo ability) = isTrivialManaAbility ability == Just ty
        case List.find cond abilities of
          Just ability -> pure $ PriorityAction $ ActivateAbility ability
          Nothing -> askAgain
    CIInferManaAbility -> case mZoPerm of
      Nothing -> askAgain
      Just zoPerm -> do
        abilities <- internalFromPrivate $ getTrivialManaAbilities zoPerm
        -- TODO: Filter out fungible trivial mana abilities before casing.
        -- An example would be if a LLanowar Elves became a Forest in addition to
        -- being a creature. Here it would have two fungible mana abilities.
        case abilities of
          [ability] -> pure $ PriorityAction $ ActivateAbility ability
          _ -> tryAgain
 where
  tryAgain :: Magic 'Public 'RO Terminal (PriorityAction ())
  tryAgain = pure $ AskPriorityActionAgain Nothing

  goIndex :: forall zone. IsZone zone => AbsoluteActivatedAbilityIndex -> Magic 'Public 'RO Terminal (Maybe (PriorityAction ()))
  goIndex index = do
    mAbility <- internalFromPrivate $ indexToActivated index
    case mAbility of
      Just ability -> pure $ Just $ PriorityAction $ ActivateAbility (ability :: SomeActivatedAbility zone OTNAny)
      Nothing -> pure Nothing

castSpell :: ObjectId -> [ObjectId] -> Magic 'Public 'RO Terminal (PriorityAction ())
castSpell spellId _extraIds = do
  let zo0 = toZO0 @ 'ZHand spellId
      zo = zo0ToSpell zo0
  pure $ PriorityAction $ CastSpell zo

playLand :: ObjectId -> [ObjectId] -> Magic 'Public 'RO Terminal (PriorityAction ())
playLand landId _extraIds = do
  let zo0 = toZO0 @ 'ZHand landId
      zo = toZO1 zo0
  pure $ PriorityAction $ SpecialAction $ PlayLand zo

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
terminalPriorityAction ::
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  Magic 'Public 'RO Terminal (PriorityAction ())
terminalPriorityAction attempt opaque oPlayer = M.lift do
  (action, commandInput) <- queryMagic opaque do
    M.liftIO clearScreen
    M.lift $ printGameState opaque
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- M.lift $ prompt $ "PriorityAction: " ++ show oPlayer ++ ": "
    let commandInput = case runParseCommandInput defaultCommandAliases text of
          Left _err -> CIAskAgain
          Right x -> x
    action <- parseCommandInput commandInput
    pure (action, commandInput)
  Terminal (State.gets terminal_replayLog) >>= \case
    Nothing -> pure ()
    Just file -> do
      info <- getPriorityInfo opaque oPlayer
      liftIO $ appendFile file $ show commandInput ++ " ; " ++ info ++ "\n"
  pure action

getPriorityInfo :: OpaqueGameState Terminal -> Object 'OTPlayer -> Terminal String
getPriorityInfo opaque oPlayer = queryMagic opaque do
  phaseStep <- internalFromPrivate $ gets magicPhaseStep
  turnNumber <- internalFromPrivate $ gets magicCurrentTurn
  pure $ show oPlayer ++ " " ++ prettyPhaseStep phaseStep ++ " Turn" ++ show turnNumber

terminalPromptPayDynamicMana ::
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  DynamicManaCost 'NoVar ->
  Magic 'Public 'RO Terminal ManaPayment
terminalPromptPayDynamicMana attempt opaque oPlayer dyn = M.lift do
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

terminalChooseOption ::
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  NatList user n elem ->
  Terminal (Fin user n)
terminalChooseOption = undefined

terminalLogCallPush :: OpaqueGameState Terminal -> CallFrameInfo -> Terminal ()
terminalLogCallPush opaque frame = case name == show 'queryMagic of
  True -> Terminal $ State.modify' \st -> st{terminal_logDisabled = terminal_logDisabled st + 1}
  False ->
    Terminal (State.gets terminal_logDisabled) >>= \case
      0 -> case logIgnore name of
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

terminalLogCallPop :: OpaqueGameState Terminal -> CallFrameInfo -> Terminal ()
terminalLogCallPop _opaque frame = case name == show 'queryMagic of
  True -> Terminal $ State.modify' \st ->
    assert (terminal_logDisabled st > 0) st{terminal_logDisabled = terminal_logDisabled st - 1}
  False ->
    Terminal (State.gets terminal_logDisabled) >>= \case
      0 -> case logIgnore name of
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
printGameState :: OpaqueGameState Terminal -> Terminal ()
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

printHand :: Hand -> Magic 'Public 'RO Terminal ()
printHand (Hand zos) = do
  names <- T.for zos getHandCardName
  liftIO $ print ("hand", names)

getHandCardName :: ZO 'ZHand OTNCard -> Magic 'Public 'RO Terminal String
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

logIgnore :: String -> Maybe IgnoreBehavior
--logIgnore = flip Map.lookup _logIgnoreMap
logIgnore = const (Just IgnoreAll)

_logIgnoreMap :: Map.Map String IgnoreBehavior
_logIgnoreMap =
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

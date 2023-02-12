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

module MtgPure.Client.Terminal.PriorityAction (
  playTerminalGame,
) where

import safe Ansi.Box (clearScreenWithoutPaging, withHiddenCursor)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadIO (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (Attempt, Attempt' (..))
import safe qualified Data.Char as Char
import safe qualified Data.List as List
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Maybe as Maybe
import safe Data.Monoid (First (..))
import safe Data.Nat (Fin, NatList)
import safe MtgPure.Client.Terminal.CommandInput (
  CommandAbilityIndex (..),
  CommandInput (..),
  defaultCommandAliases,
  runParseCommandInput,
 )
import safe MtgPure.Client.Terminal.Fwd.Api (
  printGameState,
 )
import safe MtgPure.Client.Terminal.Monad (
  Terminal,
  TerminalState (..),
  getsTerminalState,
  pause,
  prompt,
  quitTerminal,
  terminalLogCallPop,
  terminalLogCallPush,
 )
import safe MtgPure.Engine.Fwd.Api (
  getIntrinsicManaAbilities,
  getTrivialManaAbilities,
  indexToActivated,
  queryObjectId,
  toZO,
 )
import safe MtgPure.Engine.Monad (gets, internalFromPrivate)
import safe MtgPure.Engine.PlayGame (playGame)
import safe MtgPure.Engine.Prompt (
  AbsoluteActivatedAbilityIndex (AbsoluteActivatedAbilityIndex),
  CardCount (..),
  CardIndex (..),
  PlayerIndex (PlayerIndex),
  PriorityAction (..),
  Prompt' (..),
  QueryObjectResult (..),
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
  getOpaqueGameState,
  queryMagic,
 )
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Mana.IsManaAbility (isTrivialManaAbility)
import safe MtgPure.Model.Mana.ManaCost (DynamicManaCost (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool, ManaPayment (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ToManaPool (toCompleteManaPool)
import safe MtgPure.Model.Mulligan (Mulligan (..))
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNPermanent,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (ObjectId (..), getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.PhaseStep (prettyPhaseStep)
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Variable (Var (..))
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, toZO1, zo0ToSpell)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO)
import safe System.Console.ANSI (clearLine, setCursorPosition)

chunk :: Int -> [a] -> [[a]]
chunk n = go
 where
  go xs = case splitAt n xs of
    (ys, []) -> [ys]
    (ys, zs) -> ys : go zs

playTerminalGame :: [(Deck, Sideboard)] -> Terminal ()
playTerminalGame decks = do
  getsTerminalState terminal_replayLog >>= \case
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
  CIExamineAbility objId abilityIndex -> examineAbility objId abilityIndex
  CIExamineObject objId -> examineObject objId

help :: Magic 'Public 'RO Terminal (PriorityAction ())
help = M.liftIO do
  setCursorPosition 0 0
  clearScreenWithoutPaging
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
  putStrLn "\"#\" can be used to comment out the rest of a command. Useful in replay files."
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
  putStrLn "  Decimal numbers used to uniquely identify objects. This generalizes genuine MTG \"object\"s (CR 109.1)"
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
  pure $ AskPriorityActionAgain $ Just $ Attempt 0

askAgain :: Magic 'Public 'RO Terminal (PriorityAction ())
askAgain = pure $ AskPriorityActionAgain Nothing

quit :: Magic 'Public 'RO Terminal (PriorityAction ())
quit = M.lift quitTerminal

concede :: Magic 'Public 'RO Terminal (PriorityAction ())
concede = pure Concede

passPriority :: Magic 'Public 'RO Terminal (PriorityAction ())
passPriority = pure PassPriority

examineObject :: ObjectId -> Magic 'Public 'RO Terminal (PriorityAction ())
examineObject objId = do
  mQor <- internalFromPrivate $ queryObjectId objId
  let chunk' = chunk 90
  let showCard = \case
        Nothing -> error "examineObject: no card exists"
        Just card -> show card
  let msg = case mQor of
        Nothing -> "Object does not exist"
        Just qor -> case qorZone qor of
          ZStack -> "TODO: examine stack item"
          ZLibrary -> unlines . chunk' . showCard $ qorCard qor
          ZHand -> unlines . chunk' . showCard $ qorCard qor
          ZGraveyard -> unlines . chunk' . showCard $ qorCard qor
          ZExile -> unlines . chunk' . showCard $ qorCard qor
          ZBattlefield -> case qorPermanent qor of
            Nothing -> case qorPlayer qor of
              Nothing -> error "impossible"
              Just player -> unlines . chunk' $ show player
            Just perm ->
              let card = either show show $ permanentCard perm
                  goFacet :: Show a => a -> String
                  goFacet = show
                  facets =
                    Maybe.catMaybes
                      [ goFacet <$> permanentArtifact perm
                      , goFacet <$> permanentEnchantment perm
                      , goFacet <$> permanentLand perm
                      , goFacet <$> permanentCreature perm
                      , goFacet <$> permanentPlaneswalker perm
                      ]
               in List.intercalate "\n\n" $ card : facets
  let msg' = "Examining [" ++ show (unObjectId objId) ++ "]:\n\n" ++ msg
  M.liftIO $ withHiddenCursor do
    clearScreenWithoutPaging
    setCursorPosition 0 0
    putStrLn msg'
    pause
  pure $ AskPriorityActionAgain $ Just $ Attempt 0

examineAbility :: ObjectId -> CommandAbilityIndex -> Magic 'Public 'RO Terminal (PriorityAction ())
examineAbility _objId _abilityIndex = do
  let msg = "TODO"
  opaque <- getOpaqueGameState
  M.lift $ printGameState opaque $ Just msg
  M.liftIO clearLine
  M.liftIO $ withHiddenCursor pause
  pure $ AskPriorityActionAgain $ Just $ Attempt 0

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

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
terminalPriorityAction ::
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  Magic 'Public 'RO Terminal (PriorityAction ())
terminalPriorityAction attempt opaque oPlayer = M.lift do
  (action, commandInput) <- queryMagic opaque do
    clearScreenWithoutPaging
    M.lift $ printGameState opaque Nothing
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- M.lift $ prompt "> "
    let commandInput = case runParseCommandInput defaultCommandAliases text of
          Left _err -> CIAskAgain
          Right x -> x
    action <- parseCommandInput commandInput
    pure (action, commandInput)
  getsTerminalState terminal_replayLog >>= \case
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

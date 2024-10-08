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
import safe Control.Monad.Util (Attempt, Attempt' (..), untilJust)
import safe qualified Data.Char as Char
import safe Data.Functor ((<&>))
import safe Data.List (foldl')
import safe qualified Data.List as List
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes)
import safe qualified Data.Maybe as Maybe
import safe Data.Monoid (First (..))
import safe Data.Nat (Fin, IsNat, NatList, finToInt, intToFin, natListUsers)
import safe qualified Data.Traversable as T
import safe Data.Typeable (Typeable)
import safe MtgPure.Client.Terminal.CommandInput (
  CIAttack (..),
  CIBlock (..),
  CIChoice (..),
  CIPriorityAction (..),
  CommandAbilityIndex (..),
  defaultCommandAliases,
  runParseCIAttack,
  runParseCIBlock,
  runParseCIChoice,
  runParseCIPriorityAction,
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
  AbsoluteActivatedAbilityIndex (..),
  AttackingPlayer (..),
  CardCount (..),
  CardIndex (..),
  DeclaredAttacker (..),
  DeclaredBlocker (..),
  DefendingPlayer (..),
  Pause (..),
  PickVariety (..),
  PlayerIndex (PlayerIndex),
  PriorityAction (..),
  Prompt' (..),
  QueryObjectResult (..),
  RelativeAbilityIndex (RelativeAbilityIndex),
  SomeActivatedAbility (..),
  SpecialAction (..),
 )
import safe MtgPure.Engine.State (
  GameCheats,
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
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNPermanent,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (GetObjectId, ObjectId (..), getObjectId)
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.PhaseStep (prettyPhaseStep)
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Variable (Var (..))
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (oToZO1, toZO0, toZO1, toZO2, zo0ToSpell)
import safe System.Console.ANSI (clearLine, setCursorPosition)
import safe System.IO (hFlush, stdout)
import safe qualified System.IO as IO

chunk :: Int -> [a] -> [[a]]
chunk n = go
 where
  go xs = case splitAt n xs of
    (ys, []) -> [ys]
    (ys, zs) -> ys : go zs

playTerminalGame :: GameCheats -> [(Deck, Sideboard)] -> Terminal ()
playTerminalGame cheats decks = do
  getsTerminalState terminal_replayLog >>= \case
    Nothing -> pure ()
    Just file -> liftIO $ appendFile file "---------------------\n"
  result <- playGame $ gameInput cheats decks
  liftIO $ print result

gameInput :: GameCheats -> [(Deck, Sideboard)] -> GameInput Terminal
gameInput cheats decks =
  GameInput
    { gameInput_ = ()
    , gameInput_decks = decks
    , gameInput_gameCheats = cheats
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
          , promptChooseAttackers = terminalChooseAttackers
          , promptChooseBlockers = terminalChooseBlockers
          , promptChooseOption = terminalChooseOption
          , promptDebugMessage = \p msg -> liftIO do
              putStrLn $ "DEBUG: " ++ msg
              IO.hFlush IO.stdout
              M.when (p == Pause) pause
          , promptGetStartingPlayer = \_attempt _count -> pure $ PlayerIndex 0
          , promptLogCallPop = terminalLogCallPop
          , promptLogCallPush = terminalLogCallPush
          , promptPayDynamicMana = terminalPromptPayDynamicMana
          , promptPerformMulligan = \_attempt _p _hand -> pure False
          , promptPick = terminalPick
          , promptPriorityAction = terminalPriorityAction
          , promptShuffle = \_attempt (CardCount n) _player -> pure $ map CardIndex [0 .. n - 1]
          }
    }

parsePriorityAction :: CIPriorityAction -> Magic 'Public 'RO Terminal (PriorityAction ())
parsePriorityAction = \case
  CIActivateAbility objId abilityIndex extraIds -> activateAbility objId abilityIndex extraIds
  CIAskAgain -> askAgain
  CICastSpell spellId extraIds -> castSpell spellId extraIds
  CIConcede -> concede
  CIExamineAbility objId abilityIndex -> examineAbility objId abilityIndex
  CIExamineObject objId -> examineObject objId
  CIHelp _ -> help
  CIPass -> passPriority
  CIPlayLand landId extraIds -> playLand landId extraIds
  CIQuit -> quit

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
                  goCharacteristic :: (Show a) => a -> String
                  goCharacteristic = show
                  characters =
                    Maybe.catMaybes
                      [ goCharacteristic <$> permanentArtifact perm
                      , goCharacteristic <$> permanentEnchantment perm
                      , goCharacteristic <$> permanentLand perm
                      , goCharacteristic <$> permanentCreature perm
                      , goCharacteristic <$> permanentPlaneswalker perm
                      ]
               in List.intercalate "\n\n" $ card : characters
  let msg' = "Examining [" ++ show (unObjectId objId) ++ "]:\n\n" ++ msg
  M.liftIO $ withHiddenCursor do
    clearScreenWithoutPaging
    setCursorPosition 0 0
    putStrLn msg'
    hFlush stdout
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
  mZoAny <- internalFromPrivate $ toZO @'ZBattlefield @OTNAny objId
  mZoPerm <- internalFromPrivate $ toZO @'ZBattlefield @OTNPermanent objId
  case abilityIndex of
    CIAbilityIndex relIndex -> do
      case mZoAny of
        Nothing -> askAgain
        Just _zoAny -> do
          let index = AbsoluteActivatedAbilityIndex objId $ RelativeAbilityIndex relIndex
          mAction <-
            mconcat . map First
              <$> sequence
                [ goIndex @'ZBattlefield index
                -- , goIndex @'ZExile index
                -- , goIndex @'ZGraveyard index
                -- , goIndex @'ZHand index
                -- , goIndex @'ZLibrary index
                -- , goIndex @'ZStack index
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

  goIndex :: forall zone. (IsZone zone) => AbsoluteActivatedAbilityIndex -> Magic 'Public 'RO Terminal (Maybe (PriorityAction ()))
  goIndex index = do
    mAbility <- internalFromPrivate $ indexToActivated index
    case mAbility of
      Just ability -> pure $ Just $ PriorityAction $ ActivateAbility (ability :: SomeActivatedAbility zone OTNAny)
      Nothing -> pure Nothing

castSpell :: ObjectId -> [ObjectId] -> Magic 'Public 'RO Terminal (PriorityAction ())
castSpell spellId _extraIds = do
  let zo0 = toZO0 @'ZHand spellId
      zo = zo0ToSpell zo0
  pure $ PriorityAction $ CastSpell zo

playLand :: ObjectId -> [ObjectId] -> Magic 'Public 'RO Terminal (PriorityAction ())
playLand landId _extraIds = do
  let zo0 = toZO0 @'ZHand landId
      zo = toZO1 zo0
  pure $ PriorityAction $ SpecialAction $ PlayLand zo

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
terminalPriorityAction ::
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  Terminal (PriorityAction ())
terminalPriorityAction attempt opaque oPlayer = do
  (action, commandInput) <- queryMagic opaque do
    clearScreenWithoutPaging
    M.lift $ printGameState opaque Nothing
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- M.lift $ prompt "priorityAction> "
    let commandInput = case runParseCIPriorityAction defaultCommandAliases text of
          Left _err -> CIAskAgain
          Right x -> x
    action <- parsePriorityAction commandInput
    pure (action, commandInput)
  getsTerminalState terminal_replayLog >>= \case
    Nothing -> pure ()
    Just file -> do
      info <- getPriorityInfo opaque oPlayer
      liftIO $ appendFile file $ show commandInput ++ " # " ++ info ++ "\n"
  pure action

terminalChooseAttackers ::
  Attempt ->
  OpaqueGameState Terminal ->
  AttackingPlayer ->
  DefendingPlayer ->
  Terminal [DeclaredAttacker]
terminalChooseAttackers attempt opaque (AttackingPlayer oPlayer) (DefendingPlayer oDefender) = do
  zoAttackers <- queryMagic opaque do
    clearScreenWithoutPaging
    M.lift $ printGameState opaque Nothing
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- M.lift $ prompt "attack> "
    case runParseCIAttack text of
      Left _err -> pure [] -- TODO: ask again
      Right (CIAttack oAttackers) -> do
        zos <- fmap catMaybes $ internalFromPrivate $ T.for oAttackers toZO
        pure case length zos == length oAttackers of
          True -> zos
          False -> [] -- TODO: ask again
  getsTerminalState terminal_replayLog >>= \case
    Nothing -> pure ()
    Just file -> do
      info <- getPriorityInfo opaque oPlayer
      let strIds = unwords $ map (show . unObjectId . getObjectId) zoAttackers
      liftIO $ appendFile file $ strIds ++ " # Attack ; " ++ info ++ "\n"
  let zoVictim = toZO2 $ oToZO1 oDefender
  pure $
    zoAttackers <&> \zoAttacker ->
      DeclaredAttacker
        { declaredAttacker_attacker = zoAttacker
        , declaredAttacker_victim = zoVictim
        }

terminalChooseBlockers ::
  Attempt ->
  OpaqueGameState Terminal ->
  AttackingPlayer ->
  DefendingPlayer ->
  NonEmpty DeclaredAttacker ->
  Terminal [DeclaredBlocker]
terminalChooseBlockers attempt opaque _attackingPlayer (DefendingPlayer oPlayer) _attackers = do
  attackersBlockers <- queryMagic opaque do
    clearScreenWithoutPaging
    M.lift $ printGameState opaque Nothing
    liftIO case attempt of
      Attempt 0 -> pure ()
      Attempt n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- M.lift $ prompt "block> "
    case runParseCIBlock text of
      Left _err -> pure [] -- TODO: ask again
      Right (CIBlock pairs) -> do
        let oAttackers = map fst pairs
            oBlockers = map snd pairs
        zoAttackers <- fmap catMaybes $ internalFromPrivate $ T.for oAttackers toZO
        zoBlockers <- fmap catMaybes $ internalFromPrivate $ T.for oBlockers toZO
        let nAttackers = length zoAttackers
            nBlockers = length zoBlockers
        case nAttackers == length pairs && nBlockers == length pairs of
          True -> pure $ zip zoAttackers zoBlockers
          False -> pure [] -- TODO: ask again
  getsTerminalState terminal_replayLog >>= \case
    Nothing -> pure ()
    Just file -> do
      info <- getPriorityInfo opaque oPlayer
      let showIds (a, b) = show (unObjectId $ getObjectId a) ++ " " ++ show (unObjectId $ getObjectId b)
          strIds = unwords $ map showIds attackersBlockers
      liftIO $ appendFile file $ strIds ++ " # Block ; " ++ info ++ "\n"
  let go m (a, b) = Map.insertWith (<>) b (a :| []) m
      blockerToAttackers = foldl' go Map.empty attackersBlockers
  pure $
    Map.toList blockerToAttackers <&> \(zoBlocker, zoAttackers) ->
      DeclaredBlocker
        { declaredBlocker_blocker = zoBlocker
        , declaredBlocker_attackers = zoAttackers
        }

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
  Terminal ManaPayment
terminalPromptPayDynamicMana attempt opaque oPlayer dyn = do
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
  'S' : 'W' : s -> (toCompleteManaPool SW <>) <$> parseManaPool' s
  'S' : 'U' : s -> (toCompleteManaPool SU <>) <$> parseManaPool' s
  'S' : 'B' : s -> (toCompleteManaPool SB <>) <$> parseManaPool' s
  'S' : 'R' : s -> (toCompleteManaPool SR <>) <$> parseManaPool' s
  'S' : 'G' : s -> (toCompleteManaPool SG <>) <$> parseManaPool' s
  'S' : 'C' : s -> (toCompleteManaPool SC <>) <$> parseManaPool' s
  _ -> Nothing -- TODO: life payments for phyrexian mana

terminalPick ::
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  PickVariety a ->
  NonEmpty a ->
  Terminal a
terminalPick attempt opaque oPlayer variety xs = case xs of
  x :| [] -> pure x
  _ -> case variety of
    PickZO -> terminalPickByObjectId attempt opaque oPlayer tag xs
 where
  tag :: String
  tag = case variety of
    PickZO -> "PickZO"

terminalPickByObjectId ::
  (GetObjectId a) =>
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  String ->
  NonEmpty a ->
  Terminal a
terminalPickByObjectId (Attempt attempt) _opaque _oPlayer tag xs = do
  let list = NonEmpty.toList xs
  let msg = unlines $ chunk 90 $ show $ map getObjectId list
  let msg' = msg ++ "\n" ++ tag ++ ":\n> "
  x <- untilJust \(Attempt attempt') -> do
    let attempt'' = attempt + attempt'
    M.liftIO do
      clearScreenWithoutPaging
      setCursorPosition 0 0
    liftIO case attempt'' of
      0 -> pure ()
      n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- prompt msg'
    pure case runParseCIChoice text of
      Left _err -> Nothing
      Right (CIChoice i) -> case List.find (\x -> ObjectId i == getObjectId x) list of
        Nothing -> Nothing
        Just x -> Just x
  getsTerminalState terminal_replayLog >>= \case
    Nothing -> pure ()
    Just file -> do
      liftIO $ appendFile file $ show (unObjectId $ getObjectId x) ++ " # " ++ tag ++ "\n"
  pure x

_terminalPickByListIndex ::
  (Show a) =>
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  String ->
  NonEmpty a ->
  Terminal a
_terminalPickByListIndex (Attempt attempt) _opaque _oPlayer tag xs = do
  let list = NonEmpty.toList xs
  let msg = unlines $ chunk 90 $ show list
  let indices = [0 .. length list - 1]
  let msg' = msg ++ "\n" ++ tag ++ " " ++ show indices ++ ":\n> "
  (x, i) <- untilJust \(Attempt attempt') -> do
    let attempt'' = attempt + attempt'
    M.liftIO do
      clearScreenWithoutPaging
      setCursorPosition 0 0
    liftIO case attempt'' of
      0 -> pure ()
      n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- prompt msg'
    pure case runParseCIChoice text of
      Left _err -> Nothing
      Right (CIChoice i) -> case 0 <= i && i < length list of
        False -> Nothing
        True -> Just (list !! i, i)
  getsTerminalState terminal_replayLog >>= \case
    Nothing -> pure ()
    Just file -> do
      liftIO $ appendFile file $ show i ++ " # " ++ tag ++ "\n"
  pure x

terminalChooseOption ::
  (IsNat n, Show user, Typeable user) =>
  Attempt ->
  OpaqueGameState Terminal ->
  Object 'OTPlayer ->
  NatList user n elem ->
  Terminal (Fin user n)
terminalChooseOption (Attempt attempt) _opaque _oPlayer natList = do
  let list = natListUsers natList
  let msg = unlines $ chunk 90 $ show list
  let indices = [0 .. length list - 1]
  let msg' = msg ++ "\nChooseOption " ++ show indices ++ ":\n> "
  fin <- untilJust \(Attempt attempt') -> do
    let attempt'' = attempt + attempt'
    M.liftIO do
      clearScreenWithoutPaging
      setCursorPosition 0 0
    liftIO case attempt'' of
      0 -> pure ()
      n -> putStrLn $ "Retrying[" ++ show n ++ "]..."
    text <- prompt msg'
    pure case runParseCIChoice text of
      Left _err -> Nothing
      Right (CIChoice i) -> intToFin i
  getsTerminalState terminal_replayLog >>= \case
    Nothing -> pure ()
    Just file -> do
      liftIO $ appendFile file $ show (finToInt fin) ++ " # ChooseOption\n"
  pure fin

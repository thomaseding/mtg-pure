{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Core (
  Magic,
  OpaqueGameState,
  GameInput (..),
  GameState (..),
  Prompt (..),
  PlayerIndex,
  CardCount,
  CardIndex,
  CastSpell (..),
  InvalidCastSpell (..),
  InvalidSpecialAction (..),
  SpecialAction (..),
  GameFormat (..),
  GameResult (..),
  queryMagic,
  playGame,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadTrans (..))
import safe Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe Data.Typeable (Typeable)
import safe Data.Void (Void, absurd)
import safe MtgPure.Engine.Monad (
  Magic',
  get,
  gets,
  modify,
  put,
  runMagicRO,
  runMagicRW,
  safeFromPublic,
  safeFromPublicRO,
  safeFromRO,
  unsafeFromPrivate,
 )
import safe MtgPure.Model.Battlefield (Battlefield (Battlefield))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.IsCardList (
  IsCardList (fromCardList, toCardList),
  containsCard,
  popCard,
  pushCard,
 )
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mulligan (Mulligan)
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectId (ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectType (OT0, ObjectType (..), SObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTAbility, OTLand, OTSpell, OTStackObject)
import safe MtgPure.Model.Permanent (Permanent (..), Phased (..), Tapped (Untapped), cardToPermanent)
import safe MtgPure.Model.Phase (Phase (..))
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Card (..), SomeCard)
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Zone (IsZone, SZone (..), Zone (..), singZone)
import safe MtgPure.Model.ZoneObject (ZO, toZO0)

data InternalLogicError
  = ExpectedCardToBeAPermanentCard
  | InvalidPermanent (ZO 'ZBattlefield OT0)
  | InvalidPlayer (Object 'OTPlayer)

deriving instance Show InternalLogicError

newtype PlayerCount = PlayerCount {unPlayerCount :: Int}
  deriving (Eq, Ord, Show)

newtype PlayerIndex = PlayerIndex {unPlayerIndex :: Int}
  deriving (Eq, Ord, Show)

newtype CardCount = CardCount {unCardCount :: Int}
  deriving (Eq, Ord, Show)

newtype CardIndex = CardIndex {unCardIndex :: Int}
  deriving (Eq, Ord, Show)

data Prompt (m :: Type -> Type) = Prompt
  { exceptionCantBeginGameWithoutPlayers :: m ()
  , exceptionInvalidCastSpell :: OpaqueGameState m -> Object 'OTPlayer -> InvalidCastSpell -> m ()
  , exceptionInvalidShuffle :: CardCount -> [CardIndex] -> m ()
  , exceptionInvalidSpecialActionSlow :: OpaqueGameState m -> Object 'OTPlayer -> InvalidSpecialAction 'SpecialSlow -> m ()
  , exceptionInvalidSpecialActionFast :: OpaqueGameState m -> Object 'OTPlayer -> InvalidSpecialAction 'SpecialFast -> m ()
  , exceptionInvalidStartingPlayer :: PlayerCount -> PlayerIndex -> m ()
  , promptCastSpell :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe CastSpell)
  , promptDebugMessage :: String -> m ()
  , promptGetStartingPlayer :: PlayerCount -> m PlayerIndex
  , promptPerformMulligan :: Object 'OTPlayer -> [Card ()] -> m Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptShuffle :: CardCount -> Object 'OTPlayer -> m [CardIndex]
  , promptSpecialActionSlow :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe (SpecialAction 'SpecialSlow))
  , promptSpecialActionFast :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe (SpecialAction 'SpecialFast))
  }

data SpecialActionSpeed
  = SpecialSlow
  | SpecialFast

-- NB: All special actions require player priority (116.1).
data SpecialAction (speed :: SpecialActionSpeed) :: Type where
  PlayLand :: IsZone zone => ZO zone OTLand -> SpecialAction 'SpecialSlow

data InvalidSpecialAction (speeed :: SpecialActionSpeed) :: Type where
  PlayLand_AtMaxLands :: IsZone zone => ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow
  PlayLand_CannotPlayFromZone :: IsZone zone => ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow
  PlayLand_NoPriority :: IsZone zone => ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow
  PlayLand_NotALand :: IsZone zone => ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow
  PlayLand_NotInZone :: ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow
  PlayLand_NotOwned :: IsZone zone => ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow
  PlayLand_NotMainPhase :: IsZone zone => ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow
  PlayLand_StackNonEmpty :: IsZone zone => ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow

-- NB: Unfortuantely OTSpell intersects OTArtifactLand. Such is life.
-- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
-- though it's probably unnecessary for Authoring (thankfully).
newtype CastSpell = CastSpell (SomeCard OTSpell)

data InvalidCastSpell :: Type where
  CastSpell_NotASpell :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell
  CastSpell_CannotPlayFromZone :: IsZone zone => ZO zone OTSpell -> InvalidCastSpell

mkPlayer :: GameFormat -> Deck -> Sideboard -> Player
mkPlayer format deck sideboard =
  Player
    { playerDrewFromEmptyLibrary = False
    , playerGraveyard = Graveyard []
    , playerHand = Hand []
    , playerLandsPlayedThisTurn = 0
    , playerLibrary = mkLibrary deck
    , playerLife = life
    , playerLost = False
    , playerMana = mempty
    , playerStartingDeck = deck
    , playerStartingHandSize = 7
    , playerStartingLife = life
    , playerStartingSideboard = sideboard
    }
 where
  -- 103.3
  life = Life $ case format of
    Vintage -> 20

mkLibrary :: Deck -> Library
mkLibrary (Deck cards) = Library cards

-- NB: This hangs if there are not enough unique items.
takeUnique :: Eq a => Int -> Stream.Stream a -> [a]
takeUnique n s = case n <= 0 of
  True -> []
  False ->
    let x = Stream.head s
        s' = Stream.tail $ Stream.filter (/= x) s
     in x : takeUnique (n - 1) s'

mkAPNAP :: PlayerCount -> Stream.Stream (Object 'OTPlayer) -> Stream.Stream (Object 'OTPlayer)
mkAPNAP (PlayerCount n) = Stream.cycle . takeUnique n

data GameState (m :: Type -> Type) where
  GameState ::
    { magicBattlefield :: Battlefield
    , magicCurrentTurn :: Int
    , magicGraveyardCards :: Map.Map (ZO 'ZGraveyard OT0) (Card ())
    , magicHandCards :: Map.Map (ZO 'ZHand OT0) (Card ())
    , magicManaBurn :: Bool
    , magicNextObjectId :: ObjectId
    , magicPermanents :: Map.Map (ZO 'ZBattlefield OT0) Permanent
    , magicPhaseStep :: PhaseStep
    , magicPlayers :: Map.Map (Object 'OTPlayer) Player -- contains all players
    , magicPlayerOrderAPNAP :: Stream.Stream (Object 'OTPlayer) -- does not contain losers
    , magicPlayerOrderPriority :: [Object 'OTPlayer] -- does not contain losers
    , magicPlayerOrderTurn :: Stream.Stream (Object 'OTPlayer) -- does not contain losers
    , magicPrompt :: Prompt m
    , magicStartingPlayer :: Object 'OTPlayer
    , magicStack :: Stack
    } ->
    GameState m
  deriving (Typeable)

mkGameState :: GameInput m -> Maybe (GameState m)
mkGameState input = case playerObjects of
  [] -> Nothing
  oPlayer : _ ->
    Just
      GameState
        { magicBattlefield = Battlefield []
        , magicCurrentTurn = 0
        , magicGraveyardCards = mempty
        , magicHandCards = mempty
        , magicManaBurn = False
        , magicNextObjectId = ObjectId playerCount
        , magicPermanents = mempty
        , magicPhaseStep = PSBeginningPhase UntapStep
        , magicPlayers = playerMap
        , magicPlayerOrderAPNAP = Stream.cycle playerObjects
        , magicPlayerOrderPriority = []
        , magicPlayerOrderTurn = Stream.cycle playerObjects
        , magicPrompt = gameInput_prompt input
        , magicStack = Stack []
        , magicStartingPlayer = oPlayer
        }
 where
  format = gameInput_gameFormat input
  players = map (uncurry $ mkPlayer format) $ gameInput_decks input
  playerCount = length players
  playerObjects = map (Object SPlayer . ObjectId) [0 .. playerCount - 1]
  playerMap = Map.fromList $ zip playerObjects players

newtype OpaqueGameState m = OpaqueGameState (GameState m)

data GameFormat
  = Vintage
  deriving (Eq, Ord, Show, Typeable)

data GameInput m = GameInput
  { gameInput_decks :: [(Deck, Sideboard)]
  , gameInput_gameFormat :: GameFormat
  , gameInput_mulligan :: Mulligan
  , gameInput_prompt :: Prompt m
  }

data GameResult m = GameResult
  { gameEndState :: GameState m
  , gameWinners :: [PlayerIndex] -- 🏆🥇🏆
  , gameLosers :: [PlayerIndex]
  }
  deriving (Typeable)

type Magic v rw m = Magic' (GameResult m) (GameState m) v rw m

queryMagic :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic (OpaqueGameState st) = runMagicRO st

playGame :: Monad m => GameInput m -> m (Maybe (GameResult m))
playGame input = case mkGameState input of
  Nothing -> do
    exceptionCantBeginGameWithoutPlayers $ gameInput_prompt input
    pure Nothing
  Just st ->
    runMagicRW st startGame >>= \case
      Left result -> pure $ Just result
      Right v -> absurd v

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m =
  m >>= \case
    Just x -> pure x
    Nothing -> untilJust m

getAlivePlayerCount :: Monad m => Magic 'Public 'RO m PlayerCount
getAlivePlayerCount = undefined

getAPNAP :: Monad m => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAP = unsafeFromPrivate $ gets magicPlayerOrderAPNAP

getActivePlayer :: Monad m => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayer = Stream.head <$> getAPNAP

getPlayers :: Monad m => Magic 'Public 'RO m [Object 'OTPlayer]
getPlayers = do
  st <- unsafeFromPrivate get
  let ps = Map.assocs $ magicPlayers st
  pure $ map fst $ filter (not . playerLost . snd) ps

withEachPlayer :: (IsReadWrite rw, Monad m) => (Object 'OTPlayer -> Magic v rw m ()) -> Magic v rw m ()
withEachPlayer f = safeFromPublic (safeFromRO getPlayers) >>= M.mapM_ f

getPermanents :: Monad m => Magic v 'RO m [ZO 'ZBattlefield OT0]
getPermanents = unsafeFromPrivate $ gets $ Map.keys . magicPermanents

withEachPermanent :: (IsReadWrite rw, Monad m) => (ZO 'ZBattlefield OT0 -> Magic v rw m ()) -> Magic v rw m ()
withEachPermanent f = safeFromRO getPermanents >>= M.mapM_ f

withEachControlledPermanent ::
  (IsReadWrite rw, Monad m) =>
  Object 'OTPlayer ->
  (ZO 'ZBattlefield OT0 -> Magic v rw m ()) ->
  Magic v rw m ()
withEachControlledPermanent oPlayer f = withEachPermanent $ \oPerm -> do
  perm <- unsafeFromPrivate $ safeFromRO $ getPermanent oPerm
  M.when (permanentController perm == oPlayer) $ f oPerm

-- (103)
startGame :: Monad m => Magic 'Private 'RW m Void
startGame = do
  determineStartingPlayer -- (103.1)
  withEachPlayer shuffleLibrary -- (103.2)
  -- (103.3) See `mkPlayer`
  drawStartingHands -- (103.4)
  -- (103.5) TODO: leylines and such
  -- (103.6) TODO: planechase stuff
  tickPhaseStep -- (103.7)

tickPhaseStep :: Monad m => Magic 'Private 'RW m Void
tickPhaseStep =
  M.forever $
    safeFromRO (gets magicPhaseStep) >>= \case
      PSBeginningPhase step -> tickBeginningPhase step
      PSPreCombatMainPhase -> tickMainPhase
      PSCombatPhase step -> tickCombatPhase step
      PSPostCombatMainPhase -> tickMainPhase
      PSEndingPhase step -> tickEndingPhase step

tickBeginningPhase :: Monad m => Step 'BeginningPhase -> Magic 'Private 'RW m ()
tickBeginningPhase = \case
  UntapStep -> tickUntapStep
  UpkeepStep -> tickUpkeepStep
  DrawStep -> tickDrawStep

-- (502)
tickUntapStep :: Monad m => Magic 'Private 'RW m ()
tickUntapStep = do
  do
    n <- safeFromPublicRO getAlivePlayerCount
    ps <- safeFromRO $ gets magicPlayerOrderTurn
    modify $ \st ->
      st
        { magicCurrentTurn = magicCurrentTurn st + 1
        , magicPlayerOrderAPNAP = mkAPNAP n ps
        }
  oPlayer <- safeFromPublicRO getActivePlayer
  do
    player <- safeFromRO $ getPlayer oPlayer
    setPlayer oPlayer player{playerLandsPlayedThisTurn = 0}
  withEachControlledPermanent oPlayer togglePermanentPhase -- (502.1)
  pure () -- (502.2) TODO: day/night
  withEachControlledPermanent oPlayer untapPermanent -- (502.3) TODO: fine-grained untapping
  pure () -- (502.4) Rule states that players can't get priority, so nothing to do here.
  advancePhaseStep

advancePhaseStepNatural :: PhaseStep -> PhaseStep
advancePhaseStepNatural = \case
  PSBeginningPhase step -> case step of
    UntapStep -> PSBeginningPhase UpkeepStep
    UpkeepStep -> PSBeginningPhase DrawStep
    DrawStep -> PSPreCombatMainPhase
  PSPreCombatMainPhase -> PSCombatPhase BeginningOfCombatStep
  PSCombatPhase step -> case step of
    BeginningOfCombatStep -> PSCombatPhase DeclareAttackersStep
    DeclareAttackersStep -> PSCombatPhase DeclareBlockersStep
    DeclareBlockersStep -> PSCombatPhase CombatDamageStep
    CombatDamageStep -> PSCombatPhase EndOfCombatStep
    EndOfCombatStep -> PSPostCombatMainPhase
  PSPostCombatMainPhase -> PSEndingPhase EndStep
  PSEndingPhase step -> case step of
    EndStep -> PSEndingPhase CleanupStep
    CleanupStep -> PSBeginningPhase UntapStep

advancePhaseStep :: Monad m => Magic 'Private 'RW m ()
advancePhaseStep = do
  -- TODO: This will eventually need to be more elaborate due to some effects.
  modify $ \st -> st{magicPhaseStep = advancePhaseStepNatural $ magicPhaseStep st}

togglePermanentPhase :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RW m ()
togglePermanentPhase oPerm = do
  perm <- safeFromRO $ getPermanent oPerm
  setPermanent
    oPerm
    perm
      { permanentPhased = case permanentPhased perm of
          PhasedIn -> PhasedOut
          PhasedOut -> PhasedIn
      }

untapPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RW m ()
untapPermanent oPerm = do
  perm <- safeFromRO $ getPermanent oPerm
  setPermanent oPerm perm{permanentTapped = Untapped}

tickUpkeepStep :: Monad m => Magic 'Private 'RW m ()
tickUpkeepStep = do
  advancePhaseStep

tickDrawStep :: Monad m => Magic 'Private 'RW m ()
tickDrawStep = do
  st <- safeFromRO get
  oPlayer <- safeFromPublicRO getActivePlayer
  case magicCurrentTurn st of
    1 -> pure () -- (103.7.*) TODO: this needs to account for game format
    _ -> drawCard oPlayer
  advancePhaseStep

-- (505)
tickMainPhase :: Monad m => Magic 'Private 'RW m ()
tickMainPhase = do
  pure () -- (505.1) Rule just states nomenclature. Nothing special to do
  pure () -- (505.2) Rule just states this phase has no steps
  pure () -- (505.3) TODO: Archenemy
  pure () -- (505.4) TOOD: Sage lore counters
  oPlayer <- safeFromPublicRO getActivePlayer
  gainPriority oPlayer

gainPriority :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
gainPriority oPlayer = do
  pure () -- (117.5) TODO: state-based actions
  PlayerCount n <- safeFromPublicRO getAlivePlayerCount
  ps <- safeFromRO $ Stream.take n . Stream.dropWhile (/= oPlayer) <$> getAPNAP
  modify $ \st -> st{magicPlayerOrderPriority = ps}
  tickPriority

type MagicEx v rw ex m a = ExceptT ex (Magic v rw m) a

type MagicCont v rw m a b = MagicEx v rw (Magic v rw m a) m b

tickPriority :: forall m. Monad m => Magic 'Private 'RW m ()
tickPriority =
  runExceptT m >>= \case
    Left cont -> cont
    Right v -> absurd v
 where
  m :: MagicCont 'Private 'RW m () Void
  m = do
    oPlayers <- lift $ safeFromRO $ gets magicPlayerOrderPriority
    case oPlayers of
      [] -> throwE tickStack
      oPlayer : _ -> do
        tryCastSpell oPlayer -- (117.1a)
        tryActivateAbility oPlayer -- (117.1b) (117.1d)
        trySpecialAction oPlayer -- (117.1c)
        throwE passPriority -- (117.3d)

-- (117.3d)
passPriority :: Monad m => Magic 'Private 'RW m ()
passPriority = do
  oPlayers <- safeFromRO $ gets magicPlayerOrderPriority
  case oPlayers of
    [] -> assert False $ pure ()
    _ : oPlayers' -> modify $ \st -> st{magicPlayerOrderPriority = oPlayers'}

-- (117.1a)
tryCastSpell :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
tryCastSpell oPlayer = do
  pure () -- TODO
  let spellIsCast = True
  case spellIsCast of
    True -> throwE $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1b) (117.1d)
tryActivateAbility :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
tryActivateAbility oPlayer = do
  pure () -- TODO
  let abilityIsActivated = True
  case abilityIsActivated of
    True -> throwE $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1c)
trySpecialAction :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
trySpecialAction oPlayer = do
  trySpecialActionSlow oPlayer
  trySpecialActionFast oPlayer

trySpecialActionSlow :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
trySpecialActionSlow oPlayer = do
  st <- lift $ safeFromRO get
  let isMainPhase = magicPhaseStep st `elem` [PSPreCombatMainPhase, PSPostCombatMainPhase]
      Stack stack = magicStack st
      prompt = magicPrompt st
      opaque = OpaqueGameState st
  oActive <- lift $ safeFromPublicRO getActivePlayer
  case isMainPhase && null stack && oPlayer == oActive of
    False -> pure ()
    True -> do
      mSpecial <- lift $ lift $ promptSpecialActionSlow prompt opaque oPlayer
      case mSpecial of
        Nothing -> pure ()
        Just special -> do
          success <- lift $ performSpecialAction oPlayer special
          case success of
            True -> throwE $ gainPriority oPlayer -- (117.3c)
            False -> pure ()

trySpecialActionFast :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
trySpecialActionFast oPlayer = do
  st <- lift $ safeFromRO get
  let prompt = magicPrompt st
      opaque = OpaqueGameState st
  mSpecial <- lift $ lift $ promptSpecialActionFast prompt opaque oPlayer
  case mSpecial of
    Nothing -> pure ()
    Just special -> do
      success <- lift $ performSpecialAction oPlayer special
      case success of
        True -> throwE $ gainPriority oPlayer -- (117.3c)
        False -> pure ()

performSpecialAction :: Monad m => Object 'OTPlayer -> SpecialAction speed -> Magic 'Private 'RW m Bool
performSpecialAction oPlayer = \case
  PlayLand oLand -> playLand oPlayer oLand

playLand ::
  forall zone m.
  (IsZone zone, Monad m) =>
  Object 'OTPlayer ->
  ZO zone OTLand ->
  Magic 'Private 'RW m Bool
playLand oPlayer oLand = do
  st <- unsafeFromPrivate $ safeFromRO get
  let opaque = OpaqueGameState st
      prompt = magicPrompt st
      atMaxLands = False -- (305.2) TODO
      hasPriority = True -- TODO
      isMainPhase = True -- TODO
      stackEmpty = True -- TODO
      --
      invalid :: (ZO zone OTLand -> InvalidSpecialAction 'SpecialSlow) -> Magic 'Private 'RW m Bool
      invalid ex = do
        lift $ exceptionInvalidSpecialActionSlow prompt opaque oPlayer $ ex oLand
        pure False
      --
      success :: Card () -> Player -> Magic 'Private 'RW m ()
      success card player = do
        setPlayer
          oPlayer
          player
            { playerLandsPlayedThisTurn = playerLandsPlayedThisTurn player + 1
            -- TODO: remove from hand
            }
        i <- newObjectId
        let oLand' = toZO0 i
            perm = case cardToPermanent oPlayer card of
              Nothing -> error $ show ExpectedCardToBeAPermanentCard
              Just perm' -> perm'
        setPermanent oLand' perm
        pure () -- TODO: add to `magicBattlefield`
  case hasPriority of
    False -> invalid PlayLand_NoPriority
    True -> case isMainPhase of
      False -> invalid PlayLand_NotMainPhase
      True -> case stackEmpty of
        False -> invalid PlayLand_StackNonEmpty
        True -> case atMaxLands of
          True -> invalid PlayLand_AtMaxLands
          False -> case singZone @zone of
            SZBattlefield -> invalid PlayLand_CannotPlayFromZone
            SZExile -> invalid PlayLand_CannotPlayFromZone
            SZLibrary -> invalid PlayLand_CannotPlayFromZone
            SZStack -> invalid PlayLand_CannotPlayFromZone
            SZGraveyard -> invalid PlayLand_CannotPlayFromZone -- TODO: [Crucible of Worlds]
            SZHand -> do
              mCard <- safeFromRO $ gets $ Map.lookup (toZO0 oLand) . magicHandCards
              case mCard of
                Nothing -> invalid PlayLand_NotInZone
                Just card -> do
                  player <- safeFromRO $ getPlayer oPlayer
                  let hand = playerHand player
                  case containsCard card hand of
                    False -> invalid PlayLand_NotOwned
                    True -> case card of
                      Card _name wCard _def -> case wCard of
                      TribalCard _name wCard _def -> case wCard of
                      --
                      ArtifactCard{} -> invalid PlayLand_NotALand
                      ArtifactCreatureCard{} -> invalid PlayLand_NotALand
                      CreatureCard{} -> invalid PlayLand_NotALand
                      EnchantmentCard{} -> invalid PlayLand_NotALand
                      EnchantmentCreatureCard{} -> invalid PlayLand_NotALand
                      InstantCard{} -> invalid PlayLand_NotALand
                      PlaneswalkerCard{} -> invalid PlayLand_NotALand
                      SorceryCard{} -> invalid PlayLand_NotALand
                      --
                      LandCard{} -> success card player >> pure True

tickStack :: Monad m => Magic 'Private 'RW m ()
tickStack = do
  Stack stack <- safeFromRO $ gets magicStack
  case stack of
    [] -> pure () -- (117.4)
    oItem : oItems -> do
      item <- getStackObject oItem
      modify $ \st -> st{magicStack = Stack oItems}
      case item of
        Left ability -> resolveAbility ability
        Right spell -> resolveSpell spell
      tickPriority

resolveSpell :: ObjectN OTSpell -> Magic 'Private 'RW m ()
resolveSpell = undefined

resolveAbility :: ObjectN OTAbility -> Magic 'Private 'RW m ()
resolveAbility = undefined

getStackObject :: ZO 'ZStack OTStackObject -> Magic 'Private 'RW m (Either (ObjectN OTAbility) (ObjectN OTSpell))
getStackObject = undefined

tickCombatPhase :: Monad m => Step 'CombatPhase -> Magic 'Private 'RW m ()
tickCombatPhase = \case
  BeginningOfCombatStep -> tickBeginingOfCombatStep
  DeclareAttackersStep -> tickDeckareAttackersStep
  DeclareBlockersStep -> tickDeclareBlockersStep
  CombatDamageStep -> tickCombatDamageStep
  EndOfCombatStep -> tickEndOfCombatStep

tickBeginingOfCombatStep :: Monad m => Magic 'Private 'RW m ()
tickBeginingOfCombatStep = undefined

tickDeckareAttackersStep :: Monad m => Magic 'Private 'RW m ()
tickDeckareAttackersStep = undefined

tickDeclareBlockersStep :: Monad m => Magic 'Private 'RW m ()
tickDeclareBlockersStep = undefined

tickCombatDamageStep :: Monad m => Magic 'Private 'RW m ()
tickCombatDamageStep = undefined

tickEndOfCombatStep :: Monad m => Magic 'Private 'RW m ()
tickEndOfCombatStep = undefined

tickEndingPhase :: Monad m => Step 'EndingPhase -> Magic 'Private 'RW m ()
tickEndingPhase = \case
  EndStep -> tickEndStep
  CleanupStep -> tickCleanupStep

tickEndStep :: Monad m => Magic 'Private 'RW m ()
tickEndStep = undefined

tickCleanupStep :: Monad m => Magic 'Private 'RW m ()
tickCleanupStep = undefined

-- (103.1)
determineStartingPlayer :: Monad m => Magic 'Private 'RW m ()
determineStartingPlayer = do
  st <- safeFromRO get
  let prompt = magicPrompt st
      playerCount = Map.size $ magicPlayers st
  startingIndex <- lift $
    untilJust $ do
      PlayerIndex playerIndex <- promptGetStartingPlayer prompt $ PlayerCount playerCount
      case playerIndex < playerCount of
        True -> pure $ Just playerIndex
        False -> do
          exceptionInvalidStartingPlayer prompt (PlayerCount playerCount) $ PlayerIndex playerIndex
          pure Nothing
  let ps = Stream.drop startingIndex $ magicPlayerOrderTurn st
  put $
    st
      { magicPlayerOrderAPNAP = ps
      , magicPlayerOrderPriority = []
      , magicPlayerOrderTurn = ps
      }

shuffleLibrary :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
shuffleLibrary oPlayer = do
  prompt <- safeFromRO $ gets magicPrompt
  player <- safeFromRO $ getPlayer oPlayer
  let library = fromCardList $ playerLibrary player
      count = length library
      ordered = [0 .. count - 1]
  ordering <- lift $
    untilJust $ do
      ordering <- promptShuffle prompt (CardCount count) oPlayer
      case List.sort (map unCardIndex ordering) == ordered of
        True -> pure $ Just ordering
        False -> do
          exceptionInvalidShuffle prompt (CardCount count) ordering
          pure Nothing
  let library' = map snd $ List.sortOn fst $ zip ordering library
  setPlayer oPlayer $ player{playerLibrary = toCardList library'}

-- (103.4)
drawStartingHands :: Monad m => Magic 'Private 'RW m ()
drawStartingHands = withEachPlayer drawStartingHand

drawStartingHand :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
drawStartingHand oPlayer = do
  player <- safeFromRO $ getPlayer oPlayer
  drawCards (playerStartingHandSize player) oPlayer

newObjectId :: Monad m => Magic 'Private 'RW m ObjectId
newObjectId = do
  ObjectId i <- safeFromRO $ gets magicNextObjectId
  modify $ \st -> st{magicNextObjectId = ObjectId $ i + 1}
  pure $ ObjectId i

findPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RO m (Maybe Permanent)
findPermanent oPerm = gets $ Map.lookup oPerm . magicPermanents

getPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RO m Permanent
getPermanent oPerm =
  findPermanent oPerm >>= \case
    Nothing -> error $ show $ InvalidPermanent oPerm
    Just perm -> pure perm

setPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Permanent -> Magic 'Private 'RW m ()
setPermanent oPerm perm = modify $ \st ->
  let permMap = magicPermanents st
      permMap' = Map.insert oPerm perm permMap
   in st{magicPermanents = permMap'}

findPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m (Maybe Player)
findPlayer oPlayer = gets $ Map.lookup oPlayer . magicPlayers

getPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m Player
getPlayer oPlayer =
  findPlayer oPlayer >>= \case
    Nothing -> error $ show $ InvalidPlayer oPlayer
    Just player -> pure player

setPlayer :: Monad m => Object 'OTPlayer -> Player -> Magic 'Private 'RW m ()
setPlayer oPlayer player = modify $ \st ->
  let playerMap = magicPlayers st
      playerMap' = Map.insertWith (\_ _ -> player) oPlayer fatal playerMap
      fatal = error $ show $ InvalidPlayer oPlayer
   in st{magicPlayers = playerMap'}

drawCard :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
drawCard oPlayer = do
  player <- safeFromRO $ getPlayer oPlayer
  let library = playerLibrary player
  case popCard library of
    Nothing -> setPlayer oPlayer player{playerDrewFromEmptyLibrary = True}
    Just (card, library') -> do
      setPlayer
        oPlayer
        player
          { playerHand = pushCard card $ playerHand player
          , playerLibrary = library'
          }
      i <- newObjectId
      let zo = toZO0 i
      modify $ \st -> st{magicHandCards = Map.insert zo card $ magicHandCards st}

drawCards :: Monad m => Int -> Object 'OTPlayer -> Magic 'Private 'RW m ()
drawCards n = M.replicateM_ n . drawCard

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Core (
  --Magic,
  --OpaqueGameState,
  --GameInput (..),
  --GameState (..),
  --Prompt (..),
  --PlayerIndex,
  --CardCount,
  --CardIndex,
  --CastSpell (..),
  --InvalidCastSpell (..),
  --InvalidPlayLand (..),
  --SpecialAction (..),
  --GameFormat (..),
  --GameResult (..),
  --queryMagic,
  --playGame,
  module MtgPure.Engine.Core,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (MonadTrans (..))
import safe Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import safe qualified Data.DList as DList
import safe Data.Functor ((<&>))
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (First (..))
import safe qualified Data.Stream as Stream
import safe Data.Typeable (Typeable)
import safe Data.Void (Void, absurd)
import safe MtgPure.Engine.Monad (
  Magic',
  fromPublic,
  fromPublicRO,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  magicCatch,
  magicThrow,
  modify,
  put,
  runMagicRO,
  runMagicRW,
 )
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.IsCardList (
  IsCardList (fromCardList, toCardList),
  containsCard,
  popCard,
  pushCard,
  removeCard,
 )
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana (IsManaNoVar, IsSnow, Mana, Snow (..))
import safe MtgPure.Model.ManaCost (ManaCost (..))
import safe MtgPure.Model.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mulligan (Mulligan)
import safe MtgPure.Model.Object (
  IsObjectType (..),
  OT (..),
  OT0,
  OT1,
  Object (..),
  ObjectDiscriminant,
  ObjectType (..),
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectType.Card (WCard (..))
import safe MtgPure.Model.ObjectType.Index (IndexOT (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTInstant,
  OTLand,
  OTSorcery,
  OTSpell,
 )
import safe MtgPure.Model.Permanent (
  Permanent (..),
  Phased (..),
  Tapped (..),
  cardToPermanent,
 )
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  Card (..),
  CardTypeDef (..),
  Case (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectPrePost,
  Requirement (..),
  Some (..),
  SomeCard,
  SomeTerm (..),
  WithMaskedObject (..),
  WithThis (..),
 )
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.ToObjectN.Classes (
  ToObject2' (..),
  ToObject6' (..),
 )
import safe MtgPure.Model.Tribal (IsTribal (..), STribal (..), Tribal (..))
import safe MtgPure.Model.Variable (ForceVars (..), Var (..), Variable (..), readVariable)
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.Zone (
  IsZone,
  SZone (..),
  Zone (..),
  singZone,
 )
import safe MtgPure.Model.ZoneObject (
  IsZO,
  OCreaturePlayerPlaneswalker,
  ODamageSource,
  OPlayer,
  ZO,
  ZoneObject (..),
  objectToZO,
  toZO0,
  zo1ToObject,
 )

data InternalLogicError
  = ExpectedCardToBeAPermanentCard
  | ExpectedStackObjectToExist
  | InvalidPermanent (ZO 'ZBattlefield OT0)
  | InvalidPlayer (Object 'OTPlayer)
  | ImpossibleGameOver
  | NotSureWhatThisEntails
  deriving (Typeable)

deriving instance Eq InternalLogicError

deriving instance Ord InternalLogicError

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
  , exceptionInvalidPlayLand :: OpaqueGameState m -> Object 'OTPlayer -> InvalidPlayLand -> m ()
  , exceptionInvalidShuffle :: CardCount -> [CardIndex] -> m ()
  , exceptionInvalidStartingPlayer :: PlayerCount -> PlayerIndex -> m ()
  , promptActivateAbility :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe ActivateAbility)
  , promptCastSpell :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe CastSpell)
  , promptDebugMessage :: String -> m ()
  , promptGetStartingPlayer :: PlayerCount -> m PlayerIndex
  , promptPerformMulligan :: Object 'OTPlayer -> [Card ()] -> m Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptPickZO :: forall zone ot. IsZO zone ot => Object 'OTPlayer -> [ZO zone ot] -> m (ZO zone ot)
  , promptPlayLand :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe PlayLand)
  , promptShuffle :: CardCount -> Object 'OTPlayer -> m [CardIndex]
  }

data ActivateAbility :: Type where
  ActivateAbility :: ZO zone ot -> ActivatedAbility zone ot -> ActivateAbility

-- NB (305.9): Lands + other types can never be cast
-- Unfortuantely OTSpell intersects OTArtifactLand. Such is life.
-- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
-- though it's probably unnecessary for Authoring (thankfully).
newtype CastSpell :: Type where
  CastSpell :: SomeCard OTSpell -> CastSpell

data PlayLand :: Type where
  PlayLand :: IsZone zone => ZO zone OTLand -> PlayLand

data SpecialAction :: Type where
  SA_PlayLand :: PlayLand -> SpecialAction

data InvalidPlayLand :: Type where
  PlayLand_AtMaxLands :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_CannotPlayFromZone :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NoPriority :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotActive :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotALand :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotInZone :: ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotMainPhase :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_NotOwned :: IsZone zone => ZO zone OTLand -> InvalidPlayLand
  PlayLand_StackNonEmpty :: IsZone zone => ZO zone OTLand -> InvalidPlayLand

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

type TargetId = ObjectId

data AnyRequirement :: Type where
  AnyRequirement :: IsZO zone ot => Requirement zone ot -> AnyRequirement

data GameState (m :: Type -> Type) where
  GameState ::
    { magicCurrentTurn :: Int
    , magicGraveyardCards :: Map.Map (ZO 'ZGraveyard OT0) (Card ())
    , magicHandCards :: Map.Map (ZO 'ZHand OT0) (Card ())
    , magicManaBurn :: Bool
    , magicNextObjectDiscriminant :: ObjectDiscriminant
    , magicNextObjectId :: ObjectId
    , magicPermanents :: Map.Map (ZO 'ZBattlefield OT0) Permanent
    , magicPhaseStep :: PhaseStep
    , magicPlayers :: Map.Map (Object 'OTPlayer) Player -- contains all players
    , magicPlayerOrderAPNAP :: Stream.Stream (Object 'OTPlayer) -- does not contain losers
    , magicPlayerOrderPriority :: [Object 'OTPlayer] -- does not contain losers
    , magicPlayerOrderTurn :: Stream.Stream (Object 'OTPlayer) -- does not contain losers
    , magicPrompt :: Prompt m
    , magicStack :: Stack
    , magicStackEntryAbilityMap :: Map.Map (ZO 'ZStack OT0) (StackEntry 'Nothing)
    , magicStackEntryNonTribalMap :: Map.Map (ZO 'ZStack OT0) (StackEntry ( 'Just 'NonTribal))
    , magicStackEntryTribalMap :: Map.Map (ZO 'ZStack OT0) (StackEntry ( 'Just 'Tribal))
    , magicStartingPlayer :: Object 'OTPlayer
    , magicTargetProperties :: Map.Map TargetId AnyRequirement
    } ->
    GameState m
  deriving (Typeable)

data StackEntry (mTribal :: Maybe Tribal) = StackEntry
  { stackEntryTargets :: [TargetId]
  , stackEntryElected :: AnyElected 'Post 'Pre mTribal
  }

mkGameState :: GameInput m -> Maybe (GameState m)
mkGameState input = case playerObjects of
  [] -> Nothing
  oPlayer : _ ->
    Just
      GameState
        { magicCurrentTurn = 0
        , magicGraveyardCards = mempty
        , magicHandCards = mempty
        , magicManaBurn = False
        , magicNextObjectDiscriminant = (1 +) <$> DefaultObjectDiscriminant
        , magicNextObjectId = ObjectId playerCount
        , magicPermanents = mempty
        , magicPhaseStep = PSBeginningPhase UntapStep
        , magicPlayers = playerMap
        , magicPlayerOrderAPNAP = Stream.cycle playerObjects
        , magicPlayerOrderPriority = []
        , magicPlayerOrderTurn = Stream.cycle playerObjects
        , magicPrompt = gameInput_prompt input
        , magicStack = Stack []
        , magicStackEntryAbilityMap = mempty
        , magicStackEntryNonTribalMap = mempty
        , magicStackEntryTribalMap = mempty
        , magicStartingPlayer = oPlayer
        , magicTargetProperties = mempty
        }
 where
  format = gameInput_gameFormat input
  players = map (uncurry $ mkPlayer format) $ gameInput_decks input
  playerCount = length players
  playerObjects = map (idToObject . ObjectId) [0 .. playerCount - 1]
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
  , gameLosers :: [PlayerIndex] -- 👎👎👎
  }
  deriving (Typeable)

type Magic v rw m = Magic' (GameResult m) (GameState m) v rw m

type MagicEx ex v rw m a = ExceptT ex (Magic v rw m) a

type MagicCont v rw m a b = MagicEx (Magic v rw m a) v rw m b

runMagicEx :: (IsReadWrite rw, Monad m) => (Either ex a -> b) -> MagicEx ex v rw m a -> Magic v rw m b
runMagicEx f = fmap f . runExceptT

runMagicCont :: (IsReadWrite rw, Monad m) => (Either a b -> c) -> MagicCont v rw m a b -> Magic v rw m c
runMagicCont f = M.join . runMagicEx g
 where
  g = \case
    Left cont -> f . Left <$> cont
    Right b -> pure $ f $ Right b

queryMagic :: Monad m => OpaqueGameState m -> Magic 'Public 'RO m a -> m a
queryMagic (OpaqueGameState st) = runMagicRO st

playGame :: Monad m => GameInput m -> m (Maybe (GameResult m))
playGame input = case mkGameState input of
  Nothing -> do
    exceptionCantBeginGameWithoutPlayers $ gameInput_prompt input
    pure Nothing
  Just st ->
    runMagicRW st startGame <&> \case
      Left result -> Just result
      Right v -> absurd v

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m =
  m >>= \case
    Just x -> pure x
    Nothing -> untilJust m

class AndLike a where
  trueM :: Monad m => m a
  falseM :: Monad m => m a
  andM :: Monad m => [m a] -> m a

instance AndLike () where
  trueM = pure ()
  falseM = pure ()
  andM = sequence_

instance AndLike Bool where
  trueM = pure True
  falseM = pure False
  andM = \case
    [] -> pure True
    m : ms ->
      m >>= \case
        True -> andM ms
        False -> pure False

instance AndLike Legality where
  trueM = pure Legal
  falseM = pure Illegal
  andM m = do
    let bs = map (fmap fromLegality) m
    b <- andM bs
    pure $ toLegality b

instance AndLike a => AndLike (Maybe a) where
  trueM = Just <$> trueM
  falseM = pure Nothing
  andM = \case
    [] -> trueM
    m : ms ->
      m >>= \case
        Just x -> case ms of
          [] -> pure $ Just x
          _ -> andM ms
        Nothing -> pure Nothing

getAlivePlayerCount :: Monad m => Magic 'Public 'RO m PlayerCount
getAlivePlayerCount = undefined

getAPNAP :: Monad m => Magic v 'RO m (Stream.Stream (Object 'OTPlayer))
getAPNAP = internalFromPrivate $ gets magicPlayerOrderAPNAP

getActivePlayer :: Monad m => Magic 'Public 'RO m (Object 'OTPlayer)
getActivePlayer = Stream.head <$> getAPNAP

getPlayers :: Monad m => Magic 'Public 'RO m [Object 'OTPlayer]
getPlayers = do
  st <- internalFromPrivate get
  let ps = Map.assocs $ magicPlayers st
  pure $ map fst $ filter (not . playerLost . snd) ps

withEachPlayer_ :: (IsReadWrite rw, Monad m) => (Object 'OTPlayer -> Magic v rw m ()) -> Magic v rw m ()
withEachPlayer_ f = fromPublicRO getPlayers >>= mapM_ f

getPermanents :: Monad m => Magic v 'RO m [ZO 'ZBattlefield OT0]
getPermanents = internalFromPrivate $ gets $ Map.keys . magicPermanents

withEachPermanent ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OT0 -> Magic v rw m a) ->
  Magic v rw m [a]
withEachPermanent f = fromRO getPermanents >>= mapM f

withEachPermanent_ ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OT0 -> Magic v rw m ()) ->
  Magic v rw m ()
withEachPermanent_ f = fromRO getPermanents >>= mapM_ f

withEachControlledPermanent_ ::
  (IsReadWrite rw, Monad m) =>
  Object 'OTPlayer ->
  (ZO 'ZBattlefield OT0 -> Magic v rw m ()) ->
  Magic v rw m ()
withEachControlledPermanent_ oPlayer f = withEachPermanent_ $ \oPerm -> do
  perm <- internalFromPrivate $ fromRO $ getPermanent oPerm
  M.when (permanentController perm == oPlayer) $ f oPerm

-- (103)
startGame :: Monad m => Magic 'Private 'RW m Void
startGame = do
  determineStartingPlayer -- (103.1)
  withEachPlayer_ shuffleLibrary -- (103.2)
  pure () -- (103.3) See `mkPlayer`
  drawStartingHands -- (103.4)
  pure () -- (103.5) TODO: leylines and such
  pure () -- (103.6) TODO: planechase stuff

  -- NOTE: MagicCont is appropriate in order to nicely support cards like [Stasis] and [Time Stop]
  runMagicCont (either id id) untapStep

setPhaseStep :: PhaseStep -> Monad m => MagicCont 'Private 'RW m Void ()
setPhaseStep phaseStep = lift $ modify $ \st -> st{magicPhaseStep = phaseStep}

-- (502)
untapStep :: Monad m => MagicCont 'Private 'RW m Void Void
untapStep = do
  setPhaseStep $ PSBeginningPhase UntapStep
  lift $ do
    do
      n <- fromPublicRO getAlivePlayerCount
      ps <- fromRO $ gets magicPlayerOrderTurn
      modify $ \st ->
        st
          { magicCurrentTurn = magicCurrentTurn st + 1
          , magicPlayerOrderAPNAP = mkAPNAP n ps
          }
    oPlayer <- fromPublicRO getActivePlayer
    do
      player <- fromRO $ getPlayer oPlayer
      setPlayer oPlayer player{playerLandsPlayedThisTurn = 0}
    withEachControlledPermanent_ oPlayer togglePermanentPhase -- (502.1)
    pure () -- (502.2) TODO: day/night
    withEachControlledPermanent_ oPlayer (M.void . untapPermanent) -- (502.3) TODO: fine-grained untapping
    pure () -- (502.4) Rule states that players can't get priority, so nothing to do here.
  upkeepStep

togglePermanentPhase :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RW m ()
togglePermanentPhase oPerm = do
  perm <- fromRO $ getPermanent oPerm
  setPermanent
    oPerm
    perm
      { permanentPhased = case permanentPhased perm of
          PhasedIn -> PhasedOut
          PhasedOut -> PhasedIn
      }

upkeepStep :: Monad m => MagicCont 'Private 'RW m Void Void
upkeepStep = do
  setPhaseStep $ PSBeginningPhase UpkeepStep
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  drawStep

drawStep :: Monad m => MagicCont 'Private 'RW m Void Void
drawStep = do
  setPhaseStep $ PSBeginningPhase DrawStep
  lift $ do
    st <- fromRO get
    oActive <- fromPublicRO getActivePlayer
    case magicCurrentTurn st of
      1 -> pure () -- (103.7.*) TODO: this needs to account for game format
      _ -> drawCard oActive
    gainPriority oActive
  precombatMainPhase

precombatMainPhase :: Monad m => MagicCont 'Private 'RW m Void Void
precombatMainPhase = do
  setPhaseStep PSPreCombatMainPhase
  lift mainPhaseCommon
  beginningOfCombatStep

postcombatMainPhase :: Monad m => MagicCont 'Private 'RW m Void Void
postcombatMainPhase = do
  setPhaseStep PSPreCombatMainPhase
  lift mainPhaseCommon
  endStep

-- (505)
mainPhaseCommon :: Monad m => Magic 'Private 'RW m ()
mainPhaseCommon = do
  pure () -- (505.1) Rule just states nomenclature. Nothing special to do
  pure () -- (505.2) Rule just states this phase has no steps
  pure () -- (505.3) TODO: Archenemy
  pure () -- (505.4) TOOD: Sage lore counters
  oActive <- fromPublicRO getActivePlayer
  gainPriority oActive

gainPriority :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
gainPriority oPlayer = do
  pure () -- (117.5) TODO: state-based actions
  PlayerCount n <- fromPublicRO getAlivePlayerCount
  ps <- fromRO $ Stream.take n . Stream.dropWhile (/= oPlayer) <$> getAPNAP
  modify $ \st -> st{magicPlayerOrderPriority = ps}
  runMagicCont (either id absurd) runPriorityQueue

runPriorityQueue :: Monad m => MagicCont 'Private 'RW m () Void
runPriorityQueue = do
  lift (fromRO $ gets magicPlayerOrderPriority) >>= \case
    [] -> throwE resolveTopOfStack -- (117.4)
    oPlayer : oPlayers -> do
      askCastSpell oPlayer -- (117.1a)
      askActivateAbility oPlayer -- (117.1b) (117.1d)
      askSpecialAction oPlayer -- (117.1c)
      lift $ modify $ \st -> st{magicPlayerOrderPriority = oPlayers} -- (117.3d)
      runPriorityQueue

getPlayerWithPriority :: Monad m => Magic 'Public 'RO m (Maybe (Object 'OTPlayer))
getPlayerWithPriority = do
  oPlayers <- internalFromPrivate $ gets magicPlayerOrderPriority
  pure $ case oPlayers of
    oPlayer : _ -> Just oPlayer
    [] -> Nothing

getHasPriority :: Monad m => Object 'OTPlayer -> Magic 'Public 'RO m Bool
getHasPriority oPlayer =
  getPlayerWithPriority <&> \case
    Nothing -> False
    Just p -> oPlayer == p

-- (117.1a)
askCastSpell :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askCastSpell oPlayer = do
  pure () -- TODO
  let spellIsCast = False
  pure () -- (305.9) TODO: dont forget this rule: lands + other types can never be cast
  case spellIsCast of
    True -> throwE $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1b) (117.1d)
askActivateAbility :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askActivateAbility oPlayer = do
  pure () -- TODO
  let abilityIsActivated = False
  case abilityIsActivated of
    True -> throwE $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1c)
askSpecialAction :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askSpecialAction oPlayer = do
  askPlayLand oPlayer

performSpecialAction :: Monad m => Object 'OTPlayer -> SpecialAction -> Magic 'Private 'RW m Legality
performSpecialAction oPlayer = \case
  SA_PlayLand (PlayLand oLand) -> playLand oPlayer oLand

data Legality
  = Legal
  | Illegal
  deriving (Eq, Ord, Show, Typeable)

rewindIllegal :: Monad m => Magic 'Private 'RW m Legality -> Magic 'Private 'RW m Bool
rewindIllegal m = do
  -- (104.1) (727.1) XXX: Is it possible for GameResult to be thrown during an illegal action?
  -- If so, is should it sometimes/always/never be rewound?
  let m' = magicCatch m $ \case
        GameResult{gameWinners = []} -> pure Illegal
        ex -> magicThrow ex
  st <- fromRO get
  m' >>= \case
    Legal -> pure True
    Illegal -> put st >> pure False

activateAbility :: forall m. Monad m => Object 'OTPlayer -> ActivateAbility -> Magic 'Private 'RW m Legality
activateAbility oPlayer = \case
  ActivateAbility oThis ability -> go oThis ability
 where
  go :: forall zone ot. ZO zone ot -> ActivatedAbility zone ot -> Magic 'Private 'RW m Legality
  go oThis = \case
    Ability inCost inEffect -> do
      let isThisInCorrectZone = True -- TODO
          isController = True -- TODO
      case (isThisInCorrectZone, isController) of
        (False, _) -> pure Illegal -- TODO prompt complaint
        (_, False) -> pure Illegal -- TODO prompt complaint
        (True, True) -> do
          abilityId <- newObjectId
          let zoAbility = toZO0 @ 'ZStack abilityId

              toMaybe = \case
                Legal -> Just ()
                Illegal -> Nothing

              cont cost effect =
                fmap toMaybe $
                  payElectedAndPutOnStack @ 'SKAbility $
                    ElectedActivatedAbility oPlayer oThis cost effect

          playPendingStackItem @ 'Nothing zoAbility inCost inEffect cont <&> \case
            Nothing -> Illegal
            Just{} -> Legal

playPendingStackItem ::
  forall mTribal m ot x.
  (IsMaybeTribal mTribal, AndLike x) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  ElectPrePost (Cost ot) ot ->
  ElectPrePost (Effect 'OneShot) ot ->
  (Pending (Cost ot) ot -> Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingStackItem zoStack inCost inEffect goCont = performElections @mTribal zoStack (goCost . Pending) inCost
 where
  goCost :: Pending (Cost ot) ot -> Magic 'Private 'RW m (Maybe x)
  goCost cost =
    let goElectEffect :: ElectPrePost (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)
        goElectEffect = \case
          Elect effect -> goEffect $ Pending effect
          _ -> undefined

        goEffect :: Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)
        goEffect = goCont cost
     in goElectEffect inEffect

performElections ::
  forall mTribal ot m p el x.
  (IsMaybeTribal mTribal, Monad m, AndLike x) =>
  ZO 'ZStack OT0 ->
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect p el ot ->
  Magic 'Private 'RW m (Maybe x)
performElections zoStack goTerm = \case
  All masked -> electAll goRec masked
  CardTypeDef def -> goTerm def
  Choose oPlayer thisToElect -> electA @mTribal Choose' zoStack goRec oPlayer thisToElect
  ElectCase case_ -> performCase goRec case_
  Target oPlayer thisToElect -> electA @mTribal Target' zoStack goRec oPlayer thisToElect
  _ -> undefined
 where
  goRec = performElections @mTribal zoStack goTerm

data CastEnv (tribal :: Tribal) (ot :: Type) :: Type where
  CastEnv ::
    { castEnv_ElectedTribal ::
        Object 'OTPlayer ->
        Card ot ->
        CardTypeDef 'Tribal ot ->
        PendingReady 'Pre (Cost ot) ot ->
        PendingReady 'Pre (Effect 'OneShot) ot ->
        Elected 'Pre 'Pre ( 'Just 'Tribal) ot
    , castEnv_ElectedNonTribal ::
        Object 'OTPlayer ->
        Card ot ->
        CardTypeDef 'NonTribal ot ->
        PendingReady 'Pre (Cost ot) ot ->
        PendingReady 'Pre (Effect 'OneShot) ot ->
        Elected 'Pre 'Pre ( 'Just 'NonTribal) ot
    , castEnv_effect :: CardTypeDef 'NonTribal ot -> ElectPrePost (Effect 'OneShot) ot
    , castEnv_cost :: CardTypeDef 'NonTribal ot -> ElectPrePost (Cost ot) ot
    } ->
    CastEnv tribal ot

castSpell :: forall m. Monad m => Object 'OTPlayer -> CastSpell -> Magic 'Private 'RW m Legality
castSpell oCaster (CastSpell someCard) = case someCard of
  Some6a (SomeArtifact card) -> undefined card
  Some6b (SomeCreature card) -> undefined card
  Some6c (SomeEnchantment card) -> undefined card
  Some6d (SomeInstant card@Card{}) ->
    castSpell' @ 'NonTribal oCaster card $
      CastEnv
        { castEnv_ElectedTribal = ElectedInstant
        , castEnv_ElectedNonTribal = ElectedInstant
        , castEnv_effect = instant_effect
        , castEnv_cost = instant_cost
        }
  Some6d (SomeInstant card@TribalCard{}) ->
    castSpell' @ 'Tribal oCaster card $
      CastEnv
        { castEnv_ElectedTribal = ElectedInstant
        , castEnv_ElectedNonTribal = ElectedInstant
        , castEnv_effect = instant_effect
        , castEnv_cost = instant_cost
        }
  Some6e (SomePlaneswalker card) -> undefined card
  Some6f (SomeSorcery card@Card{}) ->
    castSpell' @ 'NonTribal oCaster card $
      CastEnv
        { castEnv_ElectedTribal = ElectedSorcery
        , castEnv_ElectedNonTribal = ElectedSorcery
        , castEnv_effect = sorcery_effect
        , castEnv_cost = sorcery_cost
        }
  Some6f (SomeSorcery card@TribalCard{}) ->
    castSpell' @ 'Tribal oCaster card $
      CastEnv
        { castEnv_ElectedTribal = ElectedSorcery
        , castEnv_ElectedNonTribal = ElectedSorcery
        , castEnv_effect = sorcery_effect
        , castEnv_cost = sorcery_cost
        }
  Some6ab (SomeArtifactCreature card) -> undefined card
  Some6bc (SomeEnchantmentCreature card) -> undefined card

castSpell' ::
  forall tribal otk ot m.
  (ot ~ OT otk, Monad m) =>
  IsTribal tribal =>
  Object 'OTPlayer ->
  Card (OT otk) ->
  CastEnv tribal ot ->
  Magic 'Private 'RW m Legality
castSpell' oCaster card env = case card of
  Card _name wCard (T1 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId)
  Card _name wCard (T2 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId, viewThis thisId)
  TribalCard _name wCard (T1 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId)
  TribalCard _name wCard (T2 thisToElectDef) -> do
    thisId <- newObjectId
    goLegality wCard thisToElectDef (viewThis thisId, viewThis thisId)
 where
  viewThis :: (IsZone zone, IsObjectType a) => ObjectId -> ZO zone (OT1 a)
  viewThis = ZO singZone . O1 . idToObject

  goLegality ::
    forall ret tribal' this.
    IsTribal tribal' =>
    ret ~ Maybe (Elected 'Pre 'Pre ( 'Just tribal') ot) =>
    WCard ot ->
    (this -> Elect 'Pre (CardTypeDef tribal' ot) ot) ->
    this ->
    Magic 'Private 'RW m Legality
  goLegality wCard thisToElectDef zoThis = do
    mElected <- goMaybe wCard thisToElectDef zoThis
    pure $ case mElected of
      Nothing -> Illegal
      Just () -> Legal

  goMaybe ::
    forall ret tribal' this.
    IsTribal tribal' =>
    ret ~ Maybe () =>
    WCard ot ->
    (this -> Elect 'Pre (CardTypeDef tribal' ot) ot) ->
    this ->
    Magic 'Private 'RW m ret
  goMaybe wCard thisToElectDef zoThis = do
    spellId <- newObjectId
    let zoSpell = toZO0 @ 'ZStack spellId

        goElectDef :: Elect 'Pre (CardTypeDef tribal' ot) ot -> Magic 'Private 'RW m ret
        goElectDef = performElections @( 'Just tribal) zoSpell goDef

        goDef :: CardTypeDef tribal' ot -> Magic 'Private 'RW m ret
        goDef def = goDef' def def
         where
          goDef' topDef bottomDef = case bottomDef of
            ArtifactLandDef{} -> undefined -- TODO: Not a spell
            LandDef{} -> undefined -- TODO: Not a spell
            --
            ArtifactDef{} -> goDefNonTribal topDef bottomDef
            ArtifactCreatureDef{} -> goDefNonTribal topDef bottomDef
            CreatureDef{} -> goDefNonTribal topDef bottomDef
            EnchantmentDef{} -> goDefNonTribal topDef bottomDef
            EnchantmentCreatureDef{} -> goDefNonTribal topDef bottomDef
            PlaneswalkerDef{} -> goDefNonTribal topDef bottomDef
            --
            InstantDef{} -> goDefNonTribal topDef bottomDef
            SorceryDef{} -> goDefNonTribal topDef bottomDef
            --
            TribalDef{} -> goDefTribal topDef bottomDef
            --
            VariableDef varToDef -> do
              let var = ReifiedVariable undefined undefined
              goDef' topDef $ varToDef var

        goDefNonTribal :: CardTypeDef 'NonTribal ot -> CardTypeDef 'NonTribal ot -> Magic 'Private 'RW m ret
        goDefNonTribal topDef def = playPendingStackItem @( 'Just 'NonTribal) zoSpell (castEnv_cost env def) (castEnv_effect env def) $ \cost effect -> do
          case wCard of
            WCardSorcery -> do
              legality <- payElectedAndPutOnStack @ 'SKSpell $ castEnv_ElectedNonTribal env oCaster card topDef cost effect
              pure $ case legality of
                Illegal -> Nothing
                Legal -> Just ()
            _ -> undefined

        goDefTribal :: CardTypeDef 'Tribal ot -> CardTypeDef 'Tribal ot -> Magic 'Private 'RW m ret
        goDefTribal topDef (TribalDef _ _ def) = playPendingStackItem @( 'Just 'Tribal) zoSpell (castEnv_cost env def) (castEnv_effect env def) $ \cost effect -> do
          case wCard of
            WCardSorcery -> do
              legality <- payElectedAndPutOnStack @ 'SKSpell $ castEnv_ElectedTribal env oCaster card topDef cost effect
              pure $ case legality of
                Illegal -> Nothing
                Legal -> Just ()
            _ -> undefined

    goElectDef $ thisToElectDef zoThis

data Selection
  = Choose'
  | Target'

electA ::
  forall mTribal p zone m el ot x.
  (IsMaybeTribal mTribal, IsZO zone ot, Monad m) =>
  Selection ->
  ZO 'ZStack OT0 ->
  (Elect p el ot -> Magic 'Private 'RW m (Maybe x)) ->
  OPlayer ->
  WithMaskedObject zone (Elect p el ot) ->
  Magic 'Private 'RW m (Maybe x)
electA sel zoStack goElect oPlayer = \case
  M1 reqs zoToElect -> go reqs zoToElect
  M2 reqs zoToElect -> go reqs zoToElect
  M3 reqs zoToElect -> go reqs zoToElect
  M4 reqs zoToElect -> go reqs zoToElect
  M5 reqs zoToElect -> go reqs zoToElect
  _ -> undefined
 where
  go ::
    (IsZO zone ot', Eq (ZO zone ot')) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> Elect p el ot) ->
    Magic 'Private 'RW m (Maybe x)
  go reqs zoToElect = do
    prompt <- fromRO $ gets magicPrompt
    fromRO (findObjectsSatisfying (RAnd reqs)) >>= \case
      [] -> pure Nothing
      zos -> do
        zo <- lift $
          untilJust $ do
            zo <- promptPickZO prompt (zo1ToObject oPlayer) zos
            pure $ case zo `elem` zos of
              False -> Nothing
              True -> Just zo
        zo' <- case sel of
          Choose' -> pure zo
          Target' -> newTarget @mTribal zoStack zo $ RAnd reqs
        let elect = zoToElect zo'
        goElect elect

newTarget ::
  forall mTribal zone ot m.
  (IsMaybeTribal mTribal, Monad m, IsZO zone ot) =>
  ZO 'ZStack OT0 ->
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RW m (ZO zone ot)
newTarget zoStack zoTargetBase req = do
  discr <- fromRO $ gets magicNextObjectDiscriminant
  let (ZO sZone objN) = zoTargetBase
      go :: Object a -> Object a
      go = \case
        Object wit oldDisc i ->
          assert (oldDisc == DefaultObjectDiscriminant) $ Object wit discr i
      objN' = mapObjectN go objN
      zoTarget = ZO sZone objN'
  modify $ \st ->
    let targetId = getObjectId zoTarget
        propMap = magicTargetProperties st
        propMap' = Map.insert targetId (AnyRequirement req) propMap
     in case singMaybeTribal @mTribal of
          SNothingTribal ->
            let entry = case Map.lookup zoStack $ magicStackEntryAbilityMap st of
                  Just x -> x
                  Nothing -> error $ show ExpectedStackObjectToExist
                entry' = entry{stackEntryTargets = targetId : stackEntryTargets entry}
                itemMap = magicStackEntryAbilityMap st
                itemMap' = Map.insert zoStack entry' itemMap
             in st
                  { magicNextObjectDiscriminant = (+ 1) <$> discr
                  , magicStackEntryAbilityMap = itemMap'
                  , magicTargetProperties = propMap'
                  }
          SJustTribal ->
            let entry = case Map.lookup zoStack $ magicStackEntryTribalMap st of
                  Just x -> x
                  Nothing -> error $ show ExpectedStackObjectToExist
                entry' = entry{stackEntryTargets = targetId : stackEntryTargets entry}
                itemMap = magicStackEntryTribalMap st
                itemMap' = Map.insert zoStack entry' itemMap
             in st
                  { magicNextObjectDiscriminant = (+ 1) <$> discr
                  , magicStackEntryTribalMap = itemMap'
                  , magicTargetProperties = propMap'
                  }
          SJustNonTribal ->
            let entry = case Map.lookup zoStack $ magicStackEntryNonTribalMap st of
                  Just x -> x
                  Nothing -> error $ show ExpectedStackObjectToExist
                entry' = entry{stackEntryTargets = targetId : stackEntryTargets entry}
                itemMap = magicStackEntryNonTribalMap st
                itemMap' = Map.insert zoStack entry' itemMap
             in st
                  { magicNextObjectDiscriminant = (+ 1) <$> discr
                  , magicStackEntryNonTribalMap = itemMap'
                  , magicTargetProperties = propMap'
                  }
  pure zoTarget

electAll ::
  forall zone m el ot a.
  (IsZO zone ot, Monad m, AndLike a) =>
  (Elect 'Post el ot -> Magic 'Private 'RW m a) ->
  WithMaskedObject zone (Elect 'Post el ot) ->
  Magic 'Private 'RW m a
electAll goElect = \case
  M1 reqs zoToElect -> go reqs zoToElect
  M2 reqs zoToElect -> go reqs zoToElect
  M3 reqs zoToElect -> go reqs zoToElect
  M4 reqs zoToElect -> go reqs zoToElect
  M5 reqs zoToElect -> go reqs zoToElect
  _ -> undefined
 where
  go ::
    (IsZO zone ot', Eq (ZO zone ot')) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> Elect 'Post el ot) ->
    Magic 'Private 'RW m a
  go reqs zoToElect = do
    zos <- fromRO $ findObjectsSatisfying $ RAnd reqs
    andM $ map (goElect . zoToElect) zos

data PendingReady (p :: PrePost) (el :: Type) (ot :: Type) where
  Pending :: {unPending :: Elect 'Post el ot} -> Pending el ot
  Ready :: {unReady :: el} -> Ready el ot

type Pending = PendingReady 'Pre

type Ready = PendingReady 'Post

data Elected (pCost :: PrePost) (pEffect :: PrePost) (mTribal :: Maybe Tribal) (ot :: Type) :: Type where
  ElectedActivatedAbility ::
    IsZO zone ot =>
    { electedActivatedAbility_controller :: Object 'OTPlayer
    , electedActivatedAbility_this :: ZO zone ot
    , electedActivatedAbility_cost :: PendingReady pCost (Cost ot) ot
    , electedActivatedAbility_effect :: PendingReady pEffect (Effect 'OneShot) ot
    } ->
    Elected pCost pEffect 'Nothing ot
  ElectedInstant ::
    IsTribal tribal =>
    { electedInstant_controller :: Object 'OTPlayer
    , electedInstant_card :: Card OTInstant
    , electedInstant_def :: CardTypeDef tribal OTInstant
    , electedInstant_cost :: PendingReady pCost (Cost OTInstant) OTInstant
    , electedInstant_effect :: PendingReady pEffect (Effect 'OneShot) OTInstant
    } ->
    Elected pCost pEffect ( 'Just tribal) OTInstant
  ElectedSorcery ::
    IsTribal tribal =>
    { electedSorcery_controller :: Object 'OTPlayer
    , electedSorcery_card :: Card OTSorcery
    , electedSorcery_def :: CardTypeDef tribal OTSorcery
    , electedSorcery_cost :: PendingReady pCost (Cost OTSorcery) OTSorcery
    , electedSorcery_effect :: PendingReady pEffect (Effect 'OneShot) OTSorcery
    } ->
    Elected pCost pEffect ( 'Just tribal) OTSorcery
  deriving (Typeable)

electedObject_controller :: Elected pCost pEffect tribal ot -> Object 'OTPlayer
electedObject_controller elected = ($ elected) $ case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_controller
  ElectedInstant{} -> electedInstant_controller
  ElectedSorcery{} -> electedSorcery_controller

electedObject_cost :: Elected pCost pEffect tribal ot -> PendingReady pCost (Cost ot) ot
electedObject_cost elected = ($ elected) $ case elected of
  ElectedActivatedAbility{} -> electedActivatedAbility_cost
  ElectedInstant{} -> electedInstant_cost
  ElectedSorcery{} -> electedSorcery_cost

setElectedObject_def ::
  IsTribal tribal' =>
  CardTypeDef tribal' ot ->
  Elected pCost pEffect ( 'Just tribal) ot ->
  Elected pCost pEffect ( 'Just tribal') ot
setElectedObject_def def = \case
  ElectedInstant controller card _def cost effect -> ElectedInstant controller card def cost effect
  ElectedSorcery controller card _def cost effect -> ElectedSorcery controller card def cost effect

setElectedCost ::
  Elected pCost pEffect tribal ot ->
  PendingReady pCost' (Cost ot) ot ->
  Elected pCost' pEffect tribal ot
setElectedCost elected cost = case elected of
  ElectedActivatedAbility{} -> elected{electedActivatedAbility_cost = cost}
  ElectedInstant{} -> elected{electedInstant_cost = cost}
  ElectedSorcery{} -> elected{electedSorcery_cost = cost}

data AnyElected (pCost :: PrePost) (pEffect :: PrePost) (mTribal :: Maybe Tribal) :: Type where
  AnyElected :: Elected pCost pEffect mTribal ot -> AnyElected pCost pEffect mTribal
  deriving (Typeable)

data StackKind
  = SKAbility
  | SKSpell

class PayElected (sk :: StackKind) (ot :: Type) where
  payElectedAndPutOnStack :: (IsMaybeTribal mTribal, Monad m) => Elected 'Pre 'Pre mTribal ot -> Magic 'Private 'RW m Legality

instance PayElected 'SKAbility ot where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackAbility . ZO SZStack . toObject2' . idToObject @ 'OTActivatedAbility

instance PayElected 'SKSpell OTInstant where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTInstant

instance PayElected 'SKSpell OTSorcery where
  payElectedAndPutOnStack = payElectedAndPutOnStack' $ StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTSorcery

data SMaybeTribal (mTribal :: Maybe Tribal) :: Type where
  SNothingTribal :: SMaybeTribal 'Nothing
  SJustTribal :: SMaybeTribal ( 'Just 'Tribal)
  SJustNonTribal :: SMaybeTribal ( 'Just 'NonTribal)
  deriving (Typeable)

class IsMaybeTribal (mTribal :: Maybe Tribal) where
  singMaybeTribal :: SMaybeTribal mTribal

instance IsMaybeTribal 'Nothing where
  singMaybeTribal = SNothingTribal

instance IsTribal tribal => IsMaybeTribal ( 'Just tribal) where
  singMaybeTribal = case singTribal @tribal of
    STribal -> SJustTribal
    SNonTribal -> SJustNonTribal

payElectedAndPutOnStack' ::
  forall mTribal ot m.
  (IsMaybeTribal mTribal, Monad m) =>
  (ObjectId -> StackObject) ->
  Elected 'Pre 'Pre mTribal ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStack' idToStackObject elected =
  let goElectCost :: Monad m => Elect 'Post (Cost ot) ot -> Magic 'Private 'RW m Legality
      goElectCost = \case
        All masked -> electAll goElectCost masked
        Cost cost -> goCost cost
        _ -> undefined

      goCost :: Monad m => Cost ot -> Magic 'Private 'RW m Legality
      goCost cost =
        payCost (electedObject_controller elected) cost >>= \case
          Illegal -> pure Illegal
          Legal -> do
            stackId <- newObjectId
            let zoStack = idToStackObject stackId
                entry =
                  StackEntry
                    { stackEntryTargets = []
                    , stackEntryElected = AnyElected $ setElectedCost elected $ Ready cost
                    }
            modify $ \st -> st{magicStack = Stack $ zoStack : unStack (magicStack st)}
            modify $ \st -> case singMaybeTribal @mTribal of
              SNothingTribal -> st{magicStackEntryAbilityMap = Map.insert (toZO0 stackId) entry $ magicStackEntryAbilityMap st}
              SJustNonTribal -> st{magicStackEntryNonTribalMap = Map.insert (toZO0 stackId) entry $ magicStackEntryNonTribalMap st}
              SJustTribal -> st{magicStackEntryTribalMap = Map.insert (toZO0 stackId) entry $ magicStackEntryTribalMap st}
            pure Legal
   in goElectCost $ unPending $ electedObject_cost elected

toLegality :: Bool -> Legality
toLegality = \case
  True -> Legal
  False -> Illegal

fromLegality :: Legality -> Bool
fromLegality = \case
  Legal -> True
  Illegal -> False

payCost :: Monad m => Object 'OTPlayer -> Cost ot -> Magic 'Private 'RW m Legality
payCost oPlayer = \case
  AndCosts costs -> payAndCosts oPlayer costs
  ManaCost manaCost -> payManaCost oPlayer manaCost
  OrCosts costs -> payOrCosts oPlayer costs
  TapCost _wPerm reqs -> payTapCost oPlayer $ RAnd reqs
  _ -> undefined

class FindMana manas var | manas -> var where
  findMana ::
    manas ->
    ( forall snow color.
      (IsSnow snow, IsManaNoVar snow color) =>
      Mana var snow color ->
      Maybe x
    ) ->
    Maybe x

instance FindMana (ManaCost var) var where
  findMana (ManaCost' w u b r g c x s) f =
    getFirst $ mconcat $ map First [f w, f u, f b, f r, f g, f c, f x, f s]

instance IsSnow snow => FindMana (ManaPool snow) 'NoVar where
  findMana (ManaPool w u b r g c) f =
    getFirst $ mconcat $ map First [f w, f u, f b, f r, f g, f c]

instance FindMana CompleteManaPool 'NoVar where
  findMana pool f =
    getFirst $ mconcat $ map First [poolNonSnow pool `findMana` f, poolSnow pool `findMana` f]

-- TODO: Give this a legit impl.
-- TODO: Need to prompt for generic mana payments
payManaCost :: Monad m => Object 'OTPlayer -> ManaCost 'Var -> Magic 'Private 'RW m Legality
payManaCost oPlayer (forceVars -> cost) =
  fromRO (findPlayer oPlayer) >>= \case
    Nothing -> pure Illegal
    Just player -> do
      let pool = poolNonSnow $ playerMana player
          w = poolWhite pool - costWhite cost
          u = poolBlue pool - costBlue cost
          b = poolBlack pool - costBlack cost
          r = poolRed pool - costRed cost
          g = poolGreen pool - costGreen cost
          c = poolColorless pool - costColorless cost
          pool' = ManaPool w u b r g c
          isBad mana = case mana < 0 of
            True -> Nothing
            False -> Just ()
      case findMana pool' isBad of
        Just () -> pure Illegal
        Nothing -> do
          setPlayer oPlayer player{playerMana = (playerMana player){poolNonSnow = pool'}}
          pure Legal

payTapCost ::
  (Monad m, IsZO 'ZBattlefield ot) =>
  Object 'OTPlayer ->
  Requirement 'ZBattlefield ot ->
  Magic 'Private 'RW m Legality
payTapCost oPlayer req =
  fromRO (findObjectsSatisfying $ RAnd [ControlledBy $ ZO SZBattlefield $ O1 oPlayer, req]) >>= \case
    [] -> pure Illegal
    zos -> do
      prompt <- fromRO $ gets magicPrompt
      zo <- lift $
        untilJust $ do
          zo <- promptPickZO prompt oPlayer zos
          pure $ case zo `elem` zos of
            False -> Nothing
            True -> Just zo
      tapPermanent (toZO0 zo) <&> toLegality

untapPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RW m Bool
untapPermanent oPerm = do
  perm <- fromRO $ getPermanent oPerm
  setPermanent oPerm perm{permanentTapped = Untapped}
  pure $ permanentTapped perm /= Untapped

tapPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RW m Bool
tapPermanent oPerm = do
  perm <- fromRO $ getPermanent oPerm
  setPermanent oPerm perm{permanentTapped = Tapped}
  pure $ permanentTapped perm /= Tapped

allZoneObjects :: forall m zone ot. (Monad m, IsZO zone ot) => Magic 'Private 'RO m [ZO zone ot]
allZoneObjects = case singZone @zone of
  SZBattlefield -> case indexOT @ot of
    objectTypes ->
      let goPerms :: ObjectType -> Magic 'Private 'RO m [ZO zone ot]
          goPerms ot = fmap catMaybes $
            withEachPermanent $ \oPerm -> do
              perm <- getPermanent oPerm
              let goPerm ::
                    forall a x.
                    IsObjectType a =>
                    (Permanent -> Maybe x) ->
                    Maybe (ZO 'ZBattlefield ot)
                  goPerm viewPerm = case viewPerm perm of
                    Nothing -> Nothing
                    Just{} -> objectToZO $ idToObject @a $ getObjectId oPerm
              pure $ case ot of
                OTArtifact -> goPerm @ 'OTArtifact permanentArtifact
                OTCreature -> goPerm @ 'OTCreature permanentCreature
                OTEnchantment -> goPerm @ 'OTEnchantment undefined
                OTLand -> goPerm @ 'OTLand permanentLand
                OTPlaneswalker -> goPerm @ 'OTPlaneswalker undefined
                _ -> Nothing
          goPlayers :: ObjectType -> Magic 'Private 'RO m [ZO zone ot]
          goPlayers = \case
            OTPlayer -> mapMaybe objectToZO <$> fromPublic getPlayers
            _ -> pure []
          goRec :: [ObjectType] -> Magic 'Private 'RO m (DList.DList (ZO zone ot))
          goRec = \case
            [] -> pure DList.empty
            ot : ots -> do
              oPerms <- goPerms ot
              oPlayers <- goPlayers ot
              oRecs <- goRec ots
              pure $ DList.fromList oPerms <> DList.fromList oPlayers <> oRecs
       in DList.toList <$> goRec objectTypes
  _ -> undefined

objectSatisfies ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
objectSatisfies zo = \case
  ControlledBy oPlayer -> objectSatisfiesControlledBy zo oPlayer
  Is _wAny zo' -> objectSatisfiesIs zo zo'
  Not req -> objectSatisfiesNot zo req
  RAnd reqs -> objectSatisfiesRAnd zo reqs
  ROr reqs -> objectSatisfiesROr zo reqs
  _ -> undefined

objectSatisfiesControlledBy ::
  forall m zone ot.
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  OPlayer ->
  Magic 'Private 'RO m Bool
objectSatisfiesControlledBy oAny oPlayer = case singZone @zone of
  SZBattlefield ->
    findPermanent (toZO0 oAny) <&> \case
      Nothing -> getObjectId oAny == getObjectId oPlayer -- TODO: [Mindslaver]
      Just perm -> getObjectId (permanentController perm) == getObjectId oPlayer
  _ -> undefined

objectSatisfiesIs :: (Monad m, IsZO zone ot) => ZO zone ot -> ZO zone ot -> Magic 'Private 'RO m Bool
objectSatisfiesIs zo zo' = pure $ getObjectId zo == getObjectId zo'

objectSatisfiesNot ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
objectSatisfiesNot zo = fmap not . objectSatisfies zo

objectSatisfiesRAnd ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  [Requirement zone ot] ->
  Magic 'Private 'RO m Bool
objectSatisfiesRAnd zo = \case
  [] -> pure True
  req : reqs ->
    objectSatisfies zo req >>= \case
      False -> pure False
      True -> objectSatisfiesRAnd zo reqs

objectSatisfiesROr ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  [Requirement zone ot] ->
  Magic 'Private 'RO m Bool
objectSatisfiesROr zo = \case
  [] -> pure False
  req : reqs ->
    objectSatisfies zo req >>= \case
      True -> pure True
      False -> objectSatisfiesROr zo reqs

requirementFilter ::
  (Monad m, IsZO zone ot) =>
  Requirement zone ot ->
  [ZO zone ot] ->
  Magic 'Private 'RO m [ZO zone ot]
requirementFilter req = M.filterM (`objectSatisfies` req)

findObjectsSatisfying ::
  (Monad m, IsZO zone ot) =>
  Requirement zone ot ->
  Magic 'Private 'RO m [ZO zone ot]
findObjectsSatisfying req = allZoneObjects >>= requirementFilter req

payAndCosts :: Monad m => Object 'OTPlayer -> [Cost ot] -> Magic 'Private 'RW m Legality
payAndCosts oPlayer = \case
  [] -> pure Legal
  cost : costs ->
    payCost oPlayer cost >>= \case
      Illegal -> pure Illegal
      Legal -> payAndCosts oPlayer costs

payOrCosts :: Monad m => Object 'OTPlayer -> [Cost ot] -> Magic 'Private 'RW m Legality
payOrCosts oPlayer = \case
  [] -> pure Illegal
  cost : _costs ->
    payCost oPlayer cost >>= \case
      Legal -> pure Legal -- TODO: Offer other choices
      Illegal -> undefined

data PlayLandReqs = PlayLandReqs
  { playLandReqs_hasPriority :: Bool
  , playLandReqs_isActive :: Bool
  , playLandReqs_isMainPhase :: Bool
  , playLandReqs_stackEmpty :: Bool
  , playLandReqs_atMaxLands :: Bool
  }
  deriving (Eq, Ord, Show, Typeable)

-- Unfortunately pattern synonyms won't contribute to exhaustiveness checking.
pattern PlayLandReqs_Satisfied :: PlayLandReqs
pattern PlayLandReqs_Satisfied =
  PlayLandReqs
    { playLandReqs_hasPriority = True
    , playLandReqs_isActive = True
    , playLandReqs_isMainPhase = True
    , playLandReqs_stackEmpty = True
    , playLandReqs_atMaxLands = False
    }

getPlayLandReqs :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m PlayLandReqs
getPlayLandReqs oPlayer = do
  st <- internalFromPrivate $ fromRO get
  player <- fromRO $ getPlayer oPlayer
  let landsPlayed = playerLandsPlayedThisTurn player
      maxLands = 1
      isMainPhase = magicPhaseStep st `elem` [PSPreCombatMainPhase, PSPostCombatMainPhase]
      Stack stack = magicStack st
  oActive <- fromPublic getActivePlayer
  hasPriority <- fromPublic $ getHasPriority oPlayer
  pure
    PlayLandReqs
      { playLandReqs_hasPriority = hasPriority -- (116.2a)
      , playLandReqs_isActive = oPlayer == oActive -- (116.2a)
      , playLandReqs_isMainPhase = isMainPhase -- (116.2a)
      , playLandReqs_stackEmpty = null stack -- (116.2a)
      , playLandReqs_atMaxLands = landsPlayed >= maxLands -- (305.2)
      }

askPlayLand :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askPlayLand oPlayer = do
  reqs <- lift $ fromRO $ getPlayLandReqs oPlayer
  case reqs of
    PlayLandReqs_Satisfied -> do
      st <- lift $ fromRO get
      let opaque = OpaqueGameState st
          prompt = magicPrompt st
      mSpecial <- lift $ lift $ promptPlayLand prompt opaque oPlayer
      case mSpecial of
        Nothing -> pure ()
        Just special -> do
          isLegal <- lift $ rewindIllegal $ playLand oPlayer special
          throwE $ case isLegal of
            True -> gainPriority oPlayer -- (117.3c)
            False -> runMagicCont (either id id) $ askPlayLand oPlayer
    _ -> pure ()

class PlayLand' land where
  playLand :: Monad m => Object 'OTPlayer -> land -> Magic 'Private 'RW m Legality

instance PlayLand' PlayLand where
  playLand oPlayer (PlayLand oLand) = playLand oPlayer oLand

instance IsZone zone => PlayLand' (ZO zone OTLand) where
  playLand :: forall m. Monad m => Object 'OTPlayer -> ZO zone OTLand -> Magic 'Private 'RW m Legality
  playLand oPlayer oLand = do
    st <- internalFromPrivate $ fromRO get
    reqs <- fromRO $ getPlayLandReqs oPlayer
    player <- fromRO $ getPlayer oPlayer
    let opaque = OpaqueGameState st
        prompt = magicPrompt st
        hand = playerHand player
        --
        invalid :: (ZO zone OTLand -> InvalidPlayLand) -> Magic 'Private 'RW m Legality
        invalid ex = do
          lift $ exceptionInvalidPlayLand prompt opaque oPlayer $ ex oLand
          pure Illegal
        --
        success :: Card () -> Magic 'Private 'RW m ()
        success card = do
          setPlayer
            oPlayer
            player
              { playerLandsPlayedThisTurn = playerLandsPlayedThisTurn player + 1
              , playerHand = case removeCard card hand of
                  Nothing -> assert False hand
                  Just hand' -> hand'
              }
          i <- newObjectId
          let oLand' = toZO0 i
              perm = case cardToPermanent oPlayer card of
                Nothing -> error $ show ExpectedCardToBeAPermanentCard
                Just perm' -> perm'
          setPermanent oLand' perm
    case reqs of
      PlayLandReqs{playLandReqs_hasPriority = False} -> invalid PlayLand_NoPriority
      PlayLandReqs{playLandReqs_isActive = False} -> invalid PlayLand_NotActive
      PlayLandReqs{playLandReqs_isMainPhase = False} -> invalid PlayLand_NotMainPhase
      PlayLandReqs{playLandReqs_stackEmpty = False} -> invalid PlayLand_StackNonEmpty
      PlayLandReqs{playLandReqs_atMaxLands = True} -> invalid PlayLand_AtMaxLands
      PlayLandReqs
        { playLandReqs_hasPriority = True
        , playLandReqs_isActive = True
        , playLandReqs_isMainPhase = True
        , playLandReqs_stackEmpty = True
        , playLandReqs_atMaxLands = False
        } -> assert (reqs == PlayLandReqs_Satisfied) $ case singZone @zone of
          SZBattlefield -> invalid PlayLand_CannotPlayFromZone
          SZExile -> invalid PlayLand_CannotPlayFromZone
          SZLibrary -> invalid PlayLand_CannotPlayFromZone
          SZStack -> invalid PlayLand_CannotPlayFromZone
          SZGraveyard -> invalid PlayLand_CannotPlayFromZone -- TODO: [Crucible of Worlds]
          SZHand -> do
            mCard <- fromRO $ gets $ Map.lookup (toZO0 oLand) . magicHandCards
            case mCard of
              Nothing -> invalid PlayLand_NotInZone
              Just card -> do
                case containsCard card hand of
                  False -> invalid PlayLand_NotOwned
                  True -> case card of
                    ArtifactCard{} -> invalid PlayLand_NotALand
                    ArtifactCreatureCard{} -> invalid PlayLand_NotALand
                    CreatureCard{} -> invalid PlayLand_NotALand
                    EnchantmentCard{} -> invalid PlayLand_NotALand
                    EnchantmentCreatureCard{} -> invalid PlayLand_NotALand
                    InstantCard{} -> invalid PlayLand_NotALand
                    PlaneswalkerCard{} -> invalid PlayLand_NotALand
                    SorceryCard{} -> invalid PlayLand_NotALand
                    --
                    LandCard{} -> success card >> pure Legal

resolveTopOfStack :: Monad m => Magic 'Private 'RW m ()
resolveTopOfStack = do
  Stack stack <- fromRO $ gets magicStack
  case stack of -- (117.4) (405.5)
    [] -> pure ()
    item : items -> do
      modify $ \st -> st{magicStack = Stack items}
      resolveStackObject item
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive

resolveStackObject :: forall m. Monad m => StackObject -> Magic 'Private 'RW m ()
resolveStackObject = \case
  StackAbility zoAbility -> resolveAbility zoAbility
  StackSpell zoSpell -> resolveSpell zoSpell
 where
  resolveAbility = undefined

  resolveSpell :: ZO 'ZStack OTSpell -> Magic 'Private 'RW m ()
  resolveSpell zoSpell = do
    st <- fromRO get

    let go :: AnyElected 'Post 'Pre ( 'Just tribal) -> Magic 'Private 'RW m ()
        go (AnyElected elected) = case elected of
          ElectedInstant{} -> undefined
          ElectedSorcery{} -> undefined

    case Map.lookup (toZO0 zoSpell) $ magicStackEntryNonTribalMap st of
      Nothing -> case Map.lookup (toZO0 zoSpell) $ magicStackEntryTribalMap st of
        Nothing -> error $ show NotSureWhatThisEntails
        Just entry -> go $ stackEntryElected entry
      Just entry -> go $ stackEntryElected entry

class ResolveElected (mTribal :: Maybe Tribal) (ot :: Type) where
  resolveElected :: Monad m => Elected 'Post 'Pre mTribal ot -> Magic 'Private 'RW m ()

instance ResolveElected ( 'Just tribal') OTSorcery where
  resolveElected elected = do
    let goElectEffect :: Monad m => Elect 'Post (Effect 'OneShot) OTSorcery -> Magic 'Private 'RW m ()
        goElectEffect = \case
          All masked -> electAll goElectEffect masked
          Effect effect -> goEffect $ Sequence effect
          _ -> undefined

        goEffect :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m ()
        goEffect = performOneShotEffect
    goElectEffect $ unPending $ electedSorcery_effect elected
    pure () -- TODO: GC stack stuff

performOneShotEffect :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m ()
performOneShotEffect = \case
  AddMana oPlayer mana -> addToManaPool oPlayer mana
  DealDamage oSource oVictim damage -> dealDamage oSource oVictim damage
  DrawCards oPlayer amount -> drawCards amount $ zo1ToObject oPlayer
  EffectCase case_ -> performCase performOneShotEffect case_
  Sequence effects -> mapM_ performOneShotEffect effects
  _ -> undefined

addToManaPool :: Monad m => OPlayer -> ManaPool 'NonSnow -> Magic 'Private 'RW m ()
addToManaPool oPlayer mana =
  fromRO (findPlayer $ zo1ToObject oPlayer) >>= \case
    Nothing -> pure ()
    Just player -> do
      let mana' = playerMana player <> mempty{poolNonSnow = mana}
      setPlayer (zo1ToObject oPlayer) player{playerMana = mana'}

performCase :: Monad m => (x -> Magic 'Private 'RW m a) -> Case x -> Magic 'Private 'RW m a
performCase cont = \case
  CaseColor (readVariable -> color) w u b r g -> case color of
    White -> cont w
    Blue -> cont u
    Black -> cont b
    Red -> cont r
    Green -> cont g

dealDamage ::
  Monad m =>
  ODamageSource ->
  OCreaturePlayerPlaneswalker ->
  Damage var ->
  Magic 'Private 'RW m ()
dealDamage _oSource oVictim (forceVars -> Damage damage) = do
  fromRO (findPermanent (toZO0 oVictim)) >>= \case
    Nothing -> pure ()
    Just perm -> do
      -- XXX: What happens if damage is dealt to a permanent that is both a creature and a planeswalker?
      case permanentCreature perm of
        Nothing -> pure ()
        Just{} -> do
          setPermanent
            (toZO0 oVictim)
            perm
              { permanentCreatureDamage = (damage +) <$> permanentCreatureDamage perm
              }
      case permanentPlaneswalker perm of
        Nothing -> pure ()
        Just () -> undefined
  oPlayers <- fromPublicRO getPlayers
  M.forM_ oPlayers $ \oPlayer -> case getObjectId oVictim == getObjectId oPlayer of
    False -> pure ()
    True -> do
      player <- fromRO $ getPlayer oPlayer
      let life = unLife $ playerLife player
      setPlayer oPlayer player{playerLife = Life $ life - damage}

beginningOfCombatStep :: Monad m => MagicCont 'Private 'RW m Void Void
beginningOfCombatStep = do
  setPhaseStep $ PSCombatPhase BeginningOfCombatStep
  _ <- undefined
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  declareAttackersStep

declareAttackersStep :: Monad m => MagicCont 'Private 'RW m Void Void
declareAttackersStep = do
  setPhaseStep $ PSCombatPhase DeclareAttackersStep
  _ <- undefined
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  declareBlockersStep

declareBlockersStep :: Monad m => MagicCont 'Private 'RW m Void Void
declareBlockersStep = do
  setPhaseStep $ PSCombatPhase DeclareBlockersStep
  _ <- undefined
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  combatDamageStep

combatDamageStep :: Monad m => MagicCont 'Private 'RW m Void Void
combatDamageStep = do
  setPhaseStep $ PSCombatPhase CombatDamageStep
  _ <- undefined
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  endOfCombatStep

endOfCombatStep :: Monad m => MagicCont 'Private 'RW m Void Void
endOfCombatStep = do
  setPhaseStep $ PSCombatPhase EndOfCombatStep
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  postcombatMainPhase

endStep :: Monad m => MagicCont 'Private 'RW m Void Void
endStep = do
  setPhaseStep $ PSEndingPhase EndStep
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  cleanupStep

cleanupStep :: Monad m => MagicCont 'Private 'RW m Void Void
cleanupStep = do
  setPhaseStep $ PSEndingPhase CleanupStep
  _ <- undefined
  untapStep

-- (103.1)
determineStartingPlayer :: Monad m => Magic 'Private 'RW m ()
determineStartingPlayer = do
  st <- fromRO get
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
  prompt <- fromRO $ gets magicPrompt
  player <- fromRO $ getPlayer oPlayer
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
drawStartingHands = withEachPlayer_ drawStartingHand

drawStartingHand :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
drawStartingHand oPlayer = do
  player <- fromRO $ getPlayer oPlayer
  drawCards (playerStartingHandSize player) oPlayer

newObjectId :: Monad m => Magic 'Private 'RW m ObjectId
newObjectId = do
  ObjectId i <- fromRO $ gets magicNextObjectId
  modify $ \st -> st{magicNextObjectId = ObjectId $ i + 1}
  pure $ ObjectId i

findPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RO m (Maybe Permanent)
findPermanent oPerm = gets $ Map.lookup oPerm . magicPermanents

getPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Magic 'Private 'RO m Permanent
getPermanent oPerm =
  findPermanent oPerm <&> \case
    Nothing -> error $ show $ InvalidPermanent oPerm
    Just perm -> perm

setPermanent :: Monad m => ZO 'ZBattlefield OT0 -> Permanent -> Magic 'Private 'RW m ()
setPermanent oPerm perm = modify $ \st ->
  let permMap = magicPermanents st
      permMap' = Map.insert oPerm perm permMap
   in st{magicPermanents = permMap'}

findPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m (Maybe Player)
findPlayer oPlayer = gets $ Map.lookup oPlayer . magicPlayers

getPlayer :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m Player
getPlayer oPlayer =
  findPlayer oPlayer <&> \case
    Nothing -> error $ show $ InvalidPlayer oPlayer
    Just player -> player

setPlayer :: Monad m => Object 'OTPlayer -> Player -> Magic 'Private 'RW m ()
setPlayer oPlayer player = modify $ \st ->
  let playerMap = magicPlayers st
      playerMap' = Map.insertWith (\_ _ -> player) oPlayer fatal playerMap
      fatal = error $ show $ InvalidPlayer oPlayer
   in st{magicPlayers = playerMap'}

drawCard :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
drawCard oPlayer = do
  player <- fromRO $ getPlayer oPlayer
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

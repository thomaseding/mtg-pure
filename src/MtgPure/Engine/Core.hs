{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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
import safe Data.Maybe (catMaybes)
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
import safe MtgPure.Model.ManaCost (ManaCost (..))
import safe MtgPure.Model.Mulligan (Mulligan)
import safe MtgPure.Model.Object (
  IsObjectType (..),
  OT0,
  Object (..),
  ObjectType (..),
  SObjectType (..),
 )
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectType.Index (IndexOT (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTAbility,
  OTLand,
  OTSorcery,
  OTSpell,
  OTStackObject,
 )
import safe MtgPure.Model.Permanent (
  Permanent (..),
  Phased (..),
  Tapped (..),
  cardToPermanent,
 )
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  Card (..),
  CardTypeDef (..),
  Cost (..),
  Effect (..),
  Elect (..),
  Requirement (..),
  Some (..),
  SomeCard,
  SomeTerm (..),
  WithThis (..),
 )
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Tribal (IsTribal, Tribal (..))
import safe MtgPure.Model.Zone (
  IsZone,
  SZone (..),
  Zone (..),
  singZone,
 )
import safe MtgPure.Model.ZoneObject (
  IsZO,
  OPlayer,
  ZO,
  ZoneObject (..),
  objectToZO,
  toZO0,
 )

data InternalLogicError
  = ExpectedCardToBeAPermanentCard
  | InvalidPermanent (ZO 'ZBattlefield OT0)
  | InvalidPlayer (Object 'OTPlayer)
  | ImpossibleGameOver
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
  , promptCastSpell :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe CastSpell)
  , promptDebugMessage :: String -> m ()
  , promptGetStartingPlayer :: PlayerCount -> m PlayerIndex
  , promptPerformMulligan :: Object 'OTPlayer -> [Card ()] -> m Bool -- TODO: Encode limited game state about players' mulligan states and [Serum Powder].
  , promptPlayLand :: OpaqueGameState m -> Object 'OTPlayer -> m (Maybe PlayLand)
  , promptShuffle :: CardCount -> Object 'OTPlayer -> m [CardIndex]
  }

-- NB (305.9): Lands + other types can never be cast
-- Unfortuantely OTSpell intersects OTArtifactLand. Such is life.
-- Prolly don't want to model `SomeButNot allowed disallowed`? Maybe `SomeButNot` is okay for Runtime,
-- though it's probably unnecessary for Authoring (thankfully).
newtype CastSpell :: Type where
  CastSpell :: SomeCard OTSpell -> CastSpell

data PlayLand :: Type where
  PlayLand :: IsZone zone => ZO zone OTLand -> PlayLand

data SpecialAction :: Type where
  SA_CastSpell :: CastSpell -> SpecialAction
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

data GameState (m :: Type -> Type) where
  GameState ::
    { magicCurrentTurn :: Int
    , magicElected :: Map.Map (ZO 'ZStack OT0) AnyElected
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
        { magicCurrentTurn = 0
        , magicElected = mempty
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
withEachPlayer_ f = fromPublicRO getPlayers >>= M.mapM_ f

getPermanents :: Monad m => Magic v 'RO m [ZO 'ZBattlefield OT0]
getPermanents = internalFromPrivate $ gets $ Map.keys . magicPermanents

withEachPermanent ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OT0 -> Magic v rw m a) ->
  Magic v rw m [a]
withEachPermanent f = fromRO getPermanents >>= M.mapM f

withEachPermanent_ ::
  (IsReadWrite rw, Monad m) =>
  (ZO 'ZBattlefield OT0 -> Magic v rw m ()) ->
  Magic v rw m ()
withEachPermanent_ f = fromRO getPermanents >>= M.mapM_ f

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

  -- (103.3) See `mkPlayer`
  drawStartingHands -- (103.4)
  -- (103.5) TODO: leylines and such
  -- (103.6) TODO: planechase stuff
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

performSpecialAction :: Monad m => Object 'OTPlayer -> SpecialAction -> Magic 'Private 'RW m Bool
performSpecialAction oPlayer = \case
  SA_CastSpell (CastSpell someCard) -> castSpell oPlayer someCard
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

castSpell :: forall m. Monad m => Object 'OTPlayer -> SomeCard OTSpell -> Magic 'Private 'RW m Bool
castSpell oPlayer someCard = rewindIllegal $ case someCard of
  Some6a (SomeArtifact card) -> undefined card
  Some6b (SomeCreature card) -> undefined card
  Some6c (SomeEnchantment card) -> undefined card
  Some6d (SomeInstant card) -> undefined card
  Some6e (SomePlaneswalker card) -> undefined card
  Some6f (SomeSorcery card@(Card _name _wCard (T1 thisToElectDef))) -> do
    i <- newObjectId
    let this = ZOBattlefield $ O1 $ Object SSorcery i

        goElectDef :: Elect (CardTypeDef 'NonTribal OTSorcery) OTSorcery -> Magic 'Private 'RW m Legality
        goElectDef = \case
          CardTypeDef def -> goDef def
          _ -> undefined

        goDef :: CardTypeDef 'NonTribal OTSorcery -> Magic 'Private 'RW m Legality
        goDef def =
          let goElectCost :: Elect (Cost OTSorcery) OTSorcery -> Magic 'Private 'RW m Legality
              goElectCost = \case
                Cost cost -> goCost cost
                _ -> undefined

              goCost :: Cost OTSorcery -> Magic 'Private 'RW m Legality
              goCost cost =
                let goElectEffect :: Elect (Effect 'OneShot) OTSorcery -> Magic 'Private 'RW m Legality
                    goElectEffect = \case
                      Effect effect -> castElected $ ElectedSorcery oPlayer card def cost $ EAnd effect
                      _ -> undefined
                 in goElectEffect $ sorcery_effect def
           in goElectCost $ sorcery_cost def

    goElectDef $ thisToElectDef this
  Some6f (SomeSorcery TribalCard{}) -> undefined
  Some6ab (SomeArtifactCreature card) -> undefined card
  Some6bc (SomeEnchantmentCreature card) -> undefined card

data Elected (tribal :: Tribal) (ot :: Type) :: Type where
  ElectedSorcery ::
    IsTribal tribal =>
    { electedSorcery_controller :: Object 'OTPlayer
    , electedSorcery_card :: Card OTSorcery
    , electedSorcery_def :: CardTypeDef tribal OTSorcery
    , electedSorcery_cost :: Cost OTSorcery
    , electedSorcery_effect :: Effect 'OneShot
    } ->
    Elected tribal OTSorcery
  deriving (Typeable)

data AnyElected :: Type where
  AnyElectedSorcery :: Elected tribal OTSorcery -> AnyElected
  deriving (Typeable)

class CastElected (tribal :: Tribal) (ot :: Type) where
  castElected :: Monad m => Elected tribal ot -> Magic 'Private 'RW m Legality

instance CastElected tribal OTSorcery where
  castElected elected =
    payCost (electedSorcery_controller elected) (electedSorcery_cost elected) >>= \case
      Illegal -> pure Illegal
      Legal -> do
        pure () -- TODO: make zo stack object
        pure () -- TODO: map zo to elected
        pure Legal

toLegality :: Bool -> Legality
toLegality = \case
  True -> Legal
  False -> Illegal

-- fromLegality :: Legality -> Bool
-- fromLegality = \case
--   Legal -> True
--   Illegal -> False

payCost :: Monad m => Object 'OTPlayer -> Cost ot -> Magic 'Private 'RW m Legality
payCost oPlayer = \case
  AndCosts costs -> payAndCosts oPlayer costs
  ManaCost manaCost -> payManaCost oPlayer manaCost
  OrCosts costs -> payOrCosts oPlayer costs
  TapCost _wPerm reqs -> payTapCost oPlayer $ RAnd reqs
  _ -> undefined

payManaCost :: Object 'OTPlayer -> ManaCost -> Magic 'Private 'RW m Legality
payManaCost = undefined

payTapCost ::
  (Monad m, IsZO 'ZBattlefield ot) =>
  Object 'OTPlayer ->
  Requirement 'ZBattlefield ot ->
  Magic 'Private 'RW m Legality
payTapCost oPlayer req =
  fromRO (findObjectsSatisfying $ RAnd [ControlledBy $ ZOBattlefield $ O1 oPlayer, req]) >>= \case
    [] -> pure Illegal
    o : _os ->
      -- TODO: Offer other choices
      tapPermanent (toZO0 o) <&> toLegality

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
      let go :: [ObjectType] -> Magic 'Private 'RO m (DList.DList (ZO zone ot))
          go = \case
            [] -> pure DList.empty
            ot : ots -> do
              zos <- fmap catMaybes $
                withEachPermanent $ \oPerm -> do
                  perm <- getPermanent oPerm
                  let goPermanent ::
                        forall a x.
                        IsObjectType a =>
                        (Permanent -> Maybe x) ->
                        Maybe (ZO 'ZBattlefield ot)
                      goPermanent viewPerm = case viewPerm perm of
                        Nothing -> Nothing
                        Just{} -> objectToZO $ idToObject @a $ getObjectId oPerm
                  pure $ case ot of
                    OTArtifact -> goPermanent @ 'OTArtifact permanentArtifact
                    OTCreature -> goPermanent @ 'OTCreature permanentCreature
                    OTEnchantment -> goPermanent @ 'OTEnchantment undefined
                    OTLand -> goPermanent @ 'OTLand permanentLand
                    OTPlaneswalker -> goPermanent @ 'OTPlaneswalker undefined
                    _ -> Nothing
              zos' <- go ots
              pure $ DList.fromList zos <> zos'
       in DList.toList <$> go objectTypes
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
objectSatisfiesControlledBy oPerm oPlayer = case singZone @zone of
  SZBattlefield ->
    findPermanent (toZO0 oPerm) <&> \case
      Nothing -> False
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
          isLegal <- lift $ performSpecialAction oPlayer $ SA_PlayLand special
          throwE $ case isLegal of
            True -> gainPriority oPlayer -- (117.3c)
            False -> runMagicCont (either id id) $ askPlayLand oPlayer
    _ -> pure ()

playLand ::
  forall zone m.
  (IsZone zone, Monad m) =>
  Object 'OTPlayer ->
  ZO zone OTLand ->
  Magic 'Private 'RW m Bool
playLand oPlayer oLand = do
  st <- internalFromPrivate $ fromRO get
  reqs <- fromRO $ getPlayLandReqs oPlayer
  player <- fromRO $ getPlayer oPlayer
  let opaque = OpaqueGameState st
      prompt = magicPrompt st
      hand = playerHand player
      --
      invalid :: (ZO zone OTLand -> InvalidPlayLand) -> Magic 'Private 'RW m Bool
      invalid ex = do
        lift $ exceptionInvalidPlayLand prompt opaque oPlayer $ ex oLand
        pure False
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
                  LandCard{} -> success card >> pure True

resolveTopOfStack :: Monad m => Magic 'Private 'RW m ()
resolveTopOfStack = do
  Stack stack <- fromRO $ gets magicStack
  case stack of -- (117.4) (405.5)
    [] -> pure ()
    oItem : oItems -> do
      item <- getStackObject oItem
      modify $ \st -> st{magicStack = Stack oItems}
      case item of
        Left ability -> resolveAbility ability
        Right spell -> resolveSpell spell
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive

-- resolveSpell :: Object 'OTPlayer -> SomeCard OTSpell -> Magic 'Private 'RW m ()
-- resolveSpell oPlayer = \case
--   Some6a (SomeArtifact card) -> undefined
--   Some6b (SomeCreature card) -> respolveCreature oPlayer card
--   Some6c (SomeEnchantment card) -> undefined
--   Some6d (SomeInstant card) -> undefined
--   Some6e (SomePlaneswalker card) -> undefined
--   Some6f (SomeSorcery card) -> undefined
--   Some6ab (SomeArtifactCreature card) -> undefined
--   Some6bc (SomeEnchantmentCreature card) -> undefined

resolveSpell :: ObjectN OTSpell -> Magic 'Private 'RW m ()
resolveSpell = undefined

resolveAbility :: ObjectN OTAbility -> Magic 'Private 'RW m ()
resolveAbility = undefined

getStackObject :: ZO 'ZStack OTStackObject -> Magic 'Private 'RW m (Either (ObjectN OTAbility) (ObjectN OTSpell))
getStackObject = undefined

beginningOfCombatStep :: Monad m => MagicCont 'Private 'RW m Void Void
beginningOfCombatStep = do
  setPhaseStep $ PSCombatPhase BeginningOfCombatStep
  M.void undefined
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  declareBlockersStep

deckareAttackersStep :: Monad m => MagicCont 'Private 'RW m Void Void
deckareAttackersStep = do
  setPhaseStep $ PSCombatPhase DeclareAttackersStep
  M.void undefined
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  declareBlockersStep

declareBlockersStep :: Monad m => MagicCont 'Private 'RW m Void Void
declareBlockersStep = do
  setPhaseStep $ PSCombatPhase DeclareBlockersStep
  M.void undefined
  lift $ do
    oActive <- fromPublicRO getActivePlayer
    gainPriority oActive
  combatDamageStep

combatDamageStep :: Monad m => MagicCont 'Private 'RW m Void Void
combatDamageStep = do
  setPhaseStep $ PSCombatPhase CombatDamageStep
  M.void undefined
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
  M.void undefined
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

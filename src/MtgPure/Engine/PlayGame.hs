{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PlayGame (
  playGame,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Functor ((<&>))
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe Data.Void (Void, absurd)
import safe MtgPure.Engine.Fwd.Api (
  eachLogged_,
  getAlivePlayers,
  getPlayer,
  pushLibraryCard,
  startGame,
 )
import safe MtgPure.Engine.Fwd.Impl (fwdImpl)
import safe MtgPure.Engine.Monad (fromPublicRO, fromRO, runMagicRW)
import safe MtgPure.Engine.Prompt (Prompt' (..))
import safe MtgPure.Engine.State (
  EvListenerId (..),
  Fwd,
  GameFormat (..),
  GameInput (..),
  GameResult,
  GameState (..),
  Magic,
  logCall,
 )
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (..),
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Variable (VariableId' (..))

playGame :: (Monad m) => GameInput m -> m (Maybe (GameResult m))
playGame input = case mkGameState fwdImpl input of
  Nothing -> do
    exceptionCantBeginGameWithoutPlayers $ gameInput_prompt input
    pure Nothing
  Just st -> do
    runMagicRW st startGame' <&> \case
      Left result -> Just result
      Right v -> absurd v

startGame' :: (Monad m) => Magic 'Private 'RW m Void
startGame' = logCall 'startGame' do
  initLibraries
  startGame

initLibraries :: (Monad m) => Magic 'Private 'RW m ()
initLibraries = logCall 'initLibraries do
  oPlayers <- fromPublicRO getAlivePlayers
  eachLogged_ oPlayers \oPlayer -> do
    player <- fromRO $ getPlayer oPlayer
    let Deck cards = playerStartingDeck player
    mapM_ (pushLibraryCard oPlayer) cards

mkPlayer :: GameFormat -> Object 'OTPlayer -> (Deck, Sideboard) -> Player
mkPlayer format o (deck, sideboard) =
  Player
    { player_ = ()
    , playerCounters = mempty
    , playerDrewFromEmptyLibrary = False
    , playerGraveyard = Graveyard []
    , playerHand = Hand []
    , playerLandsPlayedThisTurn = 0
    , playerLibrary = Library []
    , playerLife = life
    , playerLost = False
    , playerMana = mempty
    , playerObject = o
    , playerStartingDeck = deck
    , playerStartingHandSize = 7
    , playerStartingLife = life
    , playerStartingSideboard = sideboard
    }
 where
  -- 103.3
  life = Life case format of
    Vintage -> 20

mkGameState :: Fwd m -> GameInput m -> Maybe (GameState m)
mkGameState fwd input = case playerObjects of
  [] -> Nothing
  oPlayer : _ ->
    Just
      GameState
        { magic_ = ()
        , magicControllerMap = controllerMap
        , magicCurrentTurn = 0
        , magicExiledCards = mempty
        , magicFwd = fwd
        , magicGameInput = input
        , magicGraveyardCards = mempty
        , magicHandCards = mempty
        , magicLibraryCards = mempty
        , magicListenerNextId = EvListenerId 1
        , magicListeners = mempty
        , magicManaBurn = False
        , magicNextObjectDiscriminant = (1 +) <$> DefaultObjectDiscriminant
        , magicNextObjectId = ObjectId $ 1 + playerCount
        , magicNextVariableId = VariableId 1
        , magicOwnerMap = ownerMap
        , magicPermanents = mempty
        , magicPhaseStep = PSBeginningPhase UntapStep
        , magicPlayers = playerMap
        , magicPlayerOrderAPNAP = Stream.cycle playerObjects
        , magicPlayerOrderPriority = []
        , magicPlayerOrderTurn = Stream.cycle playerObjects
        , magicPrompt = gameInput_prompt input
        , magicStack = Stack []
        , magicStackEntryTargetsMap = mempty
        , magicStackEntryElectedMap = mempty
        , magicStartingPlayer = oPlayer
        , magicTargetProperties = mempty
        }
 where
  format = gameInput_gameFormat input
  decks = gameInput_decks input
  playerCount = length decks
  playerIds = ObjectId <$> [1 .. playerCount] -- NOTE: reserving 0 for "null" for UI choice
  playerObjects = idToObject . UntypedObject DefaultObjectDiscriminant <$> playerIds
  players = zipWith (mkPlayer format) playerObjects decks
  playerMap = Map.fromList $ zip playerObjects players
  ownerMap = Map.fromList $ map (\o -> (getObjectId o, o)) playerObjects
  controllerMap = ownerMap

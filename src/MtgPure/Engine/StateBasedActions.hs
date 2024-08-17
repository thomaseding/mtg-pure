{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.StateBasedActions (
  performStateBasedActions,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Data.Foldable as F
import safe Data.Maybe (catMaybes)
import safe qualified Data.Traversable as T
import safe MtgPure.Engine.Fwd.Api (
  allPermanents,
  getAlivePlayers,
  getPermanent,
  getPlayer,
  ownerOf,
  pushGraveyardCard,
  setPermanent,
 )
import safe MtgPure.Engine.Monad (fromPublic, fromRO, get, magicCatch, magicThrow)
import safe MtgPure.Engine.Prompt (PlayerIndex (PlayerIndex))
import safe MtgPure.Engine.State (GameCheats (..), GameInput (..), GameResult (..), GameState (..), Magic, concatGameResults, logCall)
import safe MtgPure.Model.Counter (PlayerCounters (..))
import safe MtgPure.Model.Creature (Creature (..))
import safe MtgPure.Model.Damage (Damage' (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (ObjectId (ObjectId), getObjectId)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Toughness (Toughness (..))

-- | This aggregates thrown GameResult exceptions and coalesces them into a single exception.
--  This is necessary because the game can end in multiple ways at once, and we want to
--  report all the winners and losers together.
--
-- TODO: Similarly, we aggregate events and coalesce them into a single event when possible?
--
-- Used in SBA due to (704.3) "performs all applicable state-based actions simultaneously as
-- a single event".
simultaneously :: (Monad m) => [Magic 'Private 'RW m ()] -> Magic 'Private 'RW m ()
simultaneously actions = do
  st <- fromRO get
  gameResults <-
    catMaybes <$> T.for actions \action -> flip magicCatch (pure . Just) do
      action
      pure Nothing
  case gameResults of
    [] -> pure ()
    _ -> do
      magicThrow $ concatGameResults st gameResults

playerLoses :: (Monad m) => Object 'OTPlayer -> Magic 'Private 'RW m ()
playerLoses oPlayer = do
  let ObjectId i = getObjectId oPlayer
  st <- fromRO get
  case gameCheats_disableLosing $ gameInput_gameCheats $ magicGameInput st of
    True -> pure ()
    False -> do
      pure () -- TODO: Modify player state to indicate he or she has lost the game.
      pure () -- TODO: Only throw when the game ends. This is fine for now since only 1v1 is supported.
      let totalPlayers = length $ magicPlayers st
      let is = [1 .. totalPlayers]
      magicThrow
        GameResult
          { gameEndState = st
          , gameWinners = map PlayerIndex $ filter (/= i) is
          , gameLosers = [PlayerIndex i]
          }

-- | (704.) State-Based Actions
performStateBasedActions :: (Monad m) => Magic 'Private 'RW m ()
performStateBasedActions = logCall 'performStateBasedActions do
  simultaneously sbas

-- | (704.5a) If a player has 0 or less life, that player loses the game.
sbaPlayerHasZeroOrLessLife :: (Monad m) => Magic 'Private 'RW m ()
sbaPlayerHasZeroOrLessLife = do
  oPlayers <- fromPublic $ fromRO getAlivePlayers
  simultaneously $ map go oPlayers
 where
  go oPlayer = do
    player <- fromRO $ getPlayer oPlayer
    let Life life = playerLife player
    M.when False $ M.when (life <= 0) $ playerLoses oPlayer

-- | (704.5b) If a player attempted to draw a card from a library with no cards in it, that player loses the game.
sbaPlayerDrawsFromEmptyLibrary :: (Monad m) => Magic 'Private 'RW m ()
sbaPlayerDrawsFromEmptyLibrary = do
  oPlayers <- fromPublic $ fromRO getAlivePlayers
  simultaneously $ map go oPlayers
 where
  go oPlayer = do
    player <- fromRO $ getPlayer oPlayer
    let drew = playerDrewFromEmptyLibrary player
    M.when drew $ playerLoses oPlayer

-- | (704.5c) If a player has ten or more poison counters, that player loses the game.
sbaPlayerHasTenOrMorePoisonCounters :: (Monad m) => Magic 'Private 'RW m ()
sbaPlayerHasTenOrMorePoisonCounters = do
  oPlayers <- fromPublic $ fromRO getAlivePlayers
  simultaneously $ map go oPlayers
 where
  go oPlayer = do
    player <- fromRO $ getPlayer oPlayer
    let poison = playerPoisonCounters $ playerCounters player
    M.when (poison >= 10) $ playerLoses oPlayer

-- | (704.5d) If a token is in a zone other than the battlefield, it ceases to exist.
sbaTokenNotOnBattlefield :: (Monad m) => Magic 'Private 'RW m ()
sbaTokenNotOnBattlefield = pure () -- TODO

-- | (704.5e) If a copy of a spell or ability is in a zone other than the stack, it ceases to exist.
sbaCopyOfSpellNotOnStack :: (Monad m) => Magic 'Private 'RW m ()
sbaCopyOfSpellNotOnStack = pure () -- TODO

-- | (704.5f) If a creature has toughness 0 or less, it is put into its owner's graveyard.
-- Regeneration can't replace this event.
sbaCreatureHasZeroOrLessToughness :: (Monad m) => Magic 'Private 'RW m ()
sbaCreatureHasZeroOrLessToughness = do
  zoPerms <- fromPublic $ fromRO allPermanents
  F.for_ zoPerms \zoPerm -> do
    perm <- fromRO $ getPermanent zoPerm
    case permanentCreature perm of
      Nothing -> pure ()
      Just creature -> do
        let Toughness toughness = creatureToughness creature
        M.when (toughness <= 0) do
          owner <- fromRO $ ownerOf zoPerm
          setPermanent zoPerm Nothing
          case permanentCard perm of
            Left card -> M.void $ pushGraveyardCard owner card
            Right _token -> pure ()

-- | (704.5g) If a creature has lethal damage marked on it, it is put into its owner's graveyard.
-- Regeneration can replace this event.
sbaCreatureHasLethalDamage :: (Monad m) => Magic 'Private 'RW m ()
sbaCreatureHasLethalDamage = do
  zoPerms <- fromPublic $ fromRO allPermanents
  F.for_ zoPerms \zoPerm -> do
    perm <- fromRO $ getPermanent zoPerm
    case permanentCreature perm of
      Nothing -> pure ()
      Just creature -> do
        let Damage damage = permanentCreatureDamage perm
        let Toughness toughness = creatureToughness creature
        pure () -- TODO: Regeneration
        M.when (damage >= toughness) do
          owner <- fromRO $ ownerOf zoPerm
          setPermanent zoPerm Nothing
          case permanentCard perm of
            Left card -> M.void $ pushGraveyardCard owner card
            Right _token -> pure ()

-- | (704.5h) If a creature has been deathtouch-ed, it is put into its owner's graveyard.
-- Regeneration can replace this event.
sbaCreatureHasBeenDamagedByDeathtouch :: (Monad m) => Magic 'Private 'RW m ()
sbaCreatureHasBeenDamagedByDeathtouch = pure () -- TODO

-- | (704.5i) If a planeswalker has loyalty 0 or less, it is put into its owner's graveyard.
sbaPlaneswalkerHasZeroOrLessLoyalty :: (Monad m) => Magic 'Private 'RW m ()
sbaPlaneswalkerHasZeroOrLessLoyalty = pure () -- TODO

-- | (704.5j) The legend rule.
sbaLegendRule :: (Monad m) => Magic 'Private 'RW m ()
sbaLegendRule = pure () -- TODO

-- | (704.5k) The world rule.
sbaWorldRule :: (Monad m) => Magic 'Private 'RW m ()
sbaWorldRule = pure () -- TODO

-- | (704.5m) If an Aura is attached to an illegal object or player, it is put into its owner's graveyard.
sbaAuraAttachedToIllegalObjectOrPlayer :: (Monad m) => Magic 'Private 'RW m ()
sbaAuraAttachedToIllegalObjectOrPlayer = pure () -- TODO

-- | (704.5n) If an Equipment or Fortification is attached to an illegal object or player, it is put into its owner's graveyard.
sbaEquipmentOrFortificationAttachedToIllegalObjectOrPlayer :: (Monad m) => Magic 'Private 'RW m ()
sbaEquipmentOrFortificationAttachedToIllegalObjectOrPlayer = pure () -- TODO

-- | (704.5p) If a creature is attached to an object or player, it becomes unattached.
-- Likewise for other permanent types that are not Auras, Equipment, or Fortifications.
sbaUnusualAttachment :: (Monad m) => Magic 'Private 'RW m ()
sbaUnusualAttachment = pure () -- TODO

-- | (704.5q) If a permanent has both a +1/+1 counter and a -1/-1 counter on it, remove both of them.
sbaNormalizeStatCounters :: (Monad m) => Magic 'Private 'RW m ()
sbaNormalizeStatCounters = pure () -- TODO

-- | (704.5r) If a permanent with an ability that says it can't have more than N counters of a kind has more than N of that kind of counter on it, remove all but N of those counters.
sbaLimitCounters :: (Monad m) => Magic 'Private 'RW m ()
sbaLimitCounters = pure () -- TODO

-- | (704.5s) If the number of lore counters on a Saga permanent is greater or equal to its final chapter number and isn't the source of the chapter ability that has triggered but not yet been put on the stack, the Saga is sacrificed.
sbaSagaLoreCounters :: (Monad m) => Magic 'Private 'RW m ()
sbaSagaLoreCounters = pure () -- TODO

-- | (704.5t) Some D&D stuff. Extremely low priority.
sbaBottomOfDungeon :: (Monad m) => Magic 'Private 'RW m ()
sbaBottomOfDungeon = pure () -- TODO

-- | (704.5u) Unfinity "Space Sculptor".
sbaSpaceSculptor :: (Monad m) => Magic 'Private 'RW m ()
sbaSpaceSculptor = pure ()

-- | (704.) State-Based Actions
-- TODO: There are more for various game modes.
sbas :: (Monad m) => [Magic 'Private 'RW m ()]
sbas =
  [ sbaPlayerHasZeroOrLessLife
  , sbaPlayerDrawsFromEmptyLibrary
  , sbaPlayerHasTenOrMorePoisonCounters
  , sbaTokenNotOnBattlefield
  , sbaCopyOfSpellNotOnStack
  , sbaCreatureHasZeroOrLessToughness
  , sbaCreatureHasLethalDamage
  , sbaCreatureHasBeenDamagedByDeathtouch
  , sbaPlaneswalkerHasZeroOrLessLoyalty
  , sbaLegendRule
  , sbaWorldRule
  , sbaAuraAttachedToIllegalObjectOrPlayer
  , sbaEquipmentOrFortificationAttachedToIllegalObjectOrPlayer
  , sbaUnusualAttachment
  , sbaNormalizeStatCounters
  , sbaLimitCounters
  , sbaSagaLoreCounters
  , sbaBottomOfDungeon
  , sbaSpaceSculptor
  ]

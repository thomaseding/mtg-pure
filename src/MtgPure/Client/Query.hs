{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- | A friendly, read-only query layer over an 'OpaqueGameState'. Where the
-- @Test.Game.Headless.*@ replays hard-code raw 'ObjectId's plucked out of thin
-- air, these helpers let a test /find/ the ids it cares about by describing them
-- (name, zone, controller, owner). Every returned list is sorted ascending by
-- id, so a test can pattern-match @[a, b] <- ...@ deterministically.
--
-- The queries run purely: they read the captured state through 'queryMagic' in a
-- throwaway 'Headless' run (a read-only query never consumes a decision nor
-- halts), so the caller gets a plain @[ObjectId]@ back.
module MtgPure.Client.Query (
  getPlayers,
  getByNameZone,
  getByControllerNameZone,
  getByOwnerNameZone,
  getPhaseStep,
  getStackSize,
  getCurrentTurn,
  getPlayerWithPriority,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes)
import safe MtgPure.Client.Headless.Monad (
  Headless,
  HeadlessResult (..),
  runHeadless,
 )
import safe MtgPure.Engine.Fwd.Api (queryObjectId)
import safe MtgPure.Engine.Monad (gets, internalFromPrivate)
import safe MtgPure.Engine.Prompt (QueryObjectResult (..))
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  OpaqueGameState,
  queryMagic,
 )
import safe MtgPure.Model.CardName (CardName, getCardName)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (ObjectId (..), getObjectId)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.PhaseStep (PhaseStep)
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Zone (Zone)

-- | The game's players, ascending by id. Use this instead of hard-coding player
-- ids: @[p1, p2] = getPlayers ogs@.
getPlayers :: OpaqueGameState Headless -> [Object 'OTPlayer]
getPlayers opaque =
  List.sortOn getObjectId $
    runQuery opaque $
      internalFromPrivate $
        gets (Map.keys . magicPlayers)

-- | Ids of every object with the given name in the given zone.
getByNameZone :: OpaqueGameState Headless -> CardName -> Zone -> [ObjectId]
getByNameZone opaque name zone =
  matching opaque \qor ->
    qorMatchesName name qor
      && qorZone qor == zone

-- | Like 'getByNameZone', additionally restricted to objects the given player
-- controls.
getByControllerNameZone ::
  OpaqueGameState Headless -> Object 'OTPlayer -> CardName -> Zone -> [ObjectId]
getByControllerNameZone opaque controller name zone =
  matching opaque \qor ->
    qorController qor == controller
      && qorMatchesName name qor
      && qorZone qor == zone

-- | Like 'getByNameZone', additionally restricted to objects the given player
-- owns.
getByOwnerNameZone ::
  OpaqueGameState Headless -> Object 'OTPlayer -> CardName -> Zone -> [ObjectId]
getByOwnerNameZone opaque owner name zone =
  matching opaque \qor ->
    qorOwner qor == owner
      && qorMatchesName name qor
      && qorZone qor == zone

-- | The current phase\/step.
getPhaseStep :: OpaqueGameState Headless -> PhaseStep
getPhaseStep opaque = runQuery opaque $ internalFromPrivate $ gets magicPhaseStep

-- | How many objects are on the stack.
getStackSize :: OpaqueGameState Headless -> Int
getStackSize opaque = runQuery opaque $ internalFromPrivate $ gets (length . unStack . magicStack)

-- | The current turn number.
getCurrentTurn :: OpaqueGameState Headless -> Int
getCurrentTurn opaque = runQuery opaque $ internalFromPrivate $ gets magicCurrentTurn

-- | The player who currently has priority.
getPlayerWithPriority :: OpaqueGameState Headless -> Object 'OTPlayer
getPlayerWithPriority opaque =
  runQuery opaque $
    internalFromPrivate $
      gets \st -> case magicPlayerOrderPriority st of
        p : _ -> p
        [] -> error "getPlayerWithPriority: no player has priority"

--------------------------------------------------------------------------------

-- | Ids of every queryable object whose 'QueryObjectResult' satisfies the
-- predicate, sorted ascending.
matching :: OpaqueGameState Headless -> (QueryObjectResult -> Bool) -> [ObjectId]
matching opaque predicate =
  List.sort [i | (i, qor) <- queryObjects opaque, predicate qor]

-- | Snapshot every object id currently in play, paired with its query result.
-- Ids are dense in @[1 .. magicNextObjectId - 1]@; ids with no live object (e.g.
-- already left the game) are dropped.
queryObjects :: OpaqueGameState Headless -> [(ObjectId, QueryObjectResult)]
queryObjects opaque = runQuery opaque do
  ObjectId next <- internalFromPrivate $ gets magicNextObjectId
  let ids = map ObjectId [1 .. next - 1]
  fmap catMaybes $ mapM lookupId ids
 where
  lookupId i = fmap ((,) i) <$> internalFromPrivate (queryObjectId i)

-- | The name carried by a query result, if any: a spell/card name, a token name,
-- or the name of the card/token underlying a permanent.
qorMatchesName :: CardName -> QueryObjectResult -> Bool
qorMatchesName name qor = case qorName qor of
  Nothing -> False
  Just name' -> name' == name

qorName :: QueryObjectResult -> Maybe CardName
qorName qor = case qorCard qor of
  Just card -> Just $ getCardName card
  Nothing -> case qorToken qor of
    Just token -> Just $ getCardName token
    Nothing -> case qorPermanent qor of
      Just perm -> Just $ getCardName $ permanentCard perm
      Nothing -> Nothing

-- | Run a read-only query and extract its pure result. A public read-only query
-- never consumes a decision nor halts the replay, so the outcome is always
-- @Right@; a @Left@ would be an engine bug.
runQuery :: OpaqueGameState Headless -> Magic 'Public 'RO Headless a -> a
runQuery opaque magic =
  case headlessOutcome $ runHeadless [] $ queryMagic opaque magic of
    Right a -> a
    Left reason -> error $ "MtgPure.Client.Query.runQuery: unexpected halt: " ++ reason

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

module MtgPure.Client.Terminal.Render (
  printGameState,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Traversable as T
import safe MtgPure.Client.Terminal.Monad (Terminal, pause)
import safe MtgPure.Engine.Fwd.Api (eachLogged_, getAlivePlayers, getPlayer)
import safe MtgPure.Engine.Monad (get, gets, internalFromPrivate)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.State (GameState (..), Magic, OpaqueGameState, queryMagic)
import safe MtgPure.Model.CardName (CardName (..), getCardName)
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Object.OTNAliases (OTNCard)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (ObjectId (..), getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (OTPlayer))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

--------------------------------------------------------------------------------

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
    M.liftIO $ print st
  False -> do
    M.liftIO do
      horizontalLine
      print "GAME STATE BEGIN"
    oPlayers <- getAlivePlayers
    eachLogged_ oPlayers \oPlayer -> do
      let name = getPlayerName oPlayer
      M.liftIO do
        horizontalLine
        putStrLn $ name ++ ":"
      player <- internalFromPrivate $ getPlayer oPlayer
      M.liftIO do
        print ("life", playerLife player)
        print ("library", length $ unLibrary $ playerLibrary player)
        print ("mana", show $ playerMana player)
      printHand $ playerHand player
    pure () -- print priority
    pure () -- print stack
    M.liftIO do
      print "GAME STATE END"
      horizontalLine
      pause
 where
  dumpEverything = True

printHand :: Hand -> Magic 'Public 'RO Terminal ()
printHand (Hand zos) = do
  names <- T.for zos getHandCardName
  M.liftIO $ print ("hand", names)

getHandCardName :: ZO 'ZHand OTNCard -> Magic 'Public 'RO Terminal String
getHandCardName zo = do
  let zo0 = toZO0 zo
  handCards <- internalFromPrivate $ gets magicHandCards
  case Map.lookup zo0 handCards of
    Nothing -> pure $ "IllegalHandCard@" ++ show zo
    Just anyCard -> do
      let CardName name = getCardName anyCard
      pure $ name ++ "@" ++ show zo

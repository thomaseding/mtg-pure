{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Player (
  Player (..),
  emptyPlayer,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Counter (PlayerCounters)
import safe MtgPure.Model.Deck (Deck)
import safe MtgPure.Model.Graveyard (Graveyard)
import safe MtgPure.Model.Hand (Hand)
import safe MtgPure.Model.Library (Library)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool)
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (..),
  UntypedObject (..),
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.SObjectType (SObjectType (SPlayer))
import safe MtgPure.Model.Sideboard (Sideboard)

data Player :: Type where
  Player ::
    { player_ :: ()
    , playerCounters :: PlayerCounters
    , playerDrewFromEmptyLibrary :: Bool
    , playerGraveyard :: Graveyard
    , playerHand :: Hand
    , playerLandsPlayedThisTurn :: Int
    , playerLibrary :: Library
    , playerLife :: Life
    , playerLost :: Bool
    , playerMana :: CompleteManaPool
    , playerObject :: Object 'OTPlayer
    , playerStartingDeck :: Deck
    , playerStartingHandSize :: Int
    , playerStartingLife :: Life
    , playerStartingSideboard :: Sideboard
    } ->
    Player
  deriving (Typeable)

emptyPlayer :: Player
emptyPlayer =
  Player
    { player_ = ()
    , playerCounters = mempty
    , playerDrewFromEmptyLibrary = False
    , playerGraveyard = mempty
    , playerHand = mempty
    , playerLandsPlayedThisTurn = 0
    , playerLibrary = mempty
    , playerLife = Life 0
    , playerLost = False
    , playerMana = mempty
    , playerObject = Object SPlayer (UntypedObject DefaultObjectDiscriminant (ObjectId 0))
    , playerStartingDeck = mempty
    , playerStartingHandSize = 0
    , playerStartingLife = Life 0
    , playerStartingSideboard = mempty
    }

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Player (
  Player (..),
  emptyPlayer,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Deck (Deck)
import safe MtgPure.Model.Graveyard (Graveyard)
import safe MtgPure.Model.Hand (Hand)
import safe MtgPure.Model.Library (Library)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool)
import safe MtgPure.Model.Sideboard (Sideboard)

data Player :: Type where
  Player ::
    { playerDrewFromEmptyLibrary :: Bool
    , playerGraveyard :: Graveyard
    , playerHand :: Hand
    , playerLandsPlayedThisTurn :: Int
    , playerLibrary :: Library
    , playerLife :: Life
    , playerLost :: Bool
    , playerMana :: CompleteManaPool
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
    { playerDrewFromEmptyLibrary = False
    , playerGraveyard = mempty
    , playerHand = mempty
    , playerLandsPlayedThisTurn = 0
    , playerLibrary = mempty
    , playerLife = Life 0
    , playerLost = False
    , playerMana = mempty
    , playerStartingDeck = mempty
    , playerStartingHandSize = 0
    , playerStartingLife = Life 0
    , playerStartingSideboard = mempty
    }

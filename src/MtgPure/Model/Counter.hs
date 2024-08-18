{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Counter (
  Counter (..),
  PlayerCounters (..),
  PermanentCounters (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OTNAliases (
  OTNCreature,
  OTNPlayer,
  OTNPlayerPlaneswalker,
 )

-- https://boardgames.stackexchange.com/questions/16662/can-non-creature-permanents-have-1-1-counters
-- https://boardgames.stackexchange.com/questions/27296/what-counters-can-be-attached-to-a-player
--
-- Use this type for effects that grant counters
data Counter (ot :: Type) :: Type where
  LoyaltyCounter :: Counter OTNPlayerPlaneswalker
  Minus1Minus1Counter :: Counter OTNCreature
  Plus1Plus1Counter :: Counter OTNCreature
  PoisonCounter :: Counter OTNPlayer
  deriving (Typeable)

deriving instance Eq (Counter ot)

deriving instance Ord (Counter ot)

deriving instance Show (Counter ot)

data PlayerCounters = PlayerCounters
  { playerCounters_ :: ()
  , playerPoisonCounters :: Int
  }
  deriving (Eq, Ord, Show, Typeable)

instance Semigroup PlayerCounters where
  (<>) :: PlayerCounters -> PlayerCounters -> PlayerCounters
  pc1 <> pc2 =
    PlayerCounters
      { playerCounters_ = ()
      , playerPoisonCounters = playerPoisonCounters pc1 + playerPoisonCounters pc2
      }

instance Monoid PlayerCounters where
  mempty :: PlayerCounters
  mempty =
    PlayerCounters
      { playerCounters_ = ()
      , playerPoisonCounters = 0
      }

data PermanentCounters = PermanentCounters
  { permanentCounters_ :: ()
  , permanentLoyaltyCounters :: Int
  , permanentMinus1Minus1Counters :: Int
  , permanentPlus1Plus1Counters :: Int
  }
  deriving (Eq, Ord, Show, Typeable)

instance Semigroup PermanentCounters where
  (<>) :: PermanentCounters -> PermanentCounters -> PermanentCounters
  pc1 <> pc2 =
    PermanentCounters
      { permanentCounters_ = ()
      , permanentLoyaltyCounters = permanentLoyaltyCounters pc1 + permanentLoyaltyCounters pc2
      , permanentMinus1Minus1Counters = permanentMinus1Minus1Counters pc1 + permanentMinus1Minus1Counters pc2
      , permanentPlus1Plus1Counters = permanentPlus1Plus1Counters pc1 + permanentPlus1Plus1Counters pc2
      }

instance Monoid PermanentCounters where
  mempty :: PermanentCounters
  mempty =
    PermanentCounters
      { permanentCounters_ = ()
      , permanentLoyaltyCounters = 0
      , permanentMinus1Minus1Counters = 0
      , permanentPlus1Plus1Counters = 0
      }

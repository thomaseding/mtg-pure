{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CardSet (
  CardSet (..),
) where

import safe Data.Typeable (Typeable)

data CardSet -- Sorted by release date
  = Alpha
  | ArabianNights
  | Legends
  | TheDark
  | Portal
  | Odyssey
  | Invasion
  | Apocalypse
  | TimeSpiral
  | PlanarChaos
  | TenthEdition
  | OathOfTheGatewatch
  | Kaldheim
  deriving (Eq, Ord, Show, Typeable)

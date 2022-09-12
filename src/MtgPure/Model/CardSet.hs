{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CardSet (
  CardSet (..),
) where

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
  deriving (Eq, Ord, Show)

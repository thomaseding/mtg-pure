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

module MtgPure.Model.Rarity
  ( Rarity (..),
  )
where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Rarity :: Type where
  BasicLand' :: Rarity -- Wow `Woodland Chasm` has this type in reality: https://boardgames.stackexchange.com/a/56457/2209
  Common :: Rarity
  Uncommon :: Rarity
  Rare :: Rarity
  Legendary :: Rarity
  Timeshifted :: Rarity
  deriving (Eq, Ord, Show, Typeable)

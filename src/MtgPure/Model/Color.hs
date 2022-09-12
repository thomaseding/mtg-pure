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

module MtgPure.Model.Color
  ( Colorless,
    Color (..),
  )
where

import Data.Kind (Type)

data Colorless :: Type

data Color :: Type where
  White :: Color
  Blue :: Color
  Black :: Color
  Red :: Color
  Green :: Color
  deriving (Bounded, Enum, Eq, Ord, Show)

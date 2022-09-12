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

module MtgPure.Model.CardType (
  CardType (..),
) where

import safe Data.Kind (Type)

data CardType :: Type where
  CTArtifact :: CardType
  CTCreature :: CardType
  CTEnchantment :: CardType
  CTInstant :: CardType
  CTLand :: CardType
  CTPlaneswalker :: CardType
  CTSorcery :: CardType
  deriving (Bounded, Enum, Eq, Ord, Show)

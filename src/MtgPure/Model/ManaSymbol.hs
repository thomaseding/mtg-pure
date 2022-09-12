{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ManaSymbol
  ( ManaSymbol (..),
  )
where

import safe Data.Kind (Type)
import safe MtgPure.Model.ManaType (ManaType (..))

data ManaSymbol :: ManaType -> Type where
  W :: ManaSymbol 'MTWhite
  U :: ManaSymbol 'MTBlue
  B :: ManaSymbol 'MTBlack
  R :: ManaSymbol 'MTRed
  G :: ManaSymbol 'MTGreen
  C :: ManaSymbol 'MTColorless

deriving instance Eq (ManaSymbol a)

deriving instance Ord (ManaSymbol a)

deriving instance Show (ManaSymbol a)

instance Semigroup (ManaSymbol 'MTWhite) where
  ~W <> ~W = W

instance Semigroup (ManaSymbol 'MTBlue) where
  ~U <> ~U = U

instance Semigroup (ManaSymbol 'MTBlack) where
  ~B <> ~B = B

instance Semigroup (ManaSymbol 'MTRed) where
  ~R <> ~R = R

instance Semigroup (ManaSymbol 'MTGreen) where
  ~G <> ~G = G

instance Monoid (ManaSymbol 'MTWhite) where
  mempty = W

instance Monoid (ManaSymbol 'MTBlue) where
  mempty = U

instance Monoid (ManaSymbol 'MTBlack) where
  mempty = B

instance Monoid (ManaSymbol 'MTRed) where
  mempty = R

instance Monoid (ManaSymbol 'MTGreen) where
  mempty = G

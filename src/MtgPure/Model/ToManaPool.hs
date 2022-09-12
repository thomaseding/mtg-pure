{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ToManaPool (
  ToManaPool (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe MtgPure.Model.Mana (Mana (..))
import safe MtgPure.Model.ManaPool (ManaPool (..), emptyManaPool)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ManaType (ManaType (..))
import safe MtgPure.Model.ToMana (toMana)

class ToManaPool a where
  toManaPool :: a -> ManaPool

instance ToManaPool ManaPool where
  toManaPool = id

instance {-# OVERLAPPABLE #-} (Inst2 ToManaPool a b) => ToManaPool (a, b) where
  toManaPool (a, b) = toManaPool a <> toManaPool b

instance {-# OVERLAPPABLE #-} (Inst3 ToManaPool a b c) => ToManaPool (a, b, c) where
  toManaPool (a, b, c) = toManaPool a <> toManaPool b <> toManaPool c

instance {-# OVERLAPPABLE #-} (Inst4 ToManaPool a b c d) => ToManaPool (a, b, c, d) where
  toManaPool (a, b, c, d) =
    toManaPool a <> toManaPool b <> toManaPool c <> toManaPool d

instance {-# OVERLAPPABLE #-} (Inst5 ToManaPool a b c d e) => ToManaPool (a, b, c, d, e) where
  toManaPool (a, b, c, d, e) =
    toManaPool a <> toManaPool b <> toManaPool c <> toManaPool d <> toManaPool e

instance {-# OVERLAPPABLE #-} (Inst6 ToManaPool a b c d e f) => ToManaPool (a, b, c, d, e, f) where
  toManaPool (a, b, c, d, e, f) =
    toManaPool a
      <> toManaPool b
      <> toManaPool c
      <> toManaPool d
      <> toManaPool e
      <> toManaPool f

instance ToManaPool (Mana 'MTWhite) where
  toManaPool = \case
    x@(WhiteMana _) -> emptyManaPool{poolWhite = x}

instance ToManaPool (Mana 'MTBlue) where
  toManaPool = \case
    x@(BlueMana _) -> emptyManaPool{poolBlue = x}

instance ToManaPool (Mana 'MTBlack) where
  toManaPool = \case
    x@(BlackMana _) -> emptyManaPool{poolBlack = x}

instance ToManaPool (Mana 'MTRed) where
  toManaPool = \case
    x@(RedMana _) -> emptyManaPool{poolRed = x}

instance ToManaPool (Mana 'MTGreen) where
  toManaPool = \case
    x@(GreenMana _) -> emptyManaPool{poolGreen = x}

instance ToManaPool (Mana 'MTColorless) where
  toManaPool = \case
    x@ColorlessMana{} -> emptyManaPool{poolColorless = x}

instance ToManaPool (ManaSymbol a, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool (ManaSymbol a, Int) where
  toManaPool = \case
    x@(W, _) -> emptyManaPool{poolWhite = toMana x}
    x@(U, _) -> emptyManaPool{poolBlue = toMana x}
    x@(B, _) -> emptyManaPool{poolBlack = toMana x}
    x@(R, _) -> emptyManaPool{poolRed = toMana x}
    x@(G, _) -> emptyManaPool{poolGreen = toMana x}
    x@(C, _) -> emptyManaPool{poolColorless = toMana x}

instance ToManaPool (ManaSymbol a) where
  toManaPool = \case
    W -> toManaPool (W, 1 :: Int)
    U -> toManaPool (U, 1 :: Int)
    B -> toManaPool (B, 1 :: Int)
    R -> toManaPool (R, 1 :: Int)
    G -> toManaPool (G, 1 :: Int)
    C -> toManaPool (C, 1 :: Int)

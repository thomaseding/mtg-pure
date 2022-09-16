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

module MtgPure.Model.ToManaCost (
  ToManaCost (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6, Inst7)
import safe Data.Kind (Type)
import safe MtgPure.Model.GenericMana (GenericMana (GenericMana'))
import safe MtgPure.Model.Mana (Mana (..), Snow (..))
import safe MtgPure.Model.ManaCost (ManaCost (..), emptyManaCost)
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ToMana (toMana)

class ToManaCost (mana :: Type) where
  toManaCost :: mana -> ManaCost

instance ToManaCost ManaCost where
  toManaCost = id

instance {-# OVERLAPPABLE #-} (Inst2 ToManaCost a b) => ToManaCost (a, b) where
  toManaCost (a, b) = toManaCost a <> toManaCost b

instance {-# OVERLAPPABLE #-} (Inst3 ToManaCost a b c) => ToManaCost (a, b, c) where
  toManaCost (a, b, c) = toManaCost a <> toManaCost b <> toManaCost c

instance {-# OVERLAPPABLE #-} (Inst4 ToManaCost a b c d) => ToManaCost (a, b, c, d) where
  toManaCost (a, b, c, d) =
    toManaCost a <> toManaCost b <> toManaCost c <> toManaCost d

instance {-# OVERLAPPABLE #-} (Inst5 ToManaCost a b c d e) => ToManaCost (a, b, c, d, e) where
  toManaCost (a, b, c, d, e) =
    toManaCost a <> toManaCost b <> toManaCost c <> toManaCost d <> toManaCost e

instance {-# OVERLAPPABLE #-} (Inst6 ToManaCost a b c d e f) => ToManaCost (a, b, c, d, e, f) where
  toManaCost (a, b, c, d, e, f) =
    toManaCost a
      <> toManaCost b
      <> toManaCost c
      <> toManaCost d
      <> toManaCost e
      <> toManaCost f

instance {-# OVERLAPPABLE #-} (Inst7 ToManaCost a b c d e f g) => ToManaCost (a, b, c, d, e, f, g) where
  toManaCost (a, b, c, d, e, f, g) =
    toManaCost a
      <> toManaCost b
      <> toManaCost c
      <> toManaCost d
      <> toManaCost e
      <> toManaCost f
      <> toManaCost g

instance ToManaCost Integer where
  toManaCost n = toManaCost (fromInteger n :: Int)

instance ToManaCost Int where
  toManaCost = toManaCost . GenericMana'

instance ToManaCost GenericMana where
  toManaCost x = emptyManaCost{costGeneric = GenericMana x}

instance ToManaCost (Mana 'NonSnow a) where
  toManaCost = \case
    x@(WhiteMana _) -> emptyManaCost{costWhite = x}
    x@(BlueMana _) -> emptyManaCost{costBlue = x}
    x@(BlackMana _) -> emptyManaCost{costBlack = x}
    x@(RedMana _) -> emptyManaCost{costRed = x}
    x@(GreenMana _) -> emptyManaCost{costGreen = x}
    x@ColorlessMana{} -> emptyManaCost{costColorless = x}
    x@GenericMana{} -> emptyManaCost{costGeneric = x}

instance ToManaCost (ManaSymbol a, Int) where
  toManaCost = \case
    x@(W, _) -> emptyManaCost{costWhite = toMana x}
    x@(U, _) -> emptyManaCost{costBlue = toMana x}
    x@(B, _) -> emptyManaCost{costBlack = toMana x}
    x@(R, _) -> emptyManaCost{costRed = toMana x}
    x@(G, _) -> emptyManaCost{costGreen = toMana x}
    x@(C, _) -> emptyManaCost{costColorless = toMana x}
    x@(S, _) -> emptyManaCost{costSnow = toMana x}

instance ToManaCost (ManaSymbol a) where
  toManaCost = \case
    W -> toManaCost (W, 1 :: Int)
    U -> toManaCost (U, 1 :: Int)
    B -> toManaCost (B, 1 :: Int)
    R -> toManaCost (R, 1 :: Int)
    G -> toManaCost (G, 1 :: Int)
    C -> toManaCost (C, 1 :: Int)
    S -> toManaCost (S, 1 :: Int)

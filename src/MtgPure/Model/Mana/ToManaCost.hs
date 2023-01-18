{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ToManaCost (
  ToManaCost (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6, Inst7)
import safe Data.Kind (Type)
import safe MtgPure.Model.Mana.Mana (Mana (..), castManaType)
import safe MtgPure.Model.Mana.ManaCost (ManaCost (..), emptyManaCost)
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (IsManaType (..), ManaType (..), SManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Mana.ToMana (toMana)
import safe MtgPure.Model.Variable (Var (..))

-- NOTE: This takes `'Var` instead of `var :: Var` to avoid some authoring ambiguities
class ToManaCost (mana :: Type) where
  toManaCost :: mana -> ManaCost 'Var

instance ToManaCost (ManaCost 'Var) where
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
  toManaCost = toManaCost @(Mana 'Var 'NonSnow 'MTGeneric) . Mana

instance IsManaType snow mt => ToManaCost (Mana 'Var snow mt) where
  toManaCost x = case singManaType @snow @mt of
    SMTWhite -> emptyManaCost{costWhite = x}
    SMTBlue -> emptyManaCost{costBlue = x}
    SMTBlack -> emptyManaCost{costBlack = x}
    SMTRed -> emptyManaCost{costRed = x}
    SMTGreen -> emptyManaCost{costGreen = x}
    SMTColorless -> emptyManaCost{costColorless = x}
    SMTGeneric -> emptyManaCost{costGeneric = x}
    SMTSnow -> emptyManaCost{costSnow = castManaType x}
    SMTHybridBG -> emptyManaCost{costHybridBG = x}

instance ToManaCost (ManaSymbol a, Int) where
  toManaCost = \case
    x@(W, _) -> emptyManaCost{costWhite = toMana x}
    x@(U, _) -> emptyManaCost{costBlue = toMana x}
    x@(B, _) -> emptyManaCost{costBlack = toMana x}
    x@(R, _) -> emptyManaCost{costRed = toMana x}
    x@(G, _) -> emptyManaCost{costGreen = toMana x}
    x@(C, _) -> emptyManaCost{costColorless = toMana x}
    x@(S, _) -> emptyManaCost{costSnow = toMana x}
    x@(BG, _) -> emptyManaCost{costHybridBG = toMana x}

instance ToManaCost (ManaSymbol a) where
  toManaCost = \case
    W -> toManaCost (W, 1 :: Int)
    U -> toManaCost (U, 1 :: Int)
    B -> toManaCost (B, 1 :: Int)
    R -> toManaCost (R, 1 :: Int)
    G -> toManaCost (G, 1 :: Int)
    C -> toManaCost (C, 1 :: Int)
    S -> toManaCost (S, 1 :: Int)
    BG -> toManaCost (BG, 1 :: Int)

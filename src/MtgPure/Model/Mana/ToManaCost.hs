{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ToManaCost (
  ToManaCost (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6, Inst7)
import safe Data.Kind (Type)
import safe MtgPure.Model.Mana.Mana (Mana (..), castManaType, litMana, thawMana)
import safe MtgPure.Model.Mana.ManaCost (
  DynamicManaCost (..),
  HybridManaCost (hybridBG),
  ManaCost (..),
  PhyrexianManaCost (..),
  emptyManaCost,
 )
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
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
    SMTGeneric -> emptyManaCost{costDynamic = mempty{costGeneric = x}}
    SMTSnow -> emptyManaCost{costDynamic = mempty{costSnow = castManaType x}}
    SMTHybridBG -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBG = x}}}
    SMTPhyrexianWhite -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianWhite = x}}}
    SMTPhyrexianBlue -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianBlue = x}}}
    SMTPhyrexianBlack -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianBlack = x}}}
    SMTPhyrexianRed -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianRed = x}}}
    SMTPhyrexianGreen -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianGreen = x}}}
    SMTPhyrexianColorless -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianColorless = x}}}

instance ToManaCost (ManaSymbol a, Int) where
  toManaCost = \case
    x@(W, _) -> emptyManaCost{costWhite = toMana x}
    x@(U, _) -> emptyManaCost{costBlue = toMana x}
    x@(B, _) -> emptyManaCost{costBlack = toMana x}
    x@(R, _) -> emptyManaCost{costRed = toMana x}
    x@(G, _) -> emptyManaCost{costGreen = toMana x}
    x@(C, _) -> emptyManaCost{costColorless = toMana x}
    x@(S, _) -> emptyManaCost{costDynamic = mempty{costSnow = toMana x}}
    x@(BG, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBG = toMana x}}}

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

instance ToManaCost (ManaPool 'NonSnow) where
  toManaCost (ManaPool w u b r g c) =
    emptyManaCost
      { costWhite = litMana w
      , costBlue = litMana u
      , costBlack = litMana b
      , costRed = litMana r
      , costGreen = litMana g
      , costColorless = litMana c
      }

instance ToManaCost (ManaPool 'Snow) where
  toManaCost (ManaPool w u b r g c) =
    emptyManaCost
      { costWhite = go w
      , costBlue = go u
      , costBlack = go b
      , costRed = go r
      , costGreen = go g
      , costColorless = go c
      }
   where
    go = litMana . thawMana

instance ToManaCost CompleteManaPool where
  toManaCost (CompleteManaPool a b) = toManaCost a <> toManaCost b

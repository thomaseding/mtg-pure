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
  HybridManaCost (..),
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
  toManaCost = toManaCost @(Mana 'Var 'NonSnow 'Ty1) . Mana

instance IsManaType snow mt => ToManaCost (Mana 'Var snow mt) where
  toManaCost x = case singManaType @snow @mt of
    STyW -> emptyManaCost{costWhite = x}
    STyU -> emptyManaCost{costBlue = x}
    STyB -> emptyManaCost{costBlack = x}
    STyR -> emptyManaCost{costRed = x}
    STyG -> emptyManaCost{costGreen = x}
    STyC -> emptyManaCost{costColorless = x}
    STy1 -> emptyManaCost{costDynamic = mempty{costGeneric = x}}
    STyS -> emptyManaCost{costDynamic = mempty{costSnow = castManaType x}}
    STyBG -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBG = x}}}
    STyPW -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianWhite = x}}}
    STyPU -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianBlue = x}}}
    STyPB -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianBlack = x}}}
    STyPR -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianRed = x}}}
    STyPG -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianGreen = x}}}
    STyPC -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianColorless = x}}}

instance ToManaCost (ManaSymbol a, Int) where
  toManaCost = \case
    x@(W, _) -> emptyManaCost{costWhite = toMana x}
    x@(U, _) -> emptyManaCost{costBlue = toMana x}
    x@(B, _) -> emptyManaCost{costBlack = toMana x}
    x@(R, _) -> emptyManaCost{costRed = toMana x}
    x@(G, _) -> emptyManaCost{costGreen = toMana x}
    x@(C, _) -> emptyManaCost{costColorless = toMana x}
    x@(S, _) -> emptyManaCost{costDynamic = mempty{costSnow = toMana x}}
    x@(WU, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridWU = toMana x}}}
    x@(UB, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridUB = toMana x}}}
    x@(BR, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBR = toMana x}}}
    x@(RG, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridRG = toMana x}}}
    x@(GW, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridGW = toMana x}}}
    x@(WB, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridWB = toMana x}}}
    x@(UR, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridUR = toMana x}}}
    x@(BG, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBG = toMana x}}}
    x@(RW, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridRW = toMana x}}}
    x@(GU, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridGU = toMana x}}}
    x@(W2, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridW2 = toMana x}}}
    x@(U2, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridU2 = toMana x}}}
    x@(B2, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridB2 = toMana x}}}
    x@(R2, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridR2 = toMana x}}}
    x@(G2, _) -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridG2 = toMana x}}}
    x@(PW, _) -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianWhite = toMana x}}}
    x@(PU, _) -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianBlue = toMana x}}}
    x@(PB, _) -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianBlack = toMana x}}}
    x@(PR, _) -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianRed = toMana x}}}
    x@(PG, _) -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianGreen = toMana x}}}

instance ToManaCost (ManaSymbol a) where
  toManaCost = \case
    W -> toManaCost (W, 1 :: Int)
    U -> toManaCost (U, 1 :: Int)
    B -> toManaCost (B, 1 :: Int)
    R -> toManaCost (R, 1 :: Int)
    G -> toManaCost (G, 1 :: Int)
    C -> toManaCost (C, 1 :: Int)
    S -> toManaCost (S, 1 :: Int)
    WU -> toManaCost (WU, 1 :: Int)
    UB -> toManaCost (UB, 1 :: Int)
    BR -> toManaCost (BR, 1 :: Int)
    RG -> toManaCost (RG, 1 :: Int)
    GW -> toManaCost (GW, 1 :: Int)
    WB -> toManaCost (WB, 1 :: Int)
    UR -> toManaCost (UR, 1 :: Int)
    BG -> toManaCost (BG, 1 :: Int)
    RW -> toManaCost (RW, 1 :: Int)
    GU -> toManaCost (GU, 1 :: Int)
    W2 -> toManaCost (W2, 1 :: Int)
    U2 -> toManaCost (U2, 1 :: Int)
    B2 -> toManaCost (B2, 1 :: Int)
    R2 -> toManaCost (R2, 1 :: Int)
    G2 -> toManaCost (G2, 1 :: Int)
    PW -> toManaCost (PW, 1 :: Int)
    PU -> toManaCost (PU, 1 :: Int)
    PB -> toManaCost (PB, 1 :: Int)
    PR -> toManaCost (PR, 1 :: Int)
    PG -> toManaCost (PG, 1 :: Int)

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

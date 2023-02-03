{-# LANGUAGE Safe #-}
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
import safe MtgPure.Model.Mana.ManaType (IsCostType (..), ManaType (..), ManaTypeToSnow, SCostType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
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

instance (IsCostType mt, snow ~ ManaTypeToSnow mt) => ToManaCost (Mana 'Var snow mt) where
  toManaCost x = case singCostType @mt of
    SCTy1 -> emptyManaCost{costDynamic = mempty{costGeneric = x}}
    SCTyW -> emptyManaCost{costW = x}
    SCTyU -> emptyManaCost{costU = x}
    SCTyB -> emptyManaCost{costB = x}
    SCTyR -> emptyManaCost{costR = x}
    SCTyG -> emptyManaCost{costG = x}
    SCTyC -> emptyManaCost{costC = x}
    SCTyS -> emptyManaCost{costDynamic = mempty{costSnow = castManaType x}}
    SCTyWU -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridWU = x}}}
    SCTyUB -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridUB = x}}}
    SCTyBR -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBR = x}}}
    SCTyRG -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridRG = x}}}
    SCTyGW -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridGW = x}}}
    SCTyWB -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridWB = x}}}
    SCTyUR -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridUR = x}}}
    SCTyBG -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBG = x}}}
    SCTyRW -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridRW = x}}}
    SCTyGU -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridGU = x}}}
    SCTyW2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridW2 = x}}}
    SCTyU2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridU2 = x}}}
    SCTyB2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridB2 = x}}}
    SCTyR2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridR2 = x}}}
    SCTyG2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridG2 = x}}}
    SCTyC2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridC2 = x}}}
    SCTyPW -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianW = x}}}
    SCTyPU -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianU = x}}}
    SCTyPB -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianB = x}}}
    SCTyPR -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianR = x}}}
    SCTyPG -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianG = x}}}
    SCTyPC -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianC = x}}}

instance IsCostType mt => ToManaCost (ManaSymbol mt, Int) where
  toManaCost (_, n) = case singCostType @mt of
    SCTy1 -> emptyManaCost{costDynamic = mempty{costGeneric = Mana n}}
    SCTyW -> emptyManaCost{costW = Mana n}
    SCTyU -> emptyManaCost{costU = Mana n}
    SCTyB -> emptyManaCost{costB = Mana n}
    SCTyR -> emptyManaCost{costR = Mana n}
    SCTyG -> emptyManaCost{costG = Mana n}
    SCTyC -> emptyManaCost{costC = Mana n}
    SCTyS -> emptyManaCost{costDynamic = mempty{costSnow = Mana n}}
    SCTyWU -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridWU = Mana n}}}
    SCTyUB -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridUB = Mana n}}}
    SCTyBR -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBR = Mana n}}}
    SCTyRG -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridRG = Mana n}}}
    SCTyGW -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridGW = Mana n}}}
    SCTyWB -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridWB = Mana n}}}
    SCTyUR -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridUR = Mana n}}}
    SCTyBG -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridBG = Mana n}}}
    SCTyRW -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridRW = Mana n}}}
    SCTyGU -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridGU = Mana n}}}
    SCTyW2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridW2 = Mana n}}}
    SCTyU2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridU2 = Mana n}}}
    SCTyB2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridB2 = Mana n}}}
    SCTyR2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridR2 = Mana n}}}
    SCTyG2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridG2 = Mana n}}}
    SCTyC2 -> emptyManaCost{costDynamic = mempty{costHybrid = mempty{hybridC2 = Mana n}}}
    SCTyPW -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianW = Mana n}}}
    SCTyPU -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianU = Mana n}}}
    SCTyPB -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianB = Mana n}}}
    SCTyPR -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianR = Mana n}}}
    SCTyPG -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianG = Mana n}}}
    SCTyPC -> emptyManaCost{costDynamic = mempty{costPhyrexian = mempty{phyrexianC = Mana n}}}

instance IsCostType mt => ToManaCost (ManaSymbol mt) where
  toManaCost _ = case singCostType @mt of
    SCTy1 -> emptyManaCost{costDynamic = mempty{costGeneric = Mana 1}}
    SCTyW -> toManaCost (W, 1 :: Int)
    SCTyU -> toManaCost (U, 1 :: Int)
    SCTyB -> toManaCost (B, 1 :: Int)
    SCTyR -> toManaCost (R, 1 :: Int)
    SCTyG -> toManaCost (G, 1 :: Int)
    SCTyC -> toManaCost (C, 1 :: Int)
    SCTyS -> toManaCost (S, 1 :: Int)
    SCTyWU -> toManaCost (WU, 1 :: Int)
    SCTyUB -> toManaCost (UB, 1 :: Int)
    SCTyBR -> toManaCost (BR, 1 :: Int)
    SCTyRG -> toManaCost (RG, 1 :: Int)
    SCTyGW -> toManaCost (GW, 1 :: Int)
    SCTyWB -> toManaCost (WB, 1 :: Int)
    SCTyUR -> toManaCost (UR, 1 :: Int)
    SCTyBG -> toManaCost (BG, 1 :: Int)
    SCTyRW -> toManaCost (RW, 1 :: Int)
    SCTyGU -> toManaCost (GU, 1 :: Int)
    SCTyW2 -> toManaCost (W2, 1 :: Int)
    SCTyU2 -> toManaCost (U2, 1 :: Int)
    SCTyB2 -> toManaCost (B2, 1 :: Int)
    SCTyR2 -> toManaCost (R2, 1 :: Int)
    SCTyG2 -> toManaCost (G2, 1 :: Int)
    SCTyC2 -> toManaCost (C2, 1 :: Int)
    SCTyPW -> toManaCost (PW, 1 :: Int)
    SCTyPU -> toManaCost (PU, 1 :: Int)
    SCTyPB -> toManaCost (PB, 1 :: Int)
    SCTyPR -> toManaCost (PR, 1 :: Int)
    SCTyPG -> toManaCost (PG, 1 :: Int)
    SCTyPC -> toManaCost (PC, 1 :: Int)

instance ToManaCost (ManaPool 'NonSnow) where
  toManaCost (ManaPool w u b r g c) =
    emptyManaCost
      { costW = litMana w
      , costU = litMana u
      , costB = litMana b
      , costR = litMana r
      , costG = litMana g
      , costC = litMana c
      }

instance ToManaCost (ManaPool 'Snow) where
  toManaCost (ManaPool w u b r g c) =
    emptyManaCost
      { costW = go w
      , costU = go u
      , costB = go b
      , costR = go r
      , costG = go g
      , costC = go c
      }
   where
    go = litMana . thawMana

instance ToManaCost CompleteManaPool where
  toManaCost (CompleteManaPool a b) = toManaCost a <> toManaCost b

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ToManaPool (
  ToManaPool (..),
  ToCompleteManaPool (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Mana.ToMana (toMana)
import safe MtgPure.Model.Variable (Var (..))

----------------------------------------

class ToManaPool (snow :: Snow) (mana :: Type) | mana -> snow where
  toManaPool :: mana -> ManaPool snow

instance {-# OVERLAPPABLE #-} (Inst2 (ToManaPool 'NonSnow) a b) => ToManaPool 'NonSnow (a, b) where
  toManaPool (a, b) = toManaPool a <> toManaPool b

instance {-# OVERLAPPABLE #-} (Inst3 (ToManaPool 'NonSnow) a b c) => ToManaPool 'NonSnow (a, b, c) where
  toManaPool (a, b, c) = toManaPool a <> toManaPool b <> toManaPool c

instance {-# OVERLAPPABLE #-} (Inst4 (ToManaPool 'NonSnow) a b c d) => ToManaPool 'NonSnow (a, b, c, d) where
  toManaPool (a, b, c, d) =
    toManaPool a <> toManaPool b <> toManaPool c <> toManaPool d

instance {-# OVERLAPPABLE #-} (Inst5 (ToManaPool 'NonSnow) a b c d e) => ToManaPool 'NonSnow (a, b, c, d, e) where
  toManaPool (a, b, c, d, e) =
    toManaPool a
      <> toManaPool b
      <> toManaPool c
      <> toManaPool d
      <> toManaPool e

instance {-# OVERLAPPABLE #-} (Inst6 (ToManaPool 'NonSnow) a b c d e f) => ToManaPool 'NonSnow (a, b, c, d, e, f) where
  toManaPool (a, b, c, d, e, f) =
    toManaPool a
      <> toManaPool b
      <> toManaPool c
      <> toManaPool d
      <> toManaPool e
      <> toManaPool f

instance ToManaPool snow (Mana 'NoVar snow 'TyW) where
  toManaPool = \case
    x@Mana{} -> mempty{poolWhite = x}

instance ToManaPool snow (Mana 'NoVar snow 'TyU) where
  toManaPool = \case
    x@Mana{} -> mempty{poolBlue = x}

instance ToManaPool snow (Mana 'NoVar snow 'TyB) where
  toManaPool = \case
    x@Mana{} -> mempty{poolBlack = x}

instance ToManaPool snow (Mana 'NoVar snow 'TyR) where
  toManaPool = \case
    x@Mana{} -> mempty{poolRed = x}

instance ToManaPool snow (Mana 'NoVar snow 'TyG) where
  toManaPool = \case
    x@Mana{} -> mempty{poolGreen = x}

instance ToManaPool snow (Mana 'NoVar snow 'TyC) where
  toManaPool = \case
    x@Mana{} -> mempty{poolColorless = x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyW, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyU, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyB, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyR, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyG, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyC, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyW, Int) where
  toManaPool = \case
    x@(W, _) -> mempty{poolWhite = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyU, Int) where
  toManaPool = \case
    x@(U, _) -> mempty{poolBlue = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyB, Int) where
  toManaPool = \case
    x@(B, _) -> mempty{poolBlack = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyR, Int) where
  toManaPool = \case
    x@(R, _) -> mempty{poolRed = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyG, Int) where
  toManaPool = \case
    x@(G, _) -> mempty{poolGreen = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyC, Int) where
  toManaPool = \case
    x@(C, _) -> mempty{poolColorless = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyW) where
  toManaPool = \case
    W -> toManaPool (W, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyU) where
  toManaPool = \case
    U -> toManaPool (U, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyB) where
  toManaPool = \case
    B -> toManaPool (B, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyR) where
  toManaPool = \case
    R -> toManaPool (R, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyG) where
  toManaPool = \case
    G -> toManaPool (G, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyC) where
  toManaPool = \case
    C -> toManaPool (C, 1 :: Int)

instance ToManaPool 'NonSnow Color where
  toManaPool = \case
    White -> toManaPool W
    Blue -> toManaPool U
    Black -> toManaPool B
    Red -> toManaPool R
    Green -> toManaPool G

instance ToManaPool 'NonSnow (Color, Int) where
  toManaPool = \case
    (White, n) -> toManaPool (W, n)
    (Blue, n) -> toManaPool (U, n)
    (Black, n) -> toManaPool (B, n)
    (Red, n) -> toManaPool (R, n)
    (Green, n) -> toManaPool (G, n)

----------------------------------------

class ToCompleteManaPool (mana :: Type) where
  toCompleteManaPool :: mana -> CompleteManaPool

instance ToCompleteManaPool CompleteManaPool where
  toCompleteManaPool = id

instance ToCompleteManaPool (ManaPool 'Snow) where
  toCompleteManaPool pool = mempty{poolSnow = pool}

instance ToCompleteManaPool (ManaPool 'NonSnow) where
  toCompleteManaPool pool = mempty{poolNonSnow = pool}

instance {-# OVERLAPPABLE #-} (Inst2 ToCompleteManaPool a b) => ToCompleteManaPool (a, b) where
  toCompleteManaPool (a, b) = toCompleteManaPool a <> toCompleteManaPool b

instance {-# OVERLAPPABLE #-} (Inst3 ToCompleteManaPool a b c) => ToCompleteManaPool (a, b, c) where
  toCompleteManaPool (a, b, c) = toCompleteManaPool a <> toCompleteManaPool b <> toCompleteManaPool c

instance {-# OVERLAPPABLE #-} (Inst4 ToCompleteManaPool a b c d) => ToCompleteManaPool (a, b, c, d) where
  toCompleteManaPool (a, b, c, d) =
    toCompleteManaPool a <> toCompleteManaPool b <> toCompleteManaPool c <> toCompleteManaPool d

instance {-# OVERLAPPABLE #-} (Inst5 ToCompleteManaPool a b c d e) => ToCompleteManaPool (a, b, c, d, e) where
  toCompleteManaPool (a, b, c, d, e) =
    toCompleteManaPool a
      <> toCompleteManaPool b
      <> toCompleteManaPool c
      <> toCompleteManaPool d
      <> toCompleteManaPool e

instance {-# OVERLAPPABLE #-} (Inst6 ToCompleteManaPool a b c d e f) => ToCompleteManaPool (a, b, c, d, e, f) where
  toCompleteManaPool (a, b, c, d, e, f) =
    toCompleteManaPool a
      <> toCompleteManaPool b
      <> toCompleteManaPool c
      <> toCompleteManaPool d
      <> toCompleteManaPool e
      <> toCompleteManaPool f

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyW) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolWhite = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyU) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolBlue = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyB) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolBlack = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyR) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolRed = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyG) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolGreen = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyC) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolColorless = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyW) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolWhite = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyU) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolBlue = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyB) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolBlack = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyR) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolRed = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyG) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolGreen = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyC) where
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolColorless = x}}

instance ToCompleteManaPool (ManaSymbol 'TyW, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyU, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyB, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyR, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyG, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyC, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyW, Int) where
  toCompleteManaPool = \case
    x@(W, _) -> mempty{poolNonSnow = mempty{poolWhite = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyU, Int) where
  toCompleteManaPool = \case
    x@(U, _) -> mempty{poolNonSnow = mempty{poolBlue = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyB, Int) where
  toCompleteManaPool = \case
    x@(B, _) -> mempty{poolNonSnow = mempty{poolBlack = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyR, Int) where
  toCompleteManaPool = \case
    x@(R, _) -> mempty{poolNonSnow = mempty{poolRed = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyG, Int) where
  toCompleteManaPool = \case
    x@(G, _) -> mempty{poolNonSnow = mempty{poolGreen = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyC, Int) where
  toCompleteManaPool = \case
    x@(C, _) -> mempty{poolNonSnow = mempty{poolColorless = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyW) where
  toCompleteManaPool = \case
    W -> toCompleteManaPool (W, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyU) where
  toCompleteManaPool = \case
    U -> toCompleteManaPool (U, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyB) where
  toCompleteManaPool = \case
    B -> toCompleteManaPool (B, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyR) where
  toCompleteManaPool = \case
    R -> toCompleteManaPool (R, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyG) where
  toCompleteManaPool = \case
    G -> toCompleteManaPool (G, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyC) where
  toCompleteManaPool = \case
    C -> toCompleteManaPool (C, 1 :: Int)

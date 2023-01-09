{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ToManaPool (
  ToManaPool (..),
  ToCompleteManaPool (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Mana (Mana (..), Snow (..))
import safe MtgPure.Model.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ManaType (ManaType (..))
import safe MtgPure.Model.ToMana (toMana)
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

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'MTWhite) where
  toManaPool = \case
    x@(WhiteMana _) -> mempty{poolWhite = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'MTBlue) where
  toManaPool = \case
    x@(BlueMana _) -> mempty{poolBlue = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'MTBlack) where
  toManaPool = \case
    x@(BlackMana _) -> mempty{poolBlack = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'MTRed) where
  toManaPool = \case
    x@(RedMana _) -> mempty{poolRed = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'MTGreen) where
  toManaPool = \case
    x@(GreenMana _) -> mempty{poolGreen = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'MTColorless) where
  toManaPool = \case
    x@ColorlessMana{} -> mempty{poolColorless = x}

instance ToManaPool 'NonSnow (ManaSymbol 'MTWhite, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTBlue, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTBlack, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTRed, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTGreen, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTColorless, Integer) where
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTWhite, Int) where
  toManaPool = \case
    x@(W, _) -> mempty{poolWhite = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'MTBlue, Int) where
  toManaPool = \case
    x@(U, _) -> mempty{poolBlue = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'MTBlack, Int) where
  toManaPool = \case
    x@(B, _) -> mempty{poolBlack = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'MTRed, Int) where
  toManaPool = \case
    x@(R, _) -> mempty{poolRed = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'MTGreen, Int) where
  toManaPool = \case
    x@(G, _) -> mempty{poolGreen = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'MTColorless, Int) where
  toManaPool = \case
    x@(C, _) -> mempty{poolColorless = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'MTWhite) where
  toManaPool = \case
    W -> toManaPool (W, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTBlue) where
  toManaPool = \case
    U -> toManaPool (U, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTBlack) where
  toManaPool = \case
    B -> toManaPool (B, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTRed) where
  toManaPool = \case
    R -> toManaPool (R, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTGreen) where
  toManaPool = \case
    G -> toManaPool (G, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'MTColorless) where
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

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'MTWhite) where
  toCompleteManaPool = \case
    x@(WhiteMana _) -> mempty{poolNonSnow = mempty{poolWhite = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'MTBlue) where
  toCompleteManaPool = \case
    x@(BlueMana _) -> mempty{poolNonSnow = mempty{poolBlue = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'MTBlack) where
  toCompleteManaPool = \case
    x@(BlackMana _) -> mempty{poolNonSnow = mempty{poolBlack = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'MTRed) where
  toCompleteManaPool = \case
    x@(RedMana _) -> mempty{poolNonSnow = mempty{poolRed = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'MTGreen) where
  toCompleteManaPool = \case
    x@(GreenMana _) -> mempty{poolNonSnow = mempty{poolGreen = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'MTColorless) where
  toCompleteManaPool = \case
    x@ColorlessMana{} -> mempty{poolNonSnow = mempty{poolColorless = x}}

instance ToCompleteManaPool (ManaSymbol 'MTWhite, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTBlue, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTBlack, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTRed, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTGreen, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTColorless, Integer) where
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTWhite, Int) where
  toCompleteManaPool = \case
    x@(W, _) -> mempty{poolNonSnow = mempty{poolWhite = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'MTBlue, Int) where
  toCompleteManaPool = \case
    x@(U, _) -> mempty{poolNonSnow = mempty{poolBlue = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'MTBlack, Int) where
  toCompleteManaPool = \case
    x@(B, _) -> mempty{poolNonSnow = mempty{poolBlack = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'MTRed, Int) where
  toCompleteManaPool = \case
    x@(R, _) -> mempty{poolNonSnow = mempty{poolRed = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'MTGreen, Int) where
  toCompleteManaPool = \case
    x@(G, _) -> mempty{poolNonSnow = mempty{poolGreen = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'MTColorless, Int) where
  toCompleteManaPool = \case
    x@(C, _) -> mempty{poolNonSnow = mempty{poolColorless = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'MTWhite) where
  toCompleteManaPool = \case
    W -> toCompleteManaPool (W, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTBlue) where
  toCompleteManaPool = \case
    U -> toCompleteManaPool (U, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTBlack) where
  toCompleteManaPool = \case
    B -> toCompleteManaPool (B, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTRed) where
  toCompleteManaPool = \case
    R -> toCompleteManaPool (R, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTGreen) where
  toCompleteManaPool = \case
    G -> toCompleteManaPool (G, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'MTColorless) where
  toCompleteManaPool = \case
    C -> toCompleteManaPool (C, 1 :: Int)

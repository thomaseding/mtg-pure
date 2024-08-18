{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ToManaPool (
  ToManaPool (..),
  ToCompleteManaPool (..),
) where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
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

instance ToManaPool 'NonSnow () where
  toManaPool :: () -> ManaPool 'NonSnow
  toManaPool _ = mempty

-- instance {-# OVERLAPPABLE #-} (Inst2 (ToManaPool 'NonSnow) a b) => ToManaPool 'NonSnow (a, b) where
--   toManaPool (a, b) = toManaPool a <> toManaPool b

instance {-# OVERLAPPABLE #-} (Inst3 (ToManaPool 'NonSnow) a b c) => ToManaPool 'NonSnow (a, b, c) where
  toManaPool :: (Inst3 (ToManaPool 'NonSnow) a b c) => (a, b, c) -> ManaPool 'NonSnow
  toManaPool (a, b, c) = toManaPool a <> toManaPool b <> toManaPool c

instance {-# OVERLAPPABLE #-} (Inst4 (ToManaPool 'NonSnow) a b c d) => ToManaPool 'NonSnow (a, b, c, d) where
  toManaPool :: (Inst4 (ToManaPool 'NonSnow) a b c d) => (a, b, c, d) -> ManaPool 'NonSnow
  toManaPool (a, b, c, d) =
    toManaPool a <> toManaPool b <> toManaPool c <> toManaPool d

instance {-# OVERLAPPABLE #-} (Inst5 (ToManaPool 'NonSnow) a b c d e) => ToManaPool 'NonSnow (a, b, c, d, e) where
  toManaPool :: (Inst5 (ToManaPool 'NonSnow) a b c d e) => (a, b, c, d, e) -> ManaPool 'NonSnow
  toManaPool (a, b, c, d, e) =
    toManaPool a
      <> toManaPool b
      <> toManaPool c
      <> toManaPool d
      <> toManaPool e

instance {-# OVERLAPPABLE #-} (Inst6 (ToManaPool 'NonSnow) a b c d e f) => ToManaPool 'NonSnow (a, b, c, d, e, f) where
  toManaPool :: (Inst6 (ToManaPool 'NonSnow) a b c d e f) => (a, b, c, d, e, f) -> ManaPool 'NonSnow
  toManaPool (a, b, c, d, e, f) =
    toManaPool a
      <> toManaPool b
      <> toManaPool c
      <> toManaPool d
      <> toManaPool e
      <> toManaPool f

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'TyW) where
  toManaPool :: Mana 'NoVar 'NonSnow 'TyW -> ManaPool 'NonSnow
  toManaPool = \case
    x@Mana{} -> mempty{poolW = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'TyU) where
  toManaPool :: Mana 'NoVar 'NonSnow 'TyU -> ManaPool 'NonSnow
  toManaPool = \case
    x@Mana{} -> mempty{poolU = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'TyB) where
  toManaPool :: Mana 'NoVar 'NonSnow 'TyB -> ManaPool 'NonSnow
  toManaPool = \case
    x@Mana{} -> mempty{poolB = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'TyR) where
  toManaPool :: Mana 'NoVar 'NonSnow 'TyR -> ManaPool 'NonSnow
  toManaPool = \case
    x@Mana{} -> mempty{poolR = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'TyG) where
  toManaPool :: Mana 'NoVar 'NonSnow 'TyG -> ManaPool 'NonSnow
  toManaPool = \case
    x@Mana{} -> mempty{poolG = x}

instance ToManaPool 'NonSnow (Mana 'NoVar 'NonSnow 'TyC) where
  toManaPool :: Mana 'NoVar 'NonSnow 'TyC -> ManaPool 'NonSnow
  toManaPool = \case
    x@Mana{} -> mempty{poolC = x}

instance ToManaPool 'Snow (Mana 'NoVar 'Snow 'TyW) where
  toManaPool :: Mana 'NoVar 'Snow 'TyW -> ManaPool 'Snow
  toManaPool = \case
    x@Mana{} -> mempty{poolW = x}

instance ToManaPool 'Snow (Mana 'NoVar 'Snow 'TyU) where
  toManaPool :: Mana 'NoVar 'Snow 'TyU -> ManaPool 'Snow
  toManaPool = \case
    x@Mana{} -> mempty{poolU = x}

instance ToManaPool 'Snow (Mana 'NoVar 'Snow 'TyB) where
  toManaPool :: Mana 'NoVar 'Snow 'TyB -> ManaPool 'Snow
  toManaPool = \case
    x@Mana{} -> mempty{poolB = x}

instance ToManaPool 'Snow (Mana 'NoVar 'Snow 'TyR) where
  toManaPool :: Mana 'NoVar 'Snow 'TyR -> ManaPool 'Snow
  toManaPool = \case
    x@Mana{} -> mempty{poolR = x}

instance ToManaPool 'Snow (Mana 'NoVar 'Snow 'TyG) where
  toManaPool :: Mana 'NoVar 'Snow 'TyG -> ManaPool 'Snow
  toManaPool = \case
    x@Mana{} -> mempty{poolG = x}

instance ToManaPool 'Snow (Mana 'NoVar 'Snow 'TyC) where
  toManaPool :: Mana 'NoVar 'Snow 'TyC -> ManaPool 'Snow
  toManaPool = \case
    x@Mana{} -> mempty{poolC = x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyW, Integer) where
  toManaPool :: (ManaSymbol 'TyW, Integer) -> ManaPool 'NonSnow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyU, Integer) where
  toManaPool :: (ManaSymbol 'TyU, Integer) -> ManaPool 'NonSnow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyB, Integer) where
  toManaPool :: (ManaSymbol 'TyB, Integer) -> ManaPool 'NonSnow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyR, Integer) where
  toManaPool :: (ManaSymbol 'TyR, Integer) -> ManaPool 'NonSnow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyG, Integer) where
  toManaPool :: (ManaSymbol 'TyG, Integer) -> ManaPool 'NonSnow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyC, Integer) where
  toManaPool :: (ManaSymbol 'TyC, Integer) -> ManaPool 'NonSnow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySW, Integer) where
  toManaPool :: (ManaSymbol 'TySW, Integer) -> ManaPool 'Snow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySU, Integer) where
  toManaPool :: (ManaSymbol 'TySU, Integer) -> ManaPool 'Snow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySB, Integer) where
  toManaPool :: (ManaSymbol 'TySB, Integer) -> ManaPool 'Snow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySR, Integer) where
  toManaPool :: (ManaSymbol 'TySR, Integer) -> ManaPool 'Snow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySG, Integer) where
  toManaPool :: (ManaSymbol 'TySG, Integer) -> ManaPool 'Snow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySC, Integer) where
  toManaPool :: (ManaSymbol 'TySC, Integer) -> ManaPool 'Snow
  toManaPool (sym, n) = toManaPool (sym, fromInteger n :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyW, Int) where
  toManaPool :: (ManaSymbol 'TyW, Int) -> ManaPool 'NonSnow
  toManaPool = \case
    x@(W, _) -> mempty{poolW = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyU, Int) where
  toManaPool :: (ManaSymbol 'TyU, Int) -> ManaPool 'NonSnow
  toManaPool = \case
    x@(U, _) -> mempty{poolU = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyB, Int) where
  toManaPool :: (ManaSymbol 'TyB, Int) -> ManaPool 'NonSnow
  toManaPool = \case
    x@(B, _) -> mempty{poolB = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyR, Int) where
  toManaPool :: (ManaSymbol 'TyR, Int) -> ManaPool 'NonSnow
  toManaPool = \case
    x@(R, _) -> mempty{poolR = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyG, Int) where
  toManaPool :: (ManaSymbol 'TyG, Int) -> ManaPool 'NonSnow
  toManaPool = \case
    x@(G, _) -> mempty{poolG = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyC, Int) where
  toManaPool :: (ManaSymbol 'TyC, Int) -> ManaPool 'NonSnow
  toManaPool = \case
    x@(C, _) -> mempty{poolC = toMana x}

instance ToManaPool 'Snow (ManaSymbol 'TySW, Int) where
  toManaPool :: (ManaSymbol 'TySW, Int) -> ManaPool 'Snow
  toManaPool = \case
    x@(SW, _) -> mempty{poolW = toMana x}

instance ToManaPool 'Snow (ManaSymbol 'TySU, Int) where
  toManaPool :: (ManaSymbol 'TySU, Int) -> ManaPool 'Snow
  toManaPool = \case
    x@(SU, _) -> mempty{poolU = toMana x}

instance ToManaPool 'Snow (ManaSymbol 'TySB, Int) where
  toManaPool :: (ManaSymbol 'TySB, Int) -> ManaPool 'Snow
  toManaPool = \case
    x@(SB, _) -> mempty{poolB = toMana x}

instance ToManaPool 'Snow (ManaSymbol 'TySR, Int) where
  toManaPool :: (ManaSymbol 'TySR, Int) -> ManaPool 'Snow
  toManaPool = \case
    x@(SR, _) -> mempty{poolR = toMana x}

instance ToManaPool 'Snow (ManaSymbol 'TySG, Int) where
  toManaPool :: (ManaSymbol 'TySG, Int) -> ManaPool 'Snow
  toManaPool = \case
    x@(SG, _) -> mempty{poolG = toMana x}

instance ToManaPool 'Snow (ManaSymbol 'TySC, Int) where
  toManaPool :: (ManaSymbol 'TySC, Int) -> ManaPool 'Snow
  toManaPool = \case
    x@(SC, _) -> mempty{poolC = toMana x}

instance ToManaPool 'NonSnow (ManaSymbol 'TyW) where
  toManaPool :: ManaSymbol 'TyW -> ManaPool 'NonSnow
  toManaPool = \case
    W -> toManaPool (W, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyU) where
  toManaPool :: ManaSymbol 'TyU -> ManaPool 'NonSnow
  toManaPool = \case
    U -> toManaPool (U, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyB) where
  toManaPool :: ManaSymbol 'TyB -> ManaPool 'NonSnow
  toManaPool = \case
    B -> toManaPool (B, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyR) where
  toManaPool :: ManaSymbol 'TyR -> ManaPool 'NonSnow
  toManaPool = \case
    R -> toManaPool (R, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyG) where
  toManaPool :: ManaSymbol 'TyG -> ManaPool 'NonSnow
  toManaPool = \case
    G -> toManaPool (G, 1 :: Int)

instance ToManaPool 'NonSnow (ManaSymbol 'TyC) where
  toManaPool :: ManaSymbol 'TyC -> ManaPool 'NonSnow
  toManaPool = \case
    C -> toManaPool (C, 1 :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySW) where
  toManaPool :: ManaSymbol 'TySW -> ManaPool 'Snow
  toManaPool = \case
    SW -> toManaPool (SW, 1 :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySU) where
  toManaPool :: ManaSymbol 'TySU -> ManaPool 'Snow
  toManaPool = \case
    SU -> toManaPool (SU, 1 :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySB) where
  toManaPool :: ManaSymbol 'TySB -> ManaPool 'Snow
  toManaPool = \case
    SB -> toManaPool (SB, 1 :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySR) where
  toManaPool :: ManaSymbol 'TySR -> ManaPool 'Snow
  toManaPool = \case
    SR -> toManaPool (SR, 1 :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySG) where
  toManaPool :: ManaSymbol 'TySG -> ManaPool 'Snow
  toManaPool = \case
    SG -> toManaPool (SG, 1 :: Int)

instance ToManaPool 'Snow (ManaSymbol 'TySC) where
  toManaPool :: ManaSymbol 'TySC -> ManaPool 'Snow
  toManaPool = \case
    SC -> toManaPool (SC, 1 :: Int)

-- instance ToManaPool 'NonSnow Color where
--   toManaPool :: Color -> ManaPool 'NonSnow
--   toManaPool = \case
--     White -> toManaPool W
--     Blue -> toManaPool U
--     Black -> toManaPool B
--     Red -> toManaPool R
--     Green -> toManaPool G

-- instance ToManaPool 'NonSnow (Color, Int) where
--   toManaPool :: (Color, Int) -> ManaPool 'NonSnow
--   toManaPool = \case
--     (White, n) -> toManaPool (W, n)
--     (Blue, n) -> toManaPool (U, n)
--     (Black, n) -> toManaPool (B, n)
--     (Red, n) -> toManaPool (R, n)
--     (Green, n) -> toManaPool (G, n)

----------------------------------------

class ToCompleteManaPool (mana :: Type) where
  toCompleteManaPool :: mana -> CompleteManaPool

instance ToCompleteManaPool CompleteManaPool where
  toCompleteManaPool :: CompleteManaPool -> CompleteManaPool
  toCompleteManaPool = id

instance ToCompleteManaPool (ManaPool 'Snow) where
  toCompleteManaPool :: ManaPool 'Snow -> CompleteManaPool
  toCompleteManaPool pool = mempty{poolSnow = pool}

instance ToCompleteManaPool (ManaPool 'NonSnow) where
  toCompleteManaPool :: ManaPool 'NonSnow -> CompleteManaPool
  toCompleteManaPool pool = mempty{poolNonSnow = pool}

instance {-# OVERLAPPABLE #-} (Inst2 ToCompleteManaPool a b) => ToCompleteManaPool (a, b) where
  toCompleteManaPool :: (Inst2 ToCompleteManaPool a b) => (a, b) -> CompleteManaPool
  toCompleteManaPool (a, b) = toCompleteManaPool a <> toCompleteManaPool b

instance {-# OVERLAPPABLE #-} (Inst3 ToCompleteManaPool a b c) => ToCompleteManaPool (a, b, c) where
  toCompleteManaPool :: (Inst3 ToCompleteManaPool a b c) => (a, b, c) -> CompleteManaPool
  toCompleteManaPool (a, b, c) = toCompleteManaPool a <> toCompleteManaPool b <> toCompleteManaPool c

instance {-# OVERLAPPABLE #-} (Inst4 ToCompleteManaPool a b c d) => ToCompleteManaPool (a, b, c, d) where
  toCompleteManaPool :: (Inst4 ToCompleteManaPool a b c d) => (a, b, c, d) -> CompleteManaPool
  toCompleteManaPool (a, b, c, d) =
    toCompleteManaPool a <> toCompleteManaPool b <> toCompleteManaPool c <> toCompleteManaPool d

instance {-# OVERLAPPABLE #-} (Inst5 ToCompleteManaPool a b c d e) => ToCompleteManaPool (a, b, c, d, e) where
  toCompleteManaPool :: (Inst5 ToCompleteManaPool a b c d e) => (a, b, c, d, e) -> CompleteManaPool
  toCompleteManaPool (a, b, c, d, e) =
    toCompleteManaPool a
      <> toCompleteManaPool b
      <> toCompleteManaPool c
      <> toCompleteManaPool d
      <> toCompleteManaPool e

instance {-# OVERLAPPABLE #-} (Inst6 ToCompleteManaPool a b c d e f) => ToCompleteManaPool (a, b, c, d, e, f) where
  toCompleteManaPool :: (Inst6 ToCompleteManaPool a b c d e f) => (a, b, c, d, e, f) -> CompleteManaPool
  toCompleteManaPool (a, b, c, d, e, f) =
    toCompleteManaPool a
      <> toCompleteManaPool b
      <> toCompleteManaPool c
      <> toCompleteManaPool d
      <> toCompleteManaPool e
      <> toCompleteManaPool f

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyW) where
  toCompleteManaPool :: Mana 'NoVar 'NonSnow 'TyW -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolW = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyU) where
  toCompleteManaPool :: Mana 'NoVar 'NonSnow 'TyU -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolU = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyB) where
  toCompleteManaPool :: Mana 'NoVar 'NonSnow 'TyB -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolB = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyR) where
  toCompleteManaPool :: Mana 'NoVar 'NonSnow 'TyR -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolR = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyG) where
  toCompleteManaPool :: Mana 'NoVar 'NonSnow 'TyG -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolG = x}}

instance ToCompleteManaPool (Mana 'NoVar 'NonSnow 'TyC) where
  toCompleteManaPool :: Mana 'NoVar 'NonSnow 'TyC -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolNonSnow = mempty{poolC = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyW) where
  toCompleteManaPool :: Mana 'NoVar 'Snow 'TyW -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolW = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyU) where
  toCompleteManaPool :: Mana 'NoVar 'Snow 'TyU -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolU = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyB) where
  toCompleteManaPool :: Mana 'NoVar 'Snow 'TyB -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolB = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyR) where
  toCompleteManaPool :: Mana 'NoVar 'Snow 'TyR -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolR = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyG) where
  toCompleteManaPool :: Mana 'NoVar 'Snow 'TyG -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolG = x}}

instance ToCompleteManaPool (Mana 'NoVar 'Snow 'TyC) where
  toCompleteManaPool :: Mana 'NoVar 'Snow 'TyC -> CompleteManaPool
  toCompleteManaPool = \case
    x@Mana{} -> mempty{poolSnow = mempty{poolC = x}}

instance ToCompleteManaPool (ManaSymbol 'TyW, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TyW, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyU, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TyU, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyB, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TyB, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyR, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TyR, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyG, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TyG, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyC, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TyC, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySW, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TySW, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySU, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TySU, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySB, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TySB, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySR, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TySR, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySG, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TySG, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySC, Integer) where
  toCompleteManaPool :: (ManaSymbol 'TySC, Integer) -> CompleteManaPool
  toCompleteManaPool (sym, n) = toCompleteManaPool (sym, fromInteger n :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyW, Int) where
  toCompleteManaPool :: (ManaSymbol 'TyW, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(W, _) -> mempty{poolNonSnow = mempty{poolW = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyU, Int) where
  toCompleteManaPool :: (ManaSymbol 'TyU, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(U, _) -> mempty{poolNonSnow = mempty{poolU = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyB, Int) where
  toCompleteManaPool :: (ManaSymbol 'TyB, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(B, _) -> mempty{poolNonSnow = mempty{poolB = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyR, Int) where
  toCompleteManaPool :: (ManaSymbol 'TyR, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(R, _) -> mempty{poolNonSnow = mempty{poolR = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyG, Int) where
  toCompleteManaPool :: (ManaSymbol 'TyG, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(G, _) -> mempty{poolNonSnow = mempty{poolG = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyC, Int) where
  toCompleteManaPool :: (ManaSymbol 'TyC, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(C, _) -> mempty{poolNonSnow = mempty{poolC = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TySW, Int) where
  toCompleteManaPool :: (ManaSymbol 'TySW, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(SW, _) -> mempty{poolSnow = mempty{poolW = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TySU, Int) where
  toCompleteManaPool :: (ManaSymbol 'TySU, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(SU, _) -> mempty{poolSnow = mempty{poolU = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TySB, Int) where
  toCompleteManaPool :: (ManaSymbol 'TySB, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(SB, _) -> mempty{poolSnow = mempty{poolB = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TySR, Int) where
  toCompleteManaPool :: (ManaSymbol 'TySR, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(SR, _) -> mempty{poolSnow = mempty{poolR = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TySG, Int) where
  toCompleteManaPool :: (ManaSymbol 'TySG, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(SG, _) -> mempty{poolSnow = mempty{poolG = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TySC, Int) where
  toCompleteManaPool :: (ManaSymbol 'TySC, Int) -> CompleteManaPool
  toCompleteManaPool = \case
    x@(SC, _) -> mempty{poolSnow = mempty{poolC = toMana x}}

instance ToCompleteManaPool (ManaSymbol 'TyW) where
  toCompleteManaPool :: ManaSymbol 'TyW -> CompleteManaPool
  toCompleteManaPool = \case
    W -> toCompleteManaPool (W, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyU) where
  toCompleteManaPool :: ManaSymbol 'TyU -> CompleteManaPool
  toCompleteManaPool = \case
    U -> toCompleteManaPool (U, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyB) where
  toCompleteManaPool :: ManaSymbol 'TyB -> CompleteManaPool
  toCompleteManaPool = \case
    B -> toCompleteManaPool (B, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyR) where
  toCompleteManaPool :: ManaSymbol 'TyR -> CompleteManaPool
  toCompleteManaPool = \case
    R -> toCompleteManaPool (R, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyG) where
  toCompleteManaPool :: ManaSymbol 'TyG -> CompleteManaPool
  toCompleteManaPool = \case
    G -> toCompleteManaPool (G, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TyC) where
  toCompleteManaPool :: ManaSymbol 'TyC -> CompleteManaPool
  toCompleteManaPool = \case
    C -> toCompleteManaPool (C, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySW) where
  toCompleteManaPool :: ManaSymbol 'TySW -> CompleteManaPool
  toCompleteManaPool = \case
    SW -> toCompleteManaPool (SW, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySU) where
  toCompleteManaPool :: ManaSymbol 'TySU -> CompleteManaPool
  toCompleteManaPool = \case
    SU -> toCompleteManaPool (SU, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySB) where
  toCompleteManaPool :: ManaSymbol 'TySB -> CompleteManaPool
  toCompleteManaPool = \case
    SB -> toCompleteManaPool (SB, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySR) where
  toCompleteManaPool :: ManaSymbol 'TySR -> CompleteManaPool
  toCompleteManaPool = \case
    SR -> toCompleteManaPool (SR, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySG) where
  toCompleteManaPool :: ManaSymbol 'TySG -> CompleteManaPool
  toCompleteManaPool = \case
    SG -> toCompleteManaPool (SG, 1 :: Int)

instance ToCompleteManaPool (ManaSymbol 'TySC) where
  toCompleteManaPool :: ManaSymbol 'TySC -> CompleteManaPool
  toCompleteManaPool = \case
    SC -> toCompleteManaPool (SC, 1 :: Int)

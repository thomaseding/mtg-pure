{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}

module Data.Read.Sep (
  List (..),
  Tuple (..),
  TupleList (..),
) where

import safe Data.Inst (
  Inst1,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
 )
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.List.Split as List
import safe Data.Read.Symbol (LitSymbol (..))
import safe Text.Read (readMaybe)

sep :: forall (s :: Type). LitSymbol s => String -> [String]
sep = filter (not . null) . List.splitOn (litSymbol @Type @s)

unSep :: forall (s :: Type). LitSymbol s => [String] -> String
unSep = List.intercalate (litSymbol @Type @s)

newtype List (s :: Type) (a :: Type) :: Type where
  List :: [a] -> List s a
  deriving (Eq, Ord)

instance (LitSymbol s, Read a) => Read (List s a) where
  readsPrec _ s = [(List $ map read $ sep @s s, "")]

instance (LitSymbol s, Show a) => Show (List s a) where
  show (List xs) = unSep @s (map show xs)

data Tuple (s :: Type) (tup :: Type) :: Type where
  Tuple1 :: (LitSymbol s, Inst1 Read a) => a -> Tuple s (a, ())
  Tuple2 :: (LitSymbol s, Inst2 Read a b) => a -> b -> Tuple s (a, b, ())
  Tuple3 :: (LitSymbol s, Inst3 Read a b c) => a -> b -> c -> Tuple s (a, b, c, ())
  Tuple4 :: (LitSymbol s, Inst4 Read a b c d) => a -> b -> c -> d -> Tuple s (a, b, c, d, ())
  Tuple5 :: (LitSymbol s, Inst5 Read a b c d e) => a -> b -> c -> d -> e -> Tuple s (a, b, c, d, e, ())

instance (Inst1 Eq a) => Eq (Tuple s (a, ())) where
  (==) = \case
    Tuple1 a1 -> \case
      Tuple1 a2 ->
        a1 == a2

instance (Inst2 Eq a b) => Eq (Tuple s (a, b, ())) where
  (==) = \case
    Tuple2 a1 b1 -> \case
      Tuple2 a2 b2 ->
        a1 == a2 && b1 == b2

instance (Inst3 Eq a b c) => Eq (Tuple s (a, b, c, ())) where
  (==) = \case
    Tuple3 a1 b1 c1 -> \case
      Tuple3 a2 b2 c2 ->
        a1 == a2 && b1 == b2 && c1 == c2

instance (Inst4 Eq a b c d) => Eq (Tuple s (a, b, c, d, ())) where
  (==) = \case
    Tuple4 a1 b1 c1 d1 -> \case
      Tuple4 a2 b2 c2 d2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

instance (Inst5 Eq a b c d e) => Eq (Tuple s (a, b, c, d, e, ())) where
  (==) = \case
    Tuple5 a1 b1 c1 d1 e1 -> \case
      Tuple5 a2 b2 c2 d2 e2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2

instance (Inst1 Ord a) => Ord (Tuple s (a, ())) where
  compare = \case
    Tuple1 a1 -> \case
      Tuple1 a2 ->
        compare a1 a2

instance (Inst2 Ord a b) => Ord (Tuple s (a, b, ())) where
  compare = \case
    Tuple2 a1 b1 -> \case
      Tuple2 a2 b2 ->
        mconcat [compare a1 a2, compare b1 b2]

instance (Inst3 Ord a b c) => Ord (Tuple s (a, b, c, ())) where
  compare = \case
    Tuple3 a1 b1 c1 -> \case
      Tuple3 a2 b2 c2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2]

instance (Inst4 Ord a b c d) => Ord (Tuple s (a, b, c, d, ())) where
  compare = \case
    Tuple4 a1 b1 c1 d1 -> \case
      Tuple4 a2 b2 c2 d2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2]

instance (Inst5 Ord a b c d e) => Ord (Tuple s (a, b, c, d, e, ())) where
  compare = \case
    Tuple5 a1 b1 c1 d1 e1 -> \case
      Tuple5 a2 b2 c2 d2 e2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2, compare e1 e2]

instance (Inst1 Show a) => Show (Tuple s (a, ())) where
  show = \case
    Tuple1 a1 -> show a1

instance (Inst2 Show a b) => Show (Tuple s (a, b, ())) where
  show = \case
    Tuple2 a1 b1 -> show a1 ++ " " ++ show b1

instance (Inst3 Show a b c) => Show (Tuple s (a, b, c, ())) where
  show = \case
    Tuple3 a1 b1 c1 -> show a1 ++ " " ++ show b1 ++ " " ++ show c1

instance (Inst4 Show a b c d) => Show (Tuple s (a, b, c, d, ())) where
  show = \case
    Tuple4 a1 b1 c1 d1 -> show a1 ++ " " ++ show b1 ++ " " ++ show c1 ++ " " ++ show d1

instance (Inst5 Show a b c d e) => Show (Tuple s (a, b, c, d, e, ())) where
  show = \case
    Tuple5 a1 b1 c1 d1 e1 -> show a1 ++ " " ++ show b1 ++ " " ++ show c1 ++ " " ++ show d1 ++ " " ++ show e1

instance (Inst1 Read a, LitSymbol s) => Read (Tuple s (a, ())) where
  readsPrec _ s = case sep @s s of
    a : rest -> case reads a of
      [(a', "")] -> [(Tuple1 a', unSep @s rest)]
      _ -> []
    _ -> []

instance (Inst2 Read a b, LitSymbol s) => Read (Tuple s (a, b, ())) where
  readsPrec _ s = case sep @s s of
    a : b : rest -> case reads a of
      [(a', "")] -> case reads b of
        [(b', "")] -> [(Tuple2 a' b', unSep @s rest)]
        _ -> []
      _ -> []
    _ -> []

instance (Inst3 Read a b c, LitSymbol s) => Read (Tuple s (a, b, c, ())) where
  readsPrec _ s = case sep @s s of
    a : b : c : rest -> case reads @(Tuple s (a, b, ())) (unSep @s [a, b]) of
      [(Tuple2 a' b', "")] -> case reads c of
        [(c', "")] -> [(Tuple3 a' b' c', unSep @s rest)]
        _ -> []
      _ -> []
    _ -> []

instance (Inst4 Read a b c d, LitSymbol s) => Read (Tuple s (a, b, c, d, ())) where
  readsPrec _ s = case sep @s s of
    a : b : c : d : rest -> case reads @(Tuple s (a, b, c, ())) (unSep @s [a, b, c]) of
      [(Tuple3 a' b' c', "")] -> case reads d of
        [(d', "")] -> [(Tuple4 a' b' c' d', unSep @s rest)]
        _ -> []
      _ -> []
    _ -> []

instance (Inst5 Read a b c d e, LitSymbol s) => Read (Tuple s (a, b, c, d, e, ())) where
  readsPrec _ s = case sep @s s of
    a : b : c : d : e : rest -> case reads @(Tuple s (a, b, c, d, ())) (unSep @s [a, b, c, d]) of
      [(Tuple4 a' b' c' d', "")] -> case reads e of
        [(e', "")] -> [(Tuple5 a' b' c' d' e', unSep @s rest)]
        _ -> []
      _ -> []
    _ -> []

data TupleList (s :: Type) (tup :: Type) (z :: Type) :: Type where
  TupleList1 :: (LitSymbol s, Inst1 Read a) => a -> [z] -> TupleList s (a, ()) z
  TupleList2 :: (LitSymbol s, Inst2 Read a b) => a -> b -> [z] -> TupleList s (a, b, ()) z
  TupleList3 :: (LitSymbol s, Inst3 Read a b c) => a -> b -> c -> [z] -> TupleList s (a, b, c, ()) z
  TupleList4 :: (LitSymbol s, Inst4 Read a b c d) => a -> b -> c -> d -> [z] -> TupleList s (a, b, c, d, ()) z
  TupleList5 :: (LitSymbol s, Inst5 Read a b c d e) => a -> b -> c -> d -> e -> [z] -> TupleList s (a, b, c, d, e, ()) z

instance (Inst2 Eq a z) => Eq (TupleList s (a, ()) z) where
  (==) = \case
    TupleList1 a1 zs1 -> \case
      TupleList1 a2 zs2 ->
        a1 == a2 && zs1 == zs2

instance (Inst3 Eq a b z) => Eq (TupleList s (a, b, ()) z) where
  (==) = \case
    TupleList2 a1 b1 zs1 -> \case
      TupleList2 a2 b2 zs2 ->
        a1 == a2 && b1 == b2 && zs1 == zs2

instance (Inst4 Eq a b c z) => Eq (TupleList s (a, b, c, ()) z) where
  (==) = \case
    TupleList3 a1 b1 c1 zs1 -> \case
      TupleList3 a2 b2 c2 zs2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && zs1 == zs2

instance (Inst5 Eq a b c d z) => Eq (TupleList s (a, b, c, d, ()) z) where
  (==) = \case
    TupleList4 a1 b1 c1 d1 zs1 -> \case
      TupleList4 a2 b2 c2 d2 zs2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && zs1 == zs2

instance (Inst6 Eq a b c d e z) => Eq (TupleList s (a, b, c, d, e, ()) z) where
  (==) = \case
    TupleList5 a1 b1 c1 d1 e1 zs1 -> \case
      TupleList5 a2 b2 c2 d2 e2 zs2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && zs1 == zs2

instance (Inst2 Ord a z) => Ord (TupleList s (a, ()) z) where
  compare = \case
    TupleList1 a1 zs1 -> \case
      TupleList1 a2 zs2 ->
        mconcat [compare a1 a2, compare zs1 zs2]

instance (Inst3 Ord a b z) => Ord (TupleList s (a, b, ()) z) where
  compare = \case
    TupleList2 a1 b1 zs1 -> \case
      TupleList2 a2 b2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare zs1 zs2]

instance (Inst4 Ord a b c z) => Ord (TupleList s (a, b, c, ()) z) where
  compare = \case
    TupleList3 a1 b1 c1 zs1 -> \case
      TupleList3 a2 b2 c2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare zs1 zs2]

instance (Inst5 Ord a b c d z) => Ord (TupleList s (a, b, c, d, ()) z) where
  compare = \case
    TupleList4 a1 b1 c1 d1 zs1 -> \case
      TupleList4 a2 b2 c2 d2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2, compare zs1 zs2]

instance (Inst6 Ord a b c d e z) => Ord (TupleList s (a, b, c, d, e, ()) z) where
  compare = \case
    TupleList5 a1 b1 c1 d1 e1 zs1 -> \case
      TupleList5 a2 b2 c2 d2 e2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2, compare e1 e2, compare zs1 zs2]

instance (Inst2 Show a z) => Show (TupleList s (a, ()) z) where
  show = \case
    TupleList1 a zs -> unSep @s $ show a : map show zs

instance (Inst3 Show a b z) => Show (TupleList s (a, b, ()) z) where
  show = \case
    TupleList2 a b zs -> unSep @s $ show a : show b : map show zs

instance (Inst4 Show a b c z) => Show (TupleList s (a, b, c, ()) z) where
  show = \case
    TupleList3 a b c zs -> unSep @s $ show a : show b : show c : map show zs

instance (Inst5 Show a b c d z) => Show (TupleList s (a, b, c, d, ()) z) where
  show = \case
    TupleList4 a b c d zs -> unSep @s $ show a : show b : show c : show d : map show zs

instance (Inst6 Show a b c d e z) => Show (TupleList s (a, b, c, d, e, ()) z) where
  show = \case
    TupleList5 a b c d e zs -> unSep @s $ show a : show b : show c : show d : show e : map show zs

instance (Inst2 Read a z, LitSymbol s) => Read (TupleList s (a, ()) z) where
  readsPrec _ s = case sep @s s of
    a : zs -> case readMaybe a of
      Just a' -> case reads @(List s z) $ unSep @s zs of
        [(List zs', rest)] -> [(TupleList1 a' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst3 Read a b z, LitSymbol s) => Read (TupleList s (a, b, ()) z) where
  readsPrec _ s = case sep @s s of
    a : b : zs -> case readMaybe @(Tuple s (a, b, ())) $ unSep @s [a, b] of
      Just (Tuple2 a' b') -> case reads @(List s z) $ unSep @s zs of
        [(List zs', rest)] -> [(TupleList2 a' b' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst4 Read a b c z, LitSymbol s) => Read (TupleList s (a, b, c, ()) z) where
  readsPrec _ s = case sep @s s of
    a : b : c : zs -> case readMaybe @(Tuple s (a, b, c, ())) $ unSep @s [a, b, c] of
      Just (Tuple3 a' b' c') -> case reads @(List s z) $ unSep @s zs of
        [(List zs', rest)] -> [(TupleList3 a' b' c' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst5 Read a b c d z, LitSymbol s) => Read (TupleList s (a, b, c, d, ()) z) where
  readsPrec _ s = case sep @s s of
    a : b : c : d : zs -> case readMaybe @(Tuple s (a, b, c, d, ())) $ unSep @s [a, b, c, d] of
      Just (Tuple4 a' b' c' d') -> case reads @(List s z) $ unSep @s zs of
        [(List zs', rest)] -> [(TupleList4 a' b' c' d' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst6 Read a b c d e z, LitSymbol s) => Read (TupleList s (a, b, c, d, e, ()) z) where
  readsPrec _ s = case sep @s s of
    a : b : c : d : e : zs -> case readMaybe @(Tuple s (a, b, c, d, e, ())) $ unSep @s [a, b, c, d, e] of
      Just (Tuple5 a' b' c' d' e') -> case reads @(List s z) $ unSep @s zs of
        [(List zs', rest)] -> [(TupleList5 a' b' c' d' e' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

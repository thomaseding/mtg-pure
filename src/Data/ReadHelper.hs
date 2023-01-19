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

module Data.ReadHelper (
  ReadSymbol (..),
  ReadSymbol2 (..),
  ReadList (..),
  ReadTup (..),
  ReadTupList (..),
) where

import safe Data.Inst (
  Inst1,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
 )
import safe qualified Data.List as List
import safe Data.Typeable (Proxy (..))
import safe GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import safe Text.Read (readMaybe)

data ReadSymbol (s :: Symbol) where
  ReadSymbol :: KnownSymbol s => ReadSymbol s

instance KnownSymbol s => Read (ReadSymbol s) where
  readsPrec _ s = case List.stripPrefix (symbolVal (Proxy @s)) s of
    Nothing -> []
    Just rest -> [(ReadSymbol, rest)]

data ReadSymbol2 (s1 :: Symbol) (s2 :: Symbol) where
  ReadSymbol2 :: Inst2 KnownSymbol s1 s2 => ReadSymbol2 s1 s2

instance (Inst2 KnownSymbol s1 s2) => Read (ReadSymbol2 s1 s2) where
  readsPrec _ s = case reads @(ReadSymbol s1) s of
    [(ReadSymbol, rest)] -> [(ReadSymbol2, rest)]
    _ -> case reads @(ReadSymbol s2) s of
      [(ReadSymbol, rest)] -> [(ReadSymbol2, rest)]
      _ -> []

newtype ReadList a = ReadList [a]
  deriving (Eq, Ord)

instance Read a => Read (ReadList a) where
  readsPrec _ s = [(ReadList (map read $ words s), "")]

instance Show a => Show (ReadList a) where
  show (ReadList xs) = unwords (map show xs)

data ReadTup tup where
  Read1 :: Inst1 Read a => a -> ReadTup (a, ())
  Read2 :: Inst2 Read a b => a -> b -> ReadTup (a, b, ())
  Read3 :: Inst3 Read a b c => a -> b -> c -> ReadTup (a, b, c, ())
  Read4 :: Inst4 Read a b c d => a -> b -> c -> d -> ReadTup (a, b, c, d, ())
  Read5 :: Inst5 Read a b c d e => a -> b -> c -> d -> e -> ReadTup (a, b, c, d, e, ())

instance (Inst1 Eq a) => Eq (ReadTup (a, ())) where
  (==) = \case
    Read1 a1 -> \case
      Read1 a2 ->
        a1 == a2

instance (Inst2 Eq a b) => Eq (ReadTup (a, b, ())) where
  (==) = \case
    Read2 a1 b1 -> \case
      Read2 a2 b2 ->
        a1 == a2 && b1 == b2

instance (Inst3 Eq a b c) => Eq (ReadTup (a, b, c, ())) where
  (==) = \case
    Read3 a1 b1 c1 -> \case
      Read3 a2 b2 c2 ->
        a1 == a2 && b1 == b2 && c1 == c2

instance (Inst4 Eq a b c d) => Eq (ReadTup (a, b, c, d, ())) where
  (==) = \case
    Read4 a1 b1 c1 d1 -> \case
      Read4 a2 b2 c2 d2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

instance (Inst5 Eq a b c d e) => Eq (ReadTup (a, b, c, d, e, ())) where
  (==) = \case
    Read5 a1 b1 c1 d1 e1 -> \case
      Read5 a2 b2 c2 d2 e2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2

instance (Inst1 Ord a) => Ord (ReadTup (a, ())) where
  compare = \case
    Read1 a1 -> \case
      Read1 a2 ->
        compare a1 a2

instance (Inst2 Ord a b) => Ord (ReadTup (a, b, ())) where
  compare = \case
    Read2 a1 b1 -> \case
      Read2 a2 b2 ->
        mconcat [compare a1 a2, compare b1 b2]

instance (Inst3 Ord a b c) => Ord (ReadTup (a, b, c, ())) where
  compare = \case
    Read3 a1 b1 c1 -> \case
      Read3 a2 b2 c2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2]

instance (Inst4 Ord a b c d) => Ord (ReadTup (a, b, c, d, ())) where
  compare = \case
    Read4 a1 b1 c1 d1 -> \case
      Read4 a2 b2 c2 d2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2]

instance (Inst5 Ord a b c d e) => Ord (ReadTup (a, b, c, d, e, ())) where
  compare = \case
    Read5 a1 b1 c1 d1 e1 -> \case
      Read5 a2 b2 c2 d2 e2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2, compare e1 e2]

instance (Inst1 Show a) => Show (ReadTup (a, ())) where
  show = \case
    Read1 a1 -> show a1

instance (Inst2 Show a b) => Show (ReadTup (a, b, ())) where
  show = \case
    Read2 a1 b1 -> show a1 ++ " " ++ show b1

instance (Inst3 Show a b c) => Show (ReadTup (a, b, c, ())) where
  show = \case
    Read3 a1 b1 c1 -> show a1 ++ " " ++ show b1 ++ " " ++ show c1

instance (Inst4 Show a b c d) => Show (ReadTup (a, b, c, d, ())) where
  show = \case
    Read4 a1 b1 c1 d1 -> show a1 ++ " " ++ show b1 ++ " " ++ show c1 ++ " " ++ show d1

instance (Inst5 Show a b c d e) => Show (ReadTup (a, b, c, d, e, ())) where
  show = \case
    Read5 a1 b1 c1 d1 e1 -> show a1 ++ " " ++ show b1 ++ " " ++ show c1 ++ " " ++ show d1 ++ " " ++ show e1

instance Inst1 Read a => Read (ReadTup (a, ())) where
  readsPrec _ s = case words s of
    a : rest -> case reads a of
      [(a', "")] -> [(Read1 a', unwords rest)]
      _ -> []
    _ -> []

instance Inst2 Read a b => Read (ReadTup (a, b, ())) where
  readsPrec _ s = case words s of
    a : b : rest -> case reads a of
      [(a', "")] -> case reads b of
        [(b', "")] -> [(Read2 a' b', unwords rest)]
        _ -> []
      _ -> []
    _ -> []

instance Inst3 Read a b c => Read (ReadTup (a, b, c, ())) where
  readsPrec _ s = case words s of
    a : b : c : rest -> case reads @(ReadTup (a, b, ())) (unwords [a, b]) of
      [(Read2 a' b', "")] -> case reads c of
        [(c', "")] -> [(Read3 a' b' c', unwords rest)]
        _ -> []
      _ -> []
    _ -> []

instance Inst4 Read a b c d => Read (ReadTup (a, b, c, d, ())) where
  readsPrec _ s = case words s of
    a : b : c : d : rest -> case reads @(ReadTup (a, b, c, ())) (unwords [a, b, c]) of
      [(Read3 a' b' c', "")] -> case reads d of
        [(d', "")] -> [(Read4 a' b' c' d', unwords rest)]
        _ -> []
      _ -> []
    _ -> []

instance Inst5 Read a b c d e => Read (ReadTup (a, b, c, d, e, ())) where
  readsPrec _ s = case words s of
    a : b : c : d : e : rest -> case reads @(ReadTup (a, b, c, d, ())) (unwords [a, b, c, d]) of
      [(Read4 a' b' c' d', "")] -> case reads e of
        [(e', "")] -> [(Read5 a' b' c' d' e', unwords rest)]
        _ -> []
      _ -> []
    _ -> []

data ReadTupList tup z where
  Read1s :: Inst1 Read a => a -> [z] -> ReadTupList (a, ()) z
  Read2s :: Inst2 Read a b => a -> b -> [z] -> ReadTupList (a, b, ()) z
  Read3s :: Inst3 Read a b c => a -> b -> c -> [z] -> ReadTupList (a, b, c, ()) z
  Read4s :: Inst4 Read a b c d => a -> b -> c -> d -> [z] -> ReadTupList (a, b, c, d, ()) z
  Read5s :: Inst5 Read a b c d e => a -> b -> c -> d -> e -> [z] -> ReadTupList (a, b, c, d, e, ()) z

instance (Inst2 Eq a z) => Eq (ReadTupList (a, ()) z) where
  (==) = \case
    Read1s a1 zs1 -> \case
      Read1s a2 zs2 ->
        a1 == a2 && zs1 == zs2

instance (Inst3 Eq a b z) => Eq (ReadTupList (a, b, ()) z) where
  (==) = \case
    Read2s a1 b1 zs1 -> \case
      Read2s a2 b2 zs2 ->
        a1 == a2 && b1 == b2 && zs1 == zs2

instance (Inst4 Eq a b c z) => Eq (ReadTupList (a, b, c, ()) z) where
  (==) = \case
    Read3s a1 b1 c1 zs1 -> \case
      Read3s a2 b2 c2 zs2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && zs1 == zs2

instance (Inst5 Eq a b c d z) => Eq (ReadTupList (a, b, c, d, ()) z) where
  (==) = \case
    Read4s a1 b1 c1 d1 zs1 -> \case
      Read4s a2 b2 c2 d2 zs2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && zs1 == zs2

instance (Inst6 Eq a b c d e z) => Eq (ReadTupList (a, b, c, d, e, ()) z) where
  (==) = \case
    Read5s a1 b1 c1 d1 e1 zs1 -> \case
      Read5s a2 b2 c2 d2 e2 zs2 ->
        a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && zs1 == zs2

instance (Inst2 Ord a z) => Ord (ReadTupList (a, ()) z) where
  compare = \case
    Read1s a1 zs1 -> \case
      Read1s a2 zs2 ->
        mconcat [compare a1 a2, compare zs1 zs2]

instance (Inst3 Ord a b z) => Ord (ReadTupList (a, b, ()) z) where
  compare = \case
    Read2s a1 b1 zs1 -> \case
      Read2s a2 b2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare zs1 zs2]

instance (Inst4 Ord a b c z) => Ord (ReadTupList (a, b, c, ()) z) where
  compare = \case
    Read3s a1 b1 c1 zs1 -> \case
      Read3s a2 b2 c2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare zs1 zs2]

instance (Inst5 Ord a b c d z) => Ord (ReadTupList (a, b, c, d, ()) z) where
  compare = \case
    Read4s a1 b1 c1 d1 zs1 -> \case
      Read4s a2 b2 c2 d2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2, compare zs1 zs2]

instance (Inst6 Ord a b c d e z) => Ord (ReadTupList (a, b, c, d, e, ()) z) where
  compare = \case
    Read5s a1 b1 c1 d1 e1 zs1 -> \case
      Read5s a2 b2 c2 d2 e2 zs2 ->
        mconcat [compare a1 a2, compare b1 b2, compare c1 c2, compare d1 d2, compare e1 e2, compare zs1 zs2]

instance (Inst2 Show a z) => Show (ReadTupList (a, ()) z) where
  show = \case
    Read1s a zs -> unwords $ show a : map show zs

instance (Inst3 Show a b z) => Show (ReadTupList (a, b, ()) z) where
  show = \case
    Read2s a b zs -> unwords $ show a : show b : map show zs

instance (Inst4 Show a b c z) => Show (ReadTupList (a, b, c, ()) z) where
  show = \case
    Read3s a b c zs -> unwords $ show a : show b : show c : map show zs

instance (Inst5 Show a b c d z) => Show (ReadTupList (a, b, c, d, ()) z) where
  show = \case
    Read4s a b c d zs -> unwords $ show a : show b : show c : show d : map show zs

instance (Inst6 Show a b c d e z) => Show (ReadTupList (a, b, c, d, e, ()) z) where
  show = \case
    Read5s a b c d e zs -> unwords $ show a : show b : show c : show d : show e : map show zs

instance (Inst2 Read a z) => Read (ReadTupList (a, ()) z) where
  readsPrec _ s = case words s of
    a : zs -> case readMaybe a of
      Just a' -> case reads $ unwords zs of
        [(ReadList zs', rest)] -> [(Read1s a' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst3 Read a b z) => Read (ReadTupList (a, b, ()) z) where
  readsPrec _ s = case words s of
    a : b : zs -> case readMaybe @(ReadTup (a, b, ())) $ unwords [a, b] of
      Just (Read2 a' b') -> case reads $ unwords zs of
        [(ReadList zs', rest)] -> [(Read2s a' b' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst4 Read a b c z) => Read (ReadTupList (a, b, c, ()) z) where
  readsPrec _ s = case words s of
    a : b : c : zs -> case readMaybe @(ReadTup (a, b, c, ())) $ unwords [a, b, c] of
      Just (Read3 a' b' c') -> case reads $ unwords zs of
        [(ReadList zs', rest)] -> [(Read3s a' b' c' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst5 Read a b c d z) => Read (ReadTupList (a, b, c, d, ()) z) where
  readsPrec _ s = case words s of
    a : b : c : d : zs -> case readMaybe @(ReadTup (a, b, c, d, ())) $ unwords [a, b, c, d] of
      Just (Read4 a' b' c' d') -> case reads $ unwords zs of
        [(ReadList zs', rest)] -> [(Read4s a' b' c' d' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

instance (Inst6 Read a b c d e z) => Read (ReadTupList (a, b, c, d, e, ()) z) where
  readsPrec _ s = case words s of
    a : b : c : d : e : zs -> case readMaybe @(ReadTup (a, b, c, d, e, ())) $ unwords [a, b, c, d, e] of
      Just (Read5 a' b' c' d' e') -> case reads $ unwords zs of
        [(ReadList zs', rest)] -> [(Read5s a' b' c' d' e' zs', rest)]
        _ -> []
      Nothing -> []
    _ -> []

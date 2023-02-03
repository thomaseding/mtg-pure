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

module Data.Read.Symbol (
  LitSymbol (..),
  CI (..),
  CS (..),
  Or (..),
  Many1 (..),
) where

import safe qualified Data.Char as Char
import safe Data.Inst (Inst2)
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.Set as Set
import safe Data.Typeable (Proxy (..))
import safe GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

lower :: String -> String
lower = map Char.toLower

class LitSymbol (s :: k) where
  litSymbol :: String
  litSymbols :: Set.Set String

instance KnownSymbol s => LitSymbol s where
  litSymbol = symbolVal (Proxy @s)
  litSymbols = Set.singleton $ symbolVal (Proxy @s)

instance KnownSymbol s => LitSymbol (CS s) where
  litSymbol = litSymbol @Symbol @s
  litSymbols = Set.singleton $ litSymbol @Symbol @s

instance KnownSymbol s => LitSymbol (CI s) where
  litSymbol = litSymbol @Symbol @s
  litSymbols = Set.singleton $ litSymbol @Symbol @s

instance Inst2 LitSymbol a b => LitSymbol (Or a b) where
  litSymbol = litSymbol @Type @a
  litSymbols = litSymbols @Type @a <> litSymbols @Type @b

instance LitSymbol a => LitSymbol (Many1 a) where
  litSymbol = litSymbol @Type @a
  litSymbols = litSymbols @Type @a

-- Case-sensitive
data CS (s :: Symbol) :: Type where
  CS :: KnownSymbol s => CS s

-- Case-insensitive
data CI (s :: Symbol) :: Type where
  CI :: KnownSymbol s => CI s

data Or (a :: Type) (b :: Type) :: Type where
  Or :: Inst2 LitSymbol a b => Or a b

-- Don't export
data Many0 (a :: Type) :: Type where
  Many0 :: LitSymbol a => Many0 a

data Many1 (a :: Type) :: Type where
  Many1 :: LitSymbol a => Many1 a

instance KnownSymbol s => Show (CS s) where
  show CS = symbolVal (Proxy @s)

instance KnownSymbol s => Show (CI s) where
  show CI = symbolVal (Proxy @s)

instance LitSymbol s => Show (Many1 s) where
  show Many1 = litSymbol @Type @s

instance KnownSymbol s => Read (CS s) where
  readsPrec _ s = case List.stripPrefix (symbolVal (Proxy @s)) s of
    Just rest -> [(CS, rest)]
    Nothing -> []

instance KnownSymbol s => Read (CI s) where
  readsPrec _ s = case List.stripPrefix (lower $ symbolVal (Proxy @s)) (lower s) of
    Just rest -> [(CI, rest)]
    Nothing -> []

instance (Inst2 LitSymbol a b, Inst2 Read a b) => Read (Or a b) where
  readsPrec _ s = case reads @a s of
    [] -> case reads @b s of
      [] -> []
      xs -> mapFst (const Or) <$> xs
    xs -> mapFst (const Or) <$> xs

instance (LitSymbol a, Read a) => Read (Many0 a) where
  readsPrec _ s = case reads @a s of
    [] -> [(Many0, s)]
    xs -> do
      (_, rest') <- xs
      reads @(Many0 a) rest'

-- fixme: doesn't work beyond first
instance (LitSymbol a, Read a) => Read (Many1 a) where
  readsPrec _ s = case reads @a s of
    [] -> []
    xs -> do
      (_, rest') <- xs
      (_, rest'') <- reads @(Many0 a) rest'
      pure (Many1, rest'')

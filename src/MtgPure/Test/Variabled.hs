{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Test.Variabled (
  VarID,
  VarLike (..),
  IsVar,
  varLike,
  Var (..),
  Monad (..),
) where

import safe Data.Kind (Type)
import safe Prelude (Int, Read, Show)

type VarID = Int

data Var s (a :: Type) :: Type where
  Lit :: a -> Var s a
  Var :: VarID -> Var s a
  deriving (Show) -- XXX: remove this instance

-- This can support NatList of Var I think, but certainly not [] of Var
data VarLike s (a :: Type) :: Type where
  --Var0 :: VarLike ()
  --Var2 :: (Var a, Var b) -> VarLike (a, b)
  --Var3 :: (Var a, Var b, Var c) -> VarLike (a, b, c)
  --Var4 :: (Var a, Var b, Var c, Var d) -> VarLike (a, b, c, d)
  --Var5 :: (Var a, Var b, Var c, Var d, Var e) -> VarLike (a, b, c, d, e)
  Var1 :: Var s a -> VarLike s a

class (Read a, Show a, Read va, Show va) => IsVar' s va a | va -> s a where
  varLike :: va -> VarLike s a

-- instance Bindable' () () where
--   bindable () = BindVar0

instance (Read a, Show a, Read (Var s a), Show (Var s a)) => IsVar' s (Var s a) a where
  varLike = Var1

-- instance Bindable' (Var a, Var b) (a, b) where
--   bindable = BindVar2

type IsVar = IsVar'

class Monad (s :: Type) (m :: Type -> Type -> Type) where
  pure :: a -> m s a

  infixl 1 >>=
  (>>=) :: (Read a, Show a) => m s (Var s a) -> (Var s a -> m s b) -> m s b

  infixl 1 >>
  (>>) :: (Read a, Show a) => m s a -> m s b -> m s b

  return :: a -> m s a
  return = pure

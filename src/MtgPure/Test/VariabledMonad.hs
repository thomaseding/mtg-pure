{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Test.VariabledMonad (
  Monad (..),
) where

import safe Data.Kind (Type)
import safe MtgPure.Test.Variabled (RS, Var)
import safe Prelude ()

class Monad (s :: Type) (m :: Type -> Type -> Type) where
  pure :: a -> m s a

  infixl 1 >>=
  (>>=) :: RS a => m s (Var s a) -> (Var s a -> m s b) -> m s b

  infixl 1 >>
  (>>) :: RS a => m s a -> m s b -> m s b

  return :: a -> m s a
  return = pure

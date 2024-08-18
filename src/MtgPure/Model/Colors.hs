{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Colors (
  Colors (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))

data Colors :: Type where
  Colors ::
    Maybe (ManaSymbol 'TyW) ->
    Maybe (ManaSymbol 'TyU) ->
    Maybe (ManaSymbol 'TyB) ->
    Maybe (ManaSymbol 'TyR) ->
    Maybe (ManaSymbol 'TyG) ->
    Colors
  deriving (Eq, Ord, Show, Typeable)

instance Semigroup Colors where
  (<>) :: Colors -> Colors -> Colors
  Colors w1 u1 b1 r1 g1 <> Colors w2 u2 b2 r2 g2 =
    Colors (w1 <> w2) (u1 <> u2) (b1 <> b2) (r1 <> r2) (g1 <> g2)

instance Monoid Colors where
  mempty :: Colors
  mempty = Colors Nothing Nothing Nothing Nothing Nothing

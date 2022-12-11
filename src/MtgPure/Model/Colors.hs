{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Colors (
  Colors (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ManaSymbol (ManaSymbol)
import safe MtgPure.Model.ManaType (ManaType (..))

data Colors :: Type where
  Colors ::
    Maybe (ManaSymbol 'MTWhite) ->
    Maybe (ManaSymbol 'MTBlue) ->
    Maybe (ManaSymbol 'MTBlack) ->
    Maybe (ManaSymbol 'MTRed) ->
    Maybe (ManaSymbol 'MTGreen) ->
    Colors
  deriving (Eq, Ord, Show, Typeable)

instance Semigroup Colors where
  Colors w1 u1 b1 r1 g1 <> Colors w2 u2 b2 r2 g2 =
    Colors (w1 <> w2) (u1 <> u2) (b1 <> b2) (r1 <> r2) (g1 <> g2)

instance Monoid Colors where
  mempty = Colors Nothing Nothing Nothing Nothing Nothing

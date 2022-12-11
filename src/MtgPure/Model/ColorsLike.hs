{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ColorsLike (
  ColorsLike (..),
) where

import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Colors (Colors (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ManaType (ManaType (..))

class ColorsLike a where
  toColors :: a -> Colors

instance ColorsLike Colors where
  toColors = id

instance ColorsLike () where
  toColors ~() = mempty

instance ColorsLike Color where
  toColors = \case
    White -> toColors W
    Blue -> toColors U
    Black -> toColors B
    Red -> toColors R
    Green -> toColors G

instance ColorsLike (ManaSymbol 'MTWhite) where
  toColors ~W = Colors (Just W) Nothing Nothing Nothing Nothing

instance ColorsLike (ManaSymbol 'MTBlue) where
  toColors ~U = Colors Nothing (Just U) Nothing Nothing Nothing

instance ColorsLike (ManaSymbol 'MTBlack) where
  toColors ~B = Colors Nothing Nothing (Just B) Nothing Nothing

instance ColorsLike (ManaSymbol 'MTRed) where
  toColors ~R = Colors Nothing Nothing Nothing (Just R) Nothing

instance ColorsLike (ManaSymbol 'MTGreen) where
  toColors ~G = Colors Nothing Nothing Nothing Nothing (Just G)

instance (ColorsLike a, ColorsLike b) => ColorsLike (a, b) where
  toColors (a, b) = toColors a <> toColors b

instance (ColorsLike a, ColorsLike b, ColorsLike c) => ColorsLike (a, b, c) where
  toColors (a, b, c) = toColors a <> toColors b <> toColors c

instance (ColorsLike a, ColorsLike b, ColorsLike c, ColorsLike d) => ColorsLike (a, b, c, d) where
  toColors (a, b, c, d) = toColors a <> toColors b <> toColors c <> toColors d

instance (ColorsLike a, ColorsLike b, ColorsLike c, ColorsLike d, ColorsLike e) => ColorsLike (a, b, c, d, e) where
  toColors (a, b, c, d, e) =
    toColors a <> toColors b <> toColors c <> toColors d <> toColors e

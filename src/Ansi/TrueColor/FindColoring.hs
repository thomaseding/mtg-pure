{-# LANGUAGE Unsafe #-}
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
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use when" #-}

module Ansi.TrueColor.FindColoring (
  findBestColoring,
) where

import Ansi.TrueColor.Types (Tile, TwoColors (..))
import Codec.Picture.ColorQuant (PaletteCreationMethod (..), PaletteOptions (..), palettize)
import Codec.Picture.Types (Image (..), Pixel (..), PixelRGB8 (..))

--------------------------------------------------------------------------------

-- quantizeTileToTwoColors :: Tile PixelRGB8 -> TwoColors
-- quantizeTileToTwoColors tile = winner
--  where
--   -- TODO: In addition to the algo where we pick two colors before the picking the best
--   -- drawing char, we should also try the algo where we pick the best drawing char first.
--   winner = snd $ minimumBy (comparing fst) $ zip ratings (zip quantTiles quantColors)
--   quantColors =
--     concat
--       [ quantizeTileToTwoColorsA tile
--       -- , quantizeTileToTwoColorsB tile
--       -- , quantizeTileToTwoColorsC tile
--       -- , quantizeTileToTwoColorsD tile
--       ]
--   quantTiles = map (applyTwoColorQuantization tile) quantColors
--   ratings = map (rateTileDelta tile) quantTiles

-- rateTileDelta :: Tile PixelRGB8 -> Tile PixelRGB8 -> Double
-- rateTileDelta tileA tileB = sum do
--   M.unless (isValidTile tileA) $ error "rateTileDelta: invalid tile A"
--   M.unless (isValidTile tileB) $ error "rateTileDelta: invalid tile B"
--   x <- [0 .. tileW - 1]
--   y <- [0 .. tileH - 1]
--   let p = pixelAt tileA x y
--       q = pixelAt tileB x y
--   --pure $ colorDistanceSquared p q
--   pure $ sqrt $ colorDistanceSquared p q

quantizeTileToTwoColorsA :: Tile PixelRGB8 -> TwoColors
quantizeTileToTwoColorsA tile = case (imageWidth palette, imageHeight palette) of
  (2, 1) -> TwoColors (pixelAt palette 0 0) (pixelAt palette 1 0)
  (1, 1) -> TwoColors (pixelAt palette 0 0) (pixelAt palette 0 0)
  (pw, ph) -> error $ "quantizeTileToTwoColorsA: generated strange palette size: " <> show (pw, ph)
 where
  options =
    PaletteOptions
      { paletteCreationMethod = MedianMeanCut
      , enableImageDithering = False
      , paletteColorCount = 2
      }
  (_indicesImage, palette) = palettize options tile

findBestColoring :: Tile PixelRGB8 -> TwoColors
findBestColoring = quantizeTileToTwoColorsA

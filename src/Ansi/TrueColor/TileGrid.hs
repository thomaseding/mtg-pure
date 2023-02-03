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

module Ansi.TrueColor.TileGrid (
  convertImageToTileGrid,
  debugLoadTileGrid,
) where

import Ansi.TrueColor.Types (Grid, Tile, readImage, tileH, tileW)
import Codec.Picture (convertRGB8)
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Types (Image (..), Pixel (..), PixelRGB8, generateImage)

--------------------------------------------------------------------------------

roundUpToMultiple :: Int -> Int -> Int
roundUpToMultiple n m = case r of
  0 -> n
  _ -> n + m - r
 where
  r = n `mod` m

extractTileFromImage :: Int -> Int -> Image PixelRGB8 -> Tile PixelRGB8
extractTileFromImage x y img =
  if inBounds
    then generateImage go tileW tileH
    else error "extractTileFromImage: out of bounds"
 where
  inBounds = 0 <= x && x < w && 0 <= y && y < h
  w = imageWidth img `div` tileW
  h = imageHeight img `div` tileH
  offX = x * tileW
  offY = y * tileH
  go x' y' = pixelAt img (offX + x') (offY + y')

-- assumes that the image is already scaled to the correct size
imageToTileGridImpl :: Image PixelRGB8 -> Grid (Tile PixelRGB8)
imageToTileGridImpl img = case hasValidSize of
  True ->
    [ [ extractTileFromImage x y img
      | x <- [0 .. width `div` tileW - 1]
      ]
    | y <- [0 .. height `div` tileH - 1]
    ]
  False -> error $ "imageToTileGrid: invalid image size " ++ show (width, height)
 where
  width = imageWidth img
  height = imageHeight img
  hasValidSize = width `mod` tileW == 0 && height `mod` tileH == 0

convertImageToTileGrid :: Int -> Int -> Image PixelRGB8 -> Grid (Tile PixelRGB8)
convertImageToTileGrid gridW gridH = imageToTileGridImpl . outputScaledIsValid . scaleBilinear w h . desiredScaleIsValid
 where
  w = max tileW $ (`roundUpToMultiple` tileW) $ tileW * gridW
  h = max tileH $ (`roundUpToMultiple` tileH) $ (tileH * gridH) `div` 2
  isValidSize ww hh = ww `mod` tileW == 0 && hh `mod` tileH == 0
  desiredScaleIsValid = case isValidSize w h of
    True -> id
    False -> error $ "convertImageToTileGrid: invalid scale " ++ show (w, h) ++ show (gridW, gridH)
  outputScaledIsValid scaled =
    let w' = imageWidth scaled
        h' = imageHeight scaled
     in case isValidSize w' h' of
          True -> scaled
          False -> error $ "convertImageToTileGrid: invalid output scale " ++ show (w', h')

debugLoadTileGrid :: Int -> Int -> FilePath -> IO (Grid (Tile PixelRGB8))
debugLoadTileGrid gridW gridH path = do
  img <- convertRGB8 <$> readImage path
  pure $ convertImageToTileGrid gridW gridH img

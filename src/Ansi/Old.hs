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

module Ansi.Old (
  AnsiImage,
  ConvertType (..),
  convertFileImageToAnsi,
  convertImageToAnsi,
  platonicW,
  platonicH,
) where

import Ansi.TrueColor.ImageToAnsi (convertImageToDetailedAnsiImage)
import Ansi.TrueColor.Types (Grid, bytesToRgbGrid)
import Codec.Picture (convertRGB8, readJpeg, readPng)
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Types (
  DynamicImage,
  Image (..),
  PixelRGB8 (..),
  PixelYCbCr8,
  convertPixel,
  pixelMap,
 )
import safe Data.Colour.SRGB (sRGB24)
import safe Data.List (
  elemIndex,
  minimumBy,
 )
import safe Data.Maybe (fromJust)
import safe Data.Ord (comparing)
import qualified Data.Vector.Storable as VS
import safe Data.Word (Word8)
import safe System.Console.ANSI (
  ConsoleLayer (..),
  SGR (..),
  setSGRCode,
  xterm6LevelRGB,
 )
import safe System.FilePath (takeExtension)

--------------------------------------------------------------------------------

-- NOTE: These look good on my terminal for MTG card images.
-- Aspect ratio is very close to original card on Windows10 cmd.exe's default font.
-- Mathematically, the aspect ratio of the pixel card of 84x118 = 0.71186440678,
-- which is very close to the physical aspect ratio of the an MTG card which is
-- 2.5"x3.5" = 0.71428571428.
platonicW :: Int
platonicH :: Int
(platonicW, platonicH) = (scale 84, scale 118)
 where
  scale x = x `div` 1

--------------------------------------------------------------------------------

type AnsiImage = String

-- XXX: I think that cmd.exe doesn't emulate 256 colors, but does for true color.
-- Double check this. Emulation impacts drawing performance and litters the terminal
-- output history with paged previous renders. It might be worth the performance hit
-- because MTG art is intentionally restricted in color use, which makes quantizing
-- to 256 colors poor quality, where changes in value are lost.
data ConvertType
  = -- | Converts an image to ANSI xterm 256 color format.
    PaletteColor
  | -- | Converts an image to ANSI true color format (24-bit).
    TrueColor
  | -- | Converts an image to ANSI true color format (24-bit), but uses more clever
    -- characters to faux represent the image in a higher resolution.
    TrueColorDetailed

readImage :: FilePath -> IO DynamicImage
readImage path = readImage' path >>= either error pure

readImage' :: FilePath -> IO (Either String DynamicImage)
readImage' path = case ext of
  ".png" -> readPng path
  ".jpg" -> readJpeg path
  _ -> error $ "readImage: unknown extension: " <> ext
 where
  ext = takeExtension path

convertFileImageToAnsi :: ConvertType -> FilePath -> IO AnsiImage
convertFileImageToAnsi ct path = do
  img <- readImage path
  pure $ convertImageToAnsi ct img

-- The image is scaled to 84x118, which is a good size for MTG card images.
-- The image is then converted to a 2D array of logical pixels where each pixel
-- is actually a single character rendered to look like two pixels, one on top
-- of the other. If the image is not an even number of pixels tall, the last
-- character row will render the bottom half of its logical pixels as the terminal's
-- default background color.
convertImageToAnsi :: ConvertType -> DynamicImage -> AnsiImage
convertImageToAnsi ct dynImg = (++ setSGRCode [Reset]) case ct of
  PaletteColor -> goTall $ convertImageToPalettePixels dynImg
  TrueColor -> goTall $ convertImageToTrueColorPixels dynImg
  TrueColorDetailed -> convertImageToTrueColorDetailedAnsi dynImg
 where
  goTall = convertTallPixelsToAnsi ct . convertPixelsToTall

convertImageToTrueColorDetailedAnsi :: DynamicImage -> AnsiImage
convertImageToTrueColorDetailedAnsi dynImg = ansiImg
 where
  rgbImg = convertRGB8 dynImg
  ansiImg = convertImageToDetailedAnsiImage platonicW platonicH rgbImg

convertImageToPalettePixels :: DynamicImage -> Grid PixelRGB8
convertImageToPalettePixels dynImg = bytesToRgbGrid platonicW quantizedData
 where
  rgbScaled = scaleDownImage dynImg
  rgbQuantized = pixelMap (`nearestColor` xtermColors) rgbScaled
  quantizedData = VS.toList $ imageData rgbQuantized

convertImageToTrueColorPixels :: DynamicImage -> Grid PixelRGB8
convertImageToTrueColorPixels dynImg = bytesToRgbGrid platonicW rgbData
 where
  rgbScaled = scaleDownImage dynImg
  rgbData = VS.toList $ imageData rgbScaled

-- NOTE: converting to yCbCr and back for the scaling step to RGB doesn't
-- seem to improve the final image quality. Might be worth trying for the
-- nearest color step when generating palette images.
scaleDownImage :: DynamicImage -> Image PixelRGB8
scaleDownImage =
  if False
    then scaleDownImageNormal
    else scaleDownImageHighQuality

scaleDownImageNormal :: DynamicImage -> Image PixelRGB8
scaleDownImageNormal dynImg = imgScaled
 where
  img = convertRGB8 dynImg
  imgScaled = scaleBilinear platonicW platonicH img

scaleDownImageHighQuality :: DynamicImage -> Image PixelRGB8
scaleDownImageHighQuality dynImg = rgbScaled
 where
  rgb = convertRGB8 dynImg
  img :: Image PixelYCbCr8
  img = pixelMap convertPixel rgb
  imgScaled = scaleBilinear platonicW platonicH img
  rgbScaled = pixelMap convertPixel imgScaled

data TallPixel = TallPixel
  { tallPixelTop :: PixelRGB8
  , tallPixelBottom :: Maybe PixelRGB8
  }

mkTall :: PixelRGB8 -> PixelRGB8 -> TallPixel
mkTall top bottom = TallPixel top (Just bottom)

mkHalf :: PixelRGB8 -> TallPixel
mkHalf top = TallPixel top Nothing

convertPixelsToTall :: [[PixelRGB8]] -> Grid TallPixel
convertPixelsToTall = \case
  [] -> []
  row1 : row2 : rows -> zipWith mkTall row1 row2 : convertPixelsToTall rows
  [row] -> [map mkHalf row]

convertTallPixelsToAnsi :: ConvertType -> Grid TallPixel -> AnsiImage
convertTallPixelsToAnsi ct = concatMap goRow
 where
  goRow :: [TallPixel] -> String
  goRow row = concatMap goPixel row <> "\n"
  goPixel p = setSGRCode (mapPixelToAnsi ct p) ++ "▀"

mapPixelToAnsi :: ConvertType -> TallPixel -> [SGR]
mapPixelToAnsi = \case
  PaletteColor -> mapPalettePixelToAnsi
  TrueColor -> mapTrueColorPixelToAnsi
  _ -> error "mapPixelToAnsi: should be unreachable"

mapTrueColorPixelToAnsi :: TallPixel -> [SGR]
mapTrueColorPixelToAnsi tallPixel = setBg : [SetRGBColor Foreground tColor]
 where
  PixelRGB8 tR tG tB = tallPixelTop tallPixel
  tColor = sRGB24 tR tG tB
  setBg = case tallPixelBottom tallPixel of
    Nothing -> Reset
    Just (PixelRGB8 bR bG bB) ->
      let bColor = sRGB24 bR bG bB
       in SetRGBColor Background bColor

mapPalettePixelToAnsi :: TallPixel -> [SGR]
mapPalettePixelToAnsi tallPixel = setBg : [SetPaletteColor Foreground tIdx]
 where
  goWord w = fromJust $ elemIndex (fromIntegral w) xtermChannelColors
  PixelRGB8 tR tG tB = tallPixelTop tallPixel
  tIdx = xterm6LevelRGB tR' tG' tB'
  tR' = goWord tR
  tG' = goWord tG
  tB' = goWord tB
  setBg = case tallPixelBottom tallPixel of
    Nothing -> Reset
    Just (PixelRGB8 bR bG bB) ->
      let bIdx = xterm6LevelRGB bR' bG' bB'
          bR' = goWord bR
          bG' = goWord bG
          bB' = goWord bB
       in SetPaletteColor Background bIdx

xtermColors :: [PixelRGB8]
xtermColors =
  [ PixelRGB8 r g b
  | r <- xtermChannelColors
  , g <- xtermChannelColors
  , b <- xtermChannelColors
  ]

xtermChannelColors :: [Word8]
xtermChannelColors = [0, 51, 102, 153, 204, 255]

colorDistanceSquared :: PixelRGB8 -> PixelRGB8 -> Double
colorDistanceSquared (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = rr + gg + bb
 where
  r = fromIntegral $ r1 - r2
  g = fromIntegral $ g1 - g2
  b = fromIntegral $ b1 - b2
  rr = r * r
  gg = g * g
  bb = b * b

-- No need to sqrt, confirmed with real data.
-- That said, the distance does need to be in floating space and not integer space.
nearestColor :: PixelRGB8 -> [PixelRGB8] -> PixelRGB8
nearestColor p = minimumBy (comparing $ colorDistanceSquared p)

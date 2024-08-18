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

module Ansi.TrueColor.Types (
  readImage,
  Tile,
  tileW,
  tileH,
  isValidTile,
  Grid,
  bytesToRgbGrid,
  ImageToGrid (..),
  Pixel1,
  AnsiImage,
  FgBg (..),
  TwoColors (..),
  invertColors,
  isMonoColor,
  TwoColorTile (..),
) where

import safe Ansi.AnsiString (AnsiString)
import Codec.Picture (readJpeg, readPng)
import Codec.Picture.Types (DynamicImage, Image (..), Pixel8, PixelRGB8 (..))
import qualified Data.Vector.Storable as VS
import safe Data.Word (Word8)
import safe System.FilePath (takeExtension)

-------------------------------------------------------------------------------

readImage :: FilePath -> IO DynamicImage
readImage path = readImage' path >>= either error pure

readImage' :: FilePath -> IO (Either String DynamicImage)
readImage' path = case ext of
  ".png" -> readPng path
  ".jpg" -> readJpeg path
  _ -> error $ "readImage: unknown extension: " <> ext
 where
  ext = takeExtension path

type Pixel1 = Pixel8

type AnsiImage = AnsiString

-- A tile is a 8x16 image
type Tile = Image

-- A tile is a 8x16 image
tileW, tileH :: Int
tileW = 8
tileH = 16

isValidTile :: Tile a -> Bool
isValidTile tile = imageWidth tile == tileW && imageHeight tile == tileH

type Grid a = [[a]]

bytesToRgbList :: [Word8] -> [PixelRGB8]
bytesToRgbList = \case
  [] -> []
  r : g : b : xs -> PixelRGB8 r g b : bytesToRgbList xs
  _ -> error "bytesToRgbList: invalid input"

bytesToRgbGrid :: Int -> [Word8] -> Grid PixelRGB8
bytesToRgbGrid gridWidth = toGrid gridWidth . bytesToRgbList

toGrid :: Int -> [a] -> Grid a
toGrid gridWidth
  | gridWidth <= 0 = error "toGrid: invalid gridWidth"
  | otherwise = go
 where
  go [] = []
  go xs = take gridWidth xs : go (drop gridWidth xs)

class ImageToGrid p where
  imageToGrid :: Image p -> Grid p

instance ImageToGrid PixelRGB8 where
  imageToGrid :: Image PixelRGB8 -> Grid PixelRGB8
  imageToGrid img = bytesToRgbGrid gridWidth imgData
   where
    imgData = VS.toList $ imageData img
    gridWidth = imageWidth img

instance ImageToGrid Pixel1 where
  imageToGrid :: Image Pixel1 -> Grid Pixel1
  imageToGrid img = toGrid gridWidth imgData
   where
    imgData = VS.toList $ imageData img
    gridWidth = imageWidth img

data FgBg = FgBg
  { getFg :: PixelRGB8
  , getBg :: PixelRGB8
  }
  deriving (Show, Eq, Ord)

data TwoColors = TwoColors
  { getColorA :: PixelRGB8
  , getColorB :: PixelRGB8
  }
  deriving (Show, Eq, Ord)

invertColors :: TwoColors -> TwoColors
invertColors (TwoColors a b) = TwoColors b a

isMonoColor :: TwoColors -> Bool
isMonoColor (TwoColors a b) = a == b

newtype TwoColorTile = TcTile {unTcTile :: Tile PixelRGB8}
  deriving (Eq)

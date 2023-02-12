{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use <$>" #-}

module Ansi.TrueColor.FileFormat (
  currentVersion,
  File (..),
  Version (..),
  Images (..),
  Image (..),
  Pixel (..),
  Color (..),
) where

import safe qualified Control.Monad as M
import safe Data.Binary (Binary (..), getWord8, putWord8)
import safe Data.Binary.Get (getWord16be, getWord32be)
import safe Data.Binary.Put (putWord16be, putWord32be)
import safe qualified Data.Char as Char
import safe Data.Word (Word8)

--------------------------------------------------------------------------------

ansiMagicNumber :: [Word8]
ansiMagicNumber = fromIntegral . Char.ord <$> "Ansi.hs:TrueColor:"

currentVersion :: Version
currentVersion =
  Version
    { version_major = 1
    , version_minor = 0
    }

--------------------------------------------------------------------------------

data File = File
  { file_header :: Header
  , file_images :: Images
  }
  deriving (Eq, Ord, Show)

data Header = Header
  { header_magicNumber :: MagicNumber
  , header_version :: Version
  -- , header_tableOfContents :: TableOfContents
  }
  deriving (Eq, Ord, Show)

data MagicNumber = MagicNumber
  deriving (Eq, Ord, Show)

data Version = Version
  { version_major :: Int -- u16
  , version_minor :: Int -- u16
  }
  deriving (Eq, Ord, Show)

-- data TableOfContents = TableOfContents
--   { tableOfContents_images :: Int -- u32
--   , tableOfContents_imagesOffsets :: Int -- u32
--   }

data Images = Images
  { images_imageCount :: Int -- u32
  , images_images :: [Image]
  }
  deriving (Eq, Ord, Show)

data Image = Image
  { image_width :: Int -- u16
  , image_height :: Int -- u16
  , -- | `pixel = grid ! y ! x`, where all rows must have the same length
    image_pixelGrid :: [[Pixel]]
  }
  deriving (Eq, Ord, Show)

data Pixel = Pixel
  { -- | XXX: Could use '\NUL' to denote a clipped pixel.
    pixel_char :: Char -- u32
  , pixel_fg :: Color
  , pixel_bg :: Color
  }
  deriving (Eq, Ord, Show)

-- XXX: Consolidate and use Rgb type
data Color = Color
  { color_r :: Word8 -- u8
  , color_g :: Word8 -- u8
  , color_b :: Word8 -- u8
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

instance Binary File where
  get = do
    header <- get
    images <- get
    pure
      File
        { file_header = header
        , file_images = images
        }
  put
    File
      { file_header = header
      , file_images = images
      } = do
      put MagicNumber
      put header
      put images

instance Binary Header where
  get = do
    magicNumber <- get
    version <- get
    if
        | version_major version /= version_major currentVersion ->
          fail $ "Unsupported version: " ++ show version
        | version_minor version > version_minor currentVersion ->
          fail $ "Unsupported version: " ++ show version
        | otherwise -> pure ()
    pure
      Header
        { header_magicNumber = magicNumber
        , header_version = version
        }
  put
    Header
      { header_magicNumber = magicNumber
      , header_version = version
      } = do
      put magicNumber
      put version

instance Binary MagicNumber where
  get = do
    magicNumber <- M.replicateM (length ansiMagicNumber) getWord8
    if magicNumber == ansiMagicNumber
      then pure MagicNumber
      else fail "Invalid file format magic"
  put MagicNumber = mapM_ putWord8 ansiMagicNumber

instance Binary Version where
  get = do
    major <- fromIntegral <$> getWord16be
    minor <- fromIntegral <$> getWord16be
    pure
      Version
        { version_major = major
        , version_minor = minor
        }
  put
    Version
      { version_major = major
      , version_minor = minor
      } = do
      putWord16be $ fromIntegral major
      putWord16be $ fromIntegral minor

instance Binary Images where
  get = do
    imageCount <- fromIntegral <$> getWord32be
    images <- M.replicateM imageCount get
    pure
      Images
        { images_imageCount = imageCount
        , images_images = images
        }
  put
    Images
      { images_imageCount = imageCount
      , images_images = images
      } = do
      M.when (length images /= imageCount) $ error "Invalid image count"
      putWord32be $ fromIntegral imageCount
      mapM_ put images

instance Binary Image where
  get = do
    width <- fromIntegral <$> getWord16be
    height <- fromIntegral <$> getWord16be
    pixelGrid <- M.replicateM height $ M.replicateM width get
    pure
      Image
        { image_width = width
        , image_height = height
        , image_pixelGrid = pixelGrid
        }
  put
    Image
      { image_width = width
      , image_height = height
      , image_pixelGrid = pixelGrid
      } = do
      M.when (length pixelGrid /= height) $ error "Invalid pixelGrid height"
      M.when (any ((/= width) . length) pixelGrid) $ error "Invalid pixelGrid width"
      putWord16be $ fromIntegral width
      putWord16be $ fromIntegral height
      mapM_ (mapM_ put) pixelGrid

instance Binary Pixel where
  get = do
    char <- Char.chr . fromIntegral <$> getWord32be
    fg <- get
    bg <- get
    pure
      Pixel
        { pixel_char = char
        , pixel_fg = fg
        , pixel_bg = bg
        }
  put
    Pixel
      { pixel_char = char
      , pixel_fg = fg
      , pixel_bg = bg
      } = do
      putWord32be $ fromIntegral $ Char.ord char
      put fg
      put bg

instance Binary Color where
  get = do
    r <- getWord8
    g <- getWord8
    b <- getWord8
    pure
      Color
        { color_r = r
        , color_g = g
        , color_b = b
        }
  put
    Color
      { color_r = r
      , color_g = g
      , color_b = b
      } = do
      putWord8 r
      putWord8 g
      putWord8 b

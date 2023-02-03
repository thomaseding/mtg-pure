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

module Ansi.TrueColor.Debug (
  convertImageToDebugAnsiImage,
) where

import Ansi.TrueColor.Types (AnsiImage, Grid, ImageToGrid (..), Pixel1)
import Codec.Picture.Types (Image, PixelRGB8 (..))
import safe Data.Colour.SRGB (sRGB24)
import safe System.Console.ANSI.Codes (ConsoleLayer (..), SGR (..), setSGRCode)

--------------------------------------------------------------------------------

class ImageToGrid p => PixelToAnsi p where
  pixelToAnsi :: p -> AnsiImage

instance PixelToAnsi PixelRGB8 where
  pixelToAnsi p = setSGRCode [setFg, setBg] ++ "."
   where
    PixelRGB8 r g b = p
    setBg = SetRGBColor Background $ sRGB24 r g b
    setFg = SetRGBColor Foreground $ sRGB24 (255 - r) (255 - g) (255 - b)

instance PixelToAnsi Pixel1 where
  pixelToAnsi p = setSGRCode [setFg, setBg] ++ "."
   where
    setBg = case p of
      0 -> SetRGBColor Background $ sRGB24 0 0 0
      255 -> SetRGBColor Background $ sRGB24 255 255 255
      _ -> error "pixelToAnsi: invalid input"
    setFg = case p of
      0 -> SetRGBColor Foreground $ sRGB24 255 255 255
      255 -> SetRGBColor Foreground $ sRGB24 0 0 0
      _ -> error "pixelToAnsi: invalid input"

pixelsToAnsi :: PixelToAnsi p => Grid p -> AnsiImage
pixelsToAnsi grid = unlines (map (concatMap pixelToAnsi) grid) ++ setSGRCode [Reset]

convertImageToDebugAnsiImage :: PixelToAnsi p => Image p -> AnsiImage
convertImageToDebugAnsiImage = pixelsToAnsi . imageToGrid

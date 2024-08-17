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

import safe Ansi.AnsiString (
  AnsiChar (..),
  AnsiString (..),
  Layer (..),
  Rgb (..),
  Sgr (..),
  ToAnsiString (..),
 )
import Ansi.TrueColor.Types (AnsiImage, Grid, ImageToGrid (..), Pixel1)
import Codec.Picture.Types (Image, PixelRGB8 (..))
import safe Data.List (intercalate)

--------------------------------------------------------------------------------

class (ImageToGrid p) => PixelToAnsi p where
  pixelToAnsi :: p -> AnsiImage

instance PixelToAnsi PixelRGB8 where
  pixelToAnsi p = toAnsiString [setFg, setBg] <> toAnsiString '.'
   where
    PixelRGB8 r g b = p
    setBg = SgrTrueColor Bg $ Rgb r g b
    setFg = SgrTrueColor Fg $ Rgb (255 - r) (255 - g) (255 - b)

instance PixelToAnsi Pixel1 where
  pixelToAnsi p = toAnsiString [setFg, setBg] <> toAnsiString '.'
   where
    setBg = case p of
      0 -> SgrTrueColor Bg $ Rgb 0 0 0
      255 -> SgrTrueColor Bg $ Rgb 255 255 255
      _ -> error "pixelToAnsi: invalid input"
    setFg = case p of
      0 -> SgrTrueColor Fg $ Rgb 255 255 255
      255 -> SgrTrueColor Fg $ Rgb 0 0 0
      _ -> error "pixelToAnsi: invalid input"

pixelsToAnsi :: (PixelToAnsi p) => Grid p -> AnsiImage
pixelsToAnsi grid =
  AnsiString $
    intercalate
      [AnsiChar '\n']
      (map (concatMap $ unAnsiString . pixelToAnsi) grid)
      <> [AnsiSgr SgrReset]

convertImageToDebugAnsiImage :: (PixelToAnsi p) => Image p -> AnsiImage
convertImageToDebugAnsiImage = pixelsToAnsi . imageToGrid

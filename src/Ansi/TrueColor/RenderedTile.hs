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

module Ansi.TrueColor.RenderedTile (
  RenderedTile (..),
  mkAllColoredRenderings,
) where

import Ansi.TrueColor.Types (FgBg (..), Pixel1, Tile, TwoColorTile (..), isValidTile)
import Ansi.TrueColor.VirtualChar (VirtualChar, virtDrawingChars)
import Ansi.TrueColor.VirtualCharTile (virtCharToTile)
import Codec.Picture.Types (PixelRGB8 (..), pixelMap)
import safe Data.List (nubBy)

white :: PixelRGB8
white = PixelRGB8 255 255 255

black :: PixelRGB8
black = PixelRGB8 0 0 0

whiteOnBlack :: FgBg
whiteOnBlack = FgBg white black

data RenderedTile = RenderedTile
  { rtVirtChar :: VirtualChar
  , rtTileProto :: Tile Pixel1
  , rtTileRgb :: TwoColorTile
  , rtFgBg :: FgBg
  }
  deriving (Eq)

renderWhiteOnBlack :: VirtualChar -> RenderedTile
renderWhiteOnBlack vc = case isValidTile proto of
  False -> error "renderWhiteOnBlackChar: invalid tile"
  True ->
    RenderedTile
      { rtVirtChar = vc
      , rtTileProto = proto
      , rtTileRgb = colorizeProtoTile proto whiteOnBlack
      , rtFgBg = whiteOnBlack
      }
 where
  proto = virtCharToTile vc

colorizeProtoTile :: Tile Pixel1 -> FgBg -> TwoColorTile
colorizeProtoTile tile (FgBg fg bg) = TcTile $ pixelMap goPixel tile
 where
  goPixel = \case
    0 -> bg
    255 -> fg
    _ -> error "colorizeProtoTile: pixel value not 0 or 255"

allWhiteOnBlackRenderings :: [RenderedTile]
allWhiteOnBlackRenderings = case total `mod` 2 of
  0 -> case total < length virtDrawingChars of
    True -> tiles
    False -> error "allWhiteOnBlackRenderings: didn't prune any equivalent tiles"
  _ -> error "allWhiteOnBlackRenderings: somehow generated odd number of tiles"
 where
  eq a b = rtTileRgb a == rtTileRgb b
  tiles = nubBy eq $ map renderWhiteOnBlack virtDrawingChars
  total = length tiles

mkAllColoredRenderings :: FgBg -> [RenderedTile]
mkAllColoredRenderings coloring = map go allWhiteOnBlackRenderings
 where
  go rt =
    rt
      { rtTileRgb = colorizeProtoTile (rtTileProto rt) coloring
      , rtFgBg = coloring
      }

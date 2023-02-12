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

module Ansi.TrueColor.ImageToAnsi (
  renderedTileToAnsi,
  convertImageToDetailedAnsiImage,
  debugLoadRenderedTileGrid,
) where

import safe Ansi.AnsiString (
  AnsiChar (..),
  AnsiString (..),
  Layer (..),
  Rgb (..),
  Sgr (..),
  ToAnsiString (..),
 )
import Ansi.TrueColor.FindColoring (findBestColoring)
import Ansi.TrueColor.FindRendering (findBestRendering)
import Ansi.TrueColor.RenderedTile (RenderedTile (..))
import Ansi.TrueColor.TileGrid (convertImageToTileGrid)
import Ansi.TrueColor.Types (AnsiImage, FgBg (..), Grid, Tile, readImage)
import Ansi.TrueColor.VirtualChar (VirtualChar (..))
import Codec.Picture (convertRGB8)
import Codec.Picture.Types (Image, PixelRGB8 (..))
import safe Data.List (intercalate)

--------------------------------------------------------------------------------

renderedTileToAnsi :: RenderedTile -> AnsiImage
renderedTileToAnsi rt = toAnsiString [setFg, setBg] <> toAnsiString (vcChar vc)
 where
  vc = rtVirtChar rt
  coloring = rtFgBg rt
  PixelRGB8 fgR fgG fgB = getFg coloring
  PixelRGB8 bgR bgG bgB = getBg coloring
  setFg = SgrTrueColor fgLayer $ Rgb fgR fgG fgB
  setBg = SgrTrueColor bgLayer $ Rgb bgR bgG bgB
  invert = vcInvert vc
  (fgLayer, bgLayer) = case invert of
    False -> (Fg, Bg)
    True -> (Bg, Fg)

renderedGridToAnsi :: Grid RenderedTile -> AnsiImage
renderedGridToAnsi =
  AnsiString
    . intercalate
      [AnsiChar '\n']
    . map (concatMap $ unAnsiString . renderedTileToAnsi)

renderTile :: Tile PixelRGB8 -> RenderedTile
renderTile tile = findBestRendering tile coloring
 where
  coloring = findBestColoring tile

renderTileGrid :: Grid (Tile PixelRGB8) -> Grid RenderedTile
renderTileGrid = map (map renderTile)

convertImageToRenderedTiles :: Int -> Int -> Image PixelRGB8 -> Grid RenderedTile
convertImageToRenderedTiles gridW gridH = renderTileGrid . convertImageToTileGrid gridW gridH

convertImageToDetailedAnsiImage :: Int -> Int -> Image PixelRGB8 -> AnsiImage
convertImageToDetailedAnsiImage gridW gridH = renderedGridToAnsi . convertImageToRenderedTiles gridW gridH

debugLoadRenderedTileGrid :: Int -> Int -> FilePath -> IO (Grid RenderedTile)
debugLoadRenderedTileGrid gridW gridH path = do
  img <- readImage path
  pure $ convertImageToRenderedTiles gridW gridH $ convertRGB8 img

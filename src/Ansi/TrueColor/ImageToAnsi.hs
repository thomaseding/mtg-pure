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

import Ansi.TrueColor.FindColoring (findBestColoring)
import Ansi.TrueColor.FindRendering (findBestRendering)
import Ansi.TrueColor.RenderedTile (RenderedTile (..))
import Ansi.TrueColor.TileGrid (convertImageToTileGrid)
import Ansi.TrueColor.Types (AnsiImage, FgBg (..), Grid, Tile, readImage)
import Ansi.TrueColor.VirtualChar (VirtualChar (..))
import Codec.Picture (convertRGB8)
import Codec.Picture.Types (Image, PixelRGB8 (..))
import safe Data.Colour.SRGB (sRGB24)
import safe System.Console.ANSI.Codes (ConsoleLayer (..), SGR (..), setSGRCode)

--------------------------------------------------------------------------------

renderedTileToAnsi :: RenderedTile -> AnsiImage
renderedTileToAnsi rt = setSGRCode [setFg, setBg] ++ [vcChar vc]
 where
  vc = rtVirtChar rt
  coloring = rtFgBg rt
  PixelRGB8 fgR fgG fgB = getFg coloring
  PixelRGB8 bgR bgG bgB = getBg coloring
  setFg = SetRGBColor fgLayer $ sRGB24 fgR fgG fgB
  setBg = SetRGBColor bgLayer $ sRGB24 bgR bgG bgB
  invert = vcInvert vc
  (fgLayer, bgLayer) = case invert of
    False -> (Foreground, Background)
    True -> (Background, Foreground)

renderedGridToAnsi :: Grid RenderedTile -> AnsiImage
renderedGridToAnsi = unlines . map (concatMap renderedTileToAnsi)

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

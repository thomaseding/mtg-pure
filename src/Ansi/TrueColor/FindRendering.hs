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

module Ansi.TrueColor.FindRendering (
  findBestRendering,
  rateAllRenderings,
) where

import Ansi.Math (colorDistanceSquared)
import Ansi.TrueColor.RenderedTile (RenderedTile (..), mkAllColoredRenderings)
import Ansi.TrueColor.Types (
  FgBg (..),
  Tile,
  TwoColorTile (..),
  TwoColors (..),
  isMonoColor,
  isValidTile,
  tileH,
  tileW,
 )
import Codec.Picture.Types (Pixel (..), PixelRGB8 (..), generateImage)
import safe qualified Control.Monad as M
import safe Data.List (minimumBy)
import safe Data.Ord (comparing)

--------------------------------------------------------------------------------

rateTileColorDifference :: Tile PixelRGB8 -> TwoColorTile -> Double
rateTileColorDifference tileA (TcTile tileB) = sum do
  M.unless (isValidTile tileA) $ error "rateTileDelta: invalid tile A"
  M.unless (isValidTile tileB) $ error "rateTileDelta: invalid tile B"
  x <- [0 .. tileW - 1]
  y <- [0 .. tileH - 1]
  let p = pixelAt tileA x y
  let q = pixelAt tileB x y
  -- pure $ colorDistanceSquared p q
  pure $ sqrt $ colorDistanceSquared p q

rateTileVsRendering :: Tile PixelRGB8 -> RenderedTile -> Double
rateTileVsRendering source rt = rateTileColorDifference source $ rtTileRgb rt

applyTwoColorQuantization :: Tile PixelRGB8 -> TwoColors -> TwoColorTile
applyTwoColorQuantization tile (TwoColors c1 c2) = case isValidTile tile of
  True -> TcTile $ generateImage goPixel tileW tileH
  False -> error "applyTwoColorQuantization: invalid tile"
 where
  goPixel x y =
    let p = pixelAt tile x y
     in if colorDistanceSquared p c1 < colorDistanceSquared p c2
          then c1
          else c2

rateAllRenderings :: Tile PixelRGB8 -> TwoColors -> [(Double, RenderedTile)]
rateAllRenderings source coloring = zip ratings renderings
 where
  renderings = mkAllColoredRenderings $ castToFgBg coloring
  TcTile quant = applyTwoColorQuantization source coloring
  ratings =
    if False
      then map (rateTileVsRendering quant) renderings
      else map (rateTileVsRendering source) renderings

findBestRendering :: Tile PixelRGB8 -> TwoColors -> RenderedTile
findBestRendering source coloring = case isMonoColor coloring of
  True -> getUniqueRendering $ map snd ratedRenderings
  False -> snd $ minimumBy (comparing fst) ratedRenderings
 where
  ratedRenderings = rateAllRenderings source coloring

-- In theory, it shouldn't matter which color is the foreground and which is the
-- background because all the candidate renderings should be symmetric in its use
-- of virtual character with respect to inverted characters. TBD: verify this
castToFgBg :: TwoColors -> FgBg
castToFgBg (TwoColors c1 c2) = FgBg c1 c2

getUniqueRendering :: [RenderedTile] -> RenderedTile
getUniqueRendering [] = error "getUniqueRendering: empty list"
getUniqueRendering [x] = x
getUniqueRendering (x : y : zs) = case rtTileRgb x == rtTileRgb y of
  True -> getUniqueRendering $ y : zs
  False -> error "getUniqueRendering: not unique"

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

module Ansi.Math (
  colorDistanceSquared,
  nearestColor,
) where

import Codec.Picture.Types (PixelRGB8 (..))
import safe Data.List (minimumBy)
import safe Data.Ord (comparing)

--------------------------------------------------------------------------------

colorDistanceSquared :: PixelRGB8 -> PixelRGB8 -> Double
colorDistanceSquared (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = rr + gg + bb
 where
  -- NOTE: The cast needs to happen before since these are unsigned words.
  -- XXX: Casting here is dumb because it is slow. Force users to pre-convert
  -- the image so the performance is better.
  -- XXX: Also want to use a better color space
  r = fromIntegral r1 - fromIntegral r2
  g = fromIntegral g1 - fromIntegral g2
  b = fromIntegral b1 - fromIntegral b2
  rr = r * r
  gg = g * g
  bb = b * b

-- No need to sqrt, confirmed with real data.
-- That said, the distance does need to be in floating space and not integer space.
nearestColor :: PixelRGB8 -> [PixelRGB8] -> PixelRGB8
nearestColor p = minimumBy (comparing $ colorDistanceSquared p)

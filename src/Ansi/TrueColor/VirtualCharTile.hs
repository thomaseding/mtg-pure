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
{-# HLINT ignore "Redundant id" #-}

module Ansi.TrueColor.VirtualCharTile (
  virtCharToTile,
) where

import Ansi.TrueColor.Types (Pixel1, Tile)
import Ansi.TrueColor.VirtualChar (VirtualChar (..), virtDrawingChars)
import Codec.Picture.Types (Image (..), Pixel (..), generateImage)
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (fromMaybe)

virtCharToTile :: VirtualChar -> Tile Pixel1
virtCharToTile c = fromMaybe notFound $ Map.lookup c virtCharToTileCached
 where
  notFound = error $ "virtCharToTile: " <> show c

virtCharToTileCached :: Map.Map VirtualChar (Tile Pixel1)
virtCharToTileCached = Map.fromList $ zip vcs $ map f vcs
 where
  vcs = virtDrawingChars
  f vc =
    let img = virtCharToBitmap vc
     in case imageHeight img of
          8 -> stretchImageHeight 2 img
          16 -> img
          _ -> error "virtCharToTile: out of bounds"

stretchImageHeight :: (Pixel a) => Int -> Image a -> Image a
stretchImageHeight scale img = case imageHeight img `mod` scale of
  0 -> generateImage go w h
  _ -> error "stretchImageHeight: image height must be a multiple of scale"
 where
  w = imageWidth img
  h = imageHeight img * scale
  go x y = pixelAt img x $ y `div` scale

decodeTextImage :: Char -> Bool -> [[Char]] -> Image Pixel1
decodeTextImage source invert text = case is8x8 text of
  True -> generateImage go 8 8
  False -> case is8x16 text of
    True -> generateImage go 8 16
    False -> error $ "decodeDrawingCharImage: out of bounds " <> show source <> " '" <> [source] <> "'"
 where
  text' = text -- reverse $ map reverse text
  fg = if invert then 0 else 255
  bg = if invert then 255 else 0
  go x y = case text' !! y !! x of
    '.' -> bg
    '@' -> fg
    _ -> error "decodeCharImage: invalid char"

is8x8 :: [[Char]] -> Bool
is8x8 text = all ((== 8) . length) text && length text == 8

is8x16 :: [[Char]] -> Bool
is8x16 text = all ((== 8) . length) text && length text == 16

disallow :: String -> [[Char]] -> [[Char]]
disallow msg = error $ "disallowed character: " <> msg

virtCharToBitmap :: VirtualChar -> Image Pixel1
virtCharToBitmap vc = decodeTextImage (vcChar vc) (vcInvert vc) case vcChar vc of
  ' ' ->
    -- 0x0020 (space)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '┏' ->
    -- 0x250f (box drawings heavy down and right)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "..@@@@@@"
    , "..@@@@@@"
    , "..@@@@@@"
    , "..@@@@@@"
    , "..@@@@@@"
    , "..@@@..."
    , "..@@@..."
    , "..@@@..."
    , "..@@@..."
    , "..@@@..."
    ]
  '┓' ->
    -- 0x2513 (box drawings heavy down and left)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "...@@@.."
    , "...@@@.."
    , "...@@@.."
    , "...@@@.."
    , "...@@@.."
    ]
  '┗' ->
    -- 0x2517 (box drawings heavy up and right)
    [ "..@@@..."
    , "..@@@..."
    , "..@@@..."
    , "..@@@..."
    , "..@@@..."
    , "..@@@@@@"
    , "..@@@@@@"
    , "..@@@@@@"
    , "..@@@@@@"
    , "..@@@@@@"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '┛' ->
    -- 0x251b (box drawings heavy up and left)
    [ "...@@@.."
    , "...@@@.."
    , "...@@@.."
    , "...@@@.."
    , "...@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '▀' ->
    -- 0x2580 (upper half block)
    [ "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '▁' ->
    -- 0x2581 (lower one eighth block)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    ]
  '▂' ->
    -- 0x2582 (lower one quarter block)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '▃' ->
    -- 0x2583 (lower three eighths block)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '▄' ->
    -- 0x2584 (lower half block)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '▅' ->
    -- 0x2585 (lower five eighths block)
    [ "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '▆' ->
    -- 0x2586 (lower three quarters block)
    [ "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '▇' ->
    -- 0x2587 (lower seven eighths block)
    [ "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '█' ->
    disallow
      "Use space character instead. Space should always render better."
      -- 0x2588 (full block)
      [ "@@@@@@@@"
      , "@@@@@@@@"
      , "@@@@@@@@"
      , "@@@@@@@@"
      , "@@@@@@@@"
      , "@@@@@@@@"
      , "@@@@@@@@"
      , "@@@@@@@@"
      ]
  '▉' ->
    -- 0x2589 (left seven eighths block)
    [ "@@@@@@@."
    , "@@@@@@@."
    , "@@@@@@@."
    , "@@@@@@@."
    , "@@@@@@@."
    , "@@@@@@@."
    , "@@@@@@@."
    , "@@@@@@@."
    ]
  '▊' ->
    -- 0x258A (left three quarters block)
    [ "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    , "@@@@@@.."
    ]
  '▋' ->
    -- 0x258B (left five eighths block)
    [ "@@@@@..."
    , "@@@@@..."
    , "@@@@@..."
    , "@@@@@..."
    , "@@@@@..."
    , "@@@@@..."
    , "@@@@@..."
    , "@@@@@..."
    ]
  '▌' ->
    -- 0x258C (left half block)
    [ "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    ]
  '▍' ->
    -- 0x258D (left three eighths block)
    [ "@@@....."
    , "@@@....."
    , "@@@....."
    , "@@@....."
    , "@@@....."
    , "@@@....."
    , "@@@....."
    , "@@@....."
    ]
  '▎' ->
    -- 0x258E (left one quarter block)
    [ "@@......"
    , "@@......"
    , "@@......"
    , "@@......"
    , "@@......"
    , "@@......"
    , "@@......"
    , "@@......"
    ]
  '▏' ->
    -- 0x258F (left one eighth block)
    [ "@......."
    , "@......."
    , "@......."
    , "@......."
    , "@......."
    , "@......."
    , "@......."
    , "@......."
    ]
  '▐' ->
    -- 0x2590 (right half block)
    [ "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    ]
  '░' ->
    -- 0x2591 (light shade)
    -- XXX: Handling these as some value between 0 and 255 would be nice for non-TrueColor ANSI renderings.
    --      Would need to update the color application algo to accommodate this, but that should be easy
    --      with some mean average math.
    -- XXX: Supporting this for TrueColor is not useful for gradients (since the TrueColor has enough color
    --      depth to obsolete the need for using this in the literal shade fashion.) However, it could be
    --      useful for denoting grittiness of a surface, or something like that. So would pattern match vs
    --      a space character (as a full block), and in the final rendering case, sniff the original source
    --      tile for whether or not it is smooth or noisy. Depending on the noisiness, apply one of these
    --      shade characters instead of a space character. (And of course, fix up structs to accommodate,
    --      such as updating the proto tile.)
    -- XXX: Actually handling this in TrueColor without special logic might help the renders as-is.
    [ "@@...@.."
    , "...@..@@"
    , ".@..@@.."
    , "..@@...@"
    , "@@...@.."
    , "...@..@@"
    , ".@..@@.."
    , "..@@...@"
    ]
  '▒' ->
    -- 0x2592 (medium shade)
    [ "@.@.@.@."
    , ".@.@.@.@"
    , "@.@.@.@."
    , ".@.@.@.@"
    , "@.@.@.@."
    , ".@.@.@.@"
    , "@.@.@.@."
    , ".@.@.@.@"
    ]
  '▓' ->
    -- 0x2593 (dark shade)
    [ "@@...@@@"
    , "...@..@@"
    , ".@..@@.."
    , "..@@...@"
    , "@@...@@@"
    , "...@..@@"
    , ".@..@@.."
    , "..@@...@"
    ]
  '▔' ->
    -- 0x2594 (upper one eighth block)
    [ "@@@@@@@@"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '▕' ->
    -- 0x2595 (right one eighth block)
    [ ".......@"
    , ".......@"
    , ".......@"
    , ".......@"
    , ".......@"
    , ".......@"
    , ".......@"
    , ".......@"
    ]
  '▖' ->
    -- 0x2596 (quadrant lower left)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    ]
  '▗' ->
    -- 0x2597 (quadrant lower right)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    ]
  '▘' ->
    -- 0x2598 (quadrant upper left)
    [ "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '▙' ->
    -- 0x2599 (quadrant upper left and lower left and lower right)
    [ "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '▚' ->
    -- 0x259A (quadrant upper left and lower right)
    [ "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    ]
  '▛' ->
    -- 0x259B (quadrant upper left and upper right and lower left)
    [ "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    ]
  '▜' ->
    -- 0x259C (quadrant upper left and upper right and lower right)
    [ "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    ]
  '▝' ->
    -- 0x259D (quadrant upper right)
    [ "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '▞' ->
    -- 0x259E (quadrant upper right and lower left)
    [ "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    , "@@@@...."
    ]
  '▟' ->
    -- 0x259F (quadrant upper right and lower left and lower right)
    [ "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "....@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    ]
  '■' ->
    -- 0x25A0 (black square)
    [ "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "........"
    , "........"
    ]
  '▲' ->
    -- 0x25B2 (black up-pointing triangle)
    [ "........"
    , "........"
    , "...@@..."
    , "..@@@@.."
    , ".@@@@@@."
    , "@@@@@@@@"
    , "........"
    , "........"
    ]
  '▼' ->
    -- 0x25BC (black down-pointing triangle)
    [ "........"
    , "........"
    , "@@@@@@@@"
    , ".@@@@@@."
    , "..@@@@.."
    , "...@@..."
    , "........"
    , "........"
    ]
  '▶' ->
    -- 0x25B6 (black right-pointing triangle)
    [ "........"
    , "@@......"
    , "@@@@...."
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@...."
    , "@@......"
    , "........"
    ]
  '◀' ->
    -- 0x25C0 (black left-pointing triangle)
    [ "........"
    , "......@@"
    , "...@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "...@@@@@"
    , "......@@"
    , "........"
    ]
  '◢' ->
    -- 0x25E2 (lower left triangle)
    [ "........"
    , "........"
    , "........"
    , "........"
    , ".......@"
    , "......@@"
    , ".....@@@"
    , "....@@@@"
    , "...@@@@@"
    , "..@@@@@@"
    , ".@@@@@@@"
    , "@@@@@@@@"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '◣' ->
    -- 0x25E3 (lower right triangle)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "@......."
    , "@@......"
    , "@@@....."
    , "@@@@...."
    , "@@@@@..."
    , "@@@@@@.."
    , "@@@@@@@."
    , "@@@@@@@@"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '◤' ->
    -- 0x25E4 (upper left triangle)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@."
    , "@@@@@@.."
    , "@@@@@..."
    , "@@@@...."
    , "@@@....."
    , "@@......"
    , "@......."
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '◥' ->
    -- 0x25E5 (upper right triangle)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    , ".@@@@@@@"
    , "..@@@@@@"
    , "...@@@@@"
    , "....@@@@"
    , ".....@@@"
    , "......@@"
    , ".......@"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '●' ->
    -- 0x25CF (black circle)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "...@@..."
    , "..@@@@.."
    , ".@@@@@@."
    , "@@@@@@@@"
    , "@@@@@@@@"
    , ".@@@@@@."
    , "..@@@@.."
    , "...@@..."
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  '▬' ->
    -- 0x25AC (black rectangle)
    [ "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "@@@@@@@@"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    , "........"
    ]
  _ -> error "charToBitmap: invalid char"

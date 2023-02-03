{-# LANGUAGE Safe #-}
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

module Ansi.TrueColor.VirtualChar (
  VirtualChar (..),
  virtDrawingChars,
  encodeDrawingChar,
  decodeDrawingChars,
) where

import safe qualified Data.Char as Char

-- A decent font for this is "Fira Code".
-- https://github.com/hbin/top-programming-fonts

-- Other popular monospace fonts are not so good for box drawing characters because
-- they have gaps between full box characters. (Yikes!) I suppose this defect is to
-- make the font more readable in source code, rather than being a good font for box
-- drawing. Some of the simple ones that come by default on Windows for cmd.exe are
-- good for box drawing... but they are ancient and incomplete (not extended for more
-- Unicode character support).
--
-- NOTE: These exclude the full-block drawing char because space is superior. This is
-- because some fonts leave small rendering gaps between full-block chars. Space doesn't
-- seem to cause this issue. (An inverted space is equivalent to a full-block char.)
--
-- NOTE: I tried the shade characters. They just seem out of place with the current
-- direct pattern matching.
drawingChars :: [Char]
drawingChars = "▀▁▂▃▄▅▆▇▉▊▋▌▍▎▏ ▐▔▕▖▗▘▙▚▛▜▝▞▟◢◣◤◥▲▼◀▶■▬┏┓┗┛"

virtDrawingChars :: [VirtualChar]
virtDrawingChars = do
  c <- drawingChars
  invert <- [False, True]
  pure
    VirtualChar
      { vcChar = c
      , vcInvert = invert
      }

data VirtualChar = VirtualChar
  { vcChar :: Char
  , vcInvert :: Bool
  }
  deriving (Eq, Ord, Show)

encodeDrawingChar :: Char -> [Char]
encodeDrawingChar = \case
  '▀' -> "#a"
  '▁' -> "#b"
  '▂' -> "#c"
  '▃' -> "#d"
  '▄' -> "#e"
  '▅' -> "#f"
  '▆' -> "#g"
  '▇' -> "#h"
  '█' -> "#i"
  '▉' -> "#j"
  '▊' -> "#k"
  '▋' -> "#l"
  '▌' -> "#m"
  '▍' -> "#n"
  '▎' -> "#o"
  '▏' -> "#p"
  '▐' -> "#q"
  '▔' -> "#r"
  '▕' -> "#s"
  '▖' -> "#t"
  '▗' -> "#u"
  '▘' -> "#v"
  '▙' -> "#w"
  '▚' -> "#x"
  '▛' -> "#y"
  '▜' -> "#z"
  '▝' -> "#A"
  '▞' -> "#B"
  '▟' -> "#C"
  '◢' -> "#D"
  '◣' -> "#E"
  '◤' -> "#F"
  '◥' -> "#G"
  '░' -> "#H"
  '▒' -> "#I"
  '▓' -> "#J"
  '▲' -> "#K"
  '▼' -> "#L"
  '◀' -> "#M"
  '▶' -> "#N"
  '■' -> "#O"
  '●' -> "#P"
  '▬' -> "#Q"
  '┏' -> "#R"
  '┓' -> "#S"
  '┗' -> "#T"
  '┛' -> "#U"
  c -> case Char.isAscii c of
    True -> [c]
    False -> error $ "encodeDrawingChar: " ++ show c

decodeEscapedDrawingChar :: Char -> Char
decodeEscapedDrawingChar = \case
  'a' -> '▀'
  'b' -> '▁'
  'c' -> '▂'
  'd' -> '▃'
  'e' -> '▄'
  'f' -> '▅'
  'g' -> '▆'
  'h' -> '▇'
  'i' -> '█'
  'j' -> '▉'
  'k' -> '▊'
  'l' -> '▋'
  'm' -> '▌'
  'n' -> '▍'
  'o' -> '▎'
  'p' -> '▏'
  'q' -> '▐'
  'r' -> '▔'
  's' -> '▕'
  't' -> '▖'
  'u' -> '▗'
  'v' -> '▘'
  'w' -> '▙'
  'x' -> '▚'
  'y' -> '▛'
  'z' -> '▜'
  'A' -> '▝'
  'B' -> '▞'
  'C' -> '▟'
  'D' -> '◢'
  'E' -> '◣'
  'F' -> '◤'
  'G' -> '◥'
  'H' -> '░'
  'I' -> '▒'
  'J' -> '▓'
  'K' -> '▲'
  'L' -> '▼'
  'M' -> '◀'
  'N' -> '▶'
  'O' -> '■'
  'P' -> '●'
  'Q' -> '▬'
  'R' -> '┏'
  'S' -> '┓'
  'T' -> '┗'
  'U' -> '┛'
  c -> error $ "decodeDrawingChar: " ++ show c

decodeDrawingChars :: String -> String
decodeDrawingChars = go
 where
  go ('#' : c : cs) = decodeEscapedDrawingChar c : go cs
  go (c : cs) = c : go cs
  go [] = []

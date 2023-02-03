{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Ansi.AnsiString (
  Rgb (..),
  Csi (..),
  Sgr (..),
  AnsiChar (..),
  AnsiString,
  AnsiToString (..),
) where

import safe Control.Exception (assert)
import safe GHC.Word (Word8)

data Rgb = Rgb
  { rgb_r :: Word8
  , rgb_g :: Word8
  , rgb_b :: Word8
  }
  deriving (Eq, Ord, Show)

data Csi where
  CsiSetNextLine :: Csi
  CsiSetPrevLine :: Csi
  -- | 0-based coordinates. Negative values move the cursor left.
  CsiSetRelCursorX :: Int -> Csi
  -- | 0-based coordinates. Negative values move the cursor up.
  CsiSetRelCursorY :: Int -> Csi
  -- | 0-based coordinates. Must be non-negative.
  CsiSetAbsCursorX :: Int -> Csi
  -- | 0-based coordinates. Must be non-negative.
  CsiSetAbsCursorXY :: Int -> Int -> Csi

data Sgr where
  SgrReset :: Sgr
  SgrTrueColorFg :: Rgb -> Sgr
  SgrTrueColorBg :: Rgb -> Sgr

data AnsiChar where
  AnsiChar :: Char -> AnsiChar
  AnsiCsi :: Csi -> AnsiChar
  AnsiSgr :: Sgr -> AnsiChar

type AnsiString = [AnsiChar]

class AnsiToString a where
  ansiToString :: a -> String

instance AnsiToString Csi where
  ansiToString = \case
    CsiSetNextLine -> "\ESC[E"
    CsiSetPrevLine -> "\ESC[F"
    CsiSetRelCursorX x -> case compare x 0 of
      EQ -> ""
      GT -> "\ESC[" <> show x <> "C"
      LT -> "\ESC[" <> show (negate x) <> "D"
    CsiSetRelCursorY y -> case compare y 0 of
      EQ -> ""
      GT -> case y of
        1 -> "\ESC[B"
        _ -> "\ESC[" <> show y <> "B"
      LT -> case negate y of
        1 -> "\ESC[A"
        y' -> "\ESC[" <> show y' <> "A"
    CsiSetAbsCursorX x -> assert (x >= 0) case x + 1 of
      1 -> "\ESC[G"
      x' -> "\ESC[" <> show x' <> "G"
    CsiSetAbsCursorXY x y -> assert (x >= 0) $ assert (y >= 0) case (x + 1, y + 1) of
      (1, 1) -> "\ESC[;H"
      (1, y') -> "\ESC[" <> show y' <> ";H"
      (x', 1) -> "\ESC[;" <> show x' <> "H"
      (x', y') -> "\ESC[" <> show y' <> ";" <> show x' <> "H"

instance AnsiToString Sgr where
  ansiToString = \case
    SgrReset -> "\ESC[0m"
    SgrTrueColorFg (Rgb r g b) ->
      "\ESC[38;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"
    SgrTrueColorBg (Rgb r g b) ->
      "\ESC[48;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"

instance AnsiToString AnsiChar where
  ansiToString = \case
    AnsiChar c -> [c]
    AnsiCsi csi -> ansiToString csi
    AnsiSgr sgr -> ansiToString sgr

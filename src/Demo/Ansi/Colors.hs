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

module Demo.Ansi.Colors (
  main,
  mainAnsiColors,
) where

import safe qualified Control.Monad as M
import safe Data.Colour.SRGB (sRGB24)
import safe System.Console.ANSI (
  ConsoleLayer (..),
  SGR (..),
  setSGR,
 )

finallyReset :: IO a -> IO a
finallyReset action = do
  result <- action
  setSGR [Reset]
  pure result

main :: IO ()
main = mainAnsiColors

mainAnsiColors :: IO ()
mainAnsiColors = finallyReset do
  printAnsi8BitColorGrid
  printXterm256ColorGrid
  printColorGrid

printAnsi8BitColorGrid :: IO ()
printAnsi8BitColorGrid = do
  putStrLn "ANSI 8-bit colors:"
  M.forM_ [minBound ..] \colorIntensity -> do
    M.forM_ [minBound ..] \color -> do
      setSGR [SetColor Background colorIntensity color]
      putStr "  "
    setSGR [Reset]
    putStrLn ""
  setSGR [Reset]
  putStrLn ""

printXterm256ColorGrid :: IO ()
printXterm256ColorGrid = do
  putStrLn "Xterm 256 colors:"
  M.forM_ [0 .. 255] \color -> do
    setSGR [SetPaletteColor Background color]
    putStr "  "
    M.when (color `mod` 16 == 15) do
      setSGR [Reset]
      putStrLn ""
  setSGR [Reset]
  putStrLn ""

printColorGrid :: IO ()
printColorGrid = do
  let k = 32 -- 64
      kInt :: Int = fromIntegral k
      scaleInt :: Int = 256 `div` kInt
      scale = fromIntegral scaleInt
      wrap = 128
  M.forM_ [0 .. k - 1] $ \r -> do
    M.forM_ [0 .. k - 1] $ \g -> do
      M.forM_ [0 .. k - 1] $ \b -> do
        let r' = r * scale
            g' = g * scale
            b' = b * scale
        let color = sRGB24 r' g' b'
        let linear :: Int = fromIntegral r * kInt * kInt + fromIntegral g * kInt + fromIntegral b
        setSGR [SetRGBColor Background color]
        putStr " "
        M.when (linear `mod` wrap == wrap - 1) do
          setSGR [Reset]
          putStrLn ""
  setSGR [Reset]
  putStrLn ""

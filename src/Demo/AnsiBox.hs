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

module Demo.AnsiBox (
  main,
  mainAnsiBoxExample,
) where

import safe Ansi.Box (
  Box (..),
  ColorCommand (..),
  FixedOrRatio (..),
  addPopup,
  drawBox,
  withAnsi,
 )
import safe qualified Control.Monad as M
import safe System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  hideCursor,
 )
import safe System.IO (hFlush, stdout)

main :: IO ()
main = mainAnsiBoxExample

boxChars :: String
boxChars = "▀▁▂▃▄▅▆▇█▉▊▋▌▍▎▏ ▐░▒▓▔▕▖▗▘▙▚▛▜▝▞▟"

lineChars :: String
lineChars = "─━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾╿"

newlineEvery :: Int -> String -> String
newlineEvery n s =
  let (a, b) = splitAt n s
   in case b of
        "" -> a
        _ -> a ++ "\n" ++ newlineEvery n b

mainAnsiBoxExample :: IO ()
mainAnsiBoxExample = withAnsi do
  hideCursor
  drawBox 80 30 $ addPopup "This is a really special\nmessage popup" parentBox
  hFlush stdout
  M.void getChar
  drawBox 80 30 $ addPopup boxChars parentBox
  hFlush stdout
  M.void getChar
  drawBox 80 30 $ addPopup (newlineEvery 20 lineChars) parentBox
  hFlush stdout
  M.void getChar

parentBox :: Box
parentBox =
  Box
    { boxText = "012345678901234              9"
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 1
    , boxH = Ratio 1
    , boxKidsPre = [topRow, middleRow, bottomRow]
    , boxKidsPost = []
    , boxColorCommands = [SetFg (Dull, Magenta)]
    , boxBackground = Just (Dull, Yellow)
    }

topRow :: Box
topRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 1
    , boxH = Ratio 0.33
    , boxKidsPre = [box1, box2, box3]
    , boxKidsPost = []
    , boxColorCommands = [SetBg (Vivid, Blue)]
    , boxBackground = Nothing -- Just (Dull, Black)
    }

middleRow :: Box
middleRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0.33
    , boxW = Ratio 1
    , boxH = Ratio 0.33
    , boxBackground = Nothing
    , boxColorCommands = [SetBg (Vivid, Cyan)]
    , boxKidsPre = [box4, box5]
    , boxKidsPost = []
    }

bottomRow :: Box
bottomRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0.66
    , boxW = Ratio 1
    , boxH = Auto -- Relative 0.33
    , boxColorCommands = [SetBg (Vivid, Green)]
    , boxBackground = Nothing
    , boxKidsPre = [box6]
    , boxKidsPost = []
    }

box1 :: Box
box1 =
  Box
    { boxText = "Column 1"
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 0.33
    , boxH = Ratio 1
    , boxBackground = Just (Dull, Blue)
    , boxColorCommands = [SetBg (Vivid, Blue)]
    , boxKidsPre = []
    , boxKidsPost = []
    }

box2 :: Box
box2 =
  Box
    { boxText = "Column 2"
    , boxClipper = take
    , boxX = Ratio 0.33
    , boxY = Ratio 0
    , boxW = Ratio 0.33
    , boxH = Ratio 1
    , boxColorCommands = []
    , boxBackground = Just (Vivid, Black)
    , boxKidsPre = []
    , boxKidsPost = []
    }

box3 :: Box
box3 =
  Box
    { boxText = "Column 3"
    , boxClipper = take
    , boxX = Ratio 0.66
    , boxY = Ratio 0
    , boxW = Auto -- Relative 0.33
    , boxH = Ratio 1
    , boxColorCommands = [SetFg (Vivid, Blue)]
    , boxBackground = Just (Vivid, Yellow)
    , boxKidsPre = []
    , boxKidsPost = []
    }

box4 :: Box
box4 =
  Box
    { boxText = "Column 4"
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxColorCommands = [SetFg (Dull, Black)]
    , boxBackground = Just (Vivid, Red)
    , boxKidsPre = []
    , boxKidsPost = []
    }

box5 :: Box
box5 =
  Box
    { boxText = "Column 5"
    , boxClipper = take
    , boxX = Ratio 0.5
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxColorCommands = []
    , boxBackground = Just (Vivid, Magenta)
    , boxKidsPre = []
    , boxKidsPost = []
    }

box6 :: Box
box6 =
  Box
    { boxText = "Column 6"
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 1
    , boxH = Ratio 1
    , boxColorCommands = []
    , boxBackground = Just (Dull, Red)
    , boxKidsPre = []
    , boxKidsPost = []
    }

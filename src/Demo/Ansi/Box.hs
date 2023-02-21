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

module Demo.Ansi.Box (
  main,
  mainAnsiBoxExample,
) where

import safe Ansi.AnsiString (
  Layer (..),
  Sgr (..),
  dullBlack,
  dullBlue,
  dullMagenta,
  dullRed,
  dullYellow,
  takeAnsi,
  vividBlack,
  vividBlue,
  vividCyan,
  vividGreen,
  vividMagenta,
  vividRed,
  vividYellow,
 )
import safe Ansi.Box (
  Box (..),
  FixedOrRatio (..),
  addPopup,
  drawBoxIO,
  withAnsi,
 )
import safe qualified Control.Monad as M
import safe System.Console.ANSI (
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
  M.void $ drawBoxIO 80 30 $ addPopup "This is a really special\nmessage popup" parentBox
  hFlush stdout
  M.void getChar
  M.void $ drawBoxIO 80 30 $ addPopup boxChars parentBox
  hFlush stdout
  M.void getChar
  M.void $ drawBoxIO 80 30 $ addPopup (newlineEvery 20 lineChars) parentBox
  hFlush stdout
  M.void getChar

parentBox :: Box
parentBox =
  Box
    { boxText = "012345678901234              9"
    , boxClipper = takeAnsi
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 1
    , boxH = Ratio 1
    , boxKidsPre = [topRow, middleRow, bottomRow]
    , boxKidsPost = []
    , boxColorCommands = [SgrTrueColor Fg dullMagenta]
    , boxBackground = Just dullYellow
    }

topRow :: Box
topRow =
  Box
    { boxText = ""
    , boxClipper = takeAnsi
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 1
    , boxH = Ratio 0.33
    , boxKidsPre = [box1, box2, box3]
    , boxKidsPost = []
    , boxColorCommands = [SgrTrueColor Bg vividBlue]
    , boxBackground = Nothing -- Just (Dull, Black)
    }

middleRow :: Box
middleRow =
  Box
    { boxText = ""
    , boxClipper = takeAnsi
    , boxX = Ratio 0
    , boxY = Ratio 0.33
    , boxW = Ratio 1
    , boxH = Ratio 0.33
    , boxBackground = Nothing
    , boxColorCommands = [SgrTrueColor Bg vividCyan]
    , boxKidsPre = [box4, box5]
    , boxKidsPost = []
    }

bottomRow :: Box
bottomRow =
  Box
    { boxText = ""
    , boxClipper = takeAnsi
    , boxX = Ratio 0
    , boxY = Ratio 0.66
    , boxW = Ratio 1
    , boxH = Auto -- Relative 0.33
    , boxColorCommands = [SgrTrueColor Bg vividGreen]
    , boxBackground = Nothing
    , boxKidsPre = [box6]
    , boxKidsPost = []
    }

box1 :: Box
box1 =
  Box
    { boxText = "Column 1"
    , boxClipper = takeAnsi
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 0.33
    , boxH = Ratio 1
    , boxBackground = Just dullBlue
    , boxColorCommands = [SgrTrueColor Bg vividBlue]
    , boxKidsPre = []
    , boxKidsPost = []
    }

box2 :: Box
box2 =
  Box
    { boxText = "Column 2"
    , boxClipper = takeAnsi
    , boxX = Ratio 0.33
    , boxY = Ratio 0
    , boxW = Ratio 0.33
    , boxH = Ratio 1
    , boxColorCommands = []
    , boxBackground = Just vividBlack
    , boxKidsPre = []
    , boxKidsPost = []
    }

box3 :: Box
box3 =
  Box
    { boxText = "Column 3"
    , boxClipper = takeAnsi
    , boxX = Ratio 0.66
    , boxY = Ratio 0
    , boxW = Auto -- Relative 0.33
    , boxH = Ratio 1
    , boxColorCommands = [SgrTrueColor Fg vividBlue]
    , boxBackground = Just vividYellow
    , boxKidsPre = []
    , boxKidsPost = []
    }

box4 :: Box
box4 =
  Box
    { boxText = "Column 4"
    , boxClipper = takeAnsi
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxColorCommands = [SgrTrueColor Fg dullBlack]
    , boxBackground = Just vividRed
    , boxKidsPre = []
    , boxKidsPost = []
    }

box5 :: Box
box5 =
  Box
    { boxText = "Column 5"
    , boxClipper = takeAnsi
    , boxX = Ratio 0.5
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxColorCommands = []
    , boxBackground = Just vividMagenta
    , boxKidsPre = []
    , boxKidsPost = []
    }

box6 :: Box
box6 =
  Box
    { boxText = "Column 6"
    , boxClipper = takeAnsi
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 1
    , boxH = Ratio 1
    , boxColorCommands = []
    , boxBackground = Just dullRed
    , boxKidsPre = []
    , boxKidsPost = []
    }

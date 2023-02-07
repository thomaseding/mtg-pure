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

module Demo.AnsiMagicBoard (
  main,
  mainAnsiBoxMagic,
) where

import safe Ansi.AnsiString (
  Rgb,
  Sgr (..),
  dullBlack,
  dullBlue,
  dullRed,
  dullWhite,
  dullYellow,
  vividBlue,
  vividGreen,
  vividWhite,
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
import safe System.IO (hFlush, stdout)

main :: IO ()
main = mainAnsiBoxMagic

mainAnsiBoxMagic :: IO ()
mainAnsiBoxMagic = withAnsi do
  drawBoxIO 90 30 $ addPopup "The\nPopup!" magicBox
  hFlush stdout
  M.void getLine

magicBox :: Box
magicBox =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Ratio 0
    , boxW = Ratio 1
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = [environRow, playerRow, handRow, graveRow, exileRow, commandRow, battlefieldRow]
    , boxKidsPost = []
    }

environY :: Int
environY = 0

environHeight :: Int
environHeight = 1

environRow :: Box
environRow =
  Box
    { boxText = "Turn 37 - Day"
    , boxClipper = take
    , boxX = Center
    , boxY = Fixed 0
    , boxW = Ratio 1
    , boxH = Fixed environHeight
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

playerY :: Int
playerY = environY + environHeight

playerHeight :: Int
playerHeight = 4

playerRow :: Box
playerRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed playerY
    , boxW = Ratio 1
    , boxH = Fixed playerHeight
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = [hudA, hudB]
    , boxKidsPost = []
    }

mkHud :: Double -> String -> Box
mkHud relX text =
  Box
    { boxText = text
    , boxClipper = take
    , boxX = Ratio relX
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = [manaRow 0 "Norm", manaRow 1 "Snow"]
    }

hudA :: Box
hudA = mkHud 0.0 "Player1  Poison=0  Life=20   Library=60"

hudB :: Box
hudB = mkHud 0.5 "Player2  Poison=4  Life=777  Library=46"

manaWidth :: Int
manaWidth = 5

manaRow :: Int -> String -> Box
manaRow y desc =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed $ y + 1
    , boxW = Ratio 1
    , boxH = Fixed 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = [manaLegend desc, manaW, manaU, manaB, manaR, manaG, manaC]
    }

legendWidth :: Int
legendWidth = 5

manaLegend :: String -> Box
manaLegend desc =
  Box
    { boxText = desc
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed 0
    , boxW = Fixed legendWidth
    , boxH = Fixed 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' <> s

manaClipper :: Int -> String -> String
manaClipper n s = padLeft (manaWidth - 1) $ manaClipper' n s

manaClipper' :: Int -> String -> String
manaClipper' n s
  | amount <= 9999 = s
  | amount <= 99999 = take (n - 1) (decimal2 $ show (amount `div` 1000)) <> "k"
  | amount <= 999999 = take (n - 1) (show (amount `div` 1000)) <> "k"
  | amount <= 9999999 = take (n - 1) (decimal $ show (amount `div` 100000)) <> "m"
  | amount <= 99999999 = take (n - 1) (decimal2 $ show (amount `div` 1000000)) <> "m"
  | amount <= 999999999 = take (n - 1) (show (amount `div` 100000)) <> "m"
  | otherwise = "+++"
 where
  amount = read s
  decimal = \case
    c : cs -> c : '.' : cs
    [] -> []
  decimal2 = \case
    c1 : c2 : cs -> c1 : c2 : '.' : cs
    cs -> cs

mkMana :: Rgb -> Rgb -> Int -> String -> Box
mkMana fg bg relX text =
  Box
    { boxText = text
    , boxClipper = manaClipper
    , boxX = Fixed $ manaWidth * relX + legendWidth
    , boxY = Ratio 0
    , boxW = Fixed $ manaWidth - 1
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = [SgrTrueColorFg fg, SgrTrueColorBg bg]
    , boxKidsPre = []
    , boxKidsPost = []
    }

manaC :: Box
manaC = mkMana dullBlack dullWhite 0 "111111111111"

manaW :: Box
manaW = mkMana dullBlack vividYellow 1 "43"

manaU :: Box
manaU = mkMana vividWhite dullBlue 2 "6783"

manaB :: Box
manaB = mkMana dullYellow dullBlack 3 "1234567"

manaR :: Box
manaR = mkMana vividWhite dullRed 4 "123456"

manaG :: Box
manaG = mkMana dullBlack vividGreen 5 "12356"

handY :: Int
handY = playerY + playerHeight

handHeight :: Int
handHeight = 2

handRow :: Box
handRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Fixed handY
    , boxW = Ratio 1
    , boxH = Fixed handHeight
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = [handA, handB]
    , boxKidsPost = []
    }

mkHand :: Double -> String -> Box
mkHand relX text =
  Box
    { boxText = "Hand-" <> text
    , boxClipper = take
    , boxX = Ratio relX
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

handA :: Box
handA = mkHand 0 "A"

handB :: Box
handB = mkHand 0.5 "B"

graveY :: Int
graveY = handY + handHeight

graveHeight :: Int
graveHeight = 4

graveRow :: Box
graveRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Fixed graveY
    , boxW = Ratio 1
    , boxH = Fixed graveHeight
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = [graveA, graveB]
    , boxKidsPost = []
    }

mkGrave :: Double -> String -> Box
mkGrave relX text =
  Box
    { boxText = "Grave-\n" <> text
    , boxClipper = take
    , boxX = Ratio relX
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

graveA :: Box
graveA = mkGrave 0 "A"

graveB :: Box
graveB = mkGrave 0.5 "B"

exileY :: Int
exileY = graveY + graveHeight

exileHeight :: Int
exileHeight = 3

exileRow :: Box
exileRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed exileY
    , boxW = Ratio 1
    , boxH = Fixed exileHeight
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = [exileA, exileB]
    , boxKidsPost = []
    }

mkExile :: Double -> String -> Box
mkExile relX text =
  Box
    { boxText = "Exile-" <> text
    , boxClipper = take
    , boxX = Ratio relX
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

exileA :: Box
exileA = mkExile 0 "A"

exileB :: Box
exileB = mkExile 0.5 "B"

commandY :: Int
commandY = exileY + exileHeight

commandHeight :: Int
commandHeight = 2

commandRow :: Box
commandRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed commandY
    , boxW = Ratio 1
    , boxH = Fixed commandHeight
    , boxBackground = Just vividBlue
    , boxColorCommands = []
    , boxKidsPre = [commandA, commandB]
    , boxKidsPost = []
    }

mkCommand :: Double -> String -> Box
mkCommand relX text =
  Box
    { boxText = "Command-" <> text
    , boxClipper = take
    , boxX = Ratio relX
    , boxY = Ratio 0
    , boxW = Ratio 0.5
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

commandA :: Box
commandA = mkCommand 0 "A"

commandB :: Box
commandB = mkCommand 0.5 "B"

battlefieldY :: Int
battlefieldY = commandY + commandHeight

battlefieldRow :: Box
battlefieldRow =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Ratio 0
    , boxY = Fixed battlefieldY
    , boxW = Ratio 1
    , boxH = Auto
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = [landsA, artifactsEnchantmentsA, creaturesA, creaturesB, artifactsEnchantmentsB, landsB]
    , boxKidsPost = []
    }

mkBattlefield :: Double -> String -> Box
mkBattlefield relX text =
  Box
    { boxText = text
    , boxClipper = take
    , boxX = Ratio relX
    , boxY = Fixed 0
    , boxW = Ratio $ 1 / 6
    , boxH = Ratio 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

landsA :: Box
landsA = mkBattlefield 0 "Lands-A"

artifactsEnchantmentsA :: Box
artifactsEnchantmentsA = mkBattlefield (1 / 6) "Arts/Enchants-A"

creaturesA :: Box
creaturesA = mkBattlefield (2 / 6) "Creatures-A"

creaturesB :: Box
creaturesB = mkBattlefield (3 / 6) "Creatures-B"

artifactsEnchantmentsB :: Box
artifactsEnchantmentsB = mkBattlefield (4 / 6) "Arts/Enchants-B"

landsB :: Box
landsB = mkBattlefield (5 / 6) "Lands-B"

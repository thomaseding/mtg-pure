{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use when" #-}

module Ansi.Box (
  withAnsi,
  withBuffering,
  withHiddenCursor,
  clearScreenByPaging,
  clearScreenWithoutPaging,
  clearBoxViewport,
  FixedOrRatio (..),
  Box (..),
  toAbsolute,
  fromAbsolute,
  drawBox,
  drawBoxIO,
  addBorder,
  addPopup,
) where

import safe Ansi.AnsiString (
  AnsiChar (..),
  AnsiString (..),
  AnsiToString (..),
  Csi (CsiSetAbsCursorXY),
  Layer (..),
  Lines (..),
  Rgb,
  Sgr (..),
  takeAnsi,
 )
import safe Control.Applicative ((<|>))
import safe Control.Exception (assert, finally)
import safe qualified Control.Monad as M
import safe qualified Control.Monad.Trans as M
import safe qualified Control.Monad.Trans.Writer.Strict as Writer
import safe qualified Data.Char as Char
import safe qualified Data.DList as DList
import safe qualified Data.Foldable as F
import safe Data.List (intercalate)
import safe Data.String (IsString (..))
import safe System.Console.ANSI (
  clearLine,
  clearScreen,
  getTerminalSize,
  hideCursor,
  showCursor,
 )
import safe System.Exit (ExitCode (..))
import safe System.IO (
  BufferMode (..),
  Handle,
  hFlush,
  hGetBuffering,
  hGetEcho,
  hGetEncoding,
  hSetBuffering,
  hSetEcho,
  hSetEncoding,
  stdin,
  stdout,
  utf8,
 )
import safe System.Process (readProcessWithExitCode, system)
import safe Text.Read (readMaybe)

type DAnsiString = DList.DList AnsiChar

type DrawM = Writer.Writer DAnsiString

runDrawM :: DrawM () -> AnsiString
runDrawM = AnsiString . DList.toList . Writer.execWriter

tell :: [AnsiChar] -> DrawM ()
tell = Writer.tell . DList.fromList

withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h b m = do
  old <- hGetBuffering h
  hSetBuffering h b
  m `finally` hSetBuffering h old

-- This causes issues in ghci when calling `main` from the repl multiple times.
_withEcho :: Handle -> Bool -> IO a -> IO a
_withEcho h b m = do
  old <- hGetEcho h
  hSetEcho h b
  m `finally` hSetEcho h old

withUtf8 :: Handle -> IO a -> IO a
withUtf8 h m = do
  mOld <- hGetEncoding h
  hSetEncoding h utf8
  m `finally` case mOld of
    Just old -> hSetEncoding h old
    Nothing -> pure ()

withHiddenCursor :: IO a -> IO a
withHiddenCursor m = do
  hideCursor
  m `finally` showCursor

clearScreenWithoutPaging :: M.MonadIO m => m ()
clearScreenWithoutPaging = M.liftIO do
  putStr $ ansiToString SgrReset
  getTerminalSize >>= \case
    Nothing -> assert False clearScreen -- fallback to paging
    Just (_w, h) -> do
      F.for_ [0 .. h - 1] \y -> do
        putStr $ ansiToString $ CsiSetAbsCursorXY 0 y
        clearLine

clearScreenByPaging :: M.MonadIO m => m ()
clearScreenByPaging = M.liftIO do
  putStr $ ansiToString SgrReset
  clearScreen

finallyCleanup :: IO a -> IO a
finallyCleanup m = do
  m `finally` do
    putStr $ ansiToString SgrReset
    showCursor
    M.when False do
      getTerminalSize >>= \case
        Nothing -> pure ()
        Just (_w, h) -> do
          putStr $ ansiToString $ CsiSetAbsCursorXY 0 (h - 1)
    putStrLn ""
    hFlush stdout

parseCodePage :: String -> Maybe Int
parseCodePage = readMaybe . dropWhile (not . Char.isDigit)

withCodePage :: Int -> IO a -> IO a
withCodePage codePage action = do
  (code, out, _err) <- readProcessWithExitCode "chcp.com" [] ""
  case code of
    ExitFailure _ -> action
    ExitSuccess -> case parseCodePage out of
      Nothing -> action
      Just oldCodePage -> do
        M.void $ system $ "chcp.com " <> show codePage
        action `finally` do
          system $ "chcp.com " <> show oldCodePage

withEnableUnicode :: IO a -> IO a
withEnableUnicode action = do
  -- This changes code page for cmd.exe to support unicode
  withCodePage 65001 do
    withUtf8 stdout do
      withUtf8 stdin do
        action

withAnsi :: IO a -> IO a
withAnsi action = do
  withBuffering stdout (BlockBuffering Nothing) do
    withBuffering stdin NoBuffering do
      withEnableUnicode do
        finallyCleanup do
          clearScreenByPaging
          hFlush stdout
          action

data FixedOrRatio
  = Absolute Int
  | Fixed Int
  | Ratio Double
  | Auto
  | Center
  | TextDim

data Box = Box
  { boxText :: AnsiString -- TODO: Make an SgrString and allow that. CSI shouldn't be allowed.
  , boxClipper :: Int -> AnsiString -> AnsiString
  , boxX :: FixedOrRatio
  , boxY :: FixedOrRatio
  , boxW :: FixedOrRatio
  , boxH :: FixedOrRatio
  , boxBackground :: Maybe Rgb
  , boxColorCommands :: [Sgr]
  , boxKidsPre :: [Box]
  , boxKidsPost :: [Box]
  }

toAbsolute :: Int -> Int -> Int -> Int -> Box -> Box
toAbsolute maxWidth maxHeight absX absY box =
  Box
    { boxText = text
    , boxClipper = clipper
    , boxX = Absolute x'
    , boxY = Absolute y'
    , boxW = Absolute w'
    , boxH = Absolute h'
    , boxBackground = mBackgroundProto
    , boxColorCommands = sgr
    , boxKidsPre = map recurse kidsPre
    , boxKidsPost = map recurse kidsPost
    }
 where
  recurse = toAbsolute w' h' x' y'
  Box
    { boxText = text
    , boxClipper = clipper
    , boxX = x
    , boxY = y
    , boxW = w
    , boxH = h
    , boxBackground = mBackgroundProto
    , boxColorCommands = sgr
    , boxKidsPre = kidsPre
    , boxKidsPost = kidsPost
    } = box
  textLines = toLines text
  textWidth = maximum $ 0 : map (length . unAnsiString) textLines
  textHeight = length textLines
  x' = case x of
    Absolute i -> i
    Fixed i -> absX + i
    Ratio p -> absX + floor (fromIntegral maxWidth * p)
    Auto -> absX
    Center -> absX + (maxWidth - min textWidth w') `div` 2
    TextDim -> error "Invalid value for x field: TextDim" -- TODO: make impossible via type
  y' = case y of
    Absolute i -> i
    Fixed i -> absY + i
    Ratio p -> absY + floor (fromIntegral maxHeight * p)
    Auto -> absY
    Center -> absY + (maxHeight - min textHeight h') `div` 2
    TextDim -> error "Invalid value for y field: TextDim" -- TODO: make impossible via type
  w' = case w of
    Absolute i -> i
    Fixed i -> i
    Ratio p -> floor (fromIntegral maxWidth * p)
    Auto -> maxWidth - x'
    Center -> error "Invalid value for width field: Center" -- TODO: make impossible via type
    TextDim -> textWidth
  h' = case h of
    Absolute i -> i
    Fixed i -> i
    Ratio p -> floor (fromIntegral maxHeight * p)
    Auto -> maxHeight - y'
    Center -> error "Invalid value for height field: Center" -- TODO: make impossible via type
    TextDim -> textHeight

fromAbsolute :: FixedOrRatio -> Int
fromAbsolute = \case
  Absolute i -> i
  _ -> error "Invalid value for box field: not Absolute"

drawBox :: Int -> Int -> Box -> AnsiString
drawBox maxWidth maxHeight box = runDrawM do
  drawBoxImpl Nothing $ toAbsolute maxWidth maxHeight 0 0 box
  tell [AnsiCsi $ CsiSetAbsCursorXY 0 maxHeight]

drawBoxIO :: Int -> Int -> Box -> IO ()
drawBoxIO maxWidth maxHeight box = do
  -- getCursorPosition >>= \case
  --   Just (0, 0) -> pure ()
  --   _x -> pure () -- error $ show x
  putStr $ ansiToString $ CsiSetAbsCursorXY 0 0
  let ansi = drawBox maxWidth maxHeight box
  putStr $ ansiToString ansi
  putStr $ ansiToString SgrReset
  putStr $ ansiToString $ CsiSetAbsCursorXY 0 maxHeight
  hFlush stdout

clearBoxViewport :: Int -> Int -> Box -> IO ()
clearBoxViewport maxWidth maxHeight box = do
  let Box
        { boxX = (fromAbsolute -> x)
        , boxY = (fromAbsolute -> y)
        , boxW = (fromAbsolute -> w)
        , boxH = (fromAbsolute -> h)
        } = toAbsolute maxWidth maxHeight 0 0 box
  putStr $ ansiToString SgrReset
  F.forM_ [y .. y + h - 1] \y' -> do
    putStr $ ansiToString $ CsiSetAbsCursorXY x y'
    putStr $ replicate w ' '

drawBoxImpl :: Maybe Rgb -> Box -> DrawM ()
drawBoxImpl mParentBackground box = do
  applyBgColor >>= \case
    False -> pure ()
    True -> drawBgWindow

  M.forM_ kidsPre \kid -> do
    M.void applyBgColor
    drawBoxImpl mBackground kid

  tell [AnsiCsi $ CsiSetAbsCursorXY x y]
  M.void applyBgColor
  tell $ map AnsiSgr colorCommands
  printMultiline
  tell [AnsiSgr SgrReset]

  M.forM_ kidsPost \kid -> do
    M.void applyBgColor
    drawBoxImpl mBackground kid

  tell [AnsiSgr SgrReset]
 where
  Box
    { boxText = text
    , boxClipper = clipper
    , boxX = (fromAbsolute -> x)
    , boxY = (fromAbsolute -> y)
    , boxW = (fromAbsolute -> w)
    , boxH = (fromAbsolute -> h)
    , boxBackground = mBackgroundProto
    , boxColorCommands = colorCommands
    , boxKidsPre = kidsPre
    , boxKidsPost = kidsPost
    } = box
  mBackground = mBackgroundProto <|> mParentBackground
  textLines = toLines text
  applyBgColor = case mBackground of
    Nothing -> pure False
    Just bg -> do
      tell [AnsiSgr $ SgrTrueColor Bg bg]
      pure True
  printMultiline = do
    M.forM_ (zip [0 ..] textLines) \(j, textLine) -> do
      tell [AnsiCsi $ CsiSetAbsCursorXY x (y + j)]
      tell $ unAnsiString $ clipper w textLine
  drawBgWindow = do
    M.forM_ [0 .. h - 1] \j -> do
      tell [AnsiCsi $ CsiSetAbsCursorXY x (y + j)]
      tell $ replicate w $ AnsiChar ' '

addBorder :: String -> String
addBorder s
  | null s = "▄▄\n▀▀"
  | otherwise = top ++ "\n" ++ middle ++ "\n" ++ bottom
 where
  linesOfText = lines s
  width = maximum $ 0 : map length linesOfText
  top = "▄" ++ replicate width '▄' ++ "▄"
  middle = intercalate "\n" $ map (\x -> "█" ++ x ++ replicate (width - length x) ' ' ++ "█") linesOfText
  bottom = "▀" ++ replicate width '▀' ++ "▀"

addOverlay :: Box -> Box -> Box
addOverlay overlay box = box{boxKidsPost = boxKidsPost box ++ [overlay]}

addPopup :: String -> Box -> Box
addPopup msg =
  addOverlay
    Box
      { boxText = fromString $ addBorder msg
      , boxClipper = takeAnsi
      , boxX = Center
      , boxY = Fixed 0
      , boxW = TextDim
      , boxH = TextDim
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

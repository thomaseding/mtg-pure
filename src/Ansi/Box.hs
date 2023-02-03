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
  clearScreenByPaging,
  clearScreenWithoutPaging,
  clearBoxViewport,
  FixedOrRatio (..),
  ColorCommand (..),
  Box (..),
  toAbsolute,
  drawBox,
  drawBoxFast,
  addBorder,
  addPopup,
) where

import safe Control.Applicative ((<|>))
import safe Control.Exception (assert, finally)
import safe qualified Control.Monad as M
import safe qualified Control.Monad.State.Strict as State
import safe qualified Control.Monad.Trans as M
import safe qualified Data.Char as Char
import safe qualified Data.Foldable as F
import safe Data.List (foldl', intercalate)
import safe qualified Data.List as List
import safe qualified Data.Map as Map
import safe Data.Maybe (catMaybes)
import safe System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  clearLine,
  clearScreen,
  getTerminalSize,
  setCursorPosition,
  setSGR,
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

clearScreenWithoutPaging :: M.MonadIO m => m ()
clearScreenWithoutPaging = M.liftIO do
  setSGR [Reset]
  getTerminalSize >>= \case
    Nothing -> assert False clearScreen -- fallback to paging
    Just (_w, h) -> do
      F.for_ [0 .. h - 1] \y -> do
        setCursorPosition y 0
        clearLine

clearScreenByPaging :: M.MonadIO m => m ()
clearScreenByPaging = M.liftIO do
  setSGR [Reset]
  clearScreen

finallyCleanup :: IO a -> IO a
finallyCleanup m = do
  m `finally` do
    setSGR [Reset]
    clearScreenByPaging -- Paging is non-destructive, which is nice when exiting the app.
    showCursor
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
  -- change code page for cmd.exe to support unicode
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
          setSGR [Reset]
          clearScreen
          action

type AnsiColor = (ColorIntensity, Color)

data ColorCommand where
  SetFg :: AnsiColor -> ColorCommand
  SetBg :: AnsiColor -> ColorCommand
  ResetColor :: ColorCommand

toSGR :: ColorCommand -> SGR
toSGR = \case
  SetFg (intensity, color) -> SetColor Foreground intensity color
  SetBg (intensity, color) -> SetColor Background intensity color
  ResetColor -> Reset

data FixedOrRatio
  = Absolute Int
  | Fixed Int
  | Ratio Double
  | Auto
  | Center
  | TextDim

-- TODO: Use my AnsiString types
data Box = Box
  { boxText :: String
  , boxClipper :: Int -> String -> String
  , boxX :: FixedOrRatio
  , boxY :: FixedOrRatio
  , boxW :: FixedOrRatio
  , boxH :: FixedOrRatio
  , boxBackground :: Maybe AnsiColor
  , boxColorCommands :: [ColorCommand]
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
  textLines = lines text
  textWidth = maximum $ 0 : map length textLines
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

drawBox :: Int -> Int -> Box -> IO ()
drawBox maxWidth maxHeight box = do
  drawBoxImpl Nothing $ toAbsolute maxWidth maxHeight 0 0 box
  setCursorPosition maxHeight 0
  hFlush stdout

clearBoxViewport :: Int -> Int -> Box -> IO ()
clearBoxViewport maxWidth maxHeight box = do
  let Box
        { boxX = (fromAbsolute -> x)
        , boxY = (fromAbsolute -> y)
        , boxW = (fromAbsolute -> w)
        , boxH = (fromAbsolute -> h)
        } = toAbsolute maxWidth maxHeight 0 0 box
  setSGR [Reset]
  F.forM_ [y .. y + h - 1] \y' -> do
    setCursorPosition y' x
    putStr $ replicate w ' '

drawBoxImpl :: Maybe (ColorIntensity, Color) -> Box -> IO ()
drawBoxImpl mParentBackground box = do
  applyBgColor >>= \case
    False -> pure ()
    True -> drawBgWindow

  M.forM_ kidsPre \kid -> do
    M.void applyBgColor
    drawBoxImpl mBackground kid

  setCursorPosition y x
  M.void applyBgColor
  applySgr
  printMultiline

  M.forM_ kidsPost \kid -> do
    M.void applyBgColor
    drawBoxImpl mBackground kid

  setSGR [Reset]
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
  textLines = lines text
  applyBgColor = case mBackground of
    Nothing -> pure False
    Just (i, c) -> setSGR [SetColor Background i c] >> pure True
  applySgr = case map toSGR colorCommands of
    [] -> pure ()
    sgr -> setSGR sgr
  printMultiline = do
    M.forM_ (zip [0 ..] textLines) \(j, textLine) -> do
      setCursorPosition (y + j) x
      putStr $ clipper w textLine
  drawBgWindow = do
    M.forM_ [0 .. h - 1] \j -> do
      setCursorPosition (y + j) x
      putStr $ replicate w ' '

addBorder :: String -> String
addBorder s
  | null s = "++\n++"
  | otherwise = top ++ "\n" ++ middle ++ "\n" ++ bottom
 where
  linesOfText = lines s
  width = maximum $ 0 : map length linesOfText
  top = "+" ++ replicate width '-' ++ "+"
  middle = intercalate "\n" $ map (\x -> "|" ++ x ++ replicate (width - length x) ' ' ++ "|") linesOfText
  bottom = "+" ++ replicate width '-' ++ "+"

addOverlay :: Box -> Box -> Box
addOverlay overlay box = box{boxKidsPre = boxKidsPre box ++ [overlay]}

addPopup :: String -> Box -> Box
addPopup msg =
  addOverlay
    Box
      { boxText = addBorder msg
      , boxClipper = take
      , boxX = Center
      , boxY = Center
      , boxW = TextDim
      , boxH = TextDim
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

data RenderedColor where
  FatSpecified :: AnsiColor -> RenderedColor
  FatInherit :: RenderedColor
  FatReset :: RenderedColor
  deriving (Eq)

data RenderedCell = RenderedCell
  { renderedChar :: Char
  , renderedFg :: RenderedColor
  , renderedBg :: RenderedColor
  }

emptyCell :: RenderedCell
emptyCell = RenderedCell{renderedChar = ' ', renderedFg = FatInherit, renderedBg = FatInherit}

compileFgBg :: [ColorCommand] -> RenderedCell
compileFgBg = foldl' go emptyCell
 where
  go :: RenderedCell -> ColorCommand -> RenderedCell
  go rc cc = case cc of
    SetFg c -> rc{renderedFg = FatSpecified c}
    SetBg c -> rc{renderedBg = FatSpecified c}
    ResetColor -> rc{renderedFg = FatReset, renderedBg = FatReset}

type MappedRender = Map.Map (Int, Int) RenderedCell

compileString :: Int -> Int -> [ColorCommand] -> String -> MappedRender -> MappedRender
compileString x y colorCommands fullText dest = foldl' go dest (zip [0 ..] textLines)
 where
  textLines = lines fullText
  cellProto = compileFgBg colorCommands
  render c = cellProto{renderedChar = c}
  go m (j, textLine) = foldl' go' m (zip [0 ..] textLine)
   where
    go' m' (i, c) = Map.insertWith updateCell (x + i, y + j) (render c) m'

compileBox :: Int -> Int -> Box -> MappedRender
compileBox w h = flip compileAbsoluteBox mempty . toAbsolute w h 0 0

compileAbsoluteBox :: Box -> MappedRender -> MappedRender
compileAbsoluteBox box dest0 = dest4
 where
  Box
    { boxText = text
    , boxX = (fromAbsolute -> x)
    , boxY = (fromAbsolute -> y)
    , boxW = (fromAbsolute -> w)
    , boxH = (fromAbsolute -> h)
    , boxColorCommands = colorCommands
    , boxBackground = mBackground
    , boxKidsPre = kidsPre
    , boxKidsPost = kidsPost
    } = box
  boxCoords = [(x', y') | x' <- [x .. x + w - 1], y' <- [y .. y + h - 1]]
  colorCommands' = case mBackground of
    Nothing -> colorCommands
    Just (i, c) -> SetBg (i, c) : colorCommands
  dest1 = case mBackground of
    Nothing -> dest0
    Just color ->
      let render = RenderedCell{renderedChar = ' ', renderedFg = FatInherit, renderedBg = FatSpecified color}
       in foldl' (\m coord -> Map.insertWith updateCell coord render m) dest0 boxCoords
  dest2 = foldl' (flip compileAbsoluteBox) dest1 kidsPre
  dest3 = compileString x y colorCommands' text dest2
  dest4 = foldl' (flip compileAbsoluteBox) dest3 kidsPost

updateCell :: RenderedCell -> RenderedCell -> RenderedCell
updateCell new old =
  old
    { renderedChar = renderedChar new
    , renderedFg = case renderedFg new of
        FatInherit -> renderedFg old
        FatReset -> FatReset
        FatSpecified c -> FatSpecified c
    , renderedBg = case renderedBg new of
        FatInherit -> renderedBg old
        FatReset -> FatReset
        FatSpecified c -> FatSpecified c
    }

type RowRender = [RenderedCell]

type TableRender = [RowRender]

data RenderPrim where
  RenderChar :: Char -> RenderPrim
  RenderFg :: RenderedColor -> RenderPrim
  RenderBg :: RenderedColor -> RenderPrim

rowRenderToPrim :: RowRender -> [RenderPrim]
rowRenderToPrim = concatMap \case
  RenderedCell
    { renderedChar = c
    , renderedFg = fg
    , renderedBg = bg
    } -> [RenderFg fg, RenderBg bg, RenderChar c]

data OptimizeState = OptimizeState
  { optimizeLogicalFg :: RenderedColor
  , optimizeLogicalBg :: RenderedColor
  , optimizeEmittedFg :: RenderedColor
  , optimizeEmittedBg :: RenderedColor
  }

emptyOptimizeState :: OptimizeState
emptyOptimizeState =
  OptimizeState
    { optimizeLogicalFg = FatReset
    , optimizeLogicalBg = FatReset
    , optimizeEmittedFg = FatReset
    , optimizeEmittedBg = FatReset
    }

inheritToReset :: RenderPrim -> RenderPrim
inheritToReset = \case
  RenderFg FatInherit -> RenderFg FatReset
  RenderBg FatInherit -> RenderBg FatReset
  x -> x

-- This removes redundant color commands.
-- Example : [RenderFg fg1, RenderBg bg1, RenderFg fg2, RenderChar c1, RenderFg fg3, RenderChar c2]
-- optimizes to [RenderBg bg1, Render fg2, RenderChar c1, RenderChar c2, RenderFg fg3, RenderBg bg2]
--
-- Impl uses monad State that tracks the OptimizeState. It only emits a RenderPrim for fg/bg if both are satisfied:
--   1. the fg/bg is different from the current fg/bg
--   2. End of list or rendering a char.
optimizePrims :: [RenderPrim] -> [RenderPrim]
optimizePrims prims = State.evalState (go $ map inheritToReset prims) emptyOptimizeState
 where
  go :: [RenderPrim] -> State.State OptimizeState [RenderPrim]
  go = \case
    [] -> pure []
    RenderChar c : rest -> do
      st <- State.get
      let fg = optimizeLogicalFg st
      let bg = optimizeLogicalBg st
      let fg' = optimizeEmittedFg st
      let bg' = optimizeEmittedBg st
      let emitFg = fg' /= fg
      let emitBg = bg' /= bg
      State.modify' \s -> s{optimizeEmittedFg = fg, optimizeEmittedBg = bg}
      case (emitFg, emitBg) of
        (False, False) -> (RenderChar c :) <$> go rest
        (True, False) -> ([RenderFg fg, RenderChar c] ++) <$> go rest
        (False, True) -> ([RenderBg bg, RenderChar c] ++) <$> go rest
        (True, True) -> ([RenderFg fg, RenderBg bg, RenderChar c] ++) <$> go rest
    RenderFg fg : rest -> do
      State.modify' \s -> s{optimizeLogicalFg = fg}
      go rest
    RenderBg bg : rest -> do
      State.modify' \s -> s{optimizeLogicalBg = bg}
      go rest

-- XXX: This doesn't move Reset to the head of a sequence of SGR commands.
renderPrims :: [RenderPrim] -> IO ()
renderPrims = mapM_ \case
  RenderChar c -> putChar c
  RenderFg fg -> case fg of
    FatReset -> setSGR [Reset]
    FatInherit -> pure ()
    FatSpecified (i, c) -> setSGR [SetColor Foreground i c]
  RenderBg bg -> case bg of
    FatReset -> setSGR [Reset]
    FatInherit -> pure ()
    FatSpecified (i, c) -> setSGR [SetColor Background i c]

data FastPrim where
  PrimString :: String -> FastPrim
  PrimSGR :: [SGR] -> FastPrim

toFastPrims :: [RenderPrim] -> [FastPrim]
toFastPrims = \case
  [] -> []
  RenderChar c : rest -> case toFastPrims rest of
    PrimString s : rest' -> PrimString (c : s) : rest'
    rest' -> PrimString [c] : rest'
  RenderFg fg : rest -> case toFastPrims rest of
    PrimSGR sgrs : rest' -> PrimSGR (toSGR' Foreground fg : sgrs) : rest'
    rest' -> PrimSGR [toSGR' Foreground fg] : rest'
  RenderBg bg : rest -> case toFastPrims rest of
    PrimSGR sgrs : rest' -> PrimSGR (toSGR' Background bg : sgrs) : rest'
    rest' -> PrimSGR [toSGR' Background bg] : rest'
 where
  toSGR' :: ConsoleLayer -> RenderedColor -> SGR
  toSGR' layer = \case
    FatReset -> Reset
    FatInherit -> Reset
    FatSpecified (i, c) -> SetColor layer i c

renderFastPrim :: FastPrim -> IO ()
renderFastPrim = \case
  PrimString s -> putStr s
  PrimSGR sgrs -> case sgrs of
    [] -> pure ()
    _ -> assert (length sgrs <= 2) $ setSGR $ massage $ reverse sgrs
 where
  massage sgrs = case List.find (Reset ==) sgrs of
    Nothing -> sgrs
    Just _ -> Reset : filter (/= Reset) sgrs

renderFastPrims :: [FastPrim] -> IO ()
renderFastPrims = mapM_ renderFastPrim

mapRenderToTable :: MappedRender -> TableRender
mapRenderToTable mr = map goY tableYs
 where
  goY y = map (goX y) tableXs
  goX y x = Map.findWithDefault emptyCell (x, y) mr
  mapCoords = Map.keys mr
  maxCoordX = maximum $ 0 : map fst mapCoords
  maxCoordY = maximum $ 0 : map snd mapCoords
  tableXs = [0 .. maxCoordX]
  tableYs = [0 .. maxCoordY]

printRenderedRow :: RowRender -> IO ()
printRenderedRow = mapM_ go
 where
  go RenderedCell{renderedChar = c, renderedFg = fg, renderedBg = bg} = do
    case toSGR <$> colorCommands fg bg of
      [] -> pure ()
      sgr -> setSGR sgr
    putChar c
  goLayer fromLayer layer = case layer of
    FatSpecified c -> Just $ fromLayer c
    FatInherit -> Just ResetColor -- Nothing
    FatReset -> Just ResetColor
  goFg = goLayer SetFg
  goBg = goLayer SetBg
  colorCommands fg bg = catMaybes [goFg fg, goBg bg]

printRenderedTable :: TableRender -> IO ()
printRenderedTable = mapM_ \row -> do
  case (if True then 2 else undefined) of
    0 -> printRenderedRow row
    1 -> renderPrims $ optimizePrims $ rowRenderToPrim row
    2 -> renderFastPrims $ toFastPrims $ optimizePrims $ rowRenderToPrim row
    _ -> undefined
  putChar '\n'

drawBoxFast :: Int -> Int -> Box -> IO ()
drawBoxFast w h box = do
  let mapRender = compileBox w h box
      tableRender = mapRenderToTable mapRender
  setCursorPosition 0 0
  printRenderedTable tableRender
  setSGR [Reset]
  hFlush stdout

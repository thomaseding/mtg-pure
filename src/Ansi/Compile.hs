{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use list comprehension" #-}

module Ansi.Compile (
  ScreenPos (..),
  RenderInput (..),
  RenderState (..),
  RenderedCells,
  pruneRenderedCells,
  renderedCellsToAnsi,
  renderAnsiString,
  efficientAnsi,
  main,
) where

import safe Ansi.AnsiString (
  AnsiChar (..),
  AnsiString (..),
  AnsiToString (..),
  Csi (..),
  Layer (..),
  Rgb (..),
  Sgr (..),
 )
import safe Control.Exception (assert)
import safe qualified Control.Monad.State.Strict as State
import safe Data.List (foldl', sortBy)
import safe qualified Data.Map.Strict as Map
import safe Data.Ord (comparing)

-- NOTE: `Reset` is simplified to just setting the default colors
data RenderedCell = RenderedCell
  { renderedCell_char :: Char
  , renderedCell_fg :: Rgb -- XXX: Might want to support Maybe Rgb for Reset
  , renderedCell_bg :: Rgb -- XXX: Might want to support Maybe Rgb for Reset
  }
  deriving (Eq, Ord, Show)

data ScreenPos = ScreenPos
  { screenPos_x :: Int
  , screenPos_y :: Int
  }
  deriving (Eq, Show)

instance Ord ScreenPos where
  compare :: ScreenPos -> ScreenPos -> Ordering
  compare (ScreenPos x1 y1) (ScreenPos x2 y2) =
    case compare y1 y2 of
      EQ -> compare x1 x2
      other -> other

-- TODO: try out mutable arrays
--
-- Clipping must be done before this step to allow items behind clipped
-- items to be rendered. Otherwise it would just be an empty cell with
-- the default colors.
--
-- Once in this form, users can prune cells that have not changed
-- since the last render to the screen.
type RenderedCells = Map.Map ScreenPos RenderedCell

type FgBg = (Rgb, Rgb)

data RenderInput = RenderInput
  { renderInput_ :: ()
  , renderInput_defaultColors :: FgBg
  , renderInput_startPos :: ScreenPos
  , renderInput_cursorIsShown :: Bool
  }
  deriving (Eq, Ord, Show)

data RenderState = RenderState
  { renderState_ :: ()
  , renderState_input :: RenderInput
  , renderState_grid :: RenderedCells
  , renderState_cursorX :: Int
  , renderState_cursorY :: Int
  , renderState_fg :: Rgb
  , renderState_bg :: Rgb
  , renderState_cursorIsShown :: Bool
  }
  deriving (Eq, Ord, Show)

emptyRenderState :: RenderInput -> RenderState
emptyRenderState input =
  RenderState
    { renderState_ = ()
    , renderState_input = input
    , renderState_grid = Map.empty
    , renderState_cursorX = x
    , renderState_cursorY = y
    , renderState_fg = fg
    , renderState_bg = bg
    , renderState_cursorIsShown = renderInput_cursorIsShown input
    }
 where
  (fg, bg) = renderInput_defaultColors input
  ScreenPos x y = renderInput_startPos input

type RenderM = State.State RenderState

runRenderM :: RenderInput -> RenderM a -> a
runRenderM = flip State.evalState . emptyRenderState

renderAnsiString :: RenderInput -> AnsiString -> RenderState
renderAnsiString input (AnsiString str) = runRenderM input do
  mapM_ renderAnsiChar str
  State.get

-- Although this handles newline, Box probably should have already
-- turned them into CSI CsiSetPositionXY-style commands. Otherwise the
-- Box renderer would need to set SGR Reset before each newline to avoid
-- the terminal from rendering the current BG color to the edge of
-- the screen.
renderAnsiChar :: AnsiChar -> RenderM ()
renderAnsiChar = \case
  AnsiChar c -> do
    State.modify' \st ->
      let newline = c == '\n'
          x = renderState_cursorX st
          y = renderState_cursorY st
          grid = renderState_grid st
          grid' =
            Map.insert
              ScreenPos{screenPos_x = x, screenPos_y = y}
              RenderedCell
                { renderedCell_char = c
                , renderedCell_fg = renderState_fg st
                , renderedCell_bg = renderState_bg st
                }
              grid
       in st
            { renderState_cursorX = if newline then 0 else x + 1
            , renderState_cursorY = y + if newline then 1 else 0
            , renderState_grid = if newline then grid else grid'
            }
  AnsiCsi csi -> do
    State.modify' \st ->
      case csi of
        CsiSetNextLine -> st{renderState_cursorX = 0, renderState_cursorY = renderState_cursorY st + 1}
        CsiSetPrevLine -> st{renderState_cursorX = 0, renderState_cursorY = renderState_cursorY st - 1}
        CsiSetRelCursorX x -> st{renderState_cursorX = renderState_cursorX st + x}
        CsiSetRelCursorY y -> st{renderState_cursorY = renderState_cursorY st + y}
        CsiSetAbsCursorX x -> st{renderState_cursorX = x}
        CsiSetAbsCursorXY x y -> st{renderState_cursorX = x, renderState_cursorY = y}
  AnsiSgr sgr -> do
    State.modify' \st ->
      case sgr of
        SgrReset ->
          let (fg, bg) = renderInput_defaultColors $ renderState_input st
           in st{renderState_fg = fg, renderState_bg = bg}
        SgrTrueColor Fg rgb -> st{renderState_fg = rgb}
        SgrTrueColor Bg rgb -> st{renderState_bg = rgb}

--------------------------------------------------------------------------------

type BucketedCellMap = Map.Map FgBg [(ScreenPos, RenderedCell)]

type BucketedCells = (FgBg, [(ScreenPos, RenderedCell)])

-- | Each returned bucket is a list of (pos, renderedCell) pairs, sorted by screen pos.
-- No duplicate positions are returned across all/any of the buckets.
--
-- Not necessary, but this is part of an optimization step. Might want to quantize colors
-- even further to really benefit from this, since each cell is likely to have subtly
-- different colors and thus gets a unique bucket... pointless and perhaps induces more
-- ANSI overhead than it saves.
bucketRenderedCellsByFgBg :: RenderedCells -> BucketedCellMap
bucketRenderedCellsByFgBg = goSort . Map.foldrWithKey goBucket Map.empty
 where
  goBucket pos cell =
    let fgBg = (renderedCell_fg cell, renderedCell_bg cell)
     in Map.insertWith (++) fgBg [(pos, cell)]
  goSort = Map.map \entries -> flip sortBy entries \(posA, _) (posB, _) -> compare posA posB

getStartPosOfBucketedCells :: BucketedCells -> ScreenPos
getStartPosOfBucketedCells (_fgBg, entries) = case entries of
  [] -> error "empty bucket"
  entry : _ -> case entry of
    (pos, _cell) -> pos

-- | Returns a list of (fgBg, bucket) pairs, sorted by some optimizing order.
--
-- A super heavy-duty version of this would somehow sort these not necessarily by screen pos,
-- but in a way that would make differing FgBg buckets adjacent to each other when they share
-- either the same fg or bg color. This would save on SgrTrueColorFg or SgrTrueColorBg commands
-- in the final output. But this is probably overkill and not worth the complexity. Plus, it is
-- likely to lose out on some CSI efficiency.
--
-- TODO: If any of the buckets have the default color, prioritize them to start of list
bucketMapToList :: RenderInput -> BucketedCellMap -> [BucketedCells]
bucketMapToList input = sortBy goSort . Map.assocs
 where
  (iFg, iBg) = renderInput_defaultColors input
  scoreDefaultColoring z =
    let ((fg, bg), _cells) = z
     in bToI (fg == iFg) + bToI (bg == iBg)
  goSort x y = case comparing scoreDefaultColoring y x of
    EQ -> comparing getStartPosOfBucketedCells x y
    LT -> LT
    GT -> GT

bToI :: Bool -> Int
bToI = \case
  False -> 0
  True -> 1

-- | Returns the final cursor position and the rendered AnsiString
-- The input is a list of (pos, renderedCell) pairs, sorted by screen pos.
--
-- Not necessary, but this is part of an optimization step.
bucketedCellsToAnsiString :: RenderInput -> ScreenPos -> FgBg -> [(ScreenPos, RenderedCell)] -> (ScreenPos, [AnsiChar])
bucketedCellsToAnsiString input bucketStartPos (fg, bg) = fixup . foldr go initialAcc . reverse
 where
  fixup (pos, xs) = (pos, reverse xs)
  inputStartPos = renderInput_startPos input
  (defaultFg, defaultBg) = renderInput_defaultColors input
  colorFg =
    if const False $ fg == defaultFg && bucketStartPos == inputStartPos
      then []
      else [AnsiSgr $ SgrTrueColor Fg fg]
  colorBg =
    if const False $ bg == defaultBg && bucketStartPos == inputStartPos
      then []
      else [AnsiSgr $ SgrTrueColor Bg bg]
  initialAcc = (bucketStartPos, colorBg ++ colorFg)

  go :: (ScreenPos, RenderedCell) -> (ScreenPos, [AnsiChar]) -> (ScreenPos, [AnsiChar])
  go (curr, cell) (prev, acc) =
    let ScreenPos{screenPos_x = x, screenPos_y = y} = assert (ScreenPos 0 0 <= curr) curr
        ScreenPos{screenPos_x = x', screenPos_y = y'} = assert (ScreenPos (-1) 0 <= prev) prev
        isNextChar = x == x' + 1 && y == y'
        isNextLine = x == 0 && y == y' + 1
        isPrevLine = x == 0 && y == y' - 1
        isSameLine = y == y'
        isSameCol = x == x'
        relX = x - x'
        relY = y - y'
        hasExpectedFgBg = renderedCell_fg cell == fg && renderedCell_bg cell == bg
        char = AnsiChar case renderedCell_char cell of
          '\n' -> error "bucketedCellsToAnsiString: unexpected newline"
          c -> c
        setRelCursorX = AnsiCsi $ CsiSetAbsCursorX x -- CsiSetRelCursorX relX
        setRelCursorY = AnsiCsi $ CsiSetAbsCursorXY x y -- CsiSetRelCursorY relY
        setAbsPosX = AnsiCsi $ CsiSetAbsCursorX x
        setAbsPosXY = AnsiCsi $ CsiSetAbsCursorXY x y
        setNextLine = AnsiCsi CsiSetNextLine
        setPrevLine = AnsiCsi CsiSetPrevLine
        setCursorX = if abs relX < x then setRelCursorX else setAbsPosX
        setCursorY = case abs relY <= y of
          True -> setRelCursorY -- fast path
          False -> case length (ansiToString setRelCursorY) < length (ansiToString setAbsPosXY) of
            True -> setRelCursorY
            False -> setAbsPosXY
     in if
          | not hasExpectedFgBg -> error "bucketedCellsToAnsiString: unexpected fg/bg"
          | isNextChar -> (curr, char : acc)
          | isPrevLine && x == 0 -> (curr, char : setPrevLine : acc)
          | isNextLine && x == 0 -> (curr, char : setNextLine : acc)
          | isSameLine -> (curr, char : setCursorX : acc)
          | isSameCol -> (curr, char : setCursorY : acc) -- NOTE: there is no abs set cursor y w/o x
          | otherwise -> (curr, char : setAbsPosXY : acc)

--------------------------------------------------------------------------------

renderCellToAnsi :: ScreenPos -> RenderedCell -> AnsiString
renderCellToAnsi pos cell = AnsiString case renderedCell_char cell of
  '\n' ->
    [ AnsiCsi $ CsiSetAbsCursorXY (screenPos_x pos) (screenPos_y pos + 1)
    ]
  c ->
    [ AnsiCsi $ CsiSetAbsCursorXY (screenPos_x pos) (screenPos_y pos)
    , AnsiSgr $ SgrTrueColor Fg $ renderedCell_fg cell
    , AnsiSgr $ SgrTrueColor Bg $ renderedCell_bg cell
    , AnsiChar c
    ]

renderedCellsToAnsi :: RenderedCells -> AnsiString
renderedCellsToAnsi grid = mconcat renderedCells
 where
  cellList = Map.assocs grid
  renderedCells = map (uncurry renderCellToAnsi) cellList

--------------------------------------------------------------------------------

pruneRenderedCells :: RenderedCells -> RenderedCells -> RenderedCells
pruneRenderedCells old new = Map.differenceWith go new old
 where
  go new' old' = case new' == old' of
    True -> Nothing
    False -> Just new'

efficientAnsi :: RenderedCells -> RenderInput -> AnsiString -> (RenderedCells, AnsiString)
efficientAnsi oldGrid input inputAnsi = (,) newGrid $ AnsiString case Map.null prunedGrid of
  True -> [setEndPos]
  False -> setStartPos' ++ ansiStr ++ setEndPos' ++ extras
 where
  st = renderAnsiString input inputAnsi
  (endX, endY) = (renderState_cursorX st, renderState_cursorY st)
  endPos = ScreenPos endX endY
  setEndPos = AnsiCsi $ CsiSetAbsCursorXY endX endY
  newGrid = renderState_grid st
  prunedGrid = pruneRenderedCells oldGrid newGrid
  bucketMap = bucketRenderedCellsByFgBg prunedGrid
  bucketList = bucketMapToList input bucketMap
  startPos = getStartPosOfBucketedCells case bucketList of
    [] -> error "empty list"
    x : _ -> x
  ScreenPos startX startY = startPos
  setStartPos = AnsiCsi $ CsiSetAbsCursorXY startX startY
  setStartPos' = case startPos == renderInput_startPos input of
    True -> []
    False -> [setStartPos]
  goBucket :: (ScreenPos, [[AnsiChar]]) -> BucketedCells -> (ScreenPos, [[AnsiChar]])
  goBucket (prevPos, accAnsiStrs) bucket =
    let (fgBg, positionedCells) = bucket
        (nextPos, str) = bucketedCellsToAnsiString input prevPos fgBg positionedCells
     in (nextPos, str : accAnsiStrs)
  (finalPos, ansiStrs) = foldl' goBucket (ScreenPos (startX - 1) startY, []) bucketList
  ansiStr = foldl' (flip (++)) [] ansiStrs
  setEndPos' = case finalPos == endPos of
    True -> []
    False -> []
  extras = case renderState_cursorIsShown st of
    True -> []
    False -> error "TODO"

main :: IO ()
main = do
  let black = Rgb 0 0 0
  let white = Rgb 255 255 255
  let hello =
        AnsiString
          [ AnsiChar 'a'
          , AnsiChar 'A'
          , AnsiSgr $ SgrTrueColor Fg $ Rgb 3 2 1
          , AnsiChar 'b'
          , AnsiChar 'B'
          , AnsiCsi $ CsiSetAbsCursorXY 7 7
          , AnsiChar 'c'
          , AnsiChar 'C'
          , AnsiCsi $ CsiSetAbsCursorX 0
          , AnsiChar 'd'
          , AnsiChar 'D'
          , AnsiChar '\n'
          , AnsiChar 'e'
          , AnsiChar 'E'
          , AnsiCsi CsiSetNextLine
          , AnsiChar 'f'
          , AnsiChar 'F'
          , AnsiCsi $ CsiSetAbsCursorXY 0 0
          , AnsiChar '!'
          , AnsiCsi $ CsiSetRelCursorY 10
          , AnsiChar 'z'
          ]
  let input =
        RenderInput
          { renderInput_ = ()
          , renderInput_defaultColors = (white, black)
          , renderInput_startPos = ScreenPos 0 0
          , renderInput_cursorIsShown = True
          }
  print hello
  print $ renderAnsiString input hello
  let hello' = efficientAnsi mempty input hello
  print hello'

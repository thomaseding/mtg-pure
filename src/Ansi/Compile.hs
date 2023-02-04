{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Ansi.Compile (
  -- RenderedCell (..),
  -- renderAnsiString,
  module Ansi.Compile,
) where

import safe Ansi.AnsiString (AnsiChar (..), AnsiString, Csi (..), Rgb (..), Sgr (..))
import safe qualified Control.Monad.State.Strict as State
import safe Data.List (sortBy)
import safe qualified Data.Map.Strict as Map

-- NOTE: `Reset` is simplified to just setting the default colors
data RenderedCell = RenderedCell
  { renderedCell_char :: Char
  , renderedCell_fg :: Rgb
  , renderedCell_bg :: Rgb
  }
  deriving (Eq, Ord, Show)

data ScreenPos = ScreenPos
  { screenPos_x :: Int
  , screenPos_y :: Int
  }
  deriving (Eq, Show)

instance Ord ScreenPos where
  compare (ScreenPos x1 y1) (ScreenPos x2 y2) =
    case compare y1 y2 of
      EQ -> compare x1 x2
      other -> other

-- Clipping must be done before this step to allow items behind clipped
-- items to be rendered. Otherwise it would just be an empty cell with
-- the default colors.
--
-- Once in this form, users can prune cells that have not changed
-- since the last render to the screen.
type RenderedCells = Map.Map ScreenPos RenderedCell

type FgBg = (Rgb, Rgb)

data RenderState = RenderState
  { renderState_grid :: RenderedCells
  , renderState_cursorX :: Int
  , renderState_cursorY :: Int
  , renderState_fg :: Rgb
  , renderState_bg :: Rgb
  , renderState_defaultFg :: Rgb -- readonly
  , renderState_defaultBg :: Rgb -- readonly
  , renderState_showCursor :: Bool
  }

emptyRenderState :: FgBg -> RenderState
emptyRenderState (fg, bg) =
  RenderState
    { renderState_grid = Map.empty
    , renderState_cursorX = 0
    , renderState_cursorY = 0
    , renderState_fg = fg
    , renderState_bg = bg
    , renderState_defaultFg = fg
    , renderState_defaultBg = bg
    , renderState_showCursor = True
    }

type RenderM = State.State RenderState

runRenderM :: FgBg -> RenderM a -> a
runRenderM = flip State.evalState . emptyRenderState

renderAnsiString :: FgBg -> AnsiString -> RenderState
renderAnsiString fgBg input = runRenderM fgBg do
  mapM_ renderAnsiChar input
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
            , renderState_grid = if newline then grid' else grid
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
          let fg = renderState_defaultFg st
              bg = renderState_defaultBg st
           in st{renderState_fg = fg, renderState_bg = bg}
        SgrTrueColorFg rgb -> st{renderState_fg = rgb}
        SgrTrueColorBg rgb -> st{renderState_bg = rgb}

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
bucketRenderedCellsByFgBg :: Map.Map ScreenPos RenderedCell -> Map.Map FgBg [(ScreenPos, RenderedCell)]
bucketRenderedCellsByFgBg = goSort . Map.foldrWithKey goBucket Map.empty
 where
  goBucket pos cell =
    let fgBg = (renderedCell_fg cell, renderedCell_bg cell)
     in Map.insertWith (++) fgBg [(pos, cell)]
  goSort = Map.map \entries -> flip sortBy entries \(posA, _) (posB, _) -> compare posA posB

-- | Returns a list of (fgBg, bucket) pairs, sorted by the screen pos of the first cell in each bucket.
--
-- A super heavy-duty version of this would somehow sort these not necessarily by screen pos,
-- but in a way that would make differing FgBg buckets adjacent to each other when they share
-- either the same fg or bg color. This would save on SgrTrueColorFg or SgrTrueColorBg commands
-- in the final output. But this is probably overkill and not worth the complexity. Plus, it is
-- likely to lose out on some CSI efficiency.
bucketMapToList :: BucketedCellMap -> [BucketedCells]
bucketMapToList = goSort . Map.assocs
 where
  goSort = sortBy \(_, entriesA) (_, entriesB) ->
    let (posA, _) = case entriesA of
          [] -> error "bucketMapToList: empty bucket"
          e : _ -> e
        (posB, _) = case entriesB of
          [] -> error "bucketMapToList: empty bucket"
          e : _ -> e
     in compare posA posB

-- | Returns the final cursor position and the rendered AnsiString
-- The input is a list of (pos, renderedCell) pairs, sorted by screen pos.
--
-- Not necessary, but this is part of an optimization step.
bucketedCellsToAnsiString :: ScreenPos -> FgBg -> [(ScreenPos, RenderedCell)] -> (ScreenPos, AnsiString)
bucketedCellsToAnsiString startPos (fg, bg) = colorize . foldr go (startPos, []) . reverse
 where
  colorize :: (ScreenPos, AnsiString) -> (ScreenPos, AnsiString)
  colorize (pos, str) = (pos, AnsiSgr (SgrTrueColorFg fg) : AnsiSgr (SgrTrueColorBg bg) : str)

  go :: (ScreenPos, RenderedCell) -> (ScreenPos, AnsiString) -> (ScreenPos, AnsiString)
  go (curr, cell) (prev, acc) =
    let ScreenPos{screenPos_x = x, screenPos_y = y} = curr
        ScreenPos{screenPos_x = x', screenPos_y = y'} = prev
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
        setRelCursorX = AnsiCsi $ CsiSetRelCursorX relX
        setRelCursorY = AnsiCsi $ CsiSetRelCursorY relY
        setAbsPosX = AnsiCsi $ CsiSetAbsCursorX x
        setAbsPosXY = AnsiCsi $ CsiSetAbsCursorXY x y
        setNextLine = AnsiCsi CsiSetNextLine
        setPrevLine = AnsiCsi CsiSetPrevLine
        setCursorX = if abs relX < x then setRelCursorX else setAbsPosX
        setCursorXY = if abs relY < y then setRelCursorY else setAbsPosXY
     in if
            | not hasExpectedFgBg -> error "bucketedCellsToAnsiString: unexpected fg/bg"
            | isNextChar -> (curr, char : acc)
            | isPrevLine && x == 0 -> (curr, setPrevLine : char : acc)
            | isNextLine && x == 0 -> (curr, setNextLine : char : acc)
            | isSameLine -> (curr, setCursorX : char : acc)
            | isSameCol -> (curr, setRelCursorY : char : acc) -- NOTE: there is no abs set cursor y w/o x
            | otherwise -> (curr, setCursorXY : char : acc)

cellToAnsiString :: ScreenPos -> RenderedCell -> (ScreenPos, AnsiString)
cellToAnsiString startPos cell =
  let fgBg = (renderedCell_fg cell, renderedCell_bg cell)
   in bucketedCellsToAnsiString startPos fgBg [(startPos, cell)]

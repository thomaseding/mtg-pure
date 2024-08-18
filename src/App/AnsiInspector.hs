{-# LANGUAGE Unsafe #-}
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
{-# HLINT ignore "Use bimap" #-}

module App.AnsiInspector (
  main,
  mainAnsiImageDebugger,
) where

import safe Ansi.AnsiString (
  AnsiToString (..),
  Layer (..),
  Rgb (..),
  Sgr (..),
  ToAnsiString (..),
  dropAnsi,
  dullBlack,
  dullMagenta,
  dullYellow,
  parseAnsi,
  vividBlue,
  vividWhite,
 )
import safe Ansi.Box (
  Box (..),
  FixedOrRatio (..),
  clearScreenWithoutPaging,
  drawBox,
  withAnsi,
  withBuffering,
 )
import safe Ansi.Compile (
  RenderInput (..),
  RenderState (..),
  RenderedCells,
  ScreenPos (..),
  pruneRenderedCells,
  renderAnsiString,
  renderedCellsToAnsi,
 )
import Ansi.Old (
  platonicH,
  platonicW,
 )
import Ansi.TrueColor.Debug (convertImageToDebugAnsiImage)
import Ansi.TrueColor.FindRendering (rateAllRenderings)
import Ansi.TrueColor.ImageToAnsi (debugLoadRenderedTileGrid, renderedTileToAnsi)
import Ansi.TrueColor.RenderedTile (RenderedTile (..))
import Ansi.TrueColor.TileGrid (debugLoadTileGrid)
import Ansi.TrueColor.Types (
  AnsiImage,
  FgBg (..),
  Grid,
  Tile,
  TwoColors (..),
  tileH,
  tileW,
  unTcTile,
 )
import Ansi.TrueColor.VirtualChar (VirtualChar (..))
import Codec.Picture.Types (PixelRGB8 (..))
import safe qualified Control.Monad as M
import safe qualified Control.Monad.Trans as M
import safe qualified Control.Monad.Trans.State.Strict as State
import safe Data.Carousel (
  Carousel,
  carCursor,
  carFromList,
  carLeft,
  carRight,
  carSingle,
 )
import safe qualified Data.Char as Char
import safe Data.IORef (IORef, newIORef, readIORef, writeIORef)
import safe Data.List (sortOn)
import safe Data.String (IsString (..))
import safe Numeric (showHex)
import Script.GenerateGallerySingle.Main (CardAnsiInfo (..), cardNameToAnsis)
import Script.MtgPureConfig (MtgPureConfig, readMtgPureConfigFile)
import safe System.Console.ANSI (
  getTerminalSize,
  hideCursor,
  setCursorPosition,
 )
import safe System.IO (
  BufferMode (..),
  hFlush,
  stdout,
 )
import safe System.Keyboard (
  ArrowKey (..),
  Key (..),
  getKey,
  initKeyboardMain,
 )

--------------------------------------------------------------------------------

theCardName :: String
theCardName =
  (!! 1)
    [ {-0-} "PLACEHOLDER_CARD_NAME"
    , {-1-} "All Is Dust"
    , {-2-} "Counterspell"
    , {-3-} "Forest"
    , {-4-} "Island"
    , {-5-} "Raging Goblin"
    , {-6-} "Holy Day"
    ]

--------------------------------------------------------------------------------

data CardInfo = CardInfo
  { cardName :: String
  , cardSetName :: String
  , cardAnsiImage :: AnsiImage
  , cardFullTileGrid :: Grid (Tile PixelRGB8)
  , cardRenderedTileGrid :: Grid RenderedTile
  }

dummyCardInfo :: CardInfo
dummyCardInfo =
  CardInfo
    { cardName = "error cardName"
    , cardSetName = "error cardSetName"
    , cardAnsiImage = "error cardAnsiImage"
    , cardFullTileGrid = []
    , cardRenderedTileGrid = []
    }

data InspectorState = InspectorState
  { inspector_ :: ()
  , inspectorMtgPureConfig :: MtgPureConfig
  , inspectorTermSize :: IORef (Int, Int)
  , inspectorCards :: Carousel CardInfo
  , inspectorHighlightedPixel :: (Int, Int)
  , inspectorPrevCellState :: RenderedCells
  }

mkInspectorState :: IO InspectorState
mkInspectorState = do
  mtgConfig <- readMtgPureConfigFile
  refTermSize <- newIORef (-1, -1)
  pure
    InspectorState
      { inspector_ = ()
      , inspectorMtgPureConfig = mtgConfig
      , inspectorTermSize = refTermSize
      , inspectorCards = carSingle dummyCardInfo
      , inspectorHighlightedPixel = (0, 0)
      , inspectorPrevCellState = mempty
      }

newtype Inspector a = Inspector
  { unInspector :: State.StateT InspectorState IO a
  }

instance Functor Inspector where
  fmap :: (a -> b) -> Inspector a -> Inspector b
  fmap f = Inspector . fmap f . unInspector

instance Applicative Inspector where
  pure :: a -> Inspector a
  pure = Inspector . pure

  (<*>) :: Inspector (a -> b) -> Inspector a -> Inspector b
  f <*> x = Inspector $ unInspector f <*> unInspector x

instance Monad Inspector where
  (>>=) :: Inspector a -> (a -> Inspector b) -> Inspector b
  x >>= f = Inspector $ unInspector x >>= unInspector . f

instance M.MonadIO Inspector where
  liftIO :: IO a -> Inspector a
  liftIO = Inspector . M.liftIO

runInspector :: Inspector a -> IO a
runInspector action = withAnsi do
  state <- mkInspectorState
  runInspector' state action

runInspector' :: InspectorState -> Inspector a -> IO a
runInspector' state action = do
  State.evalStateT (unInspector action) state

main :: IO ()
main = mainAnsiImageDebugger

mainAnsiImageDebugger :: IO ()
mainAnsiImageDebugger = do
  initKeyboardMain
  hideCursor
  runInspector do
    fabricateCardAnsiImages
    runCarousel
    clearScreenWithoutPaging
    M.liftIO $ hFlush stdout

fabricateCardAnsiImages :: Inspector ()
fabricateCardAnsiImages = do
  clearScreenWithoutPaging
  M.liftIO $ hFlush stdout
  mtgConfig <- Inspector $ State.gets inspectorMtgPureConfig
  ansiInfos <- M.liftIO $ withBuffering stdout LineBuffering do
    concat <$> mapM (cardNameToAnsis True mtgConfig) [theCardName]
  let imgPaths = map caiSourceImage ansiInfos
  fullTileGrids <- M.liftIO do
    mapM (debugLoadTileGrid platonicW platonicH) imgPaths
  renderedGrids <- M.liftIO do
    mapM (debugLoadRenderedTileGrid platonicW platonicH) imgPaths
  clearScreenWithoutPaging
  M.liftIO $ hFlush stdout
  let cards = carFromList $ zipWith3 mkCardInfo ansiInfos fullTileGrids renderedGrids
  Inspector $ State.modify' \st' -> st'{inspectorCards = cards}

mkCardInfo :: CardAnsiInfo -> Grid (Tile PixelRGB8) -> Grid RenderedTile -> CardInfo
mkCardInfo ansiInfo fullTileGrid renderedGrid =
  CardInfo
    { cardName = caiCardName ansiInfo
    , cardSetName = caiSetName ansiInfo
    , cardAnsiImage = case parseAnsi $ caiAnsiImage ansiInfo of
        Left err -> error err
        Right x -> x
    , cardFullTileGrid = fullTileGrid
    , cardRenderedTileGrid = renderedGrid
    }

runCarousel :: Inspector ()
runCarousel = M.forever do
  fixResizeArtifacts
  printCurrentCard
  M.liftIO $ hFlush stdout
  handleKeyInput

updateHighLightedPixel :: ArrowKey -> Inspector ()
updateHighLightedPixel arrow = do
  let delta = case arrow of
        KeyLeft -> (-1, 0)
        KeyRight -> (1, 0)
        KeyUp -> (0, -1)
        KeyDown -> (0, 1)
  curr <- Inspector $ State.gets inspectorHighlightedPixel
  let curr' = (fst curr + fst delta, snd curr + snd delta)
  let curr'' = clamp (0, 0) (platonicW - 1, (platonicH `div` 2) - 1) curr'
  Inspector $ State.modify' \st -> st{inspectorHighlightedPixel = curr''}

clamp :: (Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a)
clamp (minX, minY) (maxX, maxY) (x, y) =
  ( clamp' minX maxX x
  , clamp' minY maxY y
  )
 where
  clamp' :: (Ord a) => a -> a -> a -> a
  clamp' min' max' x' = max min' $ min max' x'

handleKeyInput :: Inspector ()
handleKeyInput = do
  key <- M.liftIO getKey
  case key of
    KeyArrow arrow -> do
      updateHighLightedPixel arrow
    KeyChar c -> case c of
      '[' -> Inspector $ State.modify' \st -> st{inspectorCards = carLeft $ inspectorCards st}
      ']' -> Inspector $ State.modify' \st -> st{inspectorCards = carRight $ inspectorCards st}
      _ -> pure ()

mkHighLightedPixelBox :: Inspector Box
mkHighLightedPixelBox = do
  (x, y) <- Inspector $ State.gets inspectorHighlightedPixel
  pure
    Box
      { boxText = "X"
      , boxClipper = const id
      , boxX = Fixed x
      , boxY = Fixed y
      , boxW = Absolute 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = const [] [SgrTrueColor Fg $ Rgb 255 0 0]
      , boxKidsPre = []
      , boxKidsPost = []
      }

mkCardImageBox :: Inspector Box
mkCardImageBox = do
  highlight <- mkHighLightedPixelBox
  card <- Inspector $ State.gets $ carCursor . inspectorCards
  pure
    Box
      { boxText = cardAnsiImage card
      , boxClipper = const id
      , boxX = Absolute 0
      , boxY = Absolute 0
      , boxW = Absolute platonicW
      , boxH = Absolute platonicH
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = [highlight]
      }

mkSourceTileBox' :: Inspector Box
mkSourceTileBox' = do
  card <- Inspector $ State.gets $ carCursor . inspectorCards
  (x, y) <- Inspector $ State.gets inspectorHighlightedPixel
  let tile = cardFullTileGrid card !! y !! x
  let ansi = convertImageToDebugAnsiImage tile
  pure
    Box
      { boxText = ansi
      , boxClipper = const id
      , boxX = Fixed 2
      , boxY = Fixed 1
      , boxW = Absolute tileW
      , boxH = Absolute tileH
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

mkSourceTileBox :: Inspector Box
mkSourceTileBox = do
  box <- mkSourceTileBox'
  pure
    Box
      { boxText = fromString $ "S 01234567" ++ makeVerticalHexString 16
      , boxClipper = const id
      , boxX = Absolute platonicW
      , boxY = Absolute 0
      , boxW = Absolute $ tileW + 3
      , boxH = Absolute $ tileH + 2
      , boxBackground = Just dullYellow
      , boxColorCommands = [SgrTrueColor Fg dullBlack]
      , boxKidsPre = []
      , boxKidsPost = [box]
      }
 where
  goHexLine i = "\n " ++ map Char.toUpper (showHex i " ")
  makeVerticalHexString n = concatMap goHexLine [0 .. n - 1]

mkRenderedRgbTileBox' :: Inspector Box
mkRenderedRgbTileBox' = do
  card <- Inspector $ State.gets $ carCursor . inspectorCards
  (x, y) <- Inspector $ State.gets inspectorHighlightedPixel
  let renderedTile = cardRenderedTileGrid card !! y !! x
  let rgb = convertImageToDebugAnsiImage $ unTcTile $ rtTileRgb renderedTile
  pure
    Box
      { boxText = rgb
      , boxClipper = const id
      , boxX = Fixed 2
      , boxY = Fixed 1
      , boxW = Absolute tileW
      , boxH = Absolute tileH
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

mkRenderedRgbTileBox :: Inspector Box
mkRenderedRgbTileBox = do
  box <- mkRenderedRgbTileBox'
  pure
    Box
      { boxText = fromString $ "R 01234567" ++ makeVerticalHexString 16
      , boxClipper = const id
      , boxX = Absolute $ platonicW + 1 * (tileW + 4)
      , boxY = Absolute 0
      , boxW = Absolute $ tileW + 3
      , boxH = Absolute $ tileH + 2
      , boxBackground = Just dullYellow
      , boxColorCommands = [SgrTrueColor Fg dullBlack]
      , boxKidsPre = []
      , boxKidsPost = [box]
      }
 where
  goHexLine i = "\n " ++ map Char.toUpper (showHex i " ")
  makeVerticalHexString n = concatMap goHexLine [0 .. n - 1]

mk4x4CharBox :: Char -> Box
mk4x4CharBox c =
  Box
    { boxText = fromString $ unlines $ replicate 4 $ replicate 4 c
    , boxClipper = const id
    , boxX = Fixed 0
    , boxY = Fixed 0
    , boxW = Absolute 4
    , boxH = Absolute 4
    , boxBackground = Nothing
    , boxColorCommands = [SgrTrueColor Fg vividWhite, SgrTrueColor Bg dullBlack]
    , boxKidsPre = []
    , boxKidsPost = []
    }

mkBordered4x4CharBox :: Int -> Int -> Char -> Box
mkBordered4x4CharBox x y c =
  Box
    { boxText = ""
    , boxClipper = const id
    , boxX = Fixed x
    , boxY = Fixed y
    , boxW = Absolute 6
    , boxH = Absolute 6
    , boxBackground = Just dullMagenta
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = [wrapper]
    }
 where
  box = mk4x4CharBox c
  wrapper =
    Box
      { boxText = ""
      , boxClipper = const id
      , boxX = Fixed 1
      , boxY = Fixed 1
      , boxW = Absolute 4
      , boxH = Absolute 4
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = [box]
      }

mk1x1CharBox :: Char -> Box
mk1x1CharBox c =
  Box
    { boxText = fromString [c]
    , boxClipper = const id
    , boxX = Fixed 0
    , boxY = Fixed 0
    , boxW = Absolute 1
    , boxH = Absolute 1
    , boxBackground = Nothing
    , boxColorCommands = [SgrTrueColor Bg dullBlack, SgrTrueColor Fg vividWhite]
    , boxKidsPre = []
    , boxKidsPost = []
    }

mkBordered1x1CharBox :: Int -> Int -> Char -> Box
mkBordered1x1CharBox x y c =
  Box
    { boxText = ""
    , boxClipper = const id
    , boxX = Fixed x
    , boxY = Fixed y
    , boxW = Absolute 3
    , boxH = Absolute 3
    , boxBackground = Just dullMagenta
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = [wrapper]
    }
 where
  box = mk1x1CharBox c
  wrapper =
    Box
      { boxText = ""
      , boxClipper = const id
      , boxX = Fixed 1
      , boxY = Fixed 1
      , boxW = Absolute 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = [box]
      }

mkTextBox :: Inspector Box
mkTextBox = do
  highlight <- Inspector $ State.gets inspectorHighlightedPixel
  card <- Inspector $ State.gets $ carCursor . inspectorCards
  (x, y) <- Inspector $ State.gets inspectorHighlightedPixel
  let renderedTile = cardRenderedTileGrid card !! y !! x
  let vc = rtVirtChar renderedTile
  let c = vcChar vc
  let qc = '\'' : c : "'"
  pure
    Box
      { boxText =
          fromString $
            unlines
              [ cardName card
              , show highlight
              , show vc
              , qc
              ]
      , boxClipper = const id
      , boxX = Absolute platonicW
      , boxY = Absolute 20
      , boxW = Absolute 20
      , boxH = Absolute 2
      , boxBackground = Just dullBlack
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost =
          [ mkBordered4x4CharBox 0 4 c
          , mkBordered1x1CharBox 5 4 c
          ]
      }

mkProtoTileBox' :: Inspector Box
mkProtoTileBox' = do
  card <- Inspector $ State.gets $ carCursor . inspectorCards
  (x, y) <- Inspector $ State.gets inspectorHighlightedPixel
  let renderedTile = cardRenderedTileGrid card !! y !! x
  let proto = convertImageToDebugAnsiImage $ rtTileProto renderedTile
  -- let rgb = convertImageToDebugAnsiImage $ unTcTile $ rtTileRgb renderedTile
  pure
    Box
      { boxText = proto
      , boxClipper = const id
      , boxX = Fixed 2
      , boxY = Fixed 1
      , boxW = Absolute tileW
      , boxH = Absolute tileH
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

mkProtoTileBox :: Inspector Box
mkProtoTileBox = do
  box <- mkProtoTileBox'
  pure
    Box
      { boxText = fromString $ "P 01234567" ++ makeVerticalHexString 16
      , boxClipper = const id
      , boxX = Absolute $ platonicW + 2 * (tileW + 4)
      , boxY = Absolute 0
      , boxW = Absolute $ tileW + 3
      , boxH = Absolute $ tileH + 2
      , boxBackground = Just dullYellow
      , boxColorCommands = [SgrTrueColor Fg dullBlack]
      , boxKidsPre = []
      , boxKidsPost = [box]
      }
 where
  goHexLine i = "\n " ++ map Char.toUpper (showHex i " ")
  makeVerticalHexString n = concatMap goHexLine [0 .. n - 1]

mkRatingsBox :: Inspector Box
mkRatingsBox = do
  card <- Inspector $ State.gets $ carCursor . inspectorCards
  (x, y) <- Inspector $ State.gets inspectorHighlightedPixel
  let fullTile = cardFullTileGrid card !! y !! x
  let renderedTile = cardRenderedTileGrid card !! y !! x
  let FgBg fg bg = rtFgBg renderedTile
  let ratedRenderings = sortOn fst $ rateAllRenderings fullTile $ TwoColors fg bg
  pure
    Box
      { boxText = fromString $ unlines $ map prettyRatedRendering ratedRenderings
      , boxClipper = const id
      , boxX = Absolute platonicW
      , boxY = Absolute 31
      , boxW = Absolute 40
      , boxH = Absolute 40
      , boxBackground = Just dullBlack
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

prettyRatedRendering :: (Double, RenderedTile) -> String
prettyRatedRendering (rating, renderedTile) =
  unwords
    [ showRgb fg
    , showRgb bg
    , (case vcInvert vc of True -> "-"; False -> "+") ++ quotC
    , quotAC
    , show rating
    ]
 where
  byteHex b = case b < 16 of
    True -> "0" ++ showHex b ""
    False -> showHex b ""
  showRgb (PixelRGB8 r g b) = byteHex r ++ byteHex g ++ byteHex b
  FgBg fg bg = rtFgBg renderedTile
  vc = rtVirtChar renderedTile
  c = vcChar vc
  quotC = '\'' : c : "'"
  ac = renderedTileToAnsi renderedTile <> toAnsiString SgrReset
  quotAC = '\'' : dropAnsi ac ++ "'" -- FIXME: don't use dropAnsi

viewportW :: Int
viewportW = platonicW + 20

viewportH :: Int
viewportH = platonicH + 10

mkViewport :: Inspector Box
mkViewport = do
  cardImage <- mkCardImageBox
  sourceTile <- mkSourceTileBox
  protoTile <- mkProtoTileBox
  renderedRgbTile <- mkRenderedRgbTileBox
  textBox <- mkTextBox
  ratings <- mkRatingsBox
  pure
    Box
      { boxText = ""
      , boxClipper = const id
      , boxX = Absolute 0
      , boxY = Absolute 0
      , boxW = Absolute viewportW
      , boxH = Absolute viewportH
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost =
          [ cardImage
          , sourceTile
          , renderedRgbTile
          , protoTile
          , textBox
          , ratings
          ]
      }

printCurrentCard :: Inspector ()
printCurrentCard = do
  prevCells <- Inspector $ State.gets inspectorPrevCellState
  viewport <- drawBox 0 0 <$> mkViewport
  let input =
        RenderInput
          { renderInput_ = ()
          , renderInput_defaultColors = (vividBlue, dullBlack)
          , renderInput_startPos = ScreenPos 0 0
          , renderInput_cursorIsShown = True
          }
  let cells = renderState_grid $ renderAnsiString input viewport
  let diffCells = pruneRenderedCells prevCells cells
  let viewport' = renderedCellsToAnsi diffCells
  Inspector $ State.modify' \st -> st{inspectorPrevCellState = cells}
  M.liftIO do
    setCursorPosition 0 0
    putStr $ ansiToString viewport'
    hFlush stdout

fixResizeArtifacts :: Inspector ()
fixResizeArtifacts = do
  refTermSize <- Inspector $ State.gets inspectorTermSize
  M.liftIO do
    prevSize <- readIORef refTermSize
    getTerminalSize >>= \case
      Nothing -> pure ()
      Just currSize -> M.when (prevSize /= currSize) do
        writeIORef refTermSize currSize
        clearScreenWithoutPaging -- resizing can leave artifacts
        hideCursor -- resizing cmd.exe window causes cursor to be shown

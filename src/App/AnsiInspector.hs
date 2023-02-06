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

import safe Ansi.Box (
  Box (..),
  ColorCommand (..),
  FixedOrRatio (..),
  clearScreenWithoutPaging,
  drawBox,
  withAnsi,
  withBuffering,
 )
import Ansi.Old (
  AnsiImage,
  platonicH,
  platonicW,
 )
import Ansi.TrueColor.Debug (convertImageToDebugAnsiImage)
import Ansi.TrueColor.FindRendering (rateAllRenderings)
import Ansi.TrueColor.ImageToAnsi (debugLoadRenderedTileGrid, renderedTileToAnsi)
import Ansi.TrueColor.RenderedTile (RenderedTile (..))
import Ansi.TrueColor.TileGrid (debugLoadTileGrid)
import Ansi.TrueColor.Types (FgBg (..), Grid, Tile, TwoColors (..), tileH, tileW, unTcTile)
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
import safe Numeric (showHex)
import Script.GenerateGallerySingle.Main (CardAnsiInfo (..), cardNameToAnsis)
import Script.MtgPureConfig (MtgPureConfig, readMtgPureConfigFile)
import safe System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  SGR (..),
  getTerminalSize,
  hideCursor,
  setCursorPosition,
  setSGRCode,
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

data GalleryState = GalleryState
  { gallery_ :: ()
  , galleryMtgPureConfig :: MtgPureConfig
  , galleryTermSize :: IORef (Int, Int)
  , galleryCards :: Carousel CardInfo
  , galleryHighlightedPixel :: (Int, Int)
  }

mkGalleryState :: IO GalleryState
mkGalleryState = do
  mtgConfig <- readMtgPureConfigFile
  refTermSize <- newIORef (-1, -1)
  pure
    GalleryState
      { gallery_ = ()
      , galleryMtgPureConfig = mtgConfig
      , galleryTermSize = refTermSize
      , galleryCards = carSingle dummyCardInfo
      , galleryHighlightedPixel = (0, 0)
      }

newtype Inspector a = Gallery
  { unGallery :: State.StateT GalleryState IO a
  }

instance Functor Inspector where
  fmap f = Gallery . fmap f . unGallery

instance Applicative Inspector where
  pure = Gallery . pure
  f <*> x = Gallery $ unGallery f <*> unGallery x

instance Monad Inspector where
  x >>= f = Gallery $ unGallery x >>= unGallery . f

instance M.MonadIO Inspector where
  liftIO = Gallery . M.liftIO

runGallery :: Inspector a -> IO a
runGallery action = withAnsi do
  state <- mkGalleryState
  runGallery' state action

runGallery' :: GalleryState -> Inspector a -> IO a
runGallery' state action = do
  State.evalStateT (unGallery action) state

main :: IO ()
main = mainAnsiImageDebugger

mainAnsiImageDebugger :: IO ()
mainAnsiImageDebugger = do
  initKeyboardMain
  hideCursor
  runGallery do
    fabricateCardAnsiImages
    runCarousel
    clearScreenWithoutPaging
    M.liftIO $ hFlush stdout

fabricateCardAnsiImages :: Inspector ()
fabricateCardAnsiImages = do
  clearScreenWithoutPaging
  M.liftIO $ hFlush stdout
  mtgConfig <- Gallery $ State.gets galleryMtgPureConfig
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
  Gallery $ State.modify' \st' -> st'{galleryCards = cards}

mkCardInfo :: CardAnsiInfo -> Grid (Tile PixelRGB8) -> Grid RenderedTile -> CardInfo
mkCardInfo ansiInfo fullTileGrid renderedGrid =
  CardInfo
    { cardName = caiCardName ansiInfo
    , cardSetName = caiSetName ansiInfo
    , cardAnsiImage = caiAnsiImage ansiInfo
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
  curr <- Gallery $ State.gets galleryHighlightedPixel
  let curr' = (fst curr + fst delta, snd curr + snd delta)
  let curr'' = clamp (0, 0) (platonicW - 1, (platonicH `div` 2) - 1) curr'
  Gallery $ State.modify' \st -> st{galleryHighlightedPixel = curr''}

clamp :: Ord a => (a, a) -> (a, a) -> (a, a) -> (a, a)
clamp (minX, minY) (maxX, maxY) (x, y) =
  ( clamp' minX maxX x
  , clamp' minY maxY y
  )
 where
  clamp' :: Ord a => a -> a -> a -> a
  clamp' min' max' x' = max min' $ min max' x'

handleKeyInput :: Inspector ()
handleKeyInput = do
  key <- M.liftIO getKey
  case key of
    KeyArrow arrow -> do
      updateHighLightedPixel arrow
    KeyChar c -> case c of
      '[' -> Gallery $ State.modify' \st -> st{galleryCards = carLeft $ galleryCards st}
      ']' -> Gallery $ State.modify' \st -> st{galleryCards = carRight $ galleryCards st}
      _ -> pure ()

mkHighLightedPixelBox :: Inspector Box
mkHighLightedPixelBox = do
  (x, y) <- Gallery $ State.gets galleryHighlightedPixel
  pure
    Box
      { boxText = "X"
      , boxClipper = const id
      , boxX = Fixed x
      , boxY = Fixed y
      , boxW = Absolute 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

mkCardImageBox :: Inspector Box
mkCardImageBox = do
  highlight <- mkHighLightedPixelBox
  card <- Gallery $ State.gets $ carCursor . galleryCards
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
  card <- Gallery $ State.gets $ carCursor . galleryCards
  (x, y) <- Gallery $ State.gets galleryHighlightedPixel
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
      { boxText = "S 01234567" ++ makeVerticalHexString 16
      , boxClipper = const id
      , boxX = Absolute platonicW
      , boxY = Absolute 0
      , boxW = Absolute $ tileW + 3
      , boxH = Absolute $ tileH + 2
      , boxBackground = Just (Dull, Yellow)
      , boxColorCommands = [SetFg (Dull, Black)]
      , boxKidsPre = []
      , boxKidsPost = [box]
      }
 where
  goHexLine i = "\n " ++ map Char.toUpper (showHex i " ")
  makeVerticalHexString n = concatMap goHexLine [0 .. n - 1]

mkRenderedRgbTileBox' :: Inspector Box
mkRenderedRgbTileBox' = do
  card <- Gallery $ State.gets $ carCursor . galleryCards
  (x, y) <- Gallery $ State.gets galleryHighlightedPixel
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
      { boxText = "R 01234567" ++ makeVerticalHexString 16
      , boxClipper = const id
      , boxX = Absolute $ platonicW + 1 * (tileW + 4)
      , boxY = Absolute 0
      , boxW = Absolute $ tileW + 3
      , boxH = Absolute $ tileH + 2
      , boxBackground = Just (Dull, Yellow)
      , boxColorCommands = [SetFg (Dull, Black)]
      , boxKidsPre = []
      , boxKidsPost = [box]
      }
 where
  goHexLine i = "\n " ++ map Char.toUpper (showHex i " ")
  makeVerticalHexString n = concatMap goHexLine [0 .. n - 1]

mk4x4CharBox :: Char -> Box
mk4x4CharBox c =
  Box
    { boxText = unlines $ replicate 4 $ replicate 4 c
    , boxClipper = const id
    , boxX = Fixed 0
    , boxY = Fixed 0
    , boxW = Absolute 4
    , boxH = Absolute 4
    , boxBackground = Nothing
    , boxColorCommands = [SetBg (Dull, Black), SetFg (Vivid, White)]
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
    , boxBackground = Just (Dull, Magenta)
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
    { boxText = [c]
    , boxClipper = const id
    , boxX = Fixed 0
    , boxY = Fixed 0
    , boxW = Absolute 1
    , boxH = Absolute 1
    , boxBackground = Nothing
    , boxColorCommands = [SetBg (Dull, Black), SetFg (Vivid, White)]
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
    , boxBackground = Just (Dull, Magenta)
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
  highlight <- Gallery $ State.gets galleryHighlightedPixel
  card <- Gallery $ State.gets $ carCursor . galleryCards
  (x, y) <- Gallery $ State.gets galleryHighlightedPixel
  let renderedTile = cardRenderedTileGrid card !! y !! x
  let vc = rtVirtChar renderedTile
  let c = vcChar vc
  let qc = '\'' : c : "'"
  pure
    Box
      { boxText =
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
      , boxBackground = Just (Dull, Black)
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost =
          [ mkBordered4x4CharBox 0 4 c
          , mkBordered1x1CharBox 5 4 c
          ]
      }

mkProtoTileBox' :: Inspector Box
mkProtoTileBox' = do
  card <- Gallery $ State.gets $ carCursor . galleryCards
  (x, y) <- Gallery $ State.gets galleryHighlightedPixel
  let renderedTile = cardRenderedTileGrid card !! y !! x
  let proto = convertImageToDebugAnsiImage $ rtTileProto renderedTile
  --let rgb = convertImageToDebugAnsiImage $ unTcTile $ rtTileRgb renderedTile
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
      { boxText = "P 01234567" ++ makeVerticalHexString 16
      , boxClipper = const id
      , boxX = Absolute $ platonicW + 2 * (tileW + 4)
      , boxY = Absolute 0
      , boxW = Absolute $ tileW + 3
      , boxH = Absolute $ tileH + 2
      , boxBackground = Just (Dull, Yellow)
      , boxColorCommands = [SetFg (Dull, Black)]
      , boxKidsPre = []
      , boxKidsPost = [box]
      }
 where
  goHexLine i = "\n " ++ map Char.toUpper (showHex i " ")
  makeVerticalHexString n = concatMap goHexLine [0 .. n - 1]

mkRatingsBox :: Inspector Box
mkRatingsBox = do
  card <- Gallery $ State.gets $ carCursor . galleryCards
  (x, y) <- Gallery $ State.gets galleryHighlightedPixel
  let fullTile = cardFullTileGrid card !! y !! x
  let renderedTile = cardRenderedTileGrid card !! y !! x
  let FgBg fg bg = rtFgBg renderedTile
  let ratedRenderings = sortOn fst $ rateAllRenderings fullTile $ TwoColors fg bg
  pure
    Box
      { boxText = unlines $ map prettyRatedRendering ratedRenderings
      , boxClipper = const id
      , boxX = Absolute platonicW
      , boxY = Absolute 31
      , boxW = Absolute 40
      , boxH = Absolute 40
      , boxBackground = Just (Dull, Black)
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
  ac = renderedTileToAnsi renderedTile ++ setSGRCode [Reset]
  quotAC = '\'' : ac ++ "'"

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
  viewport <- mkViewport
  M.liftIO do
    setCursorPosition 0 0
    drawBox viewportW viewportH viewport
    hFlush stdout

fixResizeArtifacts :: Inspector ()
fixResizeArtifacts = do
  refTermSize <- Gallery $ State.gets galleryTermSize
  M.liftIO do
    prevSize <- readIORef refTermSize
    getTerminalSize >>= \case
      Nothing -> pure ()
      Just currSize -> M.when (prevSize /= currSize) do
        writeIORef refTermSize currSize
        clearScreenWithoutPaging -- resizing can leave artifacts
        hideCursor -- resizing cmd.exe window causes cursor to be shown

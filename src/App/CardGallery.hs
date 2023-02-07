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

module App.CardGallery (
  main,
  mainAnsiGallery,
) where

import safe Ansi.AnsiString (dullBlack)
import safe Ansi.Box (
  Box (..),
  FixedOrRatio (..),
  clearScreenWithoutPaging,
  drawBoxIO,
  withAnsi,
  withBuffering,
 )
import Ansi.Old (
  AnsiImage,
  platonicH,
  platonicW,
 )
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
  carToList,
 )
import safe qualified Data.Char as Char
import safe Data.IORef (IORef, newIORef, readIORef, writeIORef)
import safe Data.List (isPrefixOf)
import safe MtgPure.AllCards (allCards)
import safe MtgPure.Model.CardName (getCardName, unCardName)
import Script.GenerateGallerySingle.Main (CardAnsiInfo (..), cardNameToAnsis)
import Script.MtgPureConfig (MtgPureConfig (mtgPure_ansiImageDatabaseDir), readMtgPureConfigFile)
import safe System.Console.ANSI (
  getTerminalSize,
  hideCursor,
  setCursorPosition,
 )
import safe qualified System.Directory as D
import safe System.IO (
  BufferMode (LineBuffering),
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

-- TODO: Magic card back image
-- TODO: It would be cool to get tapped images too. Also useful for split cards.
-- TODO: Extra cool: pre-render slight rotations of the images for card fanning.
-- TODO: Likewise for scaled cards (along with scaled rotations). This will allow
--       for smooth animations of cards being drawn from a deck.
-- TODO: To get a realistic feel, render a video in blender of a card in perspective.
--       Then export a bunch of frames to use for ansi-pre-rendering. Might need
--       better transparent pixel support for this to work well. Actually better
--       would be to add more freeform clipping to the ansi renderer. Prolly just
--       use a filter function `(Int, Int) -> Bool` to determine if a pixel is
--       arbitrarily-clipped.
cardNames :: [String]
cardNames =
  (!! 7)
    [ {-0-} map (unCardName . getCardName) allCards
    , {-1-} ["Counterspell", "Forest", "Island", "Raging Goblin"]
    , {-2-} ["Island"]
    , {-3-} ["Forest"]
    , {-4-} take 20 $ map (unCardName . getCardName) allCards
    , {-5-} takeWhile (not . ("P" `isPrefixOf`)) $ map (unCardName . getCardName) allCards
    , {-6-} ["All Is Dust"]
    , {-7-} ["Pox", "Goblin Lore"]
    ]

--------------------------------------------------------------------------------

data CardInfo = CardInfo
  { cardName :: String
  , cardSetName :: String
  , cardAnsiImage :: AnsiImage
  }

dummyCardInfo :: CardInfo
dummyCardInfo =
  CardInfo
    { cardName = "error cardName"
    , cardSetName = "error cardSetName"
    , cardAnsiImage = "error cardAnsiImage"
    }

data GalleryState = GalleryState
  { gallery_ :: ()
  , galleryMtgPureConfig :: MtgPureConfig
  , galleryTermSize :: IORef (Int, Int)
  , galleryCards :: Carousel CardInfo
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
      }

newtype Gallery a = Gallery
  { unGallery :: State.StateT GalleryState IO a
  }

instance Functor Gallery where
  fmap f = Gallery . fmap f . unGallery

instance Applicative Gallery where
  pure = Gallery . pure
  f <*> x = Gallery $ unGallery f <*> unGallery x

instance Monad Gallery where
  x >>= f = Gallery $ unGallery x >>= unGallery . f

instance M.MonadIO Gallery where
  liftIO = Gallery . M.liftIO

runGallery :: Gallery a -> IO a
runGallery action = withAnsi do
  state <- mkGalleryState
  runGallery' state action

runGallery' :: GalleryState -> Gallery a -> IO a
runGallery' state action = do
  State.evalStateT (unGallery action) state

main :: IO ()
main = mainAnsiGallery

mainAnsiGallery :: IO ()
mainAnsiGallery = do
  initKeyboardMain
  hideCursor
  runGallery do
    ansiDbDir <- Gallery $ State.gets $ mtgPure_ansiImageDatabaseDir . galleryMtgPureConfig
    M.liftIO $ D.createDirectoryIfMissing True ansiDbDir
    fabricateCardAnsiImages
    runCarousel
    clearScreenWithoutPaging
    M.liftIO $ hFlush stdout

fabricateCardAnsiImages :: Gallery ()
fabricateCardAnsiImages = do
  clearScreenWithoutPaging
  M.liftIO $ hFlush stdout
  mtgConfig <- Gallery $ State.gets galleryMtgPureConfig
  ansiInfos <- M.liftIO $ withBuffering stdout LineBuffering do
    concat <$> mapM (cardNameToAnsis True mtgConfig) cardNames
  clearScreenWithoutPaging
  M.liftIO $ hFlush stdout
  let cards = carFromList $ map mkCardInfo ansiInfos
  Gallery $ State.modify' \st' -> st'{galleryCards = cards}

mkCardInfo :: CardAnsiInfo -> CardInfo
mkCardInfo ansiInfo =
  CardInfo
    { cardName = caiCardName ansiInfo
    , cardSetName = caiSetName ansiInfo
    , cardAnsiImage = caiAnsiImage ansiInfo
    }

runCarousel :: Gallery ()
runCarousel = M.forever do
  fixResizeArtifacts
  printCurrentCard
  M.liftIO $ hFlush stdout
  spinCarousel

spinCarousel :: Gallery ()
spinCarousel = do
  key <- M.liftIO getKey
  case key of
    KeyArrow arrow -> case arrow of
      KeyLeft -> spinLeft
      KeyRight -> spinRight
      _ -> pure ()
    KeyChar c -> case Char.isAscii c && Char.isAlpha c of
      True -> spinToLetter c
      False -> pure ()

spinLeft :: Gallery ()
spinLeft = Gallery $ State.modify' \st -> st{galleryCards = carLeft $ galleryCards st}

spinRight :: Gallery ()
spinRight = Gallery $ State.modify' \st -> st{galleryCards = carRight $ galleryCards st}

spinToLetter :: Char -> Gallery ()
spinToLetter c = do
  let c' = Char.toLower c
  names <- Gallery $ State.gets $ map cardName . carToList . galleryCards
  let cs = map (Char.toLower . head) names
  case c' `elem` cs of
    False -> pure ()
    True -> spinToLetter' c'

spinToLetter' :: Char -> Gallery ()
spinToLetter' c = do
  currName <- Gallery $ State.gets $ cardName . carCursor . galleryCards
  let firstLetter = Char.toLower $ head currName
  case compare c firstLetter of
    LT -> spinLeft >> spinToLetter' c
    EQ -> pure ()
    GT -> spinRight >> spinToLetter' c

mkCardImageBox :: Gallery Box
mkCardImageBox = do
  card <- Gallery $ State.gets $ carCursor . galleryCards
  pure
    Box
      { boxText = cardAnsiImage card
      , boxClipper = const id
      , boxX = Absolute 0
      , boxY = Absolute 0
      , boxW = Absolute platonicW
      , boxH = Absolute $ platonicH `div` 2
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

mkTextBox :: Gallery Box
mkTextBox = do
  card <- Gallery $ State.gets $ carCursor . galleryCards
  pure
    Box
      { boxText = cardName card
      , boxClipper = const id
      , boxX = Absolute 4
      , boxY = Absolute $ (platonicH + 1) `div` 2
      , boxW = Absolute 20
      , boxH = Absolute 1
      , boxBackground = Just dullBlack
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

viewportW :: Int
viewportW = platonicW + 20

viewportH :: Int
viewportH = platonicH + 10

mkViewport :: Gallery Box
mkViewport = do
  cardImage <- mkCardImageBox
  textBox <- mkTextBox
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
          , textBox
          ]
      }

printCurrentCard :: Gallery ()
printCurrentCard = do
  viewport <- mkViewport
  M.liftIO do
    setCursorPosition 0 0
    drawBoxIO viewportW viewportH viewport
    hFlush stdout

fixResizeArtifacts :: Gallery ()
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

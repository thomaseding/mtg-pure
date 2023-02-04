{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module Script.GenerateGallerySingle.Main (
  main,
  CardAnsiInfo (..),
  cardNameToAnsis,
) where

import Ansi.Old (ConvertType (..), convertFileImageToAnsi, platonicH, platonicW)
import Ansi.TrueColor.Types (AnsiImage)
import Ansi.TrueColor.VirtualChar (decodeDrawingChars, encodeDrawingChar)
import safe Control.Exception (evaluate)
import safe qualified Control.Monad as M
import safe Data.Time.Clock (diffUTCTime, getCurrentTime)
import safe Data.Time.Format (defaultTimeLocale, formatTime)
import safe qualified Data.Traversable as T
import safe Script.MtgPureConfig (MtgPureConfig (mtgPure_ansiImageDatabaseDir, mtgPure_scryfallImageDatabaseDir), readMtgPureConfigFile)
import Script.ScryfallDownloader (
  CardName,
  DownloadSpecificCards (..),
  SetName,
  cardDirectoryOf,
  discoverCardSetsOf,
  downloadSpecificCards,
 )
import safe qualified System.Directory as D
import safe System.Environment (getArgs)
import safe System.Exit (exitFailure)
import safe qualified System.FilePath as D
import safe System.IO (BufferMode (..), IOMode (..), hGetContents, hPutStr, hSetBuffering, withBinaryFile)

main :: IO ()
main = mainGenerateGallerySingle

-- TODO: Use GenerateGallerySingle/Args.hs
data ProgArgs = ProgArgs
  { progArgs_ :: ()
  , progArgs_cardName :: CardName
  , progArgs_verbose :: Bool
  }

parseProgArgs :: IO ProgArgs
parseProgArgs = do
  args <- getArgs
  case args of
    [cardName] ->
      pure
        ProgArgs
          { progArgs_ = ()
          , progArgs_cardName = cardName
          , progArgs_verbose = False
          }
    _ -> do
      putStrLn "Usage: generate-gallery-single.exe <card-name>"
      exitFailure

mainGenerateGallerySingle :: IO ()
mainGenerateGallerySingle = do
  args <- parseProgArgs
  mtgConfig <- readMtgPureConfigFile
  let ansiDbDir = mtgPure_ansiImageDatabaseDir mtgConfig
  D.createDirectoryIfMissing True ansiDbDir
  M.void $ cardNameToAnsis (progArgs_verbose args) mtgConfig $ progArgs_cardName args

sizedAnsi :: FilePath
sizedAnsi = show platonicW ++ "x" ++ show platonicH ++ ".ansi"

data CardAnsiInfo = CardAnsiInfo
  { caiCardName :: CardName
  , caiSetName :: SetName
  , caiSourceImage :: FilePath
  , caiAnsiPath :: FilePath
  , caiAnsiImage :: AnsiImage
  }

cardNameToAnsis :: Bool -> MtgPureConfig -> CardName -> IO [CardAnsiInfo]
cardNameToAnsis verbose mtgConfig cardName = do
  let imageDbDir = mtgPure_scryfallImageDatabaseDir mtgConfig
  let ansiDbDir = mtgPure_ansiImageDatabaseDir mtgConfig
  let imgDir = cardDirectoryOf imageDbDir cardName Nothing
  print imgDir
  downloadSpecificCards
    DownloadSpecificCards
      { downloadSpecificCards_cardNames = [cardName]
      , downloadSpecificCards_skipCardIfAnySetExists = True
      , downloadSpecificCards_forceDownload = False
      , downloadSpecificCards_verbose = verbose
      }
  setNames <- discoverCardSetsOf imageDbDir cardName
  T.for setNames \setName -> do
    let ansiPath = cardDirectoryOf ansiDbDir cardName (Just setName) D.</> sizedAnsi
    let imgPath = cardDirectoryOf imageDbDir cardName (Just setName) D.</> "normal.jpg"
    ansi <- getAnsi ansiPath imgPath
    pure
      CardAnsiInfo
        { caiCardName = cardName
        , caiSetName = setName
        , caiSourceImage = imgPath
        , caiAnsiPath = ansiPath
        , caiAnsiImage = ansi
        }

readAnsi :: FilePath -> IO AnsiImage
readAnsi path = do
  withBinaryFile path ReadMode \h -> do
    hSetBuffering h NoBuffering
    s <- hGetContents h
    M.void $ evaluate $ length s -- I verified this is needed.
    pure s

writeAnsi :: FilePath -> AnsiImage -> IO ()
writeAnsi path ansi = do
  withBinaryFile path WriteMode \h -> do
    hSetBuffering h NoBuffering
    hPutStr h ansi

getAnsi :: FilePath -> FilePath -> IO AnsiImage
getAnsi ansiPath imgPath = do
  D.createDirectoryIfMissing True $ D.takeDirectory ansiPath
  ansiExists <- D.doesFileExist ansiPath
  print ansiPath
  if ansiExists
    then decodeDrawingChars <$> readAnsi ansiPath
    else do
      startTime <- getCurrentTime
      ansi <- convertFileImageToAnsi TrueColorDetailed imgPath
      M.void $ evaluate $ length ansi
      writeAnsi ansiPath $ concatMap encodeDrawingChar ansi
      roundtrip <- decodeDrawingChars <$> readAnsi ansiPath
      case roundtrip == ansi of
        True -> pure ()
        False -> do
          printDifference ansi roundtrip
          error "roundtrip failed"
      endTime <- getCurrentTime
      let prettyTimeDelta = formatTime defaultTimeLocale "%H:%M:%S" $ diffUTCTime endTime startTime
      putStrLn $ "Generated (" ++ prettyTimeDelta ++ ")\n  " ++ ansiPath
      pure ansi

indexOfFirstDifferentChar :: String -> String -> Int
indexOfFirstDifferentChar = go 0
 where
  go i (c1 : cs1) (c2 : cs2) = case c1 == c2 of
    True -> go (i + 1) cs1 cs2
    False -> i
  go i _ _ = i

printDifference :: String -> String -> IO ()
printDifference s1 s2 = do
  let i = indexOfFirstDifferentChar s1 s2
  let preview = show . take 10
  let preview1 = preview $ drop i s1
  let preview2 = preview $ drop i s2
  putStrLn $ "Difference at index " ++ show i
  putStrLn $ "  " ++ preview1
  putStrLn $ "  " ++ preview2

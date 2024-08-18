{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use const" #-}

-- https://scryfall.com/docs/api/bulk-data
module Script.ScryfallDownloader (
  main,
  downloadAllImages,
  DownloadSpecificCards (..),
  downloadSpecificCards,
  CardName,
  SetName,
  cardDirectoryOf,
  discoverCardSetsOf,
) where

import safe qualified Control.Monad as M
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import safe qualified Data.ByteString.Lazy as B
import safe qualified Data.Char as Char
import safe qualified Data.Foldable as F
import safe Data.List (isPrefixOf)
import safe Data.Maybe (catMaybes, isJust)
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import qualified Network.HTTP.Simple as H
import safe Numeric (readHex, showHex)
import Script.MtgPureConfig (MtgPureConfig (..), readMtgPureConfigFile)
import qualified System.Directory as D
import safe qualified System.FilePath as D

type Uri = String

type CardName = String

type SetName = String

data DownloadConfig = DownloadConfig
  { configJsonPath :: FilePath
  , configSaveDir :: FilePath
  , configSkipExisting :: Bool
  , configGetLarge :: Bool
  , configGetNormal :: Bool
  , configGetSmall :: Bool
  , configGetArtCrop :: Bool
  , configGetBorderCrop :: Bool
  , configGetBest :: Bool
  , configCardFilter :: JsonCardInfo -> Bool
  , configVerbose :: Bool
  }

main :: IO ()
main = downloadAllImages

data JsonCardInfo = Card
  { json_cardName :: CardName
  , json_cardSetName :: SetName
  , json_cardImageSmall :: Maybe Uri
  , json_cardImageNormal :: Maybe Uri
  , json_cardImageLarge :: Maybe Uri
  , json_cardImageArtCrop :: Maybe Uri
  , json_cardImageBorderCrop :: Maybe Uri
  }
  deriving (Show)

instance A.FromJSON JsonCardInfo where
  parseJSON :: A.Value -> A.Parser JsonCardInfo
  parseJSON = \case
    A.Object o -> do
      name <- o .: "name"
      setName <- o .: "set_name"
      imageUris <- o .:? "image_uris"
      small <- case imageUris of
        Just imgs -> imgs .:? "small"
        Nothing -> pure Nothing
      normal <- case imageUris of
        Just imgs -> imgs .:? "normal"
        Nothing -> pure Nothing
      large <- case imageUris of
        Just imgs -> imgs .:? "large"
        Nothing -> pure Nothing
      artCrop <- case imageUris of
        Just imgs -> imgs .:? "art_crop"
        Nothing -> pure Nothing
      borderCrop <- case imageUris of
        Just imgs -> imgs .:? "border_crop"
        Nothing -> pure Nothing
      pure
        Card
          { json_cardName = name
          , json_cardSetName = setName
          , json_cardImageSmall = small
          , json_cardImageNormal = normal
          , json_cardImageLarge = large
          , json_cardImageArtCrop = artCrop
          , json_cardImageBorderCrop = borderCrop
          }
    _ -> fail "Expected an object"

extractCardsM :: [A.Value] -> A.Parser [JsonCardInfo]
extractCardsM vs =
  catMaybes <$> T.for vs \case
    v@(A.Object o) -> do
      object <- o .: "object"
      case object of
        ("card" :: String) -> Just <$> A.parseJSON v
        _ -> pure Nothing
    _ -> pure Nothing

extractCards :: [A.Value] -> [JsonCardInfo]
extractCards vs = case A.parseEither extractCardsM vs of
  Left e -> error e
  Right cards -> cards

data GetSize
  = GetLarge
  | GetNormal
  | GetSmall
  | GetArtCrop
  | GetBorderCrop
  | GetBest

getBest :: JsonCardInfo -> Maybe GetSize
getBest card
  | isJust $ json_cardImageLarge card = Just GetLarge
  | isJust $ json_cardImageNormal card = Just GetNormal
  | isJust $ json_cardImageSmall card = Just GetSmall
  | isJust $ json_cardImageArtCrop card = Just GetArtCrop
  | isJust $ json_cardImageBorderCrop card = Just GetBorderCrop
  | otherwise = Nothing

checkSkip :: DownloadConfig -> FilePath -> IO Bool
checkSkip config savePath = case configSkipExisting config of
  True -> D.doesFileExist savePath
  False -> pure False

getSavePath :: GetSize -> FilePath -> FilePath
getSavePath size savePathBase =
  savePathBase ++ case size of
    GetLarge -> "/large.jpg"
    GetNormal -> "/normal.jpg"
    GetSmall -> "/small.jpg"
    GetArtCrop -> "/art.jpg"
    GetBorderCrop -> "/crop.jpg"
    GetBest -> error "GetBest should have been handled above"

paddedLabel :: Int -> String -> String -> String
paddedLabel width label value = label' ++ padding ++ value
 where
  padding = replicate (width - length label') ' '
  label' = label ++ ":"

labelWidth :: Int
labelWidth = 12

downloadSize :: DownloadConfig -> GetSize -> JsonCardInfo -> IO ()
downloadSize config size card = do
  let verbose = configVerbose config
  let savePathBase = cardDirectoryOf (configSaveDir config) (json_cardName card) (Just $ json_cardSetName card)
  let size' = case size of
        GetBest -> case getBest card of
          Nothing -> error $ "No image found for card: " ++ json_cardName card ++ " (" ++ json_cardSetName card ++ ")"
          Just s -> s
        _ -> size
  let savePath = getSavePath size' savePathBase
  let isSpecialACard = "A-" `isPrefixOf` json_cardName card -- These appear to be redundant promo cards.
  let isStupid = isSpecialACard
  let mUri = case size' of
        GetLarge -> json_cardImageLarge card
        GetNormal -> json_cardImageNormal card
        GetSmall -> json_cardImageSmall card
        GetArtCrop -> json_cardImageArtCrop card
        GetBorderCrop -> json_cardImageBorderCrop card
        GetBest -> error "GetBest should have been handled above"
  let allowedByConfig =
        configCardFilter config card && case size' of
          GetLarge -> configGetLarge config
          GetNormal -> configGetNormal config
          GetSmall -> configGetSmall config
          GetArtCrop -> configGetArtCrop config
          GetBorderCrop -> configGetBorderCrop config
          GetBest -> configGetBest config
  case (isStupid, allowedByConfig, mUri) of
    (True, _, _) -> pure ()
    (_, False, _) -> pure ()
    (_, _, Nothing) -> pure ()
    (False, True, Just uri) -> do
      skip <- do
        D.createDirectoryIfMissing True savePathBase
        checkSkip config savePath
      case skip of
        True -> do
          M.when verbose do
            putStrLn $ paddedLabel labelWidth "Skipping" savePath
        False -> do
          M.when verbose do
            putStrLn $ paddedLabel labelWidth "Downloading" uri
          request <- H.parseRequest uri
          response <- H.httpLBS request
          B.writeFile savePath (H.getResponseBody response)
          M.when verbose do
            putStrLn $ paddedLabel labelWidth "Downloaded" savePath

downloadImages :: DownloadConfig -> IO ()
downloadImages config = do
  -- Lazy byte string because JSON file is large
  jsonData <- B.readFile $ configJsonPath config
  -- TODO: `checkSkip` should be done here in addition to in `downloadSize` in
  -- case all the images are skipped. Then we can skip reading the JSON file.
  -- This requires the config to be passed an optional list of cards to download.
  -- In this case, best to remove the `configCardFilter` field for uniformity.
  case A.eitherDecode jsonData :: Either String [A.Value] of
    Left err -> putStrLn err
    Right vs -> do
      let saveDir = configSaveDir config
      D.createDirectoryIfMissing True saveDir
      let cards = extractCards vs
      F.for_ cards \card -> do
        downloadSize config GetLarge card
        downloadSize config GetNormal card
        downloadSize config GetSmall card
        downloadSize config GetArtCrop card
        downloadSize config GetBorderCrop card
        downloadSize config GetBest card

-- Use cases:
--  * for avoiding filesystem issues with special characters
--  * for avoiding special characters that shells use
--  * avoid "." file extension issues
--  * avoid "-" for command line option parsing issues
-- Encoding is non-lossy.
encodeString :: String -> String
encodeString = concatMap \c -> case Char.isAscii c && Char.isAlphaNum c of
  True -> [c]
  False -> case c of
    ' ' -> "__"
    _ -> "_" ++ map Char.toUpper (showHex (Char.ord c) "") ++ "x"

decodeString :: String -> String
decodeString = go
 where
  go = \case
    "" -> ""
    '_' : '_' : s -> ' ' : go s
    '_' : s -> case span Char.isHexDigit s of
      (hex, 'x' : s') -> case readHex hex of
        [(code, "")] -> Char.chr code : go s'
        _ -> error "impossible"
      _ -> error $ "Invalid encoding: " ++ "_" ++ s
    c : s -> c : go s

getBucketDir :: String -> FilePath
getBucketDir name = ab ++ "/" ++ cd
 where
  encodedName = encodeString name
  ab = case map Char.toUpper $ take 2 encodedName of
    "" -> "-"
    s -> s
  cd = case map Char.toUpper $ take 2 $ drop 2 encodedName of
    "" -> "-"
    s -> s

downloadAllImages :: IO ()
downloadAllImages = do
  mtgConfig <- readMtgPureConfigFile
  downloadImages
    (protoConfig mtgConfig)
      { configCardFilter = \_ -> True
      }

data DownloadSpecificCards = DownloadSpecificCards
  { downloadSpecificCards_cardNames :: [CardName]
  , downloadSpecificCards_skipCardIfAnySetExists :: Bool
  , downloadSpecificCards_forceDownload :: Bool
  , downloadSpecificCards_verbose :: Bool
  }
  deriving ()

downloadSpecificCards :: DownloadSpecificCards -> IO ()
downloadSpecificCards config = do
  mtgConfig <- readMtgPureConfigFile
  let imgDbDir = mtgPure_scryfallImageDatabaseDir mtgConfig
  let names = downloadSpecificCards_cardNames config
  names' <- case downloadSpecificCards_forceDownload config of
    True -> pure names
    False -> case downloadSpecificCards_skipCardIfAnySetExists config of
      False -> pure names
      True -> flip M.filterM names \name -> do
        cardSets <- discoverCardSetsOf imgDbDir name
        pure $ null cardSets
  let names'' = Set.fromList names'
  downloadImages
    (protoConfig mtgConfig)
      { configCardFilter = \card -> Set.member (json_cardName card) names''
      , configVerbose = downloadSpecificCards_verbose config
      }

protoConfig :: MtgPureConfig -> DownloadConfig
protoConfig mtgConfig =
  DownloadConfig
    { configJsonPath = mtgPure_scryfallUniqueArtworkJson mtgConfig
    , configSaveDir = mtgPure_scryfallImageDatabaseDir mtgConfig
    , configSkipExisting = True
    , configGetLarge = False
    , configGetNormal = True
    , configGetSmall = False
    , configGetArtCrop = False
    , configGetBorderCrop = False
    , configGetBest = False
    , configCardFilter = \_ -> False
    , configVerbose = False
    }

cardDirectoryOf :: FilePath -> CardName -> Maybe SetName -> FilePath
cardDirectoryOf baseDir cardName = \case
  Nothing -> baseDir ++ "/" ++ getBucketDir name ++ "/" ++ name
  Just setName -> cardDirectoryOf baseDir cardName Nothing ++ "/" ++ encodeString setName
 where
  name = encodeString cardName

discoverCardSetsOf :: FilePath -> CardName -> IO [SetName]
discoverCardSetsOf baseDir cardName = do
  let dir = cardDirectoryOf baseDir cardName Nothing
  D.doesDirectoryExist dir >>= \case
    False -> pure []
    True -> do
      names <- D.listDirectory dir
      names' <- M.filterM (D.doesDirectoryExist . (dir D.</>)) names
      pure $ map decodeString names'

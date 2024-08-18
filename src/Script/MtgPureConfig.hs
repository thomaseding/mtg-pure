{-# LANGUAGE Unsafe #-}

module Script.MtgPureConfig (
  MtgPureConfig (..),
  readMtgPureConfigFile,
  findMtgPureDir,
) where

import safe Data.Functor ((<&>))
import safe qualified Data.Map.Strict as Map
import safe Data.SafeJson (
  FromSafeJson (..),
  SafeValue (..),
  ToSafeJson (..),
  fetch,
  readSafeJsonFile,
  store,
 )
import System.Directory (
  doesDirectoryExist,
  getAppUserDataDirectory,
  getHomeDirectory,
 )
import safe System.Environment (lookupEnv)
import safe System.FilePath ((</>))

data MtgPureConfig = MtgPureConfig
  { mtgPure_scryfallUniqueArtworkJson :: FilePath
  , mtgPure_scryfallImageDatabaseDir :: FilePath
  , mtgPure_scryfallScanForMissingImagesAndDownload :: Bool
  , mtgPure_ansiImageDatabaseDir :: FilePath
  , mtgPure_ansiImageConversionMaxSpawnedProcesses :: Int
  }
  deriving (Show)

instance ToSafeJson MtgPureConfig where
  toSafeJson :: MtgPureConfig -> SafeValue
  toSafeJson
    MtgPureConfig
      { mtgPure_scryfallUniqueArtworkJson = scryfallUniqueArtworkJson
      , mtgPure_scryfallImageDatabaseDir = scryfallImageDatabaseDir
      , mtgPure_scryfallScanForMissingImagesAndDownload = scryfallScanForMissingImagesAndDownload
      , mtgPure_ansiImageDatabaseDir = ansiImageDatabaseDir
      , mtgPure_ansiImageConversionMaxSpawnedProcesses = ansiImageConversionMaxSpawnedProcesses
      } =
      SafeObject $
        Map.fromList
          [ store "scryfallUniqueArtworkJson" scryfallUniqueArtworkJson
          , store "scryfallImageDatabaseDir" scryfallImageDatabaseDir
          , store "scryfallScanForMissingImagesAndDownload" scryfallScanForMissingImagesAndDownload
          , store "ansiImageDatabaseDir" ansiImageDatabaseDir
          , store "ansiImageConversionMaxSpawnedProcesses" ansiImageConversionMaxSpawnedProcesses
          ]

instance FromSafeJson MtgPureConfig where
  fromSafeJson :: SafeValue -> Either String MtgPureConfig
  fromSafeJson = \case
    SafeObject obj -> do
      scryfallUniqueArtworkJson <- fetch obj "scryfallUniqueArtworkJson"
      scryfallImageDatabaseDir <- fetch obj "scryfallImageDatabaseDir"
      scryfallScanForMissingImagesAndDownload <- fetch obj "scryfallScanForMissingImagesAndDownload"
      ansiImageDatabaseDir <- fetch obj "ansiImageDatabaseDir"
      ansiImageConversionMaxSpawnedProcesses <- fetch obj "ansiImageConversionMaxSpawnedProcesses"
      pure
        MtgPureConfig
          { mtgPure_scryfallUniqueArtworkJson = scryfallUniqueArtworkJson
          , mtgPure_scryfallImageDatabaseDir = scryfallImageDatabaseDir
          , mtgPure_scryfallScanForMissingImagesAndDownload = scryfallScanForMissingImagesAndDownload
          , mtgPure_ansiImageDatabaseDir = ansiImageDatabaseDir
          , mtgPure_ansiImageConversionMaxSpawnedProcesses = ansiImageConversionMaxSpawnedProcesses
          }
    _ -> Left "Expected SafeObject"

readMtgPureConfigFile :: IO MtgPureConfig
readMtgPureConfigFile = do
  mtgPureDir <- findMtgPureDir
  let filePath = mtgPureDir </> "mtg-pure.jsonc"
  readSafeJsonFile filePath >>= \case
    Left err -> fail err
    Right config -> pure config

findMtgPureDir :: IO FilePath
findMtgPureDir = do
  lookupEnv "MTG_PURE_DIR" >>= \case
    Just envDir -> pure envDir
    Nothing -> do
      appProjDir <- getAppUserDataDirectory proj
      doesDirectoryExist appProjDir >>= \case
        True -> pure appProjDir
        False -> do
          homeDir <- getHomeDirectory
          let homeProjDir = homeDir </> "." <> proj
          doesDirectoryExist homeProjDir <&> \case
            True -> homeProjDir
            False -> "."
 where
  proj = "mtg-pure"

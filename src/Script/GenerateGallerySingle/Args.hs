{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module Script.GenerateGallerySingle.Args (
  Args (..),
  ParseError (..),
  parseArgs,
  helpString,
) where

import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Usage
-- main.exe [ARGS]
--
-- --help                               Show this help text and exit.
-- --silent                             Don't print anything to stdout.
-- --input-image-path PATH              Path to card image. Required.
-- --output-ansi-path PATH              Path to card ansi. Required.
-- --ansi-width INT                     Width of ansi in characters. Required.
-- --ansi-height INT                    Height of ansi in characters. Required.

helpString :: String
helpString =
  unlines
    [ "Usage: generate-gallery-single.exe [ARGS]"
    , ""
    , "--help                               Show this help text and exit."
    , "--silent                             Don't print anything to stdout."
    , "--input-image-path PATH              Path to card image. Required."
    , "--output-ansi-path PATH              Path to card ansi. Required."
    , "--ansi-width INT                     Width of ansi in characters. Required."
    , "--ansi-height INT                    Height of ansi in characters. Required."
    ]

data Args = Args
  { args_ :: ()
  , args_help :: Maybe String
  , args_silent :: Bool
  , args_inputImagePath :: FilePath
  , args_outputAnsiPath :: FilePath
  , args_ansiWidth :: Int
  , args_ansiHeight :: Int
  }

data ParseError = ParseError
  { parseError_ :: ()
  , parseError_help :: String
  , parseError_message :: String
  }

emptyArgs :: Args
emptyArgs =
  Args
    { args_ = ()
    , args_help = Nothing
    , args_silent = False
    , args_inputImagePath = ""
    , args_outputAnsiPath = ""
    , args_ansiWidth = 0
    , args_ansiHeight = 0
    }

err :: String -> Either ParseError Args
err message =
  Left
    ParseError
      { parseError_ = ()
      , parseError_help = helpString
      , parseError_message = message
      }

parseArgs :: [String] -> Either ParseError Args
parseArgs input = do
  output <- go emptyArgs input
  validateArgs output
 where
  go :: Args -> [String] -> Either ParseError Args
  go curr = \case
    [] -> pure curr
    "--help" : _ -> pure curr{args_help = Just helpString}
    "--silent" : rest -> go curr{args_silent = True} rest
    "--input-image-path" : path : rest -> go curr{args_inputImagePath = path} rest
    "--output-ansi-path" : path : rest -> go curr{args_outputAnsiPath = path} rest
    "--ansi-width" : (readMaybe -> Just width) : rest -> go curr{args_ansiWidth = width} rest
    "--ansi-height" : (readMaybe -> Just height) : rest -> go curr{args_ansiHeight = height} rest
    arg : _ -> err $ "Unknown argument: " ++ arg

validateArgs :: Args -> Either ParseError Args
validateArgs args
  | args_help args == Just helpString = pure args
  | args_inputImagePath args == "" = err "Invalid --input-image-path"
  | args_outputAnsiPath args == "" = err "Invalid --output-ansi-path"
  | args_ansiWidth args <= 0 = err "Invalid --ansi-width"
  | args_ansiHeight args <= 0 = err "Invalid --ansi-height"
  | otherwise = pure args

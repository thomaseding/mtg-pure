{-# LANGUAGE Safe #-}
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

module System.Keyboard (
  getKey,
  initKeyboardMain,
  ArrowKey (..),
  Key (..),
) where

import safe Control.Exception (AsyncException (UserInterrupt), throwIO)
import safe System.IO (BufferMode (..), hSetBuffering, stdin)
import safe System.IO.NoBufferingWorkaround (
  getCharNoBuffering,
  initGetCharNoBuffering,
 )

data ArrowKey
  = KeyUp
  | KeyDown
  | KeyRight
  | KeyLeft
  deriving (Eq, Ord, Show)

data Key
  = KeyChar Char
  | KeyArrow ArrowKey
  deriving (Eq, Ord, Show)

getCh :: IO Char
getCh = getCharNoBuffering

getKey :: IO Key
getKey =
  getCh >>= \case
    '\ETX' -> throwIO UserInterrupt -- Ctrl-C
    '\ESC' ->
      fmap esc $
        getCh >>= \case
          '[' ->
            getCh >>= \case
              'A' -> pure $ Just $ KeyArrow KeyUp
              'B' -> pure $ Just $ KeyArrow KeyDown
              'C' -> pure $ Just $ KeyArrow KeyRight
              'D' -> pure $ Just $ KeyArrow KeyLeft
              _ -> pure Nothing
          _ -> pure Nothing
    '\224' ->
      getCh >>= \case
        'H' -> pure $ KeyArrow KeyUp
        'P' -> pure $ KeyArrow KeyDown
        'M' -> pure $ KeyArrow KeyRight
        'K' -> pure $ KeyArrow KeyLeft
        _ -> pure $ KeyChar '\224'
    c -> pure $ KeyChar c
 where
  esc = \case
    Nothing -> KeyChar '\ESC'
    Just key -> key

initKeyboardMain :: IO ()
initKeyboardMain = do
  if False
    then hSetBuffering stdin NoBuffering
    else initGetCharNoBuffering

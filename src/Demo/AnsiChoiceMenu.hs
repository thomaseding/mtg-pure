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

module Demo.AnsiChoiceMenu (
  main,
) where

import safe qualified Control.Monad as M
import safe qualified Control.Monad.Trans as M
import safe qualified Control.Monad.Trans.State as State
import safe System.Console.ANSI (
  Color (Yellow),
  ColorIntensity (Vivid),
  ConsoleLayer (Foreground),
  SGR (Reset, SetColor),
  clearScreen,
  setCursorPosition,
  setSGR,
 )
import safe System.Keyboard (ArrowKey (..), Key (..), getKey, initKeyboardMain)

data St = St
  { st_ :: ()
  , stCursor :: Int
  , stOptions :: [String]
  }

initialSt :: St
initialSt =
  St
    { st_ = ()
    , stCursor = 0
    , stOptions = ["Option 1", "Option 2", "Option 3", "Option 4", "Option 5"]
    }

type Choose = State.StateT St IO

moveCursorUp :: Choose ()
moveCursorUp = do
  pos <- State.gets stCursor
  let pos' = max 0 $ pos - 1
  State.modify' \st -> st{stCursor = pos'}

moveCursorDown :: Choose ()
moveCursorDown = do
  pos <- State.gets stCursor
  options <- State.gets stOptions
  let pos' = min (pos + 1) $ length options - 1
  State.modify' \st -> st{stCursor = pos'}

drawChoiceBox :: Choose ()
drawChoiceBox = do
  pos <- State.gets stCursor
  options <- State.gets stOptions
  M.liftIO do
    clearScreen
    setCursorPosition 0 0
    M.forM_
      (zip [0 ..] options)
      \(i, opt) -> do
        if i == pos
          then setSGR [SetColor Foreground Vivid Yellow] >> putStr opt >> setSGR [Reset]
          else putStr opt
        putStrLn ""
    setCursorPosition (pos + 1) 0

loop :: Choose ()
loop = M.forever $ do
  drawChoiceBox
  key <- M.liftIO getKey
  case key of
    KeyArrow KeyUp -> moveCursorUp
    KeyArrow KeyDown -> moveCursorDown
    _ -> pure ()

main :: IO ()
main = do
  initKeyboardMain
  State.evalStateT loop initialSt

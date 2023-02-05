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

module Test.Game.RagingGoblin (
  main,
  mainRagingGoblin,
) where

import safe MtgPure.Cards (mountain, ragingGoblin)
import safe MtgPure.Client.Terminal (TerminalInput (..), fwdImpl, playTerminalGame, runTerminal)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainRagingGoblin

-- NOTE: Still a WIP
mainRagingGoblin :: IO ()
mainRagingGoblin = runTerminal input do
  playTerminalGame $ replicate 2 (deck, side)
 where
  input =
    TerminalInput
      { terminalInput_ = ()
      , terminalInput_fwd = fwdImpl
      , terminalInput_replayInputs = replayInputs
      , terminalInput_replayLog = replayLog
      }

deck :: Deck
deck =
  Deck $
    concat $
      replicate
        (if True then 1 else 30)
        [ AnyCard1 mountain
        , AnyCard1 ragingGoblin
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate (if True then 1 else 7) $ AnyCard1 mountain
      , replicate (if True then 1 else 8) $ AnyCard1 ragingGoblin
      ]

replayLog :: Maybe FilePath
replayLog = Nothing -- Just "replay-RagingGoblin.log"

replayInputs :: [String]
replayInputs = []

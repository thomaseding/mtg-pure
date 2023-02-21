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

module Demo.MtgPure.Gameplay (
  main,
  mainDemoGameplay,
) where

import safe MtgPure.Cards (
  bayou,
  blackLotus,
  mountain,
  mouthOfRonom,
  moxRuby,
  plains,
  ragingGoblin,
  shock,
  snowCoveredMountain,
  stoneRain,
  stripMine,
  wasteland,
 )
import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (
  TerminalInput (..),
  runTerminal,
 )
import safe MtgPure.Client.Terminal.PriorityAction (
  playTerminalGame,
 )
import safe MtgPure.Engine.State (noGameCheats)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))

main :: IO ()
main = mainDemoGameplay

-- NOTE: Still a WIP
mainDemoGameplay :: IO ()
mainDemoGameplay = runTerminal input do
  playTerminalGame noGameCheats [(deck1, side), (deck2, side)]
 where
  input =
    TerminalInput
      { terminalInput_ = ()
      , terminalInput_fwd = fwdImpl
      , terminalInput_replayInputs = replayInputs
      , terminalInput_replayLog = replayLog
      }

deck1 :: Deck
deck1 =
  Deck $
    concat $
      replicate
        4
        [ AnyCard1 mountain
        , AnyCard1 snowCoveredMountain
        , AnyCard1 bayou
        , AnyCard1 stripMine
        , AnyCard1 wasteland
        , AnyCard1 ragingGoblin
        , AnyCard1 blackLotus
        , AnyCard1 moxRuby
        , AnyCard1 stoneRain
        , AnyCard1 mouthOfRonom
        , AnyCard1 shock
        ]

deck2 :: Deck
deck2 = Deck $ replicate (length $ unDeck deck1) $ AnyCard1 plains

side :: Sideboard
side =
  Sideboard $
    concat
      []

replayLog :: Maybe FilePath
replayLog = Nothing -- Just "replay-DemoGameplay.log"

replayInputs :: [String]
replayInputs = []

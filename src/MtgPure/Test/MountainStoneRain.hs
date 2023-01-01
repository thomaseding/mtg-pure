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

module MtgPure.Test.MountainStoneRain (
  main,
  mainMountainStoneRain,
) where

import safe MtgPure.Cards (mountain, stoneRain)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Test.Demo (runDemo)

main :: IO ()
main = mainMountainStoneRain

-- NOTE: Still a WIP
-- FIXME: currently only R gets removed from mana pool instead of RRR
mainMountainStoneRain :: IO ()
mainMountainStoneRain = runDemo replayLog replayInputs $ replicate 2 (deck, side)

deck :: Deck
deck =
  Deck $
    concat $
      replicate
        (if False then 1 else 30)
        [ AnyCard mountain
        , AnyCard stoneRain
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate (if True then 1 else 7) $ AnyCard mountain
      , replicate (if True then 1 else 8) $ AnyCard stoneRain
      ]

replayLog :: Maybe FilePath
replayLog = Nothing -- Just "replay-Mountain-StoneRain.log"

replayInputs :: [String]
replayInputs = []

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

module MtgPure.Test.MountainShock (
  main,
  mainMountainShock,
) where

import safe MtgPure.Cards (mountain, shock)
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Recursive (AnyCard (..))
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Test.Demo (runDemo)

main :: IO ()
main = mainMountainShock

-- NOTE: Still a WIP
mainMountainShock :: IO ()
mainMountainShock = runDemo replayInputs $ replicate 2 (deck, side)

deck :: Deck
deck =
  Deck $
    concat $
      replicate
        (if True then 1 else 30)
        [ AnyCard mountain
        , AnyCard shock
        ]

side :: Sideboard
side =
  Sideboard $
    concat
      [ replicate (if True then 1 else 7) $ AnyCard mountain
      , replicate (if True then 1 else 8) $ AnyCard shock
      ]

replayInputs :: [String]
replayInputs =
  []
    ++ replicate 10 "0"
    ++ ["8", "0", "11 0"]
    ++ replicate 4 "0"
    ++ ["7"]

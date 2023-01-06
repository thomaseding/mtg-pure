{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.AllCards (
  allCards,
  allTokens,
) where

import safe MtgPure.Cards (
  acceptableLosses,
  allIsDust,
  ancestralVision,
  backlash,
  bayou,
  birdToken,
  blaze,
  bloodMoon,
  cityOfBrass,
  cleanse,
  conversion,
  damnation,
  fling,
  forest,
  holyStrength,
  island,
  lavaAxe,
  manaLeak,
  mountain,
  nyxbornRollicker,
  ornithopter,
  plains,
  plummet,
  pollutedDelta,
  pradeshGypsies,
  ragingGoblin,
  shock,
  sinkhole,
  snuffOut,
  soldierToken,
  stifle,
  stoneRain,
  stoneThrowingDevils,
  swamp,
  swanSong,
  vindicate,
  wastes,
  wear_tear,
  wrathOfGod,
 )
import safe MtgPure.Model.Recursive (AnyCard, AnyToken)
import safe MtgPure.ModelCombinators (ToCard (..), ToToken (..))

allCards :: [AnyCard]
allCards =
  [ toCard acceptableLosses
  , toCard allIsDust
  , toCard ancestralVision
  , toCard backlash
  , toCard bayou
  , toCard blaze
  , toCard bloodMoon
  , toCard cityOfBrass
  , toCard cleanse
  , toCard conversion
  , toCard damnation
  , toCard fling
  , toCard forest
  , toCard holyStrength
  , toCard island
  , toCard lavaAxe
  , toCard manaLeak
  , toCard mountain
  , toCard nyxbornRollicker
  , toCard ornithopter
  , toCard ragingGoblin
  , toCard plains
  , toCard plummet
  , toCard pollutedDelta
  , toCard pradeshGypsies
  , toCard shock
  , toCard sinkhole
  , toCard snuffOut
  , toCard stifle
  , toCard stoneRain
  , toCard stoneThrowingDevils
  , toCard swamp
  , toCard swanSong
  , toCard vindicate
  , toCard wastes
  , toCard wear_tear
  , toCard wrathOfGod
  ]

allTokens :: [AnyToken]
allTokens = [toToken birdToken, toToken soldierToken]

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
  ancestralRecall,
  ancestralVision,
  backlash,
  bayou,
  birdToken,
  blaze,
  bloodMoon,
  cityOfBrass,
  cleanse,
  conversion,
  counterspell,
  damnation,
  darkRitual,
  divination,
  fling,
  forest,
  grizzlyBears,
  holyStrength,
  island,
  lavaAxe,
  lightningBolt,
  llanowarElves,
  manaLeak,
  mountain,
  moxEmerald,
  moxJet,
  moxPearl,
  moxRuby,
  moxSapphire,
  nyxbornRollicker,
  ornithopter,
  plains,
  plummet,
  pollutedDelta,
  pradeshGypsies,
  ragingGoblin,
  shatter,
  shock,
  sinkhole,
  snuffOut,
  soldierToken,
  stifle,
  stoneRain,
  stoneThrowingDevils,
  swamp,
  swanSong,
  unholyStrength,
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
  , toCard ancestralRecall
  , toCard ancestralVision
  , toCard backlash
  , toCard bayou
  , toCard blaze
  , toCard bloodMoon
  , toCard cityOfBrass
  , toCard cleanse
  , toCard conversion
  , toCard counterspell
  , toCard damnation
  , toCard darkRitual
  , toCard divination
  , toCard fling
  , toCard forest
  , toCard grizzlyBears
  , toCard holyStrength
  , toCard island
  , toCard lavaAxe
  , toCard lightningBolt
  , toCard llanowarElves
  , toCard manaLeak
  , toCard mountain
  , toCard moxEmerald
  , toCard moxJet
  , toCard moxPearl
  , toCard moxRuby
  , toCard moxSapphire
  , toCard nyxbornRollicker
  , toCard ornithopter
  , toCard ragingGoblin
  , toCard plains
  , toCard plummet
  , toCard pollutedDelta
  , toCard pradeshGypsies
  , toCard shatter
  , toCard shock
  , toCard sinkhole
  , toCard snuffOut
  , toCard stifle
  , toCard stoneRain
  , toCard stoneThrowingDevils
  , toCard swamp
  , toCard swanSong
  , toCard unholyStrength
  , toCard vindicate
  , toCard wastes
  , toCard wear_tear
  , toCard wrathOfGod
  ]

allTokens :: [AnyToken]
allTokens = [toToken birdToken, toToken soldierToken]

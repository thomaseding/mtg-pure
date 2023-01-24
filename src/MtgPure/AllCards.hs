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
  alpineMeadow,
  ancestralRecall,
  ancestralVision,
  arcticFlats,
  arcticTreeline,
  backlash,
  bayou,
  birdToken,
  blackLotus,
  blaze,
  bloodMoon,
  borealDruid,
  borealShelf,
  braidwoodCup,
  cityOfBrass,
  cleanse,
  conversion,
  counterspell,
  damnation,
  darkRitual,
  deathriteShaman,
  divination,
  fling,
  forest,
  frostMarsh,
  glacialFloodplain,
  grizzlyBears,
  gutlessGhoul,
  highlandForest,
  highlandWeald,
  holyStrength,
  iceTunnel,
  icehideGolem,
  island,
  lavaAxe,
  lightningBolt,
  llanowarElves,
  manaLeak,
  mountain,
  mouthOfRonom,
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
  rimewoodFalls,
  shatter,
  shock,
  sinkhole,
  snowCoveredForest,
  snowCoveredIsland,
  snowCoveredMountain,
  snowCoveredPlains,
  snowCoveredSwamp,
  snowfieldSinkhole,
  snuffOut,
  soldierToken,
  squallDrifter,
  stifle,
  stoneRain,
  stoneThrowingDevils,
  sulfurousMire,
  swamp,
  swanSong,
  thermopod,
  tresserhornSinks,
  unholyStrength,
  vindicate,
  volatileFjord,
  wastes,
  wear_tear,
  witchEngine,
  woodlandChasm,
  wrathOfGod,
 )
import safe MtgPure.Model.Combinators (ToCard (..), ToToken (..))
import safe MtgPure.Model.Recursive (AnyCard, AnyToken)

allCards :: [AnyCard]
allCards =
  [ toCard acceptableLosses
  , toCard allIsDust
  , toCard alpineMeadow
  , toCard ancestralRecall
  , toCard ancestralVision
  , toCard arcticFlats
  , toCard arcticTreeline
  , toCard backlash
  , toCard bayou
  , toCard blackLotus
  , toCard blaze
  , toCard bloodMoon
  , toCard borealDruid
  , toCard borealShelf
  , toCard braidwoodCup
  , toCard cityOfBrass
  , toCard cleanse
  , toCard conversion
  , toCard counterspell
  , toCard damnation
  , toCard darkRitual
  , toCard deathriteShaman
  , toCard divination
  , toCard fling
  , toCard forest
  , toCard frostMarsh
  , toCard glacialFloodplain
  , toCard grizzlyBears
  , toCard gutlessGhoul
  , toCard highlandForest
  , toCard highlandWeald
  , toCard icehideGolem
  , toCard iceTunnel
  , toCard holyStrength
  , toCard island
  , toCard lavaAxe
  , toCard lightningBolt
  , toCard llanowarElves
  , toCard manaLeak
  , toCard mouthOfRonom
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
  , toCard rimewoodFalls
  , toCard shatter
  , toCard shock
  , toCard sinkhole
  , toCard snowCoveredForest
  , toCard snowCoveredIsland
  , toCard snowCoveredMountain
  , toCard snowCoveredPlains
  , toCard snowCoveredSwamp
  , toCard snowfieldSinkhole
  , toCard snuffOut
  , toCard squallDrifter
  , toCard stifle
  , toCard stoneRain
  , toCard stoneThrowingDevils
  , toCard sulfurousMire
  , toCard swamp
  , toCard swanSong
  , toCard thermopod
  , toCard tresserhornSinks
  , toCard unholyStrength
  , toCard vindicate
  , toCard volatileFjord
  , toCard wastes
  , toCard wear_tear
  , toCard witchEngine
  , toCard woodlandChasm
  , toCard wrathOfGod
  ]

allTokens :: [AnyToken]
allTokens = [toToken birdToken, toToken soldierToken]

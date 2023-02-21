{-# LANGUAGE Safe #-}
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
  birdsOfParadise,
  blackLotus,
  blaze,
  bloodMoon,
  borealDruid,
  borealShelf,
  braidwoodCup,
  cityOfBrass,
  cleanse,
  conversion,
  corrosiveGale,
  counterspell,
  damnation,
  darkRitual,
  deathriteShaman,
  dismember,
  divination,
  elvishHexhunter,
  fling,
  forest,
  frostMarsh,
  fulminatorMage,
  giantGrowth,
  glacialFloodplain,
  grizzlyBears,
  gutShot,
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
  moltensteelDragon,
  mountain,
  mouthOfRonom,
  moxEmerald,
  moxJet,
  moxPearl,
  moxRuby,
  moxSapphire,
  mutagenicGrowth,
  nyxbornRollicker,
  ornithopter,
  plains,
  plummet,
  pollutedDelta,
  porcelainLegionnaire,
  pradeshGypsies,
  ragingGoblin,
  rimewoodFalls,
  shatter,
  shock,
  sinkhole,
  slashPanther,
  snowCoveredForest,
  snowCoveredIsland,
  snowCoveredMountain,
  snowCoveredPlains,
  snowCoveredSwamp,
  snowfieldSinkhole,
  snuffOut,
  soldierToken,
  spinedThopter,
  squallDrifter,
  squallLine,
  stifle,
  stoneRain,
  stoneThrowingDevils,
  stripMine,
  sulfurousMire,
  sunkenRuins,
  swamp,
  swanSong,
  teferisIsle,
  thermopod,
  thunderingTanadon,
  tresserhornSinks,
  unholyStrength,
  vindicate,
  volatileFjord,
  wallOfEarth,
  waspLancer,
  wasteland,
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
  , toCard birdsOfParadise
  , toCard blackLotus
  , toCard blaze
  , toCard bloodMoon
  , toCard borealDruid
  , toCard borealShelf
  , toCard braidwoodCup
  , toCard cityOfBrass
  , toCard cleanse
  , toCard conversion
  , toCard corrosiveGale
  , toCard counterspell
  , toCard damnation
  , toCard darkRitual
  , toCard deathriteShaman
  , toCard dismember
  , toCard divination
  , toCard elvishHexhunter
  , toCard fling
  , toCard forest
  , toCard fulminatorMage
  , toCard frostMarsh
  , toCard giantGrowth
  , toCard glacialFloodplain
  , toCard grizzlyBears
  , toCard gutlessGhoul
  , toCard gutShot
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
  , toCard moltensteelDragon
  , toCard mouthOfRonom
  , toCard mountain
  , toCard moxEmerald
  , toCard moxJet
  , toCard moxPearl
  , toCard moxRuby
  , toCard moxSapphire
  , toCard mutagenicGrowth
  , toCard nyxbornRollicker
  , toCard ornithopter
  , toCard porcelainLegionnaire
  , toCard ragingGoblin
  , toCard plains
  , toCard plummet
  , toCard pollutedDelta
  , toCard pradeshGypsies
  , toCard rimewoodFalls
  , toCard shatter
  , toCard shock
  , toCard sinkhole
  , toCard slashPanther
  , toCard snowCoveredForest
  , toCard snowCoveredIsland
  , toCard snowCoveredMountain
  , toCard snowCoveredPlains
  , toCard snowCoveredSwamp
  , toCard snowfieldSinkhole
  , toCard snuffOut
  , toCard spinedThopter
  , toCard squallDrifter
  , toCard squallLine
  , toCard stifle
  , toCard stoneRain
  , toCard stoneThrowingDevils
  , toCard stripMine
  , toCard sulfurousMire
  , toCard sunkenRuins
  , toCard swamp
  , toCard swanSong
  , toCard teferisIsle
  , toCard thermopod
  , toCard thunderingTanadon
  , toCard tresserhornSinks
  , toCard unholyStrength
  , toCard vindicate
  , toCard volatileFjord
  , toCard wallOfEarth
  , toCard waspLancer
  , toCard wasteland
  , toCard wastes
  , toCard wear_tear
  , toCard witchEngine
  , toCard woodlandChasm
  , toCard wrathOfGod
  ]

allTokens :: [AnyToken]
allTokens = [toToken birdToken, toToken soldierToken]

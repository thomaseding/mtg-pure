{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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
  forest,
  holyStrength,
  island,
  lavaAxe,
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
  soldierToken,
  stifle,
  stoneRain,
  stoneThrowingDevils,
  swamp,
  swanSong,
  vindicate,
  wastes,
  wrathOfGod,
 )
import safe MtgPure.Model.Recursive (Card, Token)
import safe MtgPure.ModelCombinators (ToCard (..), ToToken (..))

allCards :: [Card ()]
allCards =
  [ toCard acceptableLosses
  , toCard allIsDust
  , toCard ancestralVision
  , toCard backlash
  , toCard bayou
  , toCard blaze
  , toCard bloodMoon
  , toCard cleanse
  , toCard cityOfBrass
  , toCard conversion
  , toCard damnation
  , toCard forest
  , toCard holyStrength
  , toCard island
  , toCard lavaAxe
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
  , toCard stifle
  , toCard stoneRain
  , toCard stoneThrowingDevils
  , toCard swamp
  , toCard swanSong
  , toCard vindicate
  , toCard wastes
  , toCard wrathOfGod
  ]

allTokens :: [Token ()]
allTokens = [toToken birdToken, toToken soldierToken]

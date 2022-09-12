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

module MtgPure.AllCards
  ( allCards,
    allTokens,
  )
where

import safe MtgPure.Cards
  ( acceptableLosses,
    allIsDust,
    ancestralVision,
    backlash,
    birdToken,
    blaze,
    bloodMoon,
    cityOfBrass,
    cleanse,
    conversion,
    damnation,
    forest,
    island,
    lavaAxe,
    mountain,
    plains,
    plummet,
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
import safe MtgPure.Model (OCard)
import safe MtgPure.Model.Recursive (Card, Token)
import safe MtgPure.ModelCombinators (ToCard (..), ToToken (..))

allCards :: [Card OCard]
allCards =
  [ toCard acceptableLosses,
    toCard allIsDust,
    toCard ancestralVision,
    toCard backlash,
    toCard blaze,
    toCard bloodMoon,
    toCard cleanse,
    toCard cityOfBrass,
    toCard conversion,
    toCard damnation,
    toCard forest,
    toCard island,
    toCard lavaAxe,
    toCard mountain,
    toCard ragingGoblin,
    toCard plains,
    toCard plummet,
    toCard pradeshGypsies,
    toCard shock,
    toCard sinkhole,
    toCard stifle,
    toCard stoneRain,
    toCard stoneThrowingDevils,
    toCard swamp,
    toCard swanSong,
    toCard vindicate,
    toCard wastes,
    toCard wrathOfGod
  ]

allTokens :: [Token OCard]
allTokens =
  [ toToken birdToken,
    toToken soldierToken
  ]

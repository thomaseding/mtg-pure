{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Cards where

import MtgPure.Model
import MtgPure.ModelCombinators

----------------------------------------

allCards :: [Card]
allCards =
  [ acceptableLosses,
    ancestralVision,
    backlash,
    blaze,
    cleanse,
    damnation,
    forest,
    island,
    lavaAxe,
    mountain,
    ragingGoblin,
    plains,
    shock,
    stoneThrowingDevils,
    swamp,
    wastes,
    wrathOfGod
  ]

allTokens :: [Token]
allTokens =
  [ soldierToken
  ]

----------------------------------------

mkBasicLand :: CardName -> ManaSymbol a -> Card
mkBasicLand name sym =
  Card name OathOfTheGatewatch BasicLand $
    LandDef
      [ Activated
          (\this _you -> TapCost $ permanent this)
          (\this -> controllerOf this $ \you -> Effect $ AddMana mana you)
      ]
  where
    mana = toManaPool sym

plains :: Card
plains = mkBasicLand "Plains" W

island :: Card
island = mkBasicLand "Island" U

swamp :: Card
swamp = mkBasicLand "Swamp" B

mountain :: Card
mountain = mkBasicLand "Mountain" R

forest :: Card
forest = mkBasicLand "Forest" G

wastes :: Card
wastes = mkBasicLand "Wastes" C

----------------------------------------

acceptableLosses :: Card
acceptableLosses = Card "Acceptable Losses" Odyssey Common $
  SorceryDef (toColors R) cost [] $
    \this -> controllerOf this $
      \you -> A $
        A1 (Target you) [] $
          \(target :: OCreature) -> Effect $ dealDamage this target 5
  where
    cost you =
      AndCosts
        [ ManaCostCost $ toManaCost (3, R),
          DiscardRandomCost you 1
        ]

ancestralVision :: Card
ancestralVision = Card "Ancestral Vision" TimeSpiral Rare $
  SorceryDef (toColors U) cost [Static $ Suspend 4 $ spellCost U] $
    \this -> controllerOf this $
      \you -> A $
        A1 (Target you) [] $
          \target -> Effect $ DrawCards target 3
  where
    cost = noCost

backlash :: Card
backlash = Card "Backlash" Invasion Uncommon $
  InstantDef (toColors (B, R)) cost [] $
    \this -> controllerOf this $
      \you -> A $
        A1 (Target you) [Not $ Tapped Creature] $
          \target -> controllerOf target $
            \targetController -> Effect $ dealDamage target targetController $ DamageFromPower target
  where
    cost = spellCost (1, B, R)

blaze :: Card
blaze = Card "Blaze" Portal Uncommon $
  Variable $ \x ->
    let cost = spellCost (VariableGenericMana x, R)
     in SorceryDef (toColors R) cost [] $
          \this -> controllerOf this $
            \you -> A $
              A3 (Target you) (Req3 [] [] []) $
                \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target x

cleanse :: Card
cleanse = Card "Cleanse" Legends Rare $
  SorceryDef (toColors W) cost [] $
    \_this -> All $
      All1 [OfColors $ toColors B] $
        \creatures -> Effect $ Destroy Creature creatures
  where
    cost = spellCost (2, W, W)

damnation :: Card
damnation = Card "Damnation" PlanarChaos Rare $
  SorceryDef (toColors B) cost [] $
    \_this -> All $
      All1 [] $
        \creatures -> Effect $ Destroy Creature creatures
  where
    cost = spellCost (2, B, B)

lavaAxe :: Card
lavaAxe = Card "Lava Axe" Portal Common $
  SorceryDef (toColors R) cost [] $
    \this -> controllerOf this $
      \you -> A $
        A2 (Target you) (Req2 [] []) $
          \(target :: OPlayerPlaneswalker) -> Effect $ dealDamage this target 5
  where
    cost = spellCost (4, R)

ragingGoblin :: Card
ragingGoblin =
  Card "Raging Goblin" Portal Common $
    CreatureDef
      (toColors R)
      cost
      [Goblin]
      (Power 1)
      (Toughness 1)
      [Static Haste]
  where
    cost = spellCost R

shock :: Card
shock = Card "Shock" TenthEdition Common $
  InstantDef (toColors R) cost [] $
    \this -> controllerOf this $
      \you -> A $
        A3 (Target you) (Req3 [] [] []) $
          \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target 2
  where
    cost = spellCost R

soldierToken :: Token
soldierToken =
  Token $
    Card "Soldier Token" NoCardSet Common $
      CreatureDef
        (toColors W)
        cost
        [Soldier]
        (Power 1)
        (Toughness 1)
        []
  where
    cost = noCost

stoneThrowingDevils :: Card
stoneThrowingDevils =
  Card "Stone-Throwing Devils" ArabianNights Common $
    CreatureDef
      (toColors B)
      cost
      [Devil]
      (Power 1)
      (Toughness 1)
      [ Static FirstStrike
      ]
  where
    cost = spellCost B

wrathOfGod :: Card
wrathOfGod = Card "Wrath of God" Alpha Rare $
  SorceryDef (toColors W) cost [] $
    \_this -> All $
      All1 [] $
        \creatures -> Effect $ Destroy Creature creatures
  where
    cost = spellCost (2, W, W)

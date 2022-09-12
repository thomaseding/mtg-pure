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

class ToCard card where
  toCard :: card -> Card OTCard

instance ToCard (Card OTCard) where
  toCard = id

instance ToCard (Card OTArtifact) where
  toCard = ArtifactCard

instance ToCard (Card OTCreature) where
  toCard = CreatureCard

instance ToCard (Card OTEnchantment) where
  toCard = EnchantmentCard

instance ToCard (Card OTInstant) where
  toCard = InstantCard

instance ToCard (Card OTLand) where
  toCard = LandCard

instance ToCard (Card OTPlaneswalker) where
  toCard = PlaneswalkerCard

instance ToCard (Card OTSorcery) where
  toCard = SorceryCard

class ToToken token where
  toToken :: token -> Token OTCard

instance ToToken (Token OTCard) where
  toToken = id

instance ToToken (Token OTArtifact) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTCreature) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTEnchantment) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTInstant) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTLand) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTPlaneswalker) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OTSorcery) where
  toToken (Token x) = Token $ toCard x

allCards :: [Card OTCard]
allCards =
  [ toCard acceptableLosses,
    toCard ancestralVision,
    toCard backlash,
    toCard blaze,
    toCard cleanse,
    toCard conversion,
    toCard damnation,
    toCard forest,
    toCard island,
    toCard lavaAxe,
    toCard mountain,
    toCard ragingGoblin,
    toCard plains,
    toCard shock,
    toCard stoneThrowingDevils,
    toCard swamp,
    toCard vindicate,
    toCard wastes,
    toCard wrathOfGod
  ]

allTokens :: [Token OTCard]
allTokens =
  [ toToken soldierToken
  ]

----------------------------------------

mkBasicLand :: CardName -> ManaSymbol a -> Card OTLand
mkBasicLand name sym =
  Card name OathOfTheGatewatch BasicLand $
    LandDef
      [ Activated
          (\this _you -> TapCost $ asPermanent this)
          (\this -> controllerOf this $ \you -> Effect $ AddMana mana you)
      ]
  where
    mana = toManaPool sym

plains :: Card OTLand
plains = mkBasicLand "Plains" W

island :: Card OTLand
island = mkBasicLand "Island" U

swamp :: Card OTLand
swamp = mkBasicLand "Swamp" B

mountain :: Card OTLand
mountain = mkBasicLand "Mountain" R

forest :: Card OTLand
forest = mkBasicLand "Forest" G

wastes :: Card OTLand
wastes = mkBasicLand "Wastes" C

----------------------------------------

acceptableLosses :: Card OTSorcery
acceptableLosses = Card "Acceptable Losses" Odyssey Common $
  SorceryDef (toColors R) cost [] $
    \this -> controllerOf this $
      \you -> A (Target you) $
        O1 [] $
          \(target :: OCreature) -> Effect $ dealDamage this target 5
  where
    cost you =
      AndCosts
        [ ManaCostCost $ toManaCost (3, R),
          DiscardRandomCost you 1
        ]

ancestralVision :: Card OTSorcery
ancestralVision = Card "Ancestral Vision" TimeSpiral Rare $
  SorceryDef (toColors U) cost [Static $ Suspend 4 $ spellCost U] $
    \this -> controllerOf this $
      \you -> A (Target you) $
        O1 [] $
          \target -> Effect $ DrawCards target 3
  where
    cost = noCost

backlash :: Card OTInstant
backlash = Card "Backlash" Invasion Uncommon $
  InstantDef (toColors (B, R)) cost [] $
    \this -> controllerOf this $
      \you -> A (Target you) $
        O1 [Not $ Tapped Creature] $
          \target -> controllerOf target $
            \targetController -> Effect $ dealDamage target targetController $ DamageFromPower target
  where
    cost = spellCost (1, B, R)

blaze :: Card OTSorcery
blaze = Card "Blaze" Portal Uncommon $
  Variable $ \x ->
    let cost = spellCost (VariableGenericMana x, R)
     in SorceryDef (toColors R) cost [] $
          \this -> controllerOf this $
            \you -> A (Target you) $
              O3 [] [] [] $
                \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target x

bloodMoon :: Card OTEnchantment
bloodMoon =
  Card "Blood Moon" TheDark Rare $
    EnchantmentDef
      (toColors R)
      cost
      [ Static $
          ContinuousEffect $
            \_this -> All $ O1 [NonBasic] $ \land -> Effect $ ChangeTo Land (asPermanent land) mountain
      ]
  where
    cost = spellCost (2, R)

cleanse :: Card OTSorcery
cleanse = Card "Cleanse" Legends Rare $
  SorceryDef (toColors W) cost [] $
    \_this -> All $
      O1 [OfColors $ toColors B] $
        \creatures -> Effect $ Destroy Creature $ O creatures
  where
    cost = spellCost (2, W, W)

-- TODO
conversion :: Card OTEnchantment
conversion =
  Card "Conversion" Alpha Uncommon $
    EnchantmentDef
      (toColors W)
      cost
      [ Triggered $
          At
            (Right UpkeepStep) -- TODO: Beginning of
            [ \this -> controllerOf this $ \you ->
                Condition $
                  And
                    [ Satisfies AnyPlayer (O you) [HasTurnControl],
                      Unless $ Satisfies AnyPlayer (O you) [PlayerPays $ ManaCostCost $ toManaCost (W, W)]
                    ]
            ]
            $ \this -> controllerOf this $
              \you -> Effect $ Sacrifice Enchantment you [Is (AnyPermanent Enchantment) $ O this],
        Static $
          ContinuousEffect $
            \_this -> All $ O1 [HasBasicLandType Red] $ \land -> Effect $ ChangeTo Land (asPermanent land) plains
      ]
  where
    cost = spellCost (2, W, W)

damnation :: Card OTSorcery
damnation = Card "Damnation" PlanarChaos Rare $
  SorceryDef (toColors B) cost [] $
    \_this -> All $
      O1 [] $
        \creatures -> Effect $ Destroy Creature $ O creatures
  where
    cost = spellCost (2, B, B)

lavaAxe :: Card OTSorcery
lavaAxe = Card "Lava Axe" Portal Common $
  SorceryDef (toColors R) cost [] $
    \this -> controllerOf this $
      \you -> A (Target you) $
        O2 [] [] $
          \(target :: OPlayerPlaneswalker) -> Effect $ dealDamage this target 5
  where
    cost = spellCost (4, R)

ragingGoblin :: Card OTCreature
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

shock :: Card OTInstant
shock = Card "Shock" TenthEdition Common $
  InstantDef (toColors R) cost [] $
    \this -> controllerOf this $
      \you -> A (Target you) $
        O3 [] [] [] $
          \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target 2
  where
    cost = spellCost R

sinkhole :: Card OTSorcery
sinkhole = Card "Sinkhole" Alpha Common $
  SorceryDef (toColors B) cost [] $
    \this -> controllerOf this $
      \you -> A (Target you) $
        O1 [] $
          \target -> Effect $ Destroy Land $ O target
  where
    cost = spellCost (B, B)

soldierToken :: Token OTCreature
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

stoneThrowingDevils :: Card OTCreature
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

vindicate :: Card OTSorcery
vindicate = Card "Vindicate" Apocalypse Rare $
  SorceryDef (toColors (W, B)) cost [] $
    \this -> controllerOf this $
      \you -> A (Target you) $
        O5 [] [] [] [] [] $
          \(target :: OPermanent) -> Effect $ Destroy Permanent target
  where
    cost = spellCost (1, W, B)

wrathOfGod :: Card OTSorcery
wrathOfGod = Card "Wrath of God" Alpha Rare $
  SorceryDef (toColors W) cost [] $
    \_this -> All $
      O1 [] $
        \creatures -> Effect $ Destroy Creature $ O creatures
  where
    cost = spellCost (2, W, W)

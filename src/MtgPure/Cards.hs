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

class ToSetCard card where
  toSetCard :: card -> SetCard OTCard

instance ToSetCard (SetCard OTCard) where
  toSetCard = id

instance ToSetCard (SetCard OTArtifact) where
  toSetCard (SetCard s r c) = SetCard s r $ ArtifactCard c

instance ToSetCard (SetCard OTCreature) where
  toSetCard (SetCard s r c) = SetCard s r $ CreatureCard c

instance ToSetCard (SetCard OTEnchantment) where
  toSetCard (SetCard s r c) = SetCard s r $ EnchantmentCard c

instance ToSetCard (SetCard OTInstant) where
  toSetCard (SetCard s r c) = SetCard s r $ InstantCard c

instance ToSetCard (SetCard OTLand) where
  toSetCard (SetCard s r c) = SetCard s r $ LandCard c

instance ToSetCard (SetCard OTPlaneswalker) where
  toSetCard (SetCard s r c) = SetCard s r $ PlaneswalkerCard c

instance ToSetCard (SetCard OTSorcery) where
  toSetCard (SetCard s r c) = SetCard s r $ SorceryCard c

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

class ToSetToken token where
  toSetToken :: token -> SetToken OTCard

instance ToSetToken (SetToken OTCard) where
  toSetToken = id

instance ToSetToken (SetToken OTArtifact) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTCreature) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTEnchantment) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTInstant) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTLand) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTPlaneswalker) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OTSorcery) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

allCards :: [SetCard OTCard]
allCards =
  [ toSetCard acceptableLosses,
    toSetCard ancestralVision,
    toSetCard backlash,
    toSetCard blaze,
    toSetCard cleanse,
    toSetCard conversion,
    toSetCard damnation,
    toSetCard forest,
    toSetCard island,
    toSetCard lavaAxe,
    toSetCard mountain,
    toSetCard ragingGoblin,
    toSetCard plains,
    toSetCard shock,
    toSetCard stoneThrowingDevils,
    toSetCard swamp,
    toSetCard vindicate,
    toSetCard wastes,
    toSetCard wrathOfGod
  ]

allTokens :: [SetToken OTCard]
allTokens =
  [ toSetToken soldierToken
  ]

----------------------------------------

mkBasicLand :: CardName -> ManaSymbol a -> SetCard OTLand
mkBasicLand name sym =
  SetCard OathOfTheGatewatch BasicLand $
    Card1 name $ \this ->
      LandDef
        [ Activated
            (Cost $ TapCost $ asPermanent this)
            $ controllerOf this $ \you -> Effect $ AddMana mana you
        ]
  where
    mana = toManaPool sym

plains :: SetCard OTLand
plains = mkBasicLand "Plains" W

island :: SetCard OTLand
island = mkBasicLand "Island" U

swamp :: SetCard OTLand
swamp = mkBasicLand "Swamp" B

mountain :: SetCard OTLand
mountain = mkBasicLand "Mountain" R

forest :: SetCard OTLand
forest = mkBasicLand "Forest" G

wastes :: SetCard OTLand
wastes = mkBasicLand "Wastes" C

----------------------------------------

acceptableLosses :: SetCard OTSorcery
acceptableLosses = SetCard Odyssey Common $
  Card1 "Acceptable Losses" $ \this ->
    let cost = controllerOf this $ \you ->
          Cost $
            AndCosts
              [ ManaCostCost $ toManaCost (3, R),
                DiscardRandomCost you 1
              ]
     in SorceryDef (toColors R) cost [] $
          controllerOf this $
            \you -> A (Target you) $
              O1 [] $
                \(target :: OCreature) -> Effect $ dealDamage this target 5

ancestralVision :: SetCard OTSorcery
ancestralVision = SetCard TimeSpiral Rare $
  Card1 "Ancestral Vision" $ \this ->
    SorceryDef (toColors U) cost [Static $ Suspend 4 $ spellCost U] $
      controllerOf this $
        \you -> A (Target you) $
          O1 [] $
            \target -> Effect $ DrawCards target 3
  where
    cost = noCost

backlash :: SetCard OTInstant
backlash = SetCard Invasion Uncommon $
  Card1 "Backlash" $ \this ->
    InstantDef (toColors (B, R)) cost [] $
      controllerOf this $
        \you -> A (Target you) $
          O1 [Not $ Tapped PermanentCreature] $
            \target -> controllerOf target $
              \targetController -> Effect $ dealDamage target targetController $ DamageFromPower target
  where
    cost = spellCost (1, B, R)

blaze :: SetCard OTSorcery
blaze = SetCard Portal Uncommon $
  Card1 "Blaze" $ \this ->
    VariableDef $ \x ->
      let cost = spellCost (VariableGenericMana x, R)
       in SorceryDef (toColors R) cost [] $
            controllerOf this $
              \you -> A (Target you) $
                O3 [] [] [] $
                  \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target x

bloodMoon :: SetCard OTEnchantment
bloodMoon =
  SetCard TheDark Rare $
    Card1 "Blood Moon" $ \_this ->
      EnchantmentDef
        (toColors R)
        cost
        [ Static $
            ContinuousEffect $
              All $ O1 [NonBasic] $ \land -> Effect $ changeTo land $ fromSetCard mountain
        ]
  where
    cost = spellCost (2, R)

cleanse :: SetCard OTSorcery
cleanse = SetCard Legends Rare $
  Card1 "Cleanse" $ \_this ->
    SorceryDef (toColors W) cost [] $
      All $
        O1 [OfColors $ toColors B] $
          \(creatures :: OCreature) -> Effect $ destroy creatures
  where
    cost = spellCost (2, W, W)

conversion :: SetCard OTEnchantment
conversion =
  SetCard Alpha Uncommon $
    Card1 "Conversion" $ \this ->
      EnchantmentDef
        (toColors W)
        cost
        [ Triggered $
            When $
              Conditional
                ( ActivePlayer $ \active -> controllerOf this $ \you ->
                    Condition $
                      And
                        [ satisfies you [is active],
                          Unless $ satisfies you [PlayerPays $ ManaCostCost $ toManaCost (W, W)]
                        ]
                )
                $ TimePoint (StepBegin UpkeepStep) $
                  controllerOf this $
                    \you -> Effect $ sacrifice you [is this],
          Static $
            ContinuousEffect $
              All $
                O1 [HasBasicLandType Red] $
                  \land -> Effect $ changeTo land $ fromSetCard plains
        ]
  where
    cost = spellCost (2, W, W)

damnation :: SetCard OTSorcery
damnation = SetCard PlanarChaos Rare $
  Card1 "Damnation" $ \_this ->
    SorceryDef (toColors B) cost [] $
      All $
        O1 [] $
          \(creatures :: OCreature) -> Effect $ destroy creatures
  where
    cost = spellCost (2, B, B)

lavaAxe :: SetCard OTSorcery
lavaAxe = SetCard Portal Common $
  Card1 "Lava Axe" $ \this ->
    SorceryDef (toColors R) cost [] $
      controllerOf this $
        \you -> A (Target you) $
          O2 [] [] $
            \(target :: OPlayerPlaneswalker) -> Effect $ dealDamage this target 5
  where
    cost = spellCost (4, R)

ragingGoblin :: SetCard OTCreature
ragingGoblin =
  SetCard Portal Common $
    Card1 "Raging Goblin" $ \_this ->
      CreatureDef
        (toColors R)
        cost
        [Goblin]
        (Power 1)
        (Toughness 1)
        [Static Haste]
  where
    cost = spellCost R

shock :: SetCard OTInstant
shock = SetCard TenthEdition Common $
  Card1 "Shock" $ \this ->
    InstantDef (toColors R) cost [] $
      controllerOf this $
        \you -> A (Target you) $
          O3 [] [] [] $
            \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target 2
  where
    cost = spellCost R

sinkhole :: SetCard OTSorcery
sinkhole = SetCard Alpha Common $
  Card1 "Sinkhole" $ \this ->
    SorceryDef (toColors B) cost [] $
      controllerOf this $
        \you -> A (Target you) $
          O1 [] $
            \(target :: OLand) -> Effect $ destroy target
  where
    cost = spellCost (B, B)

soldierToken :: SetToken OTCreature
soldierToken =
  SetToken TenthEdition Common $
    Token $
      Card1 "Soldier Token" $ \_this ->
        CreatureDef
          (toColors W)
          cost
          [Soldier]
          (Power 1)
          (Toughness 1)
          []
  where
    cost = noCost

stoneThrowingDevils :: SetCard OTCreature
stoneThrowingDevils =
  SetCard ArabianNights Common $
    Card1 "Stone-Throwing Devils" $ \_this ->
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

vindicate :: SetCard OTSorcery
vindicate = SetCard Apocalypse Rare $
  Card1 "Vindicate" $ \this ->
    SorceryDef (toColors (W, B)) cost [] $
      controllerOf this $
        \you -> A (Target you) $
          O5 [] [] [] [] [] $
            \(target :: OPermanent) -> Effect $ destroy target
  where
    cost = spellCost (1, W, B)

wrathOfGod :: SetCard OTSorcery
wrathOfGod = SetCard Alpha Rare $
  Card1 "Wrath of God" $ \_this ->
    SorceryDef (toColors W) cost [] $
      All $
        O1 [] $
          \(creatures :: OCreature) -> Effect $ destroy creatures
  where
    cost = spellCost (2, W, W)

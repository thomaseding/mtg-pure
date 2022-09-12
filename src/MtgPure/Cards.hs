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

allCards :: [Card OTCard]
allCards =
  [ toCard acceptableLosses,
    toCard allIsDust,
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
    toCard stoneRain,
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
mkBasicLand name sym = Card1 name $ \this ->
  LandDef
    [ Activated
        (Cost $ tapCost this)
        $ controllerOf this $ \you -> Effect $ AddMana mana you
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
acceptableLosses = Card1 "Acceptable Losses" $ \this ->
  let cost = controllerOf this $ \you ->
        Cost $
          AndCosts
            [ ManaCostCost $ toManaCost (3, R),
              DiscardRandomCost you 1
            ]
   in SorceryDef (toColors R) cost [] $
        controllerOf this $
          \you -> A (Target you) $
            o1 [] $
              \(target :: OCreature) -> Effect $ dealDamage this target 5

allIsDust :: Card OTSorcery
allIsDust = Card1 "All Is Dust" $ \_this ->
  TribalDef [Eldrazi] NonCreatureSorcery $
    SorceryDef (toColors ()) cost [] $
      All $
        o1 [] $ \player ->
          All $
            o5 [colored] $ \(perm :: OPermanent) ->
              Effect $ sacrifice player [is perm]
  where
    cost = spellCost 7
    colored = ROr $ map ofColors [toColors W, toColors U, toColors B, toColors R, toColors G]

ancestralVision :: Card OTSorcery
ancestralVision = Card1 "Ancestral Vision" $ \this ->
  SorceryDef (toColors U) cost [Static $ Suspend 4 $ spellCost U] $
    controllerOf this $
      \you -> A (Target you) $
        o1 [] $
          \target -> Effect $ DrawCards target 3
  where
    cost = noCost

backlash :: Card OTInstant
backlash = Card1 "Backlash" $ \this ->
  InstantDef (toColors (B, R)) cost [] $
    controllerOf this $
      \you -> A (Target you) $
        o1 [Not tapped] $
          \target -> controllerOf target $
            \targetController -> Effect $ dealDamage target targetController $ DamageFromPower target
  where
    cost = spellCost (1, B, R)

blaze :: Card OTSorcery
blaze = Card1 "Blaze" $ \this ->
  VariableDef $ \x ->
    let cost = spellCost (VariableGenericMana x, R)
     in SorceryDef (toColors R) cost [] $
          controllerOf this $
            \you -> A (Target you) $
              o3 [] $
                \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target x

bloodMoon :: Card OTEnchantment
bloodMoon = Card1 "Blood Moon" $ \_this ->
  EnchantmentDef
    (toColors R)
    cost
    [ Static $
        ContinuousEffect $
          All $
            o1 [NonBasic] $
              \land -> Effect $ changeTo land mountain
    ]
  where
    cost = spellCost (2, R)

cleanse :: Card OTSorcery
cleanse = Card1 "Cleanse" $ \_this ->
  SorceryDef (toColors W) cost [] $
    All $
      o1 [ofColors B] $
        \(creature :: OCreature) -> Effect $ destroy creature
  where
    cost = spellCost (2, W, W)

conversion :: Card OTEnchantment
conversion = Card1 "Conversion" $ \this ->
  EnchantmentDef
    (toColors W)
    cost
    [ Triggered $
        When $
          Conditional
            ( ActivePlayer $ \active -> controllerOf this $ \you ->
                Condition $
                  CAnd
                    [ satisfies you [is active],
                      Unless $ satisfies you [playerPays $ toManaCost (W, W)]
                    ]
            )
            $ TimePoint (StepBegin UpkeepStep) $
              controllerOf this $
                \you -> Effect $ sacrifice you [is this],
      Static $
        ContinuousEffect $
          All $
            o1 [HasBasicLandType Red] $
              \land -> Effect $ changeTo land plains
    ]
  where
    cost = spellCost (2, W, W)

damnation :: Card OTSorcery
damnation = Card1 "Damnation" $ \_this ->
  SorceryDef (toColors B) cost [] $
    All $
      o1 [] $
        \(creature :: OCreature) -> Effect $ destroy creature
  where
    cost = spellCost (2, B, B)

lavaAxe :: Card OTSorcery
lavaAxe = Card1 "Lava Axe" $ \this ->
  SorceryDef (toColors R) cost [] $
    controllerOf this $
      \you -> A (Target you) $
        o2 [] $
          \(target :: OPlayerPlaneswalker) -> Effect $ dealDamage this target 5
  where
    cost = spellCost (4, R)

ragingGoblin :: Card OTCreature
ragingGoblin = Card1 "Raging Goblin" $ \_this ->
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
shock = Card1 "Shock" $ \this ->
  InstantDef (toColors R) cost [] $
    controllerOf this $
      \you -> A (Target you) $
        o3 [] $
          \(target :: OCreaturePlayerPlaneswalker) -> Effect $ dealDamage this target 2
  where
    cost = spellCost R

sinkhole :: Card OTSorcery
sinkhole = Card1 "Sinkhole" $ \this ->
  SorceryDef (toColors B) cost [] $
    controllerOf this $
      \you -> A (Target you) $
        o1 [] $
          \(target :: OLand) -> Effect $ destroy target
  where
    cost = spellCost (B, B)

soldierToken :: Token OTCreature
soldierToken = Token $
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

stoneRain :: Card OTSorcery
stoneRain = Card1 "Stone Rain" $ \this ->
  SorceryDef (toColors R) cost [] $
    controllerOf this $
      \you -> A (Target you) $
        o1 [] $
          \(target :: OLand) -> Effect $ destroy target
  where
    cost = spellCost (2, R)

stoneThrowingDevils :: Card OTCreature
stoneThrowingDevils = Card1 "Stone-Throwing Devils" $ \_this ->
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
vindicate = Card1 "Vindicate" $ \this ->
  SorceryDef (toColors (W, B)) cost [] $
    controllerOf this $
      \you -> A (Target you) $
        o5 [] $
          \(target :: OPermanent) -> Effect $ destroy target
  where
    cost = spellCost (1, W, B)

wrathOfGod :: Card OTSorcery
wrathOfGod = Card1 "Wrath of God" $ \_this ->
  SorceryDef (toColors W) cost [] $
    All $
      o1 [] $
        \(creature :: OCreature) -> Effect $ destroy creature
  where
    cost = spellCost (2, W, W)

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

import safe MtgPure.Model
import safe MtgPure.ModelCombinators

----------------------------------------

fromSetCard :: SetCard a -> Card a
fromSetCard (SetCard _ _ card) = card

fromSetToken :: SetToken a -> Token a
fromSetToken (SetToken _ _ token) = token

class ToCard card where
  toCard :: card -> Card OCard

instance ToCard (Card OCard) where
  toCard = id

instance ToCard (Card OArtifact) where
  toCard = ArtifactCard

instance ToCard (Card OCreature) where
  toCard = CreatureCard

instance ToCard (Card OEnchantment) where
  toCard = EnchantmentCard

instance ToCard (Card OInstant) where
  toCard = InstantCard

instance ToCard (Card OLand) where
  toCard = LandCard

instance ToCard (Card OPlaneswalker) where
  toCard = PlaneswalkerCard

instance ToCard (Card OSorcery) where
  toCard = SorceryCard

class ToSetCard card where
  toSetCard :: card -> SetCard OCard

instance ToSetCard (SetCard OCard) where
  toSetCard = id

instance ToSetCard (SetCard OArtifact) where
  toSetCard (SetCard s r c) = SetCard s r $ ArtifactCard c

instance ToSetCard (SetCard OCreature) where
  toSetCard (SetCard s r c) = SetCard s r $ CreatureCard c

instance ToSetCard (SetCard OEnchantment) where
  toSetCard (SetCard s r c) = SetCard s r $ EnchantmentCard c

instance ToSetCard (SetCard OInstant) where
  toSetCard (SetCard s r c) = SetCard s r $ InstantCard c

instance ToSetCard (SetCard OLand) where
  toSetCard (SetCard s r c) = SetCard s r $ LandCard c

instance ToSetCard (SetCard OPlaneswalker) where
  toSetCard (SetCard s r c) = SetCard s r $ PlaneswalkerCard c

instance ToSetCard (SetCard OSorcery) where
  toSetCard (SetCard s r c) = SetCard s r $ SorceryCard c

class ToToken token where
  toToken :: token -> Token OCard

instance ToToken (Token OCard) where
  toToken = id

instance ToToken (Token OArtifact) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OCreature) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OEnchantment) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OInstant) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OLand) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OPlaneswalker) where
  toToken (Token x) = Token $ toCard x

instance ToToken (Token OSorcery) where
  toToken (Token x) = Token $ toCard x

class ToSetToken token where
  toSetToken :: token -> SetToken OCard

instance ToSetToken (SetToken OCard) where
  toSetToken = id

instance ToSetToken (SetToken OArtifact) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OCreature) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OEnchantment) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OInstant) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OLand) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OPlaneswalker) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

instance ToSetToken (SetToken OSorcery) where
  toSetToken (SetToken s r (Token x)) = SetToken s r $ Token $ toCard x

allCards :: [Card OCard]
allCards =
  [ toCard acceptableLosses,
    toCard allIsDust,
    toCard ancestralVision,
    toCard backlash,
    toCard blaze,
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
    toCard shock,
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

----------------------------------------

mkBasicLand :: CardName -> ManaSymbol a -> Card OLand
mkBasicLand name sym = mkCard name $ \this ->
  LandDef
    [ Activated
        (Cost $ tapCost this)
        $ controllerOf this $ \you -> effect $ AddMana you mana
    ]
  where
    mana = toManaPool sym

plains :: Card OLand
plains = mkBasicLand "Plains" W

island :: Card OLand
island = mkBasicLand "Island" U

swamp :: Card OLand
swamp = mkBasicLand "Swamp" B

mountain :: Card OLand
mountain = mkBasicLand "Mountain" R

forest :: Card OLand
forest = mkBasicLand "Forest" G

wastes :: Card OLand
wastes = mkBasicLand "Wastes" C

----------------------------------------

acceptableLosses :: Card OSorcery
acceptableLosses = mkCard "Acceptable Losses" $ \this ->
  let cost =
        Cost $
          AndCosts
            [ ManaCost $ toManaCost (3, R),
              DiscardRandomCost 1
            ]
   in SorceryDef (toColors R) cost [] $
        controllerOf this $
          \you -> A Target you $
            masked [] $
              \(target :: OCreature) -> effect $ dealDamage this target 5

allIsDust :: Card OSorcery
allIsDust = mkCard "All Is Dust" $ \_this ->
  TribalDef [Eldrazi] WNonCreatureSorcery $
    SorceryDef (toColors ()) cost [] $
      All $
        masked [] $ \player ->
          All $
            masked [colored] $ \(perm :: OPermanent) ->
              effect $ sacrifice player [is perm]
  where
    cost = spellCost 7

ancestralVision :: Card OSorcery
ancestralVision = mkCard "Ancestral Vision" $ \this ->
  SorceryDef (toColors U) cost [Static $ Suspend 4 $ spellCost U] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \target -> effect $ DrawCards target 3
  where
    cost = noCost

backlash :: Card OInstant
backlash = mkCard "Backlash" $ \this ->
  InstantDef (toColors (B, R)) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [Not tapped] $
          \target -> VariableFromPower target $
            \power -> controllerOf target $
              \targetController -> effect $ dealDamage target targetController $ VariableDamage power
  where
    cost = spellCost (1, B, R)

birdToken :: Token OCreature
birdToken = mkToken "Bird Token" $ \_this ->
  CreatureDef
    (toColors U)
    cost
    [Bird]
    (Power 2)
    (Toughness 2)
    [Static Flying]
  where
    cost = noCost

blaze :: Card OSorcery
blaze = mkCard "Blaze" $ \this ->
  VariableDef $ \x ->
    let cost = spellCost (VariableGenericMana x, R)
     in SorceryDef (toColors R) cost [] $
          controllerOf this $
            \you -> A Target you $
              masked [] $
                \(target :: OCreaturePlayerPlaneswalker) -> effect $ dealDamage this target x

bloodMoon :: Card OEnchantment
bloodMoon = mkCard "Blood Moon" $ \_this ->
  EnchantmentDef
    (toColors R)
    cost
    [ Static $
        StaticContinuous $
          All $
            masked [nonBasic] $
              \land -> effect $ changeTo land mountain
    ]
  where
    cost = spellCost (2, R)

cleanse :: Card OSorcery
cleanse = mkCard "Cleanse" $ \_this ->
  SorceryDef (toColors W) cost [] $
    All $
      masked [ofColors B] $
        \(creature :: OCreature) -> effect $ destroy creature
  where
    cost = spellCost (2, W, W)

cityOfBrass :: Card OLand
cityOfBrass = mkCard "City of Brass" $ \this ->
  LandDef
    [ Triggered $
        When $
          event $
            becomesTapped $
              linked [is this] $
                \_ ->
                  controllerOf this $
                    \you -> effect $ dealDamage this you 1,
      Activated
        (Cost $ tapCost this)
        $ controllerOf this $
          \you -> effect $ addManaAnyColor you 1
    ]

conversion :: Card OEnchantment
conversion = mkCard "Conversion" $ \this ->
  EnchantmentDef
    (toColors W)
    cost
    [ Triggered $
        When $
          ActivePlayer $
            \active -> controllerOf this $
              \you ->
                let cond =
                      COr
                        [ satisfies you [Not $ is active],
                          satisfies you [is active, playerPays $ toManaCost (W, W)]
                        ]
                 in ifElse cond $
                      event $
                        TimePoint (StepBegin UpkeepStep) $ effect $ sacrifice you [is this],
      Static $
        StaticContinuous $
          All $
            masked [HasBasicLandType Mountain] $
              \land -> effect $ changeTo land plains
    ]
  where
    cost = spellCost (2, W, W)

damnation :: Card OSorcery
damnation = mkCard "Damnation" $ \_this ->
  SorceryDef (toColors B) cost [] $
    All $
      masked [] $
        \(creature :: OCreature) -> effect $ destroy creature
  where
    cost = spellCost (2, B, B)

lavaAxe :: Card OSorcery
lavaAxe = mkCard "Lava Axe" $ \this ->
  SorceryDef (toColors R) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \(target :: OPlayerPlaneswalker) -> effect $ dealDamage this target 5
  where
    cost = spellCost (4, R)

plummet :: Card OInstant
plummet = mkCard "Plummet" $ \this ->
  InstantDef (toColors G) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [hasAbility $ \_this -> Static Flying] $
          \target -> effect $ destroy target
  where
    cost = spellCost (1, G)

pradeshGypsies :: Card OCreature
pradeshGypsies = mkCard "Pradesh Gypsies" $ \this ->
  CreatureDef
    (toColors G)
    cost
    [Human, Nomad]
    (Power 1)
    (Toughness 1)
    [ Activated
        (Cost $ AndCosts [tapCost this, ManaCost $ toManaCost (1, G)])
        $ controllerOf this $
          \you -> A Target you $
            masked [] $
              \creature ->
                effect $
                  untilEndOfTurn $
                    gain creature $
                      Static $
                        StaticContinuous $
                          effect $
                            StatDelta creature (Power (-2)) (Toughness 0)
    ]
  where
    cost = spellCost (2, G)

ragingGoblin :: Card OCreature
ragingGoblin = mkCard "Raging Goblin" $ \_this ->
  CreatureDef
    (toColors R)
    cost
    [Goblin]
    (Power 1)
    (Toughness 1)
    [Static Haste]
  where
    cost = spellCost R

shock :: Card OInstant
shock = mkCard "Shock" $ \this ->
  InstantDef (toColors R) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \(target :: OCreaturePlayerPlaneswalker) -> effect $ dealDamage this target 2
  where
    cost = spellCost R

sinkhole :: Card OSorcery
sinkhole = mkCard "Sinkhole" $ \this ->
  SorceryDef (toColors B) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \(target :: OLand) -> effect $ destroy target
  where
    cost = spellCost (B, B)

soldierToken :: Token OCreature
soldierToken = mkToken "Soldier Token" $ \_this ->
  CreatureDef
    (toColors W)
    cost
    [Soldier]
    (Power 1)
    (Toughness 1)
    []
  where
    cost = noCost

stifle :: Card OInstant
stifle = mkCard "Stifle" $ \this ->
  InstantDef (toColors U) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \(target :: OActivatedOrTriggeredAbility) ->
            effect $ counterAbility target
  where
    cost = spellCost U

stoneRain :: Card OSorcery
stoneRain = mkCard "Stone Rain" $ \this ->
  SorceryDef (toColors R) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \(target :: OLand) -> effect $ destroy target
  where
    cost = spellCost (2, R)

stoneThrowingDevils :: Card OCreature
stoneThrowingDevils = mkCard "Stone-Throwing Devils" $ \_this ->
  CreatureDef
    (toColors B)
    cost
    [Devil]
    (Power 1)
    (Toughness 1)
    [Static FirstStrike]
  where
    cost = spellCost B

swanSong :: Card OInstant
swanSong = mkCard "Swan Song" $ \this ->
  InstantDef (toColors U) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \(target :: ON3 'OTEnchantment 'OTInstant 'OTSorcery) ->
            controllerOf target $ \controller ->
              effect
                [ counterSpell target,
                  addToBattlefield controller birdToken
                ]
  where
    cost = spellCost U

vindicate :: Card OSorcery
vindicate = mkCard "Vindicate" $ \this ->
  SorceryDef (toColors (W, B)) cost [] $
    controllerOf this $
      \you -> A Target you $
        masked [] $
          \(target :: OPermanent) -> effect $ destroy target
  where
    cost = spellCost (1, W, B)

wrathOfGod :: Card OSorcery
wrathOfGod = mkCard "Wrath of God" $ \_this ->
  SorceryDef (toColors W) cost [] $
    All $
      masked [] $
        \(creature :: OCreature) -> effect $ destroy creature
  where
    cost = spellCost (2, W, W)

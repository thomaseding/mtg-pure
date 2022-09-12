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

module MtgPure.Cards
  ( acceptableLosses,
    allIsDust,
    ancestralVision,
    backlash,
    blaze,
    bloodMoon,
    cleanse,
    cityOfBrass,
    conversion,
    damnation,
    forest,
    island,
    lavaAxe,
    mountain,
    ragingGoblin,
    plains,
    plummet,
    pradeshGypsies,
    shock,
    sinkhole,
    stifle,
    stoneRain,
    stoneThrowingDevils,
    swamp,
    swanSong,
    vindicate,
    wastes,
    wrathOfGod,
    --
    birdToken,
    soldierToken,
  )
where

import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.ColorsLike (ColorsLike (toColors))
import safe MtgPure.Model.CreatureType (CreatureType (..))
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OCreature,
    OCreaturePlayerPlaneswalker,
    OEnchantment,
    OInstant,
    OLand,
    ON3,
    OPermanent,
    OPlayerPlaneswalker,
    OSorcery,
  )
import safe MtgPure.Model.ObjectType (ObjectType (..))
import safe MtgPure.Model.ObjectType.NonCreatureCard
  ( WNonCreatureCard (..),
  )
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Recursive
  ( Ability (Activated, Static, Triggered),
    Card,
    CardTypeDef (..),
    Condition (COr),
    Cost (AndCosts, DiscardRandomCost, ManaCost),
    Effect (AddMana, DrawCards, StatDelta),
    Elect (A, ActivePlayer, All, Cost, VariableFromPower),
    EventListener' (TimePoint),
    Requirement (HasBasicLandType, Not),
    StaticAbility
      ( FirstStrike,
        Flying,
        Haste,
        StaticContinuous,
        Suspend
      ),
    Token,
    TriggeredAbility (When),
  )
import safe MtgPure.Model.Selection (Selection (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToManaCost (ToManaCost (toManaCost))
import safe MtgPure.Model.ToManaPool (ToManaPool (toManaPool))
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.Toughness (Toughness (..))
import safe MtgPure.ModelCombinators
  ( AsWithLinkedObject (linked),
    AsWithMaskedObject (masked),
    ElectEffect (effect),
    addManaAnyColor,
    addToBattlefield,
    becomesTapped,
    changeTo,
    colored,
    controllerOf,
    counterAbility,
    counterSpell,
    dealDamage,
    destroy,
    event,
    gain,
    hasAbility,
    ifElse,
    is,
    mkCard,
    mkToken,
    noCost,
    nonBasic,
    ofColors,
    playerPays,
    sacrifice,
    satisfies,
    spellCost,
    tapCost,
    tapped,
    untilEndOfTurn,
  )

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

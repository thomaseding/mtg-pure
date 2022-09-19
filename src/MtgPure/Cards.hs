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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Cards (
  acceptableLosses,
  allIsDust,
  ancestralVision,
  backlash,
  bayou,
  blaze,
  bloodMoon,
  cleanse,
  cityOfBrass,
  conversion,
  damnation,
  fling,
  forest,
  holyStrength,
  island,
  lavaAxe,
  manaLeak,
  mountain,
  nyxbornRollicker,
  ornithopter,
  ragingGoblin,
  plains,
  plummet,
  pollutedDelta,
  pradeshGypsies,
  shock,
  sinkhole,
  snuffOut,
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
) where

import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.CardName (CardName (CardName))
import safe MtgPure.Model.ColorsLike (ColorsLike (toColors))
import safe MtgPure.Model.CreatureType (CreatureType (..))
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.ObjectType (OT3, ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (
  OTActivatedOrTriggeredAbility,
  OTArtifactCreature,
  OTCreature,
  OTCreaturePlayer,
  OTCreaturePlayerPlaneswalker,
  OTEnchantment,
  OTEnchantmentCreature,
  OTInstant,
  OTLand,
  OTPermanent,
  OTPlayerPlaneswalker,
  OTSorcery,
  OTSpell,
 )
import safe MtgPure.Model.ObjectType.NonCreatureCard (WNonCreatureCard (..))
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Recursive (
  Ability (Activated, Static, Triggered),
  Card,
  CardTypeDef (..),
  Condition (COr),
  Cost (AndCosts, DiscardRandomCost, ManaCost, OrCosts, PayLife),
  Effect (CantBeRegenerated, DrawCards, EffectContinuous, StatDelta),
  Elect (A, ActivePlayer, All, CardTypeDef, Cost, VariableFromPower),
  Enchant (Enchant),
  EnchantmentType (Aura),
  EventListener' (TimePoint),
  Requirement (ControlledBy, ControlsA, HasLandType, Not, ROr),
  StaticAbility (Bestow, FirstStrike, Flying, Haste, StaticContinuous, Suspend),
  Token,
  TriggeredAbility (When),
 )
import safe MtgPure.Model.Selection (Selection (..))
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToManaCost (ToManaCost (toManaCost))
import safe MtgPure.Model.ToObjectN.Instances ()
import safe MtgPure.Model.Toughness (Toughness (..))
import safe MtgPure.ModelCombinators (
  AsWithLinkedObject (linked),
  AsWithMaskedObject (masked),
  ElectEffect (effect),
  HasLandType (hasLandType),
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
  ifThenElse,
  is,
  mkCard,
  mkToken,
  noCost,
  nonBasic,
  nonBlack,
  ofColors,
  playerPays,
  putOntoBattlefield,
  sacrifice,
  sacrificeCost,
  satisfies,
  searchLibrary,
  spellCost,
  tapCost,
  tapped,
  untilEndOfTurn,
 )

----------------------------------------

mkBasicLand :: Maybe BasicLandType -> Card OTLand
mkBasicLand mTy = mkCard name $ \_this ->
  CardTypeDef $
    LandDef
      { land_subtypes = case mTy of
          Just ty -> [BasicLand ty]
          Nothing -> []
      , land_abilities = []
      }
 where
  name = CardName $ maybe "Wastes" show mTy

mkDualLand :: String -> BasicLandType -> BasicLandType -> Card OTLand
mkDualLand name ty1 ty2 =
  mkCard (CardName name) $ \_this ->
    CardTypeDef $
      LandDef
        { land_subtypes = [BasicLand ty1, BasicLand ty2]
        , land_abilities = []
        }

mkFetchLand :: String -> BasicLandType -> BasicLandType -> Card OTLand
mkFetchLand name ty1 ty2 = mkCard (CardName name) $ \this ->
  CardTypeDef $
    LandDef
      { land_subtypes = []
      , land_abilities =
          [ Activated
              (Cost $ AndCosts [tapCost [is this], PayLife 1, sacrificeCost [is this]])
              $ controllerOf this $
                \you ->
                  effect $
                    searchLibrary you $
                      linked
                        [ROr [HasLandType $ BasicLand ty1, HasLandType $ BasicLand ty2]]
                        $ \card -> effect $ putOntoBattlefield you card
          ]
      }

----------------------------------------

acceptableLosses :: Card OTSorcery
acceptableLosses = mkCard "Acceptable Losses" $ \this ->
  CardTypeDef $
    let cost = Cost $ AndCosts [ManaCost $ toManaCost (3, R), DiscardRandomCost 1]
     in SorceryDef
          { sorcery_colors = toColors R
          , sorcery_cost = cost
          , sorcery_abilities = []
          , sorcery_effect =
              controllerOf this $ \you ->
                A Target you $
                  masked @OTCreature [] $ \target ->
                    effect $ dealDamage this target 5
          }

allIsDust :: Card OTSorcery
allIsDust = mkCard "All Is Dust" $ \_this ->
  CardTypeDef $
    TribalDef [Eldrazi] WNonCreatureSorcery $
      SorceryDef
        { sorcery_colors = toColors ()
        , sorcery_cost = Cost $ spellCost 7
        , sorcery_abilities = []
        , sorcery_effect =
            All $
              masked [] $
                \player -> All $
                  masked @OTPermanent [ControlledBy player, colored] $
                    \perm ->
                      effect $ sacrifice player [is perm]
        }

ancestralVision :: Card OTSorcery
ancestralVision = mkCard "Ancestral Vision" $ \this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors U
      , sorcery_cost = Cost noCost
      , sorcery_abilities = [Static $ Suspend 4 $ Cost $ spellCost U]
      , sorcery_effect =
          controllerOf this $
            \you -> A Target you $ masked [] $ \target -> effect $ DrawCards target 3
      }

backlash :: Card OTInstant
backlash = mkCard "Backlash" $ \this ->
  CardTypeDef $
    InstantDef
      { instant_colors = toColors (B, R)
      , instant_cost = Cost $ spellCost (1, B, R)
      , instant_abilities = []
      , instant_effect =
          controllerOf this $ \you ->
            A Target you $
              masked [Not tapped] $ \target ->
                VariableFromPower target $ \power ->
                  controllerOf target $ \targetController ->
                    effect $ dealDamage target targetController $ VariableDamage power
      }

bayou :: Card OTLand
bayou = mkDualLand "Bayou" Forest Swamp

birdToken :: Token OTCreature
birdToken = mkToken "Bird Token" $ \_this ->
  CardTypeDef $
    CreatureDef
      { creature_colors = toColors U
      , creature_cost = Cost noCost
      , creature_subtypes = [Bird]
      , creature_power = Power 2
      , creature_toughness = Toughness 2
      , creature_abilities = [Static Flying]
      }

blaze :: Card OTSorcery
blaze = mkCard "Blaze" $ \this ->
  CardTypeDef $
    VariableDef $ \x ->
      SorceryDef
        { sorcery_colors = toColors R
        , sorcery_cost = Cost $ spellCost (VariableGenericMana x, R)
        , sorcery_abilities = []
        , sorcery_effect =
            controllerOf this $ \you ->
              A Target you $
                masked @OTCreaturePlayerPlaneswalker [] $ \target ->
                  effect $ dealDamage this target x
        }

bloodMoon :: Card OTEnchantment
bloodMoon = mkCard "Blood Moon" $ \_this ->
  CardTypeDef $
    EnchantmentDef
      { enchantment_colors = toColors R
      , enchantment_cost = Cost $ spellCost (2, R)
      , enchantment_subtypes = []
      , enchantment_abilities =
          [ Static $
              StaticContinuous $
                All $
                  masked [nonBasic] $ \land ->
                    effect $ changeTo land mountain
          ]
      }

cityOfBrass :: Card OTLand
cityOfBrass = mkCard "City of Brass" $ \this ->
  CardTypeDef $
    LandDef
      { land_subtypes = []
      , land_abilities =
          [ Triggered $
              When $
                event $
                  becomesTapped $
                    linked [is this] $
                      \_ -> controllerOf this $
                        \you -> effect $ dealDamage this you 1
          , Activated (Cost $ tapCost [is this]) $
              controllerOf this $ \you ->
                effect $ addManaAnyColor you 1
          ]
      }

cleanse :: Card OTSorcery
cleanse = mkCard "Cleanse" $ \_this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors W
      , sorcery_cost = Cost $ spellCost (2, W, W)
      , sorcery_abilities = []
      , sorcery_effect =
          All $
            masked @OTCreature [ofColors B] $
              \creature -> effect $ destroy creature
      }

conversion :: Card OTEnchantment
conversion = mkCard "Conversion" $ \this ->
  CardTypeDef $
    EnchantmentDef
      { enchantment_colors = toColors W
      , enchantment_cost = Cost $ spellCost (2, W, W)
      , enchantment_subtypes = []
      , enchantment_abilities =
          [ Triggered $
              When $
                ActivePlayer $ \active -> controllerOf this $ \you ->
                  let cond =
                        COr
                          [ satisfies you [Not $ is active]
                          , satisfies you [is active, playerPays $ toManaCost (W, W)]
                          ]
                   in ifElse cond $
                        event $
                          TimePoint (StepBegin UpkeepStep) $
                            effect $
                              sacrifice you [is this]
          , Static $
              StaticContinuous $
                All $
                  masked [hasLandType Mountain] $ \land ->
                    effect $ changeTo land plains
          ]
      }

damnation :: Card OTSorcery
damnation = mkCard "Damnation" $ \_this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors B
      , sorcery_cost = Cost $ spellCost (2, B, B)
      , sorcery_abilities = []
      , sorcery_effect =
          All $
            masked @OTCreature [] $ \creature ->
              effect $ destroy creature
      }

fling :: Card OTInstant
fling = mkCard "Fling" $ \this ->
  controllerOf this $ \you ->
    A Choose you $
      masked [ControlledBy you] $ \sacChoice ->
        VariableFromPower sacChoice $ \power ->
          CardTypeDef $
            InstantDef
              { instant_colors = toColors R
              , instant_cost = Cost $ AndCosts [spellCost (1, R), sacrificeCost [is sacChoice]]
              , instant_abilities = []
              , instant_effect =
                  A Target you $
                    masked @OTCreaturePlayer [] $ \target ->
                      effect $ dealDamage sacChoice target $ VariableDamage power
              }

forest :: Card OTLand
forest = mkBasicLand $ Just Forest

holyStrength :: Card OTEnchantment
holyStrength = mkCard "Holy Strength" $ \_this ->
  CardTypeDef $
    EnchantmentDef
      { enchantment_colors = toColors W
      , enchantment_cost = Cost $ spellCost W
      , enchantment_subtypes =
          [ Aura $
              Enchant $
                linked [] $
                  \enchanted -> effect $ StatDelta enchanted (Power 1) (Toughness 2)
          ]
      , enchantment_abilities = []
      }

island :: Card OTLand
island = mkBasicLand $ Just Island

lavaAxe :: Card OTSorcery
lavaAxe = mkCard "Lava Axe" $ \this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors R
      , sorcery_cost = Cost $ spellCost (4, R)
      , sorcery_abilities = []
      , sorcery_effect =
          controllerOf this $ \you ->
            A Target you $
              masked @OTPlayerPlaneswalker [] $ \target ->
                effect $ dealDamage this target 5
      }

manaLeak :: Card OTInstant
manaLeak = mkCard "Mana Leak" $ \this ->
  CardTypeDef $
    InstantDef
      { instant_colors = toColors U
      , instant_cost = Cost $ spellCost (1, U)
      , instant_abilities = []
      , instant_effect =
          controllerOf this $ \you ->
            A Target you $
              masked @OTSpell [] $ \spell ->
                controllerOf spell $ \controller ->
                  ifElse (satisfies controller [playerPays $ toManaCost 3]) $
                    effect $ counterSpell spell
      }

mountain :: Card OTLand
mountain = mkBasicLand $ Just Mountain

nyxbornRollicker :: Card OTEnchantmentCreature
nyxbornRollicker = mkCard "Nyxborn Rollicker" $ \_this ->
  CardTypeDef $
    EnchantmentCreatureDef
      { enchantmentCreature_colors = toColors R
      , enchantmentCreature_cost = Cost $ spellCost R
      , enchantmentCreature_creatureTypes = [Satyr]
      , enchantmentCreature_power = Power 1
      , enchantmentCreature_toughness = Toughness 1
      , enchantmentCreature_creatureAbilities = []
      , enchantmentCreature_enchantmentAbilities = []
      , enchantmentCreature_enchantmentCreatureAbilities =
          [ Static $
              Bestow (Cost $ spellCost (1, R)) $
                Enchant $
                  linked [] $
                    \enchanted -> effect $ StatDelta enchanted (Power 1) (Toughness 1)
          ]
      }

ornithopter :: Card OTArtifactCreature
ornithopter = mkCard "Ornithopter" $ \_this ->
  CardTypeDef $
    ArtifactCreatureDef
      { artifactCreature_colors = toColors ()
      , artifactCreature_cost = Cost $ spellCost 0
      , artifactCreature_creatureTypes = []
      , artifactCreature_power = Power 0
      , artifactCreature_toughness = Toughness 2
      , artifactCreature_artifactAbilities = []
      , artifactCreature_creatureAbilities = [Static Flying]
      }

plains :: Card OTLand
plains = mkBasicLand $ Just Plains

plummet :: Card OTInstant
plummet = mkCard "Plummet" $ \this ->
  CardTypeDef $
    InstantDef
      { instant_colors = toColors G
      , instant_cost = Cost $ spellCost (1, G)
      , instant_abilities = []
      , instant_effect =
          controllerOf this $ \you ->
            A Target you $
              masked [hasAbility $ \_this -> Static Flying] $ \target ->
                effect $ destroy target
      }

pollutedDelta :: Card OTLand
pollutedDelta = mkFetchLand "PollutedDelta" Island Swamp

pradeshGypsies :: Card OTCreature
pradeshGypsies = mkCard "Pradesh Gypsies" $ \this ->
  CardTypeDef $
    CreatureDef
      { creature_colors = toColors G
      , creature_cost = Cost $ spellCost (2, G)
      , creature_subtypes = [Human, Nomad]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities =
          [ Activated (Cost $ AndCosts [tapCost [is this], ManaCost $ toManaCost (1, G)]) $
              controllerOf this $
                \you -> A Target you $
                  masked [] $ \creature ->
                    effect $
                      untilEndOfTurn $
                        gain creature $
                          Static $
                            StaticContinuous $
                              effect $
                                StatDelta creature (Power (-2)) (Toughness 0)
          ]
      }

ragingGoblin :: Card OTCreature
ragingGoblin = mkCard "Raging Goblin" $ \_this ->
  CardTypeDef $
    CreatureDef
      { creature_colors = toColors R
      , creature_cost = Cost $ spellCost R
      , creature_subtypes = [Goblin]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities = [Static Haste]
      }

shock :: Card OTInstant
shock = mkCard "Shock" $ \this ->
  CardTypeDef $
    InstantDef
      { instant_colors = toColors R
      , instant_cost = Cost $ spellCost R
      , instant_abilities = []
      , instant_effect =
          controllerOf this $ \you ->
            A Target you $
              masked @OTCreaturePlayerPlaneswalker [] $ \target ->
                effect $ dealDamage this target 2
      }

sinkhole :: Card OTSorcery
sinkhole = mkCard "Sinkhole" $ \this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors B
      , sorcery_cost = Cost $ spellCost (B, B)
      , sorcery_abilities = []
      , sorcery_effect =
          controllerOf this $ \you ->
            A Target you $ masked @OTLand [] $ \target -> effect $ destroy target
      }

snuffOut :: Card OTInstant
snuffOut = mkCard "Snuff Out" $ \this ->
  CardTypeDef $
    InstantDef
      { instant_colors = toColors B
      , instant_cost =
          controllerOf this $
            \you ->
              let cost = ManaCost $ toManaCost (3, B)
               in ifThenElse
                    (satisfies you [ControlsA $ HasLandType $ BasicLand Swamp])
                    (Cost $ OrCosts [PayLife 4, cost])
                    (Cost cost)
      , instant_abilities = []
      , instant_effect =
          controllerOf this $ \you ->
            A Target you $
              masked [nonBlack] $
                \target ->
                  effect
                    [ destroy target
                    , EffectContinuous $ CantBeRegenerated target
                    ]
      }

soldierToken :: Token OTCreature
soldierToken = mkToken "Soldier Token" $ \_this ->
  CardTypeDef $
    CreatureDef
      { creature_colors = toColors W
      , creature_cost = Cost noCost
      , creature_subtypes = [Soldier]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities = []
      }

stifle :: Card OTInstant
stifle = mkCard "Stifle" $ \this ->
  CardTypeDef $
    InstantDef
      { instant_colors = toColors U
      , instant_cost = Cost $ spellCost U
      , instant_abilities = []
      , instant_effect =
          controllerOf this $ \you ->
            A Target you $
              masked @OTActivatedOrTriggeredAbility [] $ \target ->
                effect $ counterAbility target
      }

stoneRain :: Card OTSorcery
stoneRain = mkCard "Stone Rain" $ \this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors R
      , sorcery_cost = Cost $ spellCost (2, R)
      , sorcery_abilities = []
      , sorcery_effect =
          controllerOf this $ \you ->
            A Target you $ masked @OTLand [] $ \target -> effect $ destroy target
      }

stoneThrowingDevils :: Card OTCreature
stoneThrowingDevils = mkCard "Stone-Throwing Devils" $ \_this ->
  CardTypeDef $
    CreatureDef
      { creature_colors = toColors B
      , creature_cost = Cost $ spellCost B
      , creature_subtypes = [Devil]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities = [Static FirstStrike]
      }

swamp :: Card OTLand
swamp = mkBasicLand $ Just Swamp

swanSong :: Card OTInstant
swanSong = mkCard "Swan Song" $ \this ->
  CardTypeDef $
    InstantDef
      { instant_colors = toColors U
      , instant_cost = Cost $ spellCost U
      , instant_abilities = []
      , instant_effect =
          controllerOf this $ \you ->
            A Target you $
              masked @(OT3 'OTEnchantment 'OTInstant 'OTSorcery) [] $
                \target -> controllerOf target $ \controller ->
                  effect [counterSpell target, addToBattlefield controller birdToken]
      }

vindicate :: Card OTSorcery
vindicate = mkCard "Vindicate" $ \this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors (W, B)
      , sorcery_cost = Cost $ spellCost (1, W, B)
      , sorcery_abilities = []
      , sorcery_effect =
          controllerOf this $ \you ->
            A Target you $ masked @OTPermanent [] $ \target -> effect $ destroy target
      }

wastes :: Card OTLand
wastes = mkBasicLand Nothing

wrathOfGod :: Card OTSorcery
wrathOfGod = mkCard "Wrath of God" $ \_this ->
  CardTypeDef $
    SorceryDef
      { sorcery_colors = toColors W
      , sorcery_cost = Cost $ spellCost (2, W, W)
      , sorcery_abilities = []
      , sorcery_effect =
          All $
            masked @OTCreature [] $ \creature ->
              effect $ destroy creature
      }

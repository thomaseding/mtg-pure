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
  Elect (A, ActivePlayer, All, Cost, VariableFromPower),
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

mkBasicLand :: BasicLandType -> Card OTLand
mkBasicLand ty = mkCard name $ \_this ->
  LandDef
    { land_subtypes = [BasicLand ty]
    , land_abilities = []
    }
 where
  name = CardName $ show ty

mkDualLand :: String -> BasicLandType -> BasicLandType -> Card OTLand
mkDualLand name ty1 ty2 =
  mkCard (CardName name) $ \_this ->
    LandDef
      { land_subtypes = [BasicLand ty1, BasicLand ty2]
      , land_abilities = []
      }

mkFetchLand :: String -> BasicLandType -> BasicLandType -> Card OTLand
mkFetchLand name ty1 ty2 = mkCard (CardName name) $ \this ->
  LandDef
    { land_subtypes = []
    , land_abilities =
        [ Activated
            (Cost $ AndCosts [tapCost this, PayLife 1, sacrificeCost [is this]])
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
  TribalDef [Eldrazi] WNonCreatureSorcery $
    SorceryDef
      { sorcery_colors = toColors ()
      , sorcery_cost = cost
      , sorcery_abilities = []
      , sorcery_effect =
          All $
            masked [] $
              \player -> All $
                masked @OTPermanent [ControlledBy player, colored] $
                  \perm ->
                    effect $ sacrifice player [is perm]
      }
 where
  cost = spellCost 7

ancestralVision :: Card OTSorcery
ancestralVision = mkCard "Ancestral Vision" $ \this ->
  SorceryDef
    { sorcery_colors = toColors U
    , sorcery_cost = cost
    , sorcery_abilities = [Static $ Suspend 4 $ spellCost U]
    , sorcery_effect =
        controllerOf this $
          \you -> A Target you $ masked [] $ \target -> effect $ DrawCards target 3
    }
 where
  cost = noCost

backlash :: Card OTInstant
backlash = mkCard "Backlash" $ \this ->
  InstantDef
    { instant_colors = toColors (B, R)
    , instant_cost = cost
    , instant_abilities = []
    , instant_effect =
        controllerOf this $ \you ->
          A Target you $
            masked [Not tapped] $ \target ->
              VariableFromPower target $ \power ->
                controllerOf target $ \targetController ->
                  effect $ dealDamage target targetController $ VariableDamage power
    }
 where
  cost = spellCost (1, B, R)

bayou :: Card OTLand
bayou = mkDualLand "Bayou" Forest Swamp

birdToken :: Token OTCreature
birdToken = mkToken "Bird Token" $ \_this ->
  CreatureDef
    { creature_colors = toColors U
    , creature_cost = cost
    , creature_subtypes = [Bird]
    , creature_power = Power 2
    , creature_toughness = Toughness 2
    , creature_abilities = [Static Flying]
    }
 where
  cost = noCost

blaze :: Card OTSorcery
blaze = mkCard "Blaze" $ \this -> VariableDef $ \x ->
  let cost = spellCost (VariableGenericMana x, R)
   in SorceryDef
        { sorcery_colors = toColors R
        , sorcery_cost = cost
        , sorcery_abilities = []
        , sorcery_effect =
            controllerOf this $ \you ->
              A Target you $
                masked @OTCreaturePlayerPlaneswalker [] $ \target ->
                  effect $ dealDamage this target x
        }

bloodMoon :: Card OTEnchantment
bloodMoon = mkCard "Blood Moon" $ \_this ->
  EnchantmentDef
    { enchantment_colors = toColors R
    , enchantment_cost = cost
    , enchantment_subtypes = []
    , enchantment_abilities =
        [ Static $
            StaticContinuous $
              All $
                masked [nonBasic] $ \land ->
                  effect $ changeTo land mountain
        ]
    }
 where
  cost = spellCost (2, R)

cityOfBrass :: Card OTLand
cityOfBrass = mkCard "City of Brass" $ \this ->
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
        , Activated (Cost $ tapCost this) $
            controllerOf this $ \you ->
              effect $ addManaAnyColor you 1
        ]
    }

cleanse :: Card OTSorcery
cleanse = mkCard "Cleanse" $ \_this ->
  SorceryDef
    { sorcery_colors = toColors W
    , sorcery_cost = cost
    , sorcery_abilities = []
    , sorcery_effect =
        All $
          masked @OTCreature [ofColors B] $
            \creature -> effect $ destroy creature
    }
 where
  cost = spellCost (2, W, W)

conversion :: Card OTEnchantment
conversion = mkCard "Conversion" $ \this ->
  EnchantmentDef
    { enchantment_colors = toColors W
    , enchantment_cost = cost
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
 where
  cost = spellCost (2, W, W)

damnation :: Card OTSorcery
damnation = mkCard "Damnation" $ \_this ->
  SorceryDef
    { sorcery_colors = toColors B
    , sorcery_cost = cost
    , sorcery_abilities = []
    , sorcery_effect =
        All $
          masked @OTCreature [] $ \creature ->
            effect $ destroy creature
    }
 where
  cost = spellCost (2, B, B)

forest :: Card OTLand
forest = mkBasicLand Forest

holyStrength :: Card OTEnchantment
holyStrength = mkCard "Holy Strength" $ \_this ->
  EnchantmentDef
    { enchantment_colors = toColors W
    , enchantment_cost = cost
    , enchantment_subtypes =
        [ Aura $
            Enchant $
              linked [] $
                \enchanted -> effect $ StatDelta enchanted (Power 1) (Toughness 2)
        ]
    , enchantment_abilities = []
    }
 where
  cost = spellCost W

island :: Card OTLand
island = mkBasicLand Island

lavaAxe :: Card OTSorcery
lavaAxe = mkCard "Lava Axe" $ \this ->
  SorceryDef
    { sorcery_colors = toColors R
    , sorcery_cost = cost
    , sorcery_abilities = []
    , sorcery_effect =
        controllerOf this $ \you ->
          A Target you $
            masked @OTPlayerPlaneswalker [] $ \target ->
              effect $ dealDamage this target 5
    }
 where
  cost = spellCost (4, R)

manaLeak :: Card OTInstant
manaLeak = mkCard "Mana Leak" $ \this ->
  InstantDef
    { instant_colors = toColors U
    , instant_cost = cost
    , instant_abilities = []
    , instant_effect =
        controllerOf this $ \you ->
          A Target you $
            masked @OTSpell [] $ \spell ->
              controllerOf spell $ \controller ->
                ifElse (satisfies controller [playerPays $ toManaCost 3]) $
                  effect $ counterSpell spell
    }
 where
  cost = spellCost (1, U)

mountain :: Card OTLand
mountain = mkBasicLand Mountain

nyxbornRollicker :: Card OTEnchantmentCreature
nyxbornRollicker = mkCard "Nyxborn Rollicker" $ \_this ->
  EnchantmentCreatureDef
    { enchantmentCreature_colors = toColors R
    , enchantmentCreature_cost = cost
    , enchantmentCreature_creatureTypes = [Satyr]
    , enchantmentCreature_power = Power 1
    , enchantmentCreature_toughness = Toughness 1
    , enchantmentCreature_creatureAbilities = []
    , enchantmentCreature_enchantmentAbilities = []
    , enchantmentCreature_enchantmentCreatureAbilities =
        [ Static $
            Bestow (spellCost (1, R)) $
              Enchant $
                linked [] $
                  \enchanted -> effect $ StatDelta enchanted (Power 1) (Toughness 1)
        ]
    }
 where
  cost = spellCost R

ornithopter :: Card OTArtifactCreature
ornithopter = mkCard "Ornithopter" $ \_this ->
  ArtifactCreatureDef
    { artifactCreature_colors = toColors ()
    , artifactCreature_cost = cost
    , artifactCreature_creatureTypes = []
    , artifactCreature_power = Power 0
    , artifactCreature_toughness = Toughness 2
    , artifactCreature_artifactAbilities = []
    , artifactCreature_creatureAbilities = [Static Flying]
    }
 where
  cost = spellCost 0

plains :: Card OTLand
plains = mkBasicLand Plains

plummet :: Card OTInstant
plummet = mkCard "Plummet" $ \this ->
  InstantDef
    { instant_colors = toColors G
    , instant_cost = cost
    , instant_abilities = []
    , instant_effect =
        controllerOf this $ \you ->
          A Target you $
            masked [hasAbility $ \_this -> Static Flying] $ \target ->
              effect $ destroy target
    }
 where
  cost = spellCost (1, G)

pollutedDelta :: Card OTLand
pollutedDelta = mkFetchLand "PollutedDelta" Island Swamp

pradeshGypsies :: Card OTCreature
pradeshGypsies = mkCard "Pradesh Gypsies" $ \this ->
  CreatureDef
    { creature_colors = toColors G
    , creature_cost = cost
    , creature_subtypes = [Human, Nomad]
    , creature_power = Power 1
    , creature_toughness = Toughness 1
    , creature_abilities =
        [ Activated (Cost $ AndCosts [tapCost this, ManaCost $ toManaCost (1, G)]) $
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
 where
  cost = spellCost (2, G)

ragingGoblin :: Card OTCreature
ragingGoblin = mkCard "Raging Goblin" $ \_this ->
  CreatureDef
    { creature_colors = toColors R
    , creature_cost = cost
    , creature_subtypes = [Goblin]
    , creature_power = Power 1
    , creature_toughness = Toughness 1
    , creature_abilities = [Static Haste]
    }
 where
  cost = spellCost R

shock :: Card OTInstant
shock = mkCard "Shock" $ \this ->
  InstantDef
    { instant_colors = toColors R
    , instant_cost = cost
    , instant_abilities = []
    , instant_effect =
        controllerOf this $ \you ->
          A Target you $
            masked @OTCreaturePlayerPlaneswalker [] $ \target ->
              effect $ dealDamage this target 2
    }
 where
  cost = spellCost R

sinkhole :: Card OTSorcery
sinkhole = mkCard "Sinkhole" $ \this ->
  SorceryDef
    { sorcery_colors = toColors B
    , sorcery_cost = cost
    , sorcery_abilities = []
    , sorcery_effect =
        controllerOf this $ \you ->
          A Target you $ masked @OTLand [] $ \target -> effect $ destroy target
    }
 where
  cost = spellCost (B, B)

snuffOut :: Card OTInstant
snuffOut = mkCard "Snuff Out" $ \this ->
  let cost' = ManaCost $ toManaCost (3, B)
      cost = controllerOf this $
        \you ->
          ifThenElse
            (satisfies you [ControlsA $ HasLandType $ BasicLand Swamp])
            (Cost $ OrCosts [PayLife 4, cost'])
            (Cost cost')
   in InstantDef
        { instant_colors = toColors B
        , instant_cost = cost
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
  CreatureDef
    { creature_colors = toColors W
    , creature_cost = cost
    , creature_subtypes = [Soldier]
    , creature_power = Power 1
    , creature_toughness = Toughness 1
    , creature_abilities = []
    }
 where
  cost = noCost

stifle :: Card OTInstant
stifle = mkCard "Stifle" $ \this ->
  InstantDef
    { instant_colors = toColors U
    , instant_cost = cost
    , instant_abilities = []
    , instant_effect =
        controllerOf this $ \you ->
          A Target you $
            masked @OTActivatedOrTriggeredAbility [] $ \target ->
              effect $ counterAbility target
    }
 where
  cost = spellCost U

stoneRain :: Card OTSorcery
stoneRain = mkCard "Stone Rain" $ \this ->
  SorceryDef
    { sorcery_colors = toColors R
    , sorcery_cost = cost
    , sorcery_abilities = []
    , sorcery_effect =
        controllerOf this $ \you ->
          A Target you $ masked @OTLand [] $ \target -> effect $ destroy target
    }
 where
  cost = spellCost (2, R)

stoneThrowingDevils :: Card OTCreature
stoneThrowingDevils = mkCard "Stone-Throwing Devils" $ \_this ->
  CreatureDef
    { creature_colors = toColors B
    , creature_cost = cost
    , creature_subtypes = [Devil]
    , creature_power = Power 1
    , creature_toughness = Toughness 1
    , creature_abilities = [Static FirstStrike]
    }
 where
  cost = spellCost B

swamp :: Card OTLand
swamp = mkBasicLand Swamp

swanSong :: Card OTInstant
swanSong = mkCard "Swan Song" $ \this ->
  InstantDef
    { instant_colors = toColors U
    , instant_cost = cost
    , instant_abilities = []
    , instant_effect =
        controllerOf this $ \you ->
          A Target you $
            masked @(OT3 'OTEnchantment 'OTInstant 'OTSorcery) [] $
              \target -> controllerOf target $ \controller ->
                effect [counterSpell target, addToBattlefield controller birdToken]
    }
 where
  cost = spellCost U

vindicate :: Card OTSorcery
vindicate = mkCard "Vindicate" $ \this ->
  SorceryDef
    { sorcery_colors = toColors (W, B)
    , sorcery_cost = cost
    , sorcery_abilities = []
    , sorcery_effect =
        controllerOf this $ \you ->
          A Target you $ masked @OTPermanent [] $ \target -> effect $ destroy target
    }
 where
  cost = spellCost (1, W, B)

wastes :: Card OTLand
wastes = mkBasicLand Wastes

wrathOfGod :: Card OTSorcery
wrathOfGod = mkCard "Wrath of God" $ \_this ->
  SorceryDef
    { sorcery_colors = toColors W
    , sorcery_cost = cost
    , sorcery_abilities = []
    , sorcery_effect =
        All $
          masked @OTCreature [] $ \creature ->
            effect $ destroy creature
    }
 where
  cost = spellCost (2, W, W)

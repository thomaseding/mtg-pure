{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Cards (
  acceptableLosses,
  allIsDust,
  ancestralVision,
  backlash,
  bayou,
  blaze,
  bloodMoon,
  cityOfBrass,
  cleanse,
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

import safe Data.Nat (NatList (..))
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.CardName (CardName (CardName))
import safe MtgPure.Model.ColorsLike (ColorsLike (toColors))
import safe MtgPure.Model.CreatureType (CreatureType (..))
import safe MtgPure.Model.Damage (Damage' (..))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object.OTN (OT3)
import safe MtgPure.Model.Object.OTNAliases (
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
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN.Instances ()
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Recursive (
  Ability (Activated, Static, Triggered),
  ActivatedAbility (..),
  Card (..),
  CardFacet (..),
  Case (CaseFin, caseFin, ofFin),
  Condition (COr),
  Cost (
    AndCosts,
    CostCase,
    DiscardRandomCost,
    ManaCost,
    PayLife
  ),
  Effect (
    AddMana,
    CantBeRegenerated,
    DrawCards,
    EffectContinuous,
    StatDelta,
    WithList
  ),
  Elect (
    ActivePlayer,
    All,
    Choose,
    ChooseOption,
    Cost,
    ElectActivated,
    ElectCard,
    Target,
    VariableFromPower,
    VariableInt
  ),
  Enchant (Enchant),
  EnchantmentType (Aura),
  EventListener' (TimePoint),
  Requirement (
    ControlledBy,
    ControlsA,
    HasLandType,
    Not,
    ROr
  ),
  StaticAbility (
    Bestow,
    FirstStrike,
    Flying,
    Haste,
    StaticContinuous,
    Suspend
  ),
  Token (..),
  TriggeredAbility (When),
  WithList (..),
  YourCard (..),
  pattern CTrue,
 )
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.ToManaCost (ToManaCost (toManaCost))
import safe MtgPure.Model.ToManaPool (ToManaPool (..))
import safe MtgPure.Model.Toughness (Toughness (..))
import safe MtgPure.Model.Zone (Zone (ZBattlefield))
import safe MtgPure.ModelCombinators (
  AsWithLinkedObject (linked),
  AsWithMaskedObject (masked),
  AsWithMaskedObjects (..),
  AsWithThis (..),
  CoPermanent (..),
  ElectEffect (effect),
  HasLandType (hasLandType),
  addManaAnyColor,
  addToBattlefield,
  becomesTapped,
  changeTo,
  chooseAnyColor,
  colored,
  controllerOf,
  counterAbility,
  counterSpell,
  dealDamage,
  destroy,
  event,
  gainAbility,
  hasAbility,
  ifElse,
  is,
  isTapped,
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
  untilEndOfTurn,
 )

----------------------------------------

mkBasicLand :: Maybe BasicLandType -> Card OTLand
mkBasicLand mTy = Card name $
  YourLand \_you ->
    LandFacet
      { land_creatureTypes = []
      , land_landTypes = case mTy of
          Just ty -> [BasicLand ty]
          Nothing -> []
      , land_abilities =
          -- TODO: Synthesize basic land mana abilities inside the engine instead of explicitly making here.
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  ElectActivated $
                    Ability
                      { activated_cost = tapCost [is this]
                      , activated_effect = effect $ AddMana you case mTy of
                          Just Plains -> toManaPool W
                          Just Island -> toManaPool U
                          Just Swamp -> toManaPool B
                          Just Mountain -> toManaPool R
                          Just Forest -> toManaPool G
                          Nothing -> toManaPool C
                      }
          ]
      }
 where
  name = CardName $ maybe "Wastes" show mTy

mkDualLand :: String -> BasicLandType -> BasicLandType -> Card OTLand
mkDualLand name ty1 ty2 =
  Card (CardName name) $
    YourLand \_you ->
      LandFacet
        { land_creatureTypes = []
        , land_landTypes = [BasicLand ty1, BasicLand ty2]
        , land_abilities = []
        }

mkFetchLand :: String -> BasicLandType -> BasicLandType -> Card OTLand
mkFetchLand name ty1 ty2 = Card (CardName name) $
  YourLand \_you ->
    LandFacet
      { land_creatureTypes = []
      , land_landTypes = []
      , land_abilities =
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  ElectActivated $
                    Ability
                      { activated_cost =
                          AndCosts
                            [ tapCost [is this]
                            , PayLife 1
                            , sacrificeCost [is this]
                            ]
                      , activated_effect = effect $
                          searchLibrary you $
                            linked
                              [ROr [HasLandType $ BasicLand ty1, HasLandType $ BasicLand ty2]]
                              \card -> effect $ putOntoBattlefield you card
                      }
          ]
      }

----------------------------------------

acceptableLosses :: Card OTSorcery
acceptableLosses = Card "Acceptable Losses" $
  YourSorcery \you ->
    Target you $ masked @OTCreature [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors R
          , sorcery_cost =
              AndCosts
                [ ManaCost $ toManaCost (3, R)
                , DiscardRandomCost 1
                ]
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \this ->
              effect $ dealDamage this target 5
          }

allIsDust :: Card OTSorcery
allIsDust = Card "All Is Dust" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors ()
        , sorcery_cost = spellCost 7
        , sorcery_creatureTypes = [Eldrazi]
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds [] \players ->
              All $ maskeds @OTPermanent [colored] \perms ->
                effect $
                  WithList $ Each players \player ->
                    WithList $
                      SuchThat [ControlledBy player] $ Each perms \perm ->
                        sacrifice player [is perm]
        }

ancestralVision :: Card OTSorcery
ancestralVision = Card "Ancestral Vision" $
  YourSorcery \you ->
    Target you $ masked [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors U
          , sorcery_cost = noCost
          , sorcery_creatureTypes = []
          , sorcery_abilities = [Static $ Suspend 4 $ Cost $ spellCost U]
          , sorcery_effect = thisObject \_this ->
              effect $ DrawCards target 3
          }

backlash :: Card OTInstant
backlash = Card "Backlash" $
  YourInstant \you ->
    Target you $ masked [Not isTapped] \target ->
      controllerOf target \targetController ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors (B, R)
            , instant_cost = spellCost (1, B, R)
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                VariableFromPower target \power ->
                  effect $
                    dealDamage target targetController $ VariableDamage power
            }

bayou :: Card OTLand
bayou = mkDualLand "Bayou" Forest Swamp

birdToken :: Token OTCreature
birdToken = Token coPermanent $
  Card "Bird Token" $
    YourCreature \_you ->
      CreatureFacet
        { creature_colors = toColors U
        , creature_cost = noCost
        , creature_creatureTypes = [Bird]
        , creature_power = Power 2
        , creature_toughness = Toughness 2
        , creature_abilities = [Static Flying]
        }

blaze :: Card OTSorcery
blaze = Card "Blaze" $
  YourSorcery \you ->
    VariableInt \x ->
      Target you $ masked @OTCreaturePlayerPlaneswalker [] \target ->
        ElectCard $
          SorceryFacet
            { sorcery_colors = toColors R
            , sorcery_cost = spellCost (VariableGenericMana x, R)
            , sorcery_creatureTypes = []
            , sorcery_abilities = []
            , sorcery_effect = thisObject \this ->
                effect $ dealDamage this target x
            }

bloodMoon :: Card OTEnchantment
bloodMoon = Card "Blood Moon" $
  YourEnchantment \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors R
      , enchantment_cost = spellCost (2, R)
      , enchantment_creatureTypes = []
      , enchantment_enchantmentTypes = []
      , enchantment_abilities =
          [ Static $
              StaticContinuous $
                All $ maskeds [nonBasic] \lands ->
                  effect $
                    WithList $ Each lands \land ->
                      changeTo land mountain
          ]
      }

cityOfBrass :: Card OTLand
cityOfBrass = Card "City of Brass" $
  YourLand \_you ->
    LandFacet
      { land_creatureTypes = []
      , land_landTypes = []
      , land_abilities =
          [ Triggered $
              thisObject \this ->
                When $
                  event $
                    becomesTapped $ linked [is this] \_ ->
                      controllerOf this \you ->
                        effect $ dealDamage this you 1
          , Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  chooseAnyColor you \color ->
                    ElectActivated $
                      Ability
                        { activated_cost = tapCost [is this]
                        , activated_effect = effect $ addManaAnyColor color you 1
                        }
          ]
      }

cleanse :: Card OTSorcery
cleanse = Card "Cleanse" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors W
        , sorcery_cost = spellCost (2, W, W)
        , sorcery_creatureTypes = []
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds @OTCreature [ofColors B] \creatures ->
              effect $
                WithList $ Each creatures \creature ->
                  destroy creature
        }

conversion :: Card OTEnchantment
conversion = Card "Conversion" $
  YourEnchantment \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors W
      , enchantment_cost = spellCost (2, W, W)
      , enchantment_creatureTypes = []
      , enchantment_enchantmentTypes = []
      , enchantment_abilities =
          [ Triggered $
              thisObject \this ->
                When $
                  ActivePlayer \active ->
                    controllerOf this \you ->
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
                All $ maskeds [hasLandType Mountain] \lands ->
                  effect $
                    WithList $ Each lands \land ->
                      changeTo land plains
          ]
      }

damnation :: Card OTSorcery
damnation = Card "Damnation" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors B
        , sorcery_cost = spellCost (2, B, B)
        , sorcery_creatureTypes = []
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds @OTCreature [] \creatures ->
              effect $
                WithList $ Each creatures \creature ->
                  destroy creature
        }

fling :: Card OTInstant
fling = Card "Fling" $
  YourInstant \you ->
    Choose you $ masked [ControlledBy you] \sacChoice ->
      Target you $ masked @OTCreaturePlayer [] \target ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors R
            , instant_cost =
                AndCosts
                  [ spellCost (1, R)
                  , sacrificeCost [is sacChoice]
                  ]
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                VariableFromPower sacChoice \power ->
                  effect $
                    dealDamage sacChoice target $ VariableDamage power
            }

forest :: Card OTLand
forest = mkBasicLand $ Just Forest

holyStrength :: Card OTEnchantment
holyStrength = Card "Holy Strength" $
  YourEnchantment \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors W
      , enchantment_cost = spellCost W
      , enchantment_creatureTypes = []
      , enchantment_enchantmentTypes =
          [ Aura $
              Enchant $ linked [] \enchanted ->
                effect $ StatDelta enchanted (Power 1) (Toughness 2)
          ]
      , enchantment_abilities = []
      }

island :: Card OTLand
island = mkBasicLand $ Just Island

lavaAxe :: Card OTSorcery
lavaAxe = Card "Lava Axe" $
  YourSorcery \you ->
    Target you $ masked @OTPlayerPlaneswalker [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors R
          , sorcery_cost = spellCost (4, R)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \this ->
              effect $ dealDamage this target 5
          }

manaLeak :: Card OTInstant
manaLeak = Card "Mana Leak" $
  YourInstant \you ->
    Target you $ masked @OTSpell [] \spell ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors U
          , instant_cost = spellCost (1, U)
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              controllerOf spell \controller ->
                ifElse (satisfies controller [playerPays $ toManaCost 3]) $
                  effect $ counterSpell spell
          }

mountain :: Card OTLand
mountain = mkBasicLand $ Just Mountain

nyxbornRollicker :: Card OTEnchantmentCreature
nyxbornRollicker = Card "Nyxborn Rollicker" $
  YourEnchantmentCreature \_you ->
    EnchantmentCreatureFacet
      { enchantmentCreature_colors = toColors R
      , enchantmentCreature_cost = spellCost R
      , enchantmentCreature_creatureTypes = [Satyr]
      , enchantmentCreature_power = Power 1
      , enchantmentCreature_toughness = Toughness 1
      , enchantmentCreature_creatureAbilities = []
      , enchantmentCreature_enchantmentAbilities = []
      , enchantmentCreature_enchantmentCreatureAbilities =
          [ Static $
              Bestow (Cost $ spellCost (1, R)) $
                Enchant $ linked [] \enchanted ->
                  effect $ StatDelta enchanted (Power 1) (Toughness 1)
          ]
      }

ornithopter :: Card OTArtifactCreature
ornithopter = Card "Ornithopter" $
  YourArtifactCreature \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors ()
      , artifactCreature_cost = spellCost 0
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = []
      , artifactCreature_power = Power 0
      , artifactCreature_toughness = Toughness 2
      , artifactCreature_artifactAbilities = []
      , artifactCreature_creatureAbilities = [Static Flying]
      }

plains :: Card OTLand
plains = mkBasicLand $ Just Plains

plummet :: Card OTInstant
plummet = Card "Plummet" $
  YourInstant \you ->
    Target you $ masked [hasAbility \_this -> Static Flying] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors G
          , instant_cost = spellCost (1, G)
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              effect $ destroy target
          }

pollutedDelta :: Card OTLand
pollutedDelta = mkFetchLand "PollutedDelta" Island Swamp

pradeshGypsies :: Card OTCreature
pradeshGypsies = Card "Pradesh Gypsies" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_cost = spellCost (2, G)
      , creature_creatureTypes = [Human, Nomad]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities =
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  Target you $ masked [] \creature ->
                    ElectActivated $
                      Ability
                        { activated_cost =
                            AndCosts
                              [ tapCost [is this]
                              , ManaCost $ toManaCost (1, G)
                              ]
                        , activated_effect =
                            effect $
                              untilEndOfTurn $
                                gainAbility creature $
                                  Static $
                                    StaticContinuous $
                                      effect $
                                        StatDelta creature (Power (-2)) (Toughness 0)
                        }
          ]
      }

ragingGoblin :: Card OTCreature
ragingGoblin = Card "Raging Goblin" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors R
      , creature_cost = spellCost R
      , creature_creatureTypes = [Goblin]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities = [Static Haste]
      }

shock :: Card OTInstant
shock = Card "Shock" $
  YourInstant \you ->
    Target you $ masked @OTCreaturePlayerPlaneswalker [] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors R
          , instant_cost = spellCost R
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \this ->
              effect $ dealDamage this target 2
          }

sinkhole :: Card OTSorcery
sinkhole = Card "Sinkhole" $
  YourSorcery \you ->
    Target you $ masked @OTLand [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors B
          , sorcery_cost = spellCost (B, B)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \_this ->
              effect $ destroy target
          }

snuffOut :: Card OTInstant
snuffOut = Card "Snuff Out" $
  YourInstant \you ->
    ChooseOption
      you
      ( LS CTrue $
          LZ (satisfies you [ControlsA $ HasLandType $ BasicLand Swamp])
      )
      \option ->
        Target you $ masked [nonBlack] \target ->
          ElectCard $
            InstantFacet
              { instant_colors = toColors B
              , instant_cost =
                  CostCase
                    CaseFin
                      { caseFin = option
                      , ofFin =
                          LS (spellCost (3, B)) $
                            LZ @() $ PayLife 4
                      }
              , instant_creatureTypes = []
              , instant_abilities = []
              , instant_effect = thisObject \_this ->
                  effect
                    [ destroy target
                    , EffectContinuous $ CantBeRegenerated target
                    ]
              }

soldierToken :: Token OTCreature
soldierToken = Token coPermanent $
  Card "Soldier Token" $
    YourCreature \_you ->
      CreatureFacet
        { creature_colors = toColors W
        , creature_cost = noCost
        , creature_creatureTypes = [Soldier]
        , creature_power = Power 1
        , creature_toughness = Toughness 1
        , creature_abilities = []
        }

stifle :: Card OTInstant
stifle = Card "Stifle" $
  YourInstant \you ->
    Target you $ masked @OTActivatedOrTriggeredAbility [] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors U
          , instant_cost = spellCost U
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              effect $ counterAbility target
          }

stoneRain :: Card OTSorcery
stoneRain = Card "Stone Rain" $
  YourSorcery \you ->
    Target you $ masked @OTLand [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors R
          , sorcery_cost = spellCost (2, R)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \_this ->
              effect $ destroy target
          }

stoneThrowingDevils :: Card OTCreature
stoneThrowingDevils = Card "Stone-Throwing Devils" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors B
      , creature_cost = spellCost B
      , creature_creatureTypes = [Devil]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities = [Static FirstStrike]
      }

swamp :: Card OTLand
swamp = mkBasicLand $ Just Swamp

swanSong :: Card OTInstant
swanSong = Card "Swan Song" $
  YourInstant \you ->
    Target you $ masked @(OT3 'OTEnchantment 'OTInstant 'OTSorcery) [] \target ->
      controllerOf target \controller ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors U
            , instant_cost = spellCost U
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                effect
                  [ counterSpell target
                  , addToBattlefield controller birdToken
                  ]
            }

vindicate :: Card OTSorcery
vindicate = Card "Vindicate" $
  YourSorcery \you ->
    Target you $ masked @OTPermanent [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors (W, B)
          , sorcery_cost = spellCost (1, W, B)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \_this ->
              effect $ destroy target
          }

wastes :: Card OTLand
wastes = mkBasicLand Nothing

wrathOfGod :: Card OTSorcery
wrathOfGod = Card "Wrath of God" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors W
        , sorcery_cost = spellCost (2, W, W)
        , sorcery_creatureTypes = []
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds @OTCreature [] \creatures ->
              effect $
                WithList $ Each creatures \creature ->
                  destroy creature
        }

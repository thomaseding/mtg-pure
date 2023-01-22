{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use camelCase" #-}

module MtgPure.Cards (
  acceptableLosses,
  ancestralRecall,
  allIsDust,
  ancestralVision,
  backlash,
  bayou,
  blackLotus,
  blaze,
  bloodMoon,
  braidwoodCup,
  cityOfBrass,
  cleanse,
  conversion,
  counterspell,
  damnation,
  darkRitual,
  deathriteShaman,
  divination,
  fling,
  forest,
  grizzlyBears,
  holyStrength,
  island,
  lavaAxe,
  lightningBolt,
  llanowarElves,
  manaLeak,
  mountain,
  moxEmerald,
  moxJet,
  moxPearl,
  moxRuby,
  moxSapphire,
  nyxbornRollicker,
  ornithopter,
  ragingGoblin,
  plains,
  plummet,
  pollutedDelta,
  pradeshGypsies,
  shatter,
  shock,
  sinkhole,
  snuffOut,
  stifle,
  stoneRain,
  stoneThrowingDevils,
  swamp,
  swanSong,
  unholyStrength,
  vindicate,
  wastes,
  wear_tear,
  witchEngine,
  wrathOfGod,
  --
  birdToken,
  soldierToken,
) where

import safe Data.Nat (NatList (..))
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.CardName (CardName (CardName))
import safe MtgPure.Model.ColorsLike (ColorsLike (toColors))
import safe MtgPure.Model.Combinators (
  AsWithLinkedObject (linked),
  AsWithMaskedObject (masked),
  AsWithMaskedObjects (..),
  AsWithThis (..),
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
  gainControl,
  hasAbility,
  ifElse,
  is,
  isTapped,
  manaCostOf,
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
  swampwalk,
  tapCost,
  untilEndOfTurn,
 )
import safe MtgPure.Model.CreatureType (CreatureType (..))
import safe MtgPure.Model.Damage (Damage' (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Mana.Mana (Mana (VariableMana))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Mana.ToManaCost (ToManaCost (toManaCost))
import safe MtgPure.Model.Mana.ToManaPool (ToManaPool (..))
import safe MtgPure.Model.Object.OTN (OT2, OT3)
import safe MtgPure.Model.Object.OTNAliases (
  OTNActivatedOrTriggeredAbility,
  OTNArtifact,
  OTNArtifactCreature,
  OTNCreature,
  OTNCreaturePlayer,
  OTNCreaturePlayerPlaneswalker,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNInstant,
  OTNLand,
  OTNPermanent,
  OTNPlayer,
  OTNPlayerPlaneswalker,
  OTNSorcery,
  OTNSpell,
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
  Cost (..),
  Effect (
    AddMana,
    CantBeRegenerated,
    DrawCards,
    EffectContinuous,
    GainLife,
    LoseLife,
    Sequence,
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
    IsOpponentOf,
    Not,
    ROr
  ),
  StaticAbility (
    Bestow,
    FirstStrike,
    Flying,
    Fuse,
    Haste,
    StaticContinuous,
    Suspend
  ),
  Token (..),
  TriggeredAbility (When),
  WithList (..),
  YourCardFacet (..),
  pattern CTrue,
 )
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness (..))
import safe MtgPure.Model.Zone (Zone (..))

----------------------------------------

mkBasicLand :: BasicLandType -> Card OTNLand
mkBasicLand ty = Card name $
  YourLand \_you ->
    LandFacet
      { land_creatureTypes = []
      , land_landTypes = [BasicLand ty]
      , land_abilities = []
      }
 where
  name = CardName $ show ty

mkDualLand :: CardName -> BasicLandType -> BasicLandType -> Card OTNLand
mkDualLand name ty1 ty2 = Card name $
  YourLand \_you ->
    LandFacet
      { land_creatureTypes = []
      , land_landTypes = [BasicLand ty1, BasicLand ty2]
      , land_abilities = []
      }

mkFetchLand :: CardName -> BasicLandType -> BasicLandType -> Card OTNLand
mkFetchLand name ty1 ty2 = Card name $
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
                          searchLibrary you you $
                            linked
                              [ROr [HasLandType $ BasicLand ty1, HasLandType $ BasicLand ty2]]
                              \card -> effect $ putOntoBattlefield you card
                      }
          ]
      }

mkMox :: ToManaPool 'NonSnow (ManaSymbol mt) => CardName -> ManaSymbol mt -> Card OTNArtifact
mkMox name sym = Card name $
  YourArtifact \_you ->
    ArtifactFacet
      { artifact_colors = toColors ()
      , artifact_cost = manaCostOf 0
      , artifact_artifactTypes = []
      , artifact_creatureTypes = []
      , artifact_abilities =
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  ElectActivated $
                    Ability
                      { activated_cost = tapCost [is this]
                      , activated_effect = effect $ AddMana you $ toManaPool sym
                      }
          ]
      }

----------------------------------------

acceptableLosses :: Card OTNSorcery
acceptableLosses = Card "Acceptable Losses" $
  YourSorcery \you ->
    Target you $ masked @OTNCreature [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors R
          , sorcery_cost =
              AndCosts
                [ manaCostOf (3, R)
                , DiscardRandomCost 1
                ]
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \this ->
              effect $ dealDamage this target 5
          }

allIsDust :: Card OTNSorcery
allIsDust = Card "All Is Dust" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors ()
        , sorcery_cost = manaCostOf 7
        , sorcery_creatureTypes = [Eldrazi]
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds [] \players ->
              All $ maskeds @OTNPermanent [colored] \perms ->
                effect $
                  WithList $ Each players \player ->
                    WithList $
                      SuchThat [ControlledBy player] $ Each perms \perm ->
                        sacrifice player [is perm]
        }

ancestralRecall :: Card OTNInstant
ancestralRecall = Card "Ancestral Recall" $
  YourInstant \you ->
    ElectCard $
      InstantFacet
        { instant_colors = toColors U
        , instant_cost = noCost
        , instant_creatureTypes = []
        , instant_abilities = []
        , instant_effect = thisObject \_this ->
            effect $ DrawCards you 3
        }

ancestralVision :: Card OTNSorcery
ancestralVision = Card "Ancestral Vision" $
  YourSorcery \you ->
    Target you $ masked [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors U
          , sorcery_cost = noCost
          , sorcery_creatureTypes = []
          , sorcery_abilities = [Static $ Suspend 4 $ Cost $ manaCostOf U]
          , sorcery_effect = thisObject \_this ->
              effect $ DrawCards target 3
          }

backlash :: Card OTNInstant
backlash = Card "Backlash" $
  YourInstant \you ->
    Target you $ masked [Not isTapped] \target ->
      controllerOf target \targetController ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors (B, R)
            , instant_cost = manaCostOf (1, B, R)
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                VariableFromPower target \power ->
                  effect $
                    dealDamage target targetController $ VariableDamage power
            }

bayou :: Card OTNLand
bayou = mkDualLand "Bayou" Forest Swamp

birdToken :: Token OTNCreature
birdToken = Token $
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

blackLotus :: Card OTNArtifact
blackLotus = Card "Black Lotus" $
  YourArtifact \_you ->
    ArtifactFacet
      { artifact_colors = toColors ()
      , artifact_cost = manaCostOf 0
      , artifact_artifactTypes = []
      , artifact_creatureTypes = []
      , artifact_abilities =
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  chooseAnyColor you \color ->
                    ElectActivated $
                      Ability
                        { activated_cost =
                            AndCosts
                              [ tapCost [is this]
                              , sacrificeCost [is this]
                              ]
                        , activated_effect = effect $ addManaAnyColor color you 3
                        }
          ]
      }

blaze :: Card OTNSorcery
blaze = Card "Blaze" $
  YourSorcery \you ->
    VariableInt \x ->
      Target you $ masked @OTNCreaturePlayerPlaneswalker [] \target ->
        ElectCard $
          SorceryFacet
            { sorcery_colors = toColors R
            , sorcery_cost = manaCostOf (VariableMana @ 'NonSnow @ 'MTGeneric x, R)
            , sorcery_creatureTypes = []
            , sorcery_abilities = []
            , sorcery_effect = thisObject \this ->
                effect $ dealDamage this target x
            }

bloodMoon :: Card OTNEnchantment
bloodMoon = Card "Blood Moon" $
  YourEnchantment \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors R
      , enchantment_cost = manaCostOf (2, R)
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

braidwoodCup :: Card OTNArtifact
braidwoodCup = Card "Braidwood Cup" $
  YourArtifact \_you ->
    ArtifactFacet
      { artifact_colors = toColors ()
      , artifact_cost = manaCostOf 3
      , artifact_artifactTypes = []
      , artifact_creatureTypes = []
      , artifact_abilities =
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  ElectActivated $
                    Ability
                      { activated_cost = tapCost [is this]
                      , activated_effect = effect $ GainLife you 1
                      }
          ]
      }

counterspell :: Card OTNInstant
counterspell = Card "Counterspell" $
  YourInstant \you ->
    Target you $ masked @OTNSpell [] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors U
          , instant_cost = manaCostOf (U, U)
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              effect $ counterSpell target
          }

cityOfBrass :: Card OTNLand
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
                  ElectActivated $
                    Ability
                      { activated_cost = tapCost [is this]
                      , activated_effect =
                          chooseAnyColor you \color ->
                            effect $ addManaAnyColor color you 1
                      }
          ]
      }

cleanse :: Card OTNSorcery
cleanse = Card "Cleanse" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors W
        , sorcery_cost = manaCostOf (2, W, W)
        , sorcery_creatureTypes = []
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds @OTNCreature [ofColors B] \creatures ->
              effect $
                WithList $ Each creatures \creature ->
                  destroy creature
        }

conversion :: Card OTNEnchantment
conversion = Card "Conversion" $
  YourEnchantment \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors W
      , enchantment_cost = manaCostOf (2, W, W)
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

damnation :: Card OTNSorcery
damnation = Card "Damnation" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors B
        , sorcery_cost = manaCostOf (2, B, B)
        , sorcery_creatureTypes = []
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds @OTNCreature [] \creatures ->
              effect $
                WithList $ Each creatures \creature ->
                  Sequence
                    [ destroy creature
                    , EffectContinuous $ CantBeRegenerated creature
                    ]
        }

darkRitual :: Card OTNInstant
darkRitual = Card "Dark Ritual" $
  YourInstant \you ->
    ElectCard $
      InstantFacet
        { instant_colors = toColors B
        , instant_cost = manaCostOf B
        , instant_creatureTypes = []
        , instant_abilities = []
        , instant_effect = thisObject \_this ->
            effect $ AddMana you $ toManaPool (B, B, B)
        }

deathriteShaman :: Card OTNCreature
deathriteShaman = Card "Deathrite Shaman" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors (B, G)
      , creature_cost = manaCostOf BG
      , creature_creatureTypes = [Elf, Shaman]
      , creature_power = Power 1
      , creature_toughness = Toughness 2
      , creature_abilities =
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  Target you $ masked @OTNLand @ 'ZGraveyard [] \land ->
                    ElectActivated $
                      Ability
                        { activated_cost =
                            AndCosts
                              [ tapCost [is this]
                              , ExileCost [is land]
                              ]
                        , activated_effect =
                            chooseAnyColor you \color ->
                              effect $ addManaAnyColor color you 1
                        }
          , Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  Target you $ masked @(OT2 'OTInstant 'OTSorcery) @ 'ZGraveyard [] \spell ->
                    ElectActivated $
                      Ability
                        { activated_cost =
                            AndCosts
                              [ manaCostOf B
                              , tapCost [is this]
                              , ExileCost [is spell]
                              ]
                        , activated_effect =
                            All $ maskeds @OTNPlayer [IsOpponentOf you] \opponents ->
                              effect $
                                WithList $ Each opponents \opponent ->
                                  LoseLife opponent 2
                        }
          , Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  Target you $ masked @OTNCreature @ 'ZGraveyard [] \creature ->
                    ElectActivated $
                      Ability
                        { activated_cost =
                            AndCosts
                              [ manaCostOf G
                              , tapCost [is this]
                              , ExileCost [is creature]
                              ]
                        , activated_effect = effect $ GainLife you 2
                        }
          ]
      }

divination :: Card OTNSorcery
divination = Card "Divination" $
  YourSorcery \you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors U
        , sorcery_cost = manaCostOf (2, U)
        , sorcery_creatureTypes = []
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            effect $ DrawCards you 2
        }

fling :: Card OTNInstant
fling = Card "Fling" $
  YourInstant \you ->
    Choose you $ masked [ControlledBy you] \sacChoice ->
      Target you $ masked @OTNCreaturePlayer [] \target ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors R
            , instant_cost =
                AndCosts
                  [ manaCostOf (1, R)
                  , sacrificeCost [is sacChoice]
                  ]
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                VariableFromPower sacChoice \power ->
                  effect $
                    dealDamage sacChoice target $ VariableDamage power
            }

forest :: Card OTNLand
forest = mkBasicLand Forest

grizzlyBears :: Card OTNCreature
grizzlyBears = Card "Grizzly Bears" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_cost = manaCostOf (1, G)
      , creature_creatureTypes = [Bear]
      , creature_power = Power 2
      , creature_toughness = Toughness 2
      , creature_abilities = []
      }

holyStrength :: Card OTNEnchantment
holyStrength = Card "Holy Strength" $
  YourEnchantment \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors W
      , enchantment_cost = manaCostOf W
      , enchantment_creatureTypes = []
      , enchantment_enchantmentTypes =
          [ Aura $
              Enchant $ linked [] \enchanted ->
                effect $ StatDelta enchanted (Power 1) (Toughness 2)
          ]
      , enchantment_abilities = []
      }

island :: Card OTNLand
island = mkBasicLand Island

llanowarElves :: Card OTNCreature
llanowarElves = Card "Llanowar Elves" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_cost = manaCostOf G
      , creature_creatureTypes = [Elf]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities =
          [ Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  ElectActivated $
                    Ability
                      { activated_cost = tapCost [is this]
                      , activated_effect = effect $ AddMana you $ toManaPool G
                      }
          ]
      }

lavaAxe :: Card OTNSorcery
lavaAxe = Card "Lava Axe" $
  YourSorcery \you ->
    Target you $ masked @OTNPlayerPlaneswalker [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors R
          , sorcery_cost = manaCostOf (4, R)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \this ->
              effect $ dealDamage this target 5
          }

lightningBolt :: Card OTNInstant
lightningBolt = Card "Lightning Bolt" $
  YourInstant \you ->
    Target you $ masked @OTNCreaturePlayerPlaneswalker [] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors R
          , instant_cost = manaCostOf R
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \this ->
              effect $ dealDamage this target 3
          }

manaLeak :: Card OTNInstant
manaLeak = Card "Mana Leak" $
  YourInstant \you ->
    Target you $ masked @OTNSpell [] \spell ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors U
          , instant_cost = manaCostOf (1, U)
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              controllerOf spell \controller ->
                ifElse (satisfies controller [playerPays $ toManaCost 3]) $
                  effect $ counterSpell spell
          }

mountain :: Card OTNLand
mountain = mkBasicLand Mountain

moxEmerald :: Card OTNArtifact
moxEmerald = mkMox "Mox Emerald" G

moxJet :: Card OTNArtifact
moxJet = mkMox "Mox Jet" B

moxPearl :: Card OTNArtifact
moxPearl = mkMox "Mox Pearl" W

moxRuby :: Card OTNArtifact
moxRuby = mkMox "Mox Ruby" R

moxSapphire :: Card OTNArtifact
moxSapphire = mkMox "Mox Sapphire" U

nyxbornRollicker :: Card OTNEnchantmentCreature
nyxbornRollicker = Card "Nyxborn Rollicker" $
  YourEnchantmentCreature \_you ->
    EnchantmentCreatureFacet
      { enchantmentCreature_colors = toColors R
      , enchantmentCreature_cost = manaCostOf R
      , enchantmentCreature_creatureTypes = [Satyr]
      , enchantmentCreature_enchantmentTypes = []
      , enchantmentCreature_power = Power 1
      , enchantmentCreature_toughness = Toughness 1
      , enchantmentCreature_creatureAbilities = []
      , enchantmentCreature_enchantmentAbilities = []
      , enchantmentCreature_enchantmentCreatureAbilities =
          [ Static $
              Bestow (Cost $ manaCostOf (1, R)) $
                Enchant $ linked [] \enchanted ->
                  effect $ StatDelta enchanted (Power 1) (Toughness 1)
          ]
      }

ornithopter :: Card OTNArtifactCreature
ornithopter = Card "Ornithopter" $
  YourArtifactCreature \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors ()
      , artifactCreature_cost = manaCostOf 0
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = []
      , artifactCreature_power = Power 0
      , artifactCreature_toughness = Toughness 2
      , artifactCreature_artifactAbilities = []
      , artifactCreature_creatureAbilities = [Static Flying]
      , artifactCreature_artifactCreatureAbilities = []
      }

plains :: Card OTNLand
plains = mkBasicLand Plains

plummet :: Card OTNInstant
plummet = Card "Plummet" $
  YourInstant \you ->
    Target you $ masked [hasAbility \_this -> Static Flying] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors G
          , instant_cost = manaCostOf (1, G)
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              effect $ destroy target
          }

pollutedDelta :: Card OTNLand
pollutedDelta = mkFetchLand "PollutedDelta" Island Swamp

pradeshGypsies :: Card OTNCreature
pradeshGypsies = Card "Pradesh Gypsies" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_cost = manaCostOf (2, G)
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
                              [ manaCostOf (1, G)
                              , tapCost [is this]
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

ragingGoblin :: Card OTNCreature
ragingGoblin = Card "Raging Goblin" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors R
      , creature_cost = manaCostOf R
      , creature_creatureTypes = [Goblin]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities = [Static Haste]
      }

shatter :: Card OTNInstant
shatter = Card "Shatter" $
  YourInstant \you ->
    Target you $ masked @OTNArtifact [] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors R
          , instant_cost = manaCostOf (1, R)
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              effect $ destroy target
          }

shock :: Card OTNInstant
shock = Card "Shock" $
  YourInstant \you ->
    Target you $ masked @OTNCreaturePlayerPlaneswalker [] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors R
          , instant_cost = manaCostOf R
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \this ->
              effect $ dealDamage this target 2
          }

sinkhole :: Card OTNSorcery
sinkhole = Card "Sinkhole" $
  YourSorcery \you ->
    Target you $ masked @OTNLand [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors B
          , sorcery_cost = manaCostOf (B, B)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \_this ->
              effect $ destroy target
          }

snuffOut :: Card OTNInstant
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
                          LS (manaCostOf (3, B)) $
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

soldierToken :: Token OTNCreature
soldierToken = Token $
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

stifle :: Card OTNInstant
stifle = Card "Stifle" $
  YourInstant \you ->
    Target you $ masked @OTNActivatedOrTriggeredAbility [] \target ->
      ElectCard $
        InstantFacet
          { instant_colors = toColors U
          , instant_cost = manaCostOf U
          , instant_creatureTypes = []
          , instant_abilities = []
          , instant_effect = thisObject \_this ->
              effect $ counterAbility target
          }

stoneRain :: Card OTNSorcery
stoneRain = Card "Stone Rain" $
  YourSorcery \you ->
    Target you $ masked @OTNLand [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors R
          , sorcery_cost = manaCostOf (2, R)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \_this ->
              effect $ destroy target
          }

stoneThrowingDevils :: Card OTNCreature
stoneThrowingDevils = Card "Stone-Throwing Devils" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors B
      , creature_cost = manaCostOf B
      , creature_creatureTypes = [Devil]
      , creature_power = Power 1
      , creature_toughness = Toughness 1
      , creature_abilities = [Static FirstStrike]
      }

swamp :: Card OTNLand
swamp = mkBasicLand Swamp

swanSong :: Card OTNInstant
swanSong = Card "Swan Song" $
  YourInstant \you ->
    Target you $ masked @(OT3 'OTEnchantment 'OTInstant 'OTSorcery) [] \target ->
      controllerOf target \controller ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors U
            , instant_cost = manaCostOf U
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                effect
                  [ counterSpell target
                  , addToBattlefield controller birdToken
                  ]
            }

unholyStrength :: Card OTNEnchantment
unholyStrength = Card "Unholy Strength" $
  YourEnchantment \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors B
      , enchantment_cost = manaCostOf B
      , enchantment_creatureTypes = []
      , enchantment_enchantmentTypes =
          [ Aura $
              Enchant $ linked [] \enchanted ->
                effect $ StatDelta enchanted (Power 2) (Toughness 1)
          ]
      , enchantment_abilities = []
      }

vindicate :: Card OTNSorcery
vindicate = Card "Vindicate" $
  YourSorcery \you ->
    Target you $ masked @OTNPermanent [] \target ->
      ElectCard $
        SorceryFacet
          { sorcery_colors = toColors (W, B)
          , sorcery_cost = manaCostOf (1, W, B)
          , sorcery_creatureTypes = []
          , sorcery_abilities = []
          , sorcery_effect = thisObject \_this ->
              effect $ destroy target
          }

-- NOTE: Wastes does NOT have an intrinsic mana ability.
wastes :: Card OTNLand
wastes = Card "Wastes" $
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
                      { activated_cost = tapCost [is this]
                      , activated_effect = effect $ AddMana you $ toManaPool C
                      }
          ]
      }

wear_tear :: Card (OTNInstant, OTNInstant)
wear_tear = SplitCard wear tear [Static Fuse]
 where
  wear :: Card OTNInstant
  wear = Card "Wear" $
    YourInstant \you ->
      Target you $ masked @OTNArtifact [] \target ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors R
            , instant_cost = manaCostOf (1, R)
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                effect $ destroy target
            }
  tear :: Card OTNInstant
  tear = Card "Tear" $
    YourInstant \you ->
      Target you $ masked @OTNEnchantment [] \target ->
        ElectCard $
          InstantFacet
            { instant_colors = toColors W
            , instant_cost = manaCostOf W
            , instant_creatureTypes = []
            , instant_abilities = []
            , instant_effect = thisObject \_this ->
                effect $ destroy target
            }

witchEngine :: Card OTNCreature
witchEngine = Card "Witch Engine" $
  YourCreature \_you ->
    CreatureFacet
      { creature_colors = toColors B
      , creature_cost = manaCostOf (5, B)
      , creature_creatureTypes = [Horror]
      , creature_power = Power 4
      , creature_toughness = Toughness 4
      , creature_abilities =
          [ swampwalk
          , Activated @ 'ZBattlefield $
              thisObject \this ->
                controllerOf this \you ->
                  Target you $ masked [IsOpponentOf you] \opponent ->
                    ElectActivated $
                      Ability
                        { activated_cost =
                            AndCosts
                              [ tapCost [is this]
                              ]
                        , activated_effect =
                            effect
                              [ AddMana you $ toManaPool (B, B, B, B)
                              , EffectContinuous $ gainControl opponent this
                              ]
                        }
          ]
      }

wrathOfGod :: Card OTNSorcery
wrathOfGod = Card "Wrath of God" $
  YourSorcery \_you ->
    ElectCard $
      SorceryFacet
        { sorcery_colors = toColors W
        , sorcery_cost = manaCostOf (2, W, W)
        , sorcery_creatureTypes = []
        , sorcery_abilities = []
        , sorcery_effect = thisObject \_this ->
            All $ maskeds @OTNCreature [] \creatures ->
              effect $
                WithList $ Each creatures \creature ->
                  Sequence
                    [ destroy creature
                    , EffectContinuous $ CantBeRegenerated creature
                    ]
        }

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE Safe #-}
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
  alpineMeadow,
  ancestralVision,
  arcticFlats,
  arcticTreeline,
  backlash,
  bayou,
  birdsOfParadise,
  blackLotus,
  blaze,
  bloodMoon,
  borealDruid,
  borealShelf,
  braidwoodCup,
  cityOfBrass,
  cleanse,
  conversion,
  corrosiveGale,
  counterspell,
  damnation,
  darkRitual,
  deathriteShaman,
  dismember,
  divination,
  elvishHexhunter,
  fling,
  forest,
  fulminatorMage,
  frostMarsh,
  giantGrowth,
  glacialFloodplain,
  gutlessGhoul,
  gutShot,
  grizzlyBears,
  highlandForest,
  highlandWeald,
  holyStrength,
  icehideGolem,
  iceTunnel,
  island,
  lavaAxe,
  lightningBolt,
  llanowarElves,
  manaLeak,
  moltensteelDragon,
  mouthOfRonom,
  mountain,
  moxEmerald,
  moxJet,
  moxPearl,
  moxRuby,
  moxSapphire,
  mutagenicGrowth,
  nyxbornRollicker,
  ornithopter,
  porcelainLegionnaire,
  ragingGoblin,
  rimewoodFalls,
  plains,
  plummet,
  pollutedDelta,
  pradeshGypsies,
  shatter,
  shock,
  sinkhole,
  slashPanther,
  snowCoveredForest,
  snowCoveredIsland,
  snowCoveredMountain,
  snowCoveredPlains,
  snowCoveredSwamp,
  snowfieldSinkhole,
  snuffOut,
  spinedThopter,
  squallDrifter,
  squallLine,
  stifle,
  stoneRain,
  stoneThrowingDevils,
  sulfurousMire,
  sunkenRuins,
  swamp,
  swanSong,
  teferisIsle,
  thermopod,
  thunderingTanadon,
  tresserhornSinks,
  unholyStrength,
  vindicate,
  volatileFjord,
  waspLancer,
  wastes,
  wear_tear,
  witchEngine,
  woodlandChasm,
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
  ToHybrid (..),
  activated,
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
  manaCost,
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
  static,
  static',
  swampwalk,
  tapCost,
  triggered,
  untilEndOfTurn,
  yourCardFacet,
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
  Ability (Static),
  ActivatedAbility (..),
  Card (..),
  CardFacet (..),
  CardFacet' (..),
  Case (CaseFin, caseFin, ofFin),
  Condition (COr),
  Cost (..),
  Effect (
    AddMana,
    CantBeRegenerated,
    DrawCards,
    EffectCase,
    EffectContinuous,
    GainLife,
    LoseLife,
    Sequence,
    StatDelta,
    Tap,
    WithList
  ),
  Elect (
    ActivePlayer,
    All,
    Choose,
    ChooseOption,
    Cost,
    ElectActivated,
    ElectCardFacet',
    EndTargets,
    Target,
    VariableFromPower,
    VariableInt
  ),
  Enchant (Enchant),
  EnchantmentType (Aura),
  EntersStatic (EntersTapped),
  EventListener' (TimePoint),
  Requirement (
    ControlledBy,
    ControlsA,
    HasLandType,
    IsOpponentOf,
    Not,
    ROr
  ),
  SomeZone (SomeZone2),
  StaticAbility (
    Bestow,
    Enters,
    FirstStrike,
    Flying,
    Fuse,
    Haste,
    Phasing,
    StaticContinuous,
    Suspend,
    Trample
  ),
  Token (..),
  TriggeredAbility (When),
  WithList (..),
  pattern CTrue,
 )
import safe MtgPure.Model.Step (Step (..))
import safe MtgPure.Model.Supertype (Supertype (Legendary, Tribal))
import safe qualified MtgPure.Model.Supertype as Ty
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness (..))
import safe MtgPure.Model.Zone (Zone (..))

----------------------------------------

mkBasicImpl :: [Ty.Supertype OTNLand] -> CardName -> BasicLandType -> Card OTNLand
mkBasicImpl supertypes name ty = Card name $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = supertypes
      , land_landTypes = [BasicLand ty]
      , land_spec =
          LandFacet'
            { land_abilities = []
            }
      }

mkBasicLand :: BasicLandType -> Card OTNLand
mkBasicLand ty = mkBasicImpl [] name ty
 where
  name = CardName $ show ty

mkSnowCovered :: BasicLandType -> Card OTNLand
mkSnowCovered ty = mkBasicImpl [Ty.Snow] name ty
 where
  name = CardName $ "Snow-Covered " ++ show ty

mkDualTapImpl :: [Ty.Supertype OTNLand] -> CardName -> BasicLandType -> BasicLandType -> Card OTNLand
mkDualTapImpl supertypes name ty1 ty2 = Card name $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = supertypes
      , land_landTypes = [BasicLand ty1, BasicLand ty2]
      , land_spec =
          LandFacet'
            { land_abilities = [static \_this -> Enters EntersTapped]
            }
      }

mkSnowCoveredTapDualLand :: CardName -> BasicLandType -> BasicLandType -> Card OTNLand
mkSnowCoveredTapDualLand = mkDualTapImpl [Ty.Snow]

mkTapLandImpl ::
  (ToManaPool 'NonSnow (ManaSymbol mt1), ToManaPool 'NonSnow (ManaSymbol mt2)) =>
  [Ty.Supertype OTNLand] ->
  CardName ->
  ManaSymbol mt1 ->
  ManaSymbol mt2 ->
  Card OTNLand
mkTapLandImpl supertypes name sym1 sym2 = Card name $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = supertypes
      , land_landTypes = []
      , land_spec =
          LandFacet'
            { land_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ AddMana you $ toManaPool sym1
                          }
                , activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ AddMana you $ toManaPool sym2
                          }
                ]
            }
      }

mkSnowCoveredTapLand ::
  (ToManaPool 'NonSnow (ManaSymbol mt1), ToManaPool 'NonSnow (ManaSymbol mt2)) =>
  CardName ->
  ManaSymbol mt1 ->
  ManaSymbol mt2 ->
  Card OTNLand
mkSnowCoveredTapLand = mkTapLandImpl [Ty.Snow]

mkDualLand :: CardName -> BasicLandType -> BasicLandType -> Card OTNLand
mkDualLand name ty1 ty2 = Card name $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = []
      , land_landTypes = [BasicLand ty1, BasicLand ty2]
      , land_spec =
          LandFacet'
            { land_abilities = []
            }
      }

mkHybridFilterLand ::
  ToHybrid mt1 mt2 mth =>
  CardName ->
  ManaSymbol mt1 ->
  ManaSymbol mt2 ->
  Card OTNLand
mkHybridFilterLand name sym1 sym2 = Card name $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = []
      , land_landTypes = []
      , land_spec =
          LandFacet'
            { land_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated
                        Ability
                          { activated_cost =
                              AndCosts
                                [ manaCost $ toHybrid sym1 sym2
                                , tapCost [is this]
                                ]
                          , activated_effect = ChooseOption @()
                              you
                              (LS CTrue $ LS CTrue $ LZ CTrue)
                              \option ->
                                effect $
                                  EffectCase
                                    CaseFin
                                      { caseFin = option
                                      , ofFin =
                                          LS (AddMana you $ toManaPool (sym1, sym1, ())) $
                                            LS (AddMana you $ toManaPool (sym1, sym2, ())) $
                                              LZ (AddMana you $ toManaPool (sym2, sym2, ()))
                                      }
                          }
                ]
            }
      }

mkFetchLand :: CardName -> BasicLandType -> BasicLandType -> Card OTNLand
mkFetchLand name ty1 ty2 = Card name $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = []
      , land_landTypes = []
      , land_spec =
          LandFacet'
            { land_abilities =
                [ activated @ 'ZBattlefield \this ->
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
      }

mkMox :: ToManaPool 'NonSnow (ManaSymbol mt) => CardName -> ManaSymbol mt -> Card OTNArtifact
mkMox name sym = Card name $
  yourCardFacet \_you ->
    ArtifactFacet
      { artifact_colors = toColors ()
      , artifact_supertypes = []
      , artifact_artifactTypes = []
      , artifact_spec =
          ArtifactFacet'
            { artifact_cost = manaCost 0
            , artifact_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ AddMana you $ toManaPool sym
                          }
                ]
            }
      }

----------------------------------------

acceptableLosses :: Card OTNSorcery
acceptableLosses =
  Card "Acceptable Losses" $
    yourCardFacet \you ->
      SorceryFacet
        { sorcery_colors = toColors R
        , sorcery_supertypes = []
        , sorcery_spec =
            ElectCardFacet'
              SorceryFacet'
                { sorcery_cost =
                    AndCosts
                      [ manaCost (3, R)
                      , DiscardRandomCost 1
                      ]
                , sorcery_abilities = []
                , sorcery_effect = thisObject \this ->
                    Target you $ masked @OTNCreature [] \target ->
                      effect $ dealDamage this target 5
                }
        }

allIsDust :: Card OTNSorcery
allIsDust = Card "All Is Dust" $
  yourCardFacet \_you ->
    SorceryFacet
      { sorcery_colors = toColors ()
      , sorcery_supertypes = [Tribal [Eldrazi]]
      , sorcery_spec =
          ElectCardFacet'
            SorceryFacet'
              { sorcery_cost = manaCost 7
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
      }

alpineMeadow :: Card OTNLand
alpineMeadow = mkSnowCoveredTapDualLand "Alpine Meadow" Mountain Plains

arcticFlats :: Card OTNLand
arcticFlats = mkSnowCoveredTapLand "Arctic Flats" G W

arcticTreeline :: Card OTNLand
arcticTreeline = mkSnowCoveredTapDualLand "Arctic Treeline" Forest Plains

ancestralRecall :: Card OTNInstant
ancestralRecall = Card "Ancestral Recall" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors U
      , instant_supertypes = []
      , instant_spec =
          ElectCardFacet'
            InstantFacet'
              { instant_cost = noCost
              , instant_abilities = []
              , instant_effect = thisObject \_this ->
                  effect $ DrawCards you 3
              }
      }

ancestralVision :: Card OTNSorcery
ancestralVision = Card "Ancestral Vision" $
  yourCardFacet \you ->
    SorceryFacet
      { sorcery_colors = toColors U
      , sorcery_supertypes = []
      , sorcery_spec =
          ElectCardFacet'
            SorceryFacet'
              { sorcery_cost = noCost
              , sorcery_abilities = [static \_this -> Suspend 4 $ Cost $ manaCost U]
              , sorcery_effect = thisObject \_this ->
                  Target you $ masked [] \target ->
                    effect $ DrawCards target 3
              }
      }

backlash :: Card OTNInstant
backlash = Card "Backlash" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors (B, R)
      , instant_supertypes = []
      , instant_spec =
          ElectCardFacet'
            InstantFacet'
              { instant_cost = manaCost (1, B, R)
              , instant_abilities = []
              , instant_effect = thisObject \_this ->
                  Target you $ masked [Not isTapped] \target ->
                    EndTargets $
                      controllerOf target \targetController ->
                        VariableFromPower target \power ->
                          effect $
                            dealDamage target targetController $ VariableDamage power
              }
      }

bayou :: Card OTNLand
bayou = mkDualLand "Bayou" Forest Swamp

birdsOfParadise :: Card OTNCreature
birdsOfParadise = Card "Birds of Paradise" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_supertypes = []
      , creature_creatureTypes = [Bird]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost G
            , creature_power = Power 0
            , creature_toughness = Toughness 1
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
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
      }

birdToken :: Token OTNCreature
birdToken = Token $
  Card "Bird Token" $
    yourCardFacet \_you ->
      CreatureFacet
        { creature_colors = toColors U
        , creature_supertypes = []
        , creature_creatureTypes = [Bird]
        , creature_spec =
            CreatureFacet'
              { creature_cost = noCost
              , creature_power = Power 2
              , creature_toughness = Toughness 2
              , creature_abilities = [static \_this -> Flying]
              }
        }

blackLotus :: Card OTNArtifact
blackLotus = Card "Black Lotus" $
  yourCardFacet \_you ->
    ArtifactFacet
      { artifact_colors = toColors ()
      , artifact_supertypes = []
      , artifact_artifactTypes = []
      , artifact_spec =
          ArtifactFacet'
            { artifact_cost = manaCost 0
            , artifact_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost =
                              AndCosts
                                [ tapCost [is this]
                                , sacrificeCost [is this]
                                ]
                          , activated_effect =
                              chooseAnyColor you \color ->
                                effect $ addManaAnyColor color you 3
                          }
                ]
            }
      }

blaze :: Card OTNSorcery
blaze = Card "Blaze" $
  yourCardFacet \you ->
    SorceryFacet
      { sorcery_colors = toColors R
      , sorcery_supertypes = []
      , sorcery_spec =
          VariableInt \x ->
            ElectCardFacet'
              SorceryFacet'
                { sorcery_cost = manaCost (VariableMana @ 'NonSnow @ 'Ty1 x, R)
                , sorcery_abilities = []
                , sorcery_effect = thisObject \this ->
                    Target you $ masked @OTNCreaturePlayerPlaneswalker [] \target ->
                      effect $ dealDamage this target x
                }
      }

bloodMoon :: Card OTNEnchantment
bloodMoon = Card "Blood Moon" $
  yourCardFacet \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors R
      , enchantment_supertypes = []
      , enchantment_enchantmentTypes = []
      , enchantment_spec =
          EnchantmentFacet'
            { enchantment_cost = manaCost (2, R)
            , enchantment_abilities =
                [ static \_this ->
                    StaticContinuous $
                      All $ maskeds [nonBasic] \lands ->
                        effect $
                          WithList $ Each lands \land ->
                            changeTo land mountain
                ]
            }
      }

borealDruid :: Card OTNCreature
borealDruid = Card "Boreal Druid" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_supertypes = [Ty.Snow]
      , creature_creatureTypes = [Elf, Druid]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost G
            , creature_power = Power 1
            , creature_toughness = Toughness 1
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect =
                              effect $ AddMana you $ toManaPool C
                          }
                ]
            }
      }

borealShelf :: Card OTNLand
borealShelf = mkSnowCoveredTapLand "Boreal Shelf" W U

braidwoodCup :: Card OTNArtifact
braidwoodCup = Card "Braidwood Cup" $
  yourCardFacet \_you ->
    ArtifactFacet
      { artifact_colors = toColors ()
      , artifact_supertypes = []
      , artifact_artifactTypes = []
      , artifact_spec =
          ArtifactFacet'
            { artifact_cost = manaCost 3
            , artifact_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ GainLife you 1
                          }
                ]
            }
      }

counterspell :: Card OTNInstant
counterspell = Card "Counterspell" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors U
      , instant_supertypes = []
      , instant_spec =
          ElectCardFacet'
            InstantFacet'
              { instant_cost = manaCost (U, U)
              , instant_abilities = []
              , instant_effect = thisObject \_this ->
                  Target you $ masked @OTNSpell [] \target ->
                    effect $ counterSpell target
              }
      }

cityOfBrass :: Card OTNLand
cityOfBrass = Card "City of Brass" $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = []
      , land_landTypes = []
      , land_spec =
          LandFacet'
            { land_abilities =
                [ triggered \this ->
                    When $
                      event $
                        becomesTapped $ linked [is this] \_ ->
                          controllerOf this \you ->
                            effect $ dealDamage this you 1
                , activated @ 'ZBattlefield \this ->
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
      }

cleanse :: Card OTNSorcery
cleanse = Card "Cleanse" $
  yourCardFacet \_you ->
    SorceryFacet
      { sorcery_colors = toColors W
      , sorcery_supertypes = []
      , sorcery_spec =
          ElectCardFacet'
            SorceryFacet'
              { sorcery_abilities = []
              , sorcery_cost = manaCost (2, W, W)
              , sorcery_effect = thisObject \_this ->
                  All $ maskeds @OTNCreature [ofColors B] \creatures ->
                    effect $
                      WithList $ Each creatures \creature ->
                        destroy creature
              }
      }

conversion :: Card OTNEnchantment
conversion = Card "Conversion" $
  yourCardFacet \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors W
      , enchantment_supertypes = []
      , enchantment_enchantmentTypes = []
      , enchantment_spec =
          EnchantmentFacet'
            { enchantment_cost = manaCost (2, W, W)
            , enchantment_abilities =
                [ triggered \this ->
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
                , static \_this ->
                    StaticContinuous $
                      All $ maskeds [hasLandType Mountain] \lands ->
                        effect $
                          WithList $ Each lands \land ->
                            changeTo land plains
                ]
            }
      }

corrosiveGale :: Card OTNSorcery
corrosiveGale = Card "Corrosive Gale" $
  yourCardFacet \_you ->
    SorceryFacet
      { sorcery_colors = toColors G
      , sorcery_supertypes = []
      , sorcery_spec =
          VariableInt \x ->
            ElectCardFacet'
              SorceryFacet'
                { sorcery_cost = manaCost (VariableMana @ 'NonSnow @ 'Ty1 x, PG)
                , sorcery_abilities = []
                , sorcery_effect = thisObject \this ->
                    All $ maskeds @OTNCreature [hasAbility $ static \_this -> Flying] \victims ->
                      effect $
                        WithList $ Each victims \victim ->
                          dealDamage this victim x
                }
      }

damnation :: Card OTNSorcery
damnation = Card "Damnation" $
  yourCardFacet \_you ->
    SorceryFacet
      { sorcery_colors = toColors B
      , sorcery_supertypes = []
      , sorcery_spec =
          ElectCardFacet'
            SorceryFacet'
              { sorcery_cost = manaCost (2, B, B)
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
      }

darkRitual :: Card OTNInstant
darkRitual = Card "Dark Ritual" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors B
      , instant_supertypes = []
      , instant_spec =
          ElectCardFacet'
            InstantFacet'
              { instant_cost = manaCost B
              , instant_abilities = []
              , instant_effect = thisObject \_this ->
                  effect $ AddMana you $ toManaPool (B, B, B)
              }
      }

deathriteShaman :: Card OTNCreature
deathriteShaman = Card "Deathrite Shaman" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors (B, G)
      , creature_supertypes = []
      , creature_creatureTypes = [Elf, Shaman]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost BG
            , creature_power = Power 1
            , creature_toughness = Toughness 2
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
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
                , activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      Target you $ masked @(OT2 'OTInstant 'OTSorcery) @ 'ZGraveyard [] \spell ->
                        ElectActivated $
                          Ability
                            { activated_cost =
                                AndCosts
                                  [ manaCost B
                                  , tapCost [is this]
                                  , ExileCost [is spell]
                                  ]
                            , activated_effect =
                                All $ maskeds @OTNPlayer [IsOpponentOf you] \opponents ->
                                  effect $
                                    WithList $ Each opponents \opponent ->
                                      LoseLife opponent 2
                            }
                , activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      Target you $ masked @OTNCreature @ 'ZGraveyard [] \creature ->
                        ElectActivated $
                          Ability
                            { activated_cost =
                                AndCosts
                                  [ manaCost G
                                  , tapCost [is this]
                                  , ExileCost [is creature]
                                  ]
                            , activated_effect = effect $ GainLife you 2
                            }
                ]
            }
      }

dismember :: Card OTNInstant
dismember = Card "Dismember" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors B
      , instant_supertypes = []
      , instant_spec =
          ElectCardFacet'
            InstantFacet'
              { instant_cost = manaCost (1, PB, PB)
              , instant_abilities = []
              , instant_effect = thisObject \_this ->
                  Target you $ masked @OTNCreature [] \target ->
                    effect $
                      untilEndOfTurn $
                        gainAbility target $
                          static' \_this ->
                            StaticContinuous $
                              effect $ StatDelta target (Power (-5)) (Toughness (-5))
              }
      }

divination :: Card OTNSorcery
divination = Card "Divination" $
  yourCardFacet \you ->
    SorceryFacet
      { sorcery_colors = toColors U
      , sorcery_supertypes = []
      , sorcery_spec =
          ElectCardFacet'
            SorceryFacet'
              { sorcery_cost = manaCost (2, U)
              , sorcery_abilities = []
              , sorcery_effect = thisObject \_this ->
                  effect $ DrawCards you 2
              }
      }

elvishHexhunter :: Card OTNCreature
elvishHexhunter = Card "Elvish Hexhunter" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors (G, W)
      , creature_supertypes = []
      , creature_creatureTypes = [Elf, Shaman]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost GW
            , creature_power = Power 1
            , creature_toughness = Toughness 1
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      Target you $ masked @OTNEnchantment [] \target ->
                        ElectActivated $
                          Ability
                            { activated_cost =
                                AndCosts
                                  [ manaCost GW
                                  , tapCost [is this]
                                  , sacrificeCost [is this]
                                  ]
                            , activated_effect = effect $ destroy target
                            }
                ]
            }
      }

fling :: Card OTNInstant
fling = Card "Fling" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors R
      , instant_supertypes = []
      , instant_spec =
          Choose you $ masked [ControlledBy you] \sacChoice ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost =
                    AndCosts
                      [ manaCost (1, R)
                      , sacrificeCost [is sacChoice]
                      ]
                , instant_abilities = []
                , instant_effect = thisObject \_this ->
                    Target you $ masked @OTNCreaturePlayer [] \target ->
                      EndTargets $
                        VariableFromPower sacChoice \power ->
                          effect $
                            dealDamage sacChoice target $ VariableDamage power
                }
      }

forest :: Card OTNLand
forest = mkBasicLand Forest

fulminatorMage :: Card OTNCreature
fulminatorMage = Card "Fulminator Mage" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors (B, R)
      , creature_supertypes = []
      , creature_creatureTypes = [Elemental, Shaman]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (1, BR, BR)
            , creature_power = Power 2
            , creature_toughness = Toughness 2
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      Target you $ masked @OTNLand [nonBasic] \land ->
                        ElectActivated $
                          Ability
                            { activated_cost = sacrificeCost [is this]
                            , activated_effect = effect $ destroy land
                            }
                ]
            }
      }

frostMarsh :: Card OTNLand
frostMarsh = mkSnowCoveredTapLand "Frost Marsh" U B

giantGrowth :: Card OTNInstant
giantGrowth = Card "Giant Growth" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors G
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNCreature [] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost G
                , instant_abilities = []
                , instant_effect = thisObject \_this ->
                    effect $
                      untilEndOfTurn $
                        gainAbility target $
                          static' \_this ->
                            StaticContinuous $
                              effect $ StatDelta target (Power 3) (Toughness 3)
                }
      }

glacialFloodplain :: Card OTNLand
glacialFloodplain = mkSnowCoveredTapDualLand "Glacial Floodplain" Plains Island

gutlessGhoul :: Card OTNCreature
gutlessGhoul = Card "Gutless Ghoul" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors B
      , creature_supertypes = [Ty.Snow]
      , creature_creatureTypes = [Zombie]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (2, B)
            , creature_power = Power 2
            , creature_toughness = Toughness 2
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost =
                              AndCosts
                                [ manaCost 1
                                , sacrificeCost @OTNCreature []
                                ]
                          , activated_effect = effect $ GainLife you 2
                          }
                ]
            }
      }

gutShot :: Card OTNInstant
gutShot = Card "Gut Shot" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors R
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNCreaturePlayer [] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost PR
                , instant_abilities = []
                , instant_effect = thisObject \this ->
                    effect $ dealDamage this target 1
                }
      }

grizzlyBears :: Card OTNCreature
grizzlyBears = Card "Grizzly Bears" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_supertypes = []
      , creature_creatureTypes = [Bear]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (1, G)
            , creature_power = Power 2
            , creature_toughness = Toughness 2
            , creature_abilities = []
            }
      }

highlandForest :: Card OTNLand
highlandForest = mkSnowCoveredTapDualLand "Highland Forest" Mountain Forest

highlandWeald :: Card OTNLand
highlandWeald = mkSnowCoveredTapLand "Highland Weald" R G

holyStrength :: Card OTNEnchantment
holyStrength = Card "Holy Strength" $
  yourCardFacet \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors W
      , enchantment_supertypes = []
      , enchantment_enchantmentTypes =
          [ Aura $
              Enchant $ linked [] \enchanted ->
                effect $ StatDelta enchanted (Power 1) (Toughness 2)
          ]
      , enchantment_spec =
          EnchantmentFacet'
            { enchantment_cost = manaCost W
            , enchantment_abilities = []
            }
      }

icehideGolem :: Card OTNArtifactCreature
icehideGolem = Card "Icehide Golem" $
  yourCardFacet \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors ()
      , artifactCreature_supertypes = [Ty.Snow]
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = []
      , artifactCreature_spec =
          ArtifactCreatureFacet'
            { artifactCreature_cost = manaCost 1
            , artifactCreature_power = Power 2
            , artifactCreature_toughness = Toughness 2
            , artifactCreature_artifactAbilities = []
            , artifactCreature_creatureAbilities = []
            , artifactCreature_artifactCreatureAbilities = []
            }
      }

iceTunnel :: Card OTNLand
iceTunnel = mkSnowCoveredTapDualLand "Ice Tunnel" Island Swamp

island :: Card OTNLand
island = mkBasicLand Island

llanowarElves :: Card OTNCreature
llanowarElves = Card "Llanowar Elves" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_supertypes = []
      , creature_creatureTypes = [Elf]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost G
            , creature_power = Power 1
            , creature_toughness = Toughness 1
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ AddMana you $ toManaPool G
                          }
                ]
            }
      }

lavaAxe :: Card OTNSorcery
lavaAxe = Card "Lava Axe" $
  yourCardFacet \you ->
    SorceryFacet
      { sorcery_colors = toColors R
      , sorcery_supertypes = []
      , sorcery_spec =
          Target you $ masked @OTNPlayerPlaneswalker [] \target ->
            ElectCardFacet'
              SorceryFacet'
                { sorcery_cost = manaCost (4, R)
                , sorcery_abilities = []
                , sorcery_effect = thisObject \this ->
                    effect $ dealDamage this target 5
                }
      }

lightningBolt :: Card OTNInstant
lightningBolt = Card "Lightning Bolt" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors R
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNCreaturePlayerPlaneswalker [] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost R
                , instant_abilities = []
                , instant_effect = thisObject \this ->
                    effect $ dealDamage this target 3
                }
      }

manaLeak :: Card OTNInstant
manaLeak = Card "Mana Leak" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors U
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNSpell [] \spell ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost (1, U)
                , instant_abilities = []
                , instant_effect = thisObject \_this ->
                    controllerOf spell \controller ->
                      EndTargets $
                        ifElse (satisfies controller [playerPays $ toManaCost 3]) $
                          effect $ counterSpell spell
                }
      }

moltensteelDragon :: Card OTNArtifactCreature
moltensteelDragon = Card "Moltensteel Dragon" $
  yourCardFacet \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors R
      , artifactCreature_supertypes = []
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = [Dragon]
      , artifactCreature_spec =
          ArtifactCreatureFacet'
            { artifactCreature_cost = manaCost (4, PR, PR)
            , artifactCreature_power = Power 4
            , artifactCreature_toughness = Toughness 4
            , artifactCreature_artifactAbilities = []
            , artifactCreature_creatureAbilities =
                [ static \_this -> Flying
                , activated @ 'ZBattlefield \this ->
                    ElectActivated $
                      Ability
                        { activated_cost = manaCost PR
                        , activated_effect =
                            effect $
                              untilEndOfTurn $
                                gainAbility this $
                                  static' \_this ->
                                    StaticContinuous $
                                      effect $
                                        StatDelta this (Power 1) (Toughness 0)
                        }
                ]
            , artifactCreature_artifactCreatureAbilities = []
            }
      }

mountain :: Card OTNLand
mountain = mkBasicLand Mountain

mouthOfRonom :: Card OTNLand
mouthOfRonom = Card "Mouth of Ronom" $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = [Ty.Snow]
      , land_landTypes = []
      , land_spec =
          LandFacet'
            { land_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ AddMana you $ toManaPool C
                          }
                , activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      Target you $ masked @OTNCreature [] \target ->
                        ElectActivated $
                          Ability
                            { activated_cost =
                                AndCosts
                                  [ manaCost (4, S)
                                  , tapCost [is this]
                                  , sacrificeCost [is this]
                                  ]
                            , activated_effect = effect $ dealDamage this target 4
                            }
                ]
            }
      }

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

mutagenicGrowth :: Card OTNInstant
mutagenicGrowth = Card "Mutagenic Growth" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors G
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNCreature [] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost PG
                , instant_abilities = []
                , instant_effect = thisObject \_this ->
                    effect $
                      untilEndOfTurn $
                        gainAbility target $
                          static' \_this ->
                            StaticContinuous $
                              effect $ StatDelta target (Power 2) (Toughness 2)
                }
      }

nyxbornRollicker :: Card OTNEnchantmentCreature
nyxbornRollicker = Card "Nyxborn Rollicker" $
  yourCardFacet \_you ->
    EnchantmentCreatureFacet
      { enchantmentCreature_colors = toColors R
      , enchantmentCreature_supertypes = []
      , enchantmentCreature_creatureTypes = [Satyr]
      , enchantmentCreature_enchantmentTypes = []
      , enchantmentCreature_spec =
          EnchantmentCreatureFacet'
            { enchantmentCreature_cost = manaCost R
            , enchantmentCreature_power = Power 1
            , enchantmentCreature_toughness = Toughness 1
            , enchantmentCreature_creatureAbilities = []
            , enchantmentCreature_enchantmentAbilities = []
            , enchantmentCreature_enchantmentCreatureAbilities =
                [ static \_this ->
                    Bestow (Cost $ manaCost (1, R)) $
                      Enchant $ linked [] \enchanted ->
                        effect $ StatDelta enchanted (Power 1) (Toughness 1)
                ]
            }
      }

ornithopter :: Card OTNArtifactCreature
ornithopter = Card "Ornithopter" $
  yourCardFacet \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors ()
      , artifactCreature_supertypes = []
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = []
      , artifactCreature_spec =
          ArtifactCreatureFacet'
            { artifactCreature_cost = manaCost 0
            , artifactCreature_power = Power 0
            , artifactCreature_toughness = Toughness 2
            , artifactCreature_artifactAbilities = []
            , artifactCreature_creatureAbilities = [static \_this -> Flying]
            , artifactCreature_artifactCreatureAbilities = []
            }
      }

plains :: Card OTNLand
plains = mkBasicLand Plains

plummet :: Card OTNInstant
plummet = Card "Plummet" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors G
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked [hasAbility $ static \_this -> Flying] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost (1, G)
                , instant_abilities = []
                , instant_effect = thisObject \_this ->
                    effect $ destroy target
                }
      }

pollutedDelta :: Card OTNLand
pollutedDelta = mkFetchLand "Polluted Delta" Island Swamp

porcelainLegionnaire :: Card OTNArtifactCreature
porcelainLegionnaire = Card "Porcelain Legionnaire" $
  yourCardFacet \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors W
      , artifactCreature_supertypes = []
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = [Soldier]
      , artifactCreature_spec =
          ArtifactCreatureFacet'
            { artifactCreature_cost = manaCost (2, PW)
            , artifactCreature_power = Power 3
            , artifactCreature_toughness = Toughness 1
            , artifactCreature_artifactAbilities = []
            , artifactCreature_creatureAbilities = [static \_this -> FirstStrike]
            , artifactCreature_artifactCreatureAbilities = []
            }
      }

pradeshGypsies :: Card OTNCreature
pradeshGypsies = Card "Pradesh Gypsies" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors G
      , creature_supertypes = []
      , creature_creatureTypes = [Human, Nomad]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (2, G)
            , creature_power = Power 1
            , creature_toughness = Toughness 1
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      Target you $ masked [] \creature ->
                        ElectActivated $
                          Ability
                            { activated_cost =
                                AndCosts
                                  [ manaCost (1, G)
                                  , tapCost [is this]
                                  ]
                            , activated_effect =
                                effect $
                                  untilEndOfTurn $
                                    gainAbility creature $
                                      static' \_this ->
                                        StaticContinuous $
                                          effect $
                                            StatDelta creature (Power (-2)) (Toughness 0)
                            }
                ]
            }
      }

ragingGoblin :: Card OTNCreature
ragingGoblin = Card "Raging Goblin" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors R
      , creature_supertypes = []
      , creature_creatureTypes = [Goblin]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost R
            , creature_power = Power 1
            , creature_toughness = Toughness 1
            , creature_abilities = [static \_this -> Haste]
            }
      }

rimewoodFalls :: Card OTNLand
rimewoodFalls = mkSnowCoveredTapDualLand "Rimewood Falls" Forest Island

shatter :: Card OTNInstant
shatter = Card "Shatter" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors R
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNArtifact [] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost (1, R)
                , instant_abilities = []
                , instant_effect = thisObject \_this ->
                    effect $ destroy target
                }
      }

shock :: Card OTNInstant
shock = Card "Shock" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors R
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNCreaturePlayerPlaneswalker [] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost R
                , instant_abilities = []
                , instant_effect = thisObject \this ->
                    effect $ dealDamage this target 2
                }
      }

sinkhole :: Card OTNSorcery
sinkhole = Card "Sinkhole" $
  yourCardFacet \you ->
    SorceryFacet
      { sorcery_colors = toColors B
      , sorcery_supertypes = []
      , sorcery_spec =
          Target you $ masked @OTNLand [] \target ->
            ElectCardFacet'
              SorceryFacet'
                { sorcery_cost = manaCost (B, B)
                , sorcery_abilities = []
                , sorcery_effect = thisObject \_this ->
                    effect $ destroy target
                }
      }

slashPanther :: Card OTNArtifactCreature
slashPanther = Card "Slash Panther" $
  yourCardFacet \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors R
      , artifactCreature_supertypes = []
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = [Cat]
      , artifactCreature_spec =
          ArtifactCreatureFacet'
            { artifactCreature_cost = manaCost (4, PR)
            , artifactCreature_power = Power 4
            , artifactCreature_toughness = Toughness 2
            , artifactCreature_artifactAbilities = []
            , artifactCreature_creatureAbilities = [static \_this -> Haste]
            , artifactCreature_artifactCreatureAbilities = []
            }
      }

snowfieldSinkhole :: Card OTNLand
snowfieldSinkhole = mkSnowCoveredTapDualLand "Snowfield Sinkhole" Plains Swamp

snuffOut :: Card OTNInstant
snuffOut = Card "Snuff Out" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors B
      , instant_supertypes = []
      , instant_spec =
          ChooseOption
            you
            ( LS CTrue $
                LZ (satisfies you [ControlsA $ HasLandType $ BasicLand Swamp])
            )
            \option ->
              Target you $ masked [nonBlack] \target ->
                ElectCardFacet'
                  InstantFacet'
                    { instant_cost =
                        CostCase
                          CaseFin
                            { caseFin = option
                            , ofFin =
                                LS (manaCost (3, B)) $
                                  LZ @() $ PayLife 4
                            }
                    , instant_abilities = []
                    , instant_effect = thisObject \_this ->
                        effect
                          [ destroy target
                          , EffectContinuous $ CantBeRegenerated target
                          ]
                    }
      }

snowCoveredForest :: Card OTNLand
snowCoveredForest = mkSnowCovered Forest

snowCoveredIsland :: Card OTNLand
snowCoveredIsland = mkSnowCovered Island

snowCoveredMountain :: Card OTNLand
snowCoveredMountain = mkSnowCovered Mountain

snowCoveredPlains :: Card OTNLand
snowCoveredPlains = mkSnowCovered Plains

snowCoveredSwamp :: Card OTNLand
snowCoveredSwamp = mkSnowCovered Swamp

soldierToken :: Token OTNCreature
soldierToken = Token $
  Card "Soldier Token" $
    yourCardFacet \_you ->
      CreatureFacet
        { creature_colors = toColors W
        , creature_supertypes = []
        , creature_creatureTypes = [Soldier]
        , creature_spec =
            CreatureFacet'
              { creature_cost = noCost
              , creature_power = Power 1
              , creature_toughness = Toughness 1
              , creature_abilities = []
              }
        }

spinedThopter :: Card OTNArtifactCreature
spinedThopter = Card "Spined Thopter" $
  yourCardFacet \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors U
      , artifactCreature_supertypes = []
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = [Thopter]
      , artifactCreature_spec =
          ArtifactCreatureFacet'
            { artifactCreature_cost = manaCost (2, PU)
            , artifactCreature_power = Power 2
            , artifactCreature_toughness = Toughness 1
            , artifactCreature_artifactAbilities = []
            , artifactCreature_creatureAbilities = [static \_this -> Flying]
            , artifactCreature_artifactCreatureAbilities = []
            }
      }

squallDrifter :: Card OTNCreature
squallDrifter = Card "Squall Drifter" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors W
      , creature_supertypes = [Ty.Snow]
      , creature_creatureTypes = [Elemental]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (1, W)
            , creature_power = Power 1
            , creature_toughness = Toughness 1
            , creature_abilities =
                [ static \_this -> Flying
                , activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      Target you $ masked @OTNCreature [] \target ->
                        ElectActivated $
                          Ability
                            { activated_cost =
                                AndCosts
                                  [ manaCost W
                                  , tapCost [is this]
                                  ]
                            , activated_effect = effect $ Tap target
                            }
                ]
            }
      }

squallLine :: Card OTNInstant
squallLine = Card "Squall Line" $
  yourCardFacet \_you ->
    InstantFacet
      { instant_colors = toColors G
      , instant_supertypes = []
      , instant_spec =
          VariableInt \x ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost (VariableMana @ 'NonSnow @ 'Ty1 x, G, G)
                , instant_abilities = []
                , instant_effect = thisObject \this ->
                    All $ maskeds @OTNCreature [hasAbility $ static \_this -> Flying] \creatures ->
                      All $ maskeds @OTNPlayer [] \players ->
                        effect
                          [ WithList $ Each creatures \victim ->
                              dealDamage this victim x
                          , WithList $ Each players \victim ->
                              dealDamage this victim x
                          ]
                }
      }

stifle :: Card OTNInstant
stifle = Card "Stifle" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors U
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @OTNActivatedOrTriggeredAbility [] \target ->
            ElectCardFacet'
              InstantFacet'
                { instant_cost = manaCost U
                , instant_abilities = []
                , instant_effect = thisObject \_this ->
                    effect $ counterAbility target
                }
      }

stoneRain :: Card OTNSorcery
stoneRain = Card "Stone Rain" $
  yourCardFacet \you ->
    SorceryFacet
      { sorcery_colors = toColors R
      , sorcery_supertypes = []
      , sorcery_spec =
          Target you $ masked @OTNLand [] \target ->
            ElectCardFacet'
              SorceryFacet'
                { sorcery_cost = manaCost (2, R)
                , sorcery_abilities = []
                , sorcery_effect = thisObject \_this ->
                    effect $ destroy target
                }
      }

stoneThrowingDevils :: Card OTNCreature
stoneThrowingDevils = Card "Stone-Throwing Devils" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors B
      , creature_supertypes = []
      , creature_creatureTypes = [Devil]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost B
            , creature_power = Power 1
            , creature_toughness = Toughness 1
            , creature_abilities = [static \_this -> FirstStrike]
            }
      }

sulfurousMire :: Card OTNLand
sulfurousMire = mkSnowCoveredTapDualLand "Sulfurous Mire" Swamp Mountain

sunkenRuins :: Card OTNLand
sunkenRuins = mkHybridFilterLand "Sunken Ruins" U B

swamp :: Card OTNLand
swamp = mkBasicLand Swamp

swanSong :: Card OTNInstant
swanSong = Card "Swan Song" $
  yourCardFacet \you ->
    InstantFacet
      { instant_colors = toColors U
      , instant_supertypes = []
      , instant_spec =
          Target you $ masked @(OT3 'OTEnchantment 'OTInstant 'OTSorcery) [] \target ->
            controllerOf target \controller ->
              ElectCardFacet'
                InstantFacet'
                  { instant_cost = manaCost U
                  , instant_abilities = []
                  , instant_effect = thisObject \_this ->
                      effect
                        [ counterSpell target
                        , addToBattlefield controller birdToken
                        ]
                  }
      }

teferisIsle :: Card OTNLand
teferisIsle = Card "Teferi's Isle" $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = [Legendary]
      , land_landTypes = []
      , land_spec =
          LandFacet'
            { land_abilities =
                [ static \_this -> Phasing
                , static \_this -> Enters EntersTapped
                , activated @ 'ZBattlefield \this -> do
                    controllerOf this \you ->
                      ElectActivated
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ AddMana you $ toManaPool (U, 2)
                          }
                ]
            }
      }

thermopod :: Card OTNCreature
thermopod = Card "Thermopod" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors R
      , creature_supertypes = [Ty.Snow]
      , creature_creatureTypes = [Slug]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (1, R)
            , creature_power = Power 4
            , creature_toughness = Toughness 3
            , creature_abilities =
                [ activated @ 'ZBattlefield \this ->
                    ElectActivated $
                      Ability
                        { activated_cost = manaCost S
                        , activated_effect = effect $ untilEndOfTurn $ gainAbility this $ static' \_this -> Haste
                        }
                , activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost = sacrificeCost @OTNCreature []
                          , activated_effect = effect $ AddMana you $ toManaPool R
                          }
                ]
            }
      }

thunderingTanadon :: Card OTNArtifactCreature
thunderingTanadon = Card "Thundering Tanadon" $
  yourCardFacet \_you ->
    ArtifactCreatureFacet
      { artifactCreature_colors = toColors G
      , artifactCreature_supertypes = []
      , artifactCreature_artifactTypes = []
      , artifactCreature_creatureTypes = [Beast]
      , artifactCreature_spec =
          ArtifactCreatureFacet'
            { artifactCreature_cost = manaCost (4, PG, PG)
            , artifactCreature_power = Power 5
            , artifactCreature_toughness = Toughness 4
            , artifactCreature_artifactAbilities = []
            , artifactCreature_creatureAbilities = [static \_this -> Trample]
            , artifactCreature_artifactCreatureAbilities = []
            }
      }

tresserhornSinks :: Card OTNLand
tresserhornSinks = mkSnowCoveredTapLand "Tresserhorn Sinks" B R

unholyStrength :: Card OTNEnchantment
unholyStrength = Card "Unholy Strength" $
  yourCardFacet \_you ->
    EnchantmentFacet
      { enchantment_colors = toColors B
      , enchantment_supertypes = []
      , enchantment_enchantmentTypes =
          [ Aura $
              Enchant $ linked [] \enchanted ->
                effect $ StatDelta enchanted (Power 2) (Toughness 1)
          ]
      , enchantment_spec =
          EnchantmentFacet'
            { enchantment_cost = manaCost B
            , enchantment_abilities = []
            }
      }

vindicate :: Card OTNSorcery
vindicate = Card "Vindicate" $
  yourCardFacet \you ->
    SorceryFacet
      { sorcery_colors = toColors (W, B)
      , sorcery_supertypes = []
      , sorcery_spec =
          Target you $ masked @OTNPermanent [] \target ->
            ElectCardFacet'
              SorceryFacet'
                { sorcery_cost = manaCost (1, W, B)
                , sorcery_abilities = []
                , sorcery_effect = thisObject \_this ->
                    effect $ destroy target
                }
      }

volatileFjord :: Card OTNLand
volatileFjord = mkSnowCoveredTapDualLand "Volatile Fjord" Island Mountain

waspLancer :: Card OTNCreature
waspLancer = Card "Wasp Lancer" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors (U, B)
      , creature_supertypes = []
      , creature_creatureTypes = [Faerie, Soldier]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (UB, UB, UB)
            , creature_power = Power 3
            , creature_toughness = Toughness 2
            , creature_abilities = [static \_this -> Flying]
            }
      }

-- NOTE: Wastes does NOT have an intrinsic mana ability.
wastes :: Card OTNLand
wastes = Card "Wastes" $
  yourCardFacet \_you ->
    LandFacet
      { land_supertypes = [Ty.Basic]
      , land_landTypes = []
      , land_spec =
          LandFacet'
            { land_abilities =
                [ activated @ 'ZBattlefield \this ->
                    controllerOf this \you ->
                      ElectActivated $
                        Ability
                          { activated_cost = tapCost [is this]
                          , activated_effect = effect $ AddMana you $ toManaPool C
                          }
                ]
            }
      }

wear_tear :: Card (OTNInstant, OTNInstant)
wear_tear = SplitCard wear tear [SomeZone2 $ Static Fuse]
 where
  wear :: Card OTNInstant
  wear = Card "Wear" $
    yourCardFacet \you ->
      InstantFacet
        { instant_colors = toColors R
        , instant_supertypes = []
        , instant_spec =
            Target you $ masked @OTNArtifact [] \target ->
              ElectCardFacet'
                InstantFacet'
                  { instant_cost = manaCost (1, R)
                  , instant_abilities = []
                  , instant_effect = thisObject \_this ->
                      effect $ destroy target
                  }
        }
  tear :: Card OTNInstant
  tear = Card "Tear" $
    yourCardFacet \you ->
      InstantFacet
        { instant_colors = toColors W
        , instant_supertypes = []
        , instant_spec =
            Target you $ masked @OTNEnchantment [] \target ->
              ElectCardFacet'
                InstantFacet'
                  { instant_cost = manaCost W
                  , instant_abilities = []
                  , instant_effect = thisObject \_this ->
                      effect $ destroy target
                  }
        }

witchEngine :: Card OTNCreature
witchEngine = Card "Witch Engine" $
  yourCardFacet \_you ->
    CreatureFacet
      { creature_colors = toColors B
      , creature_supertypes = []
      , creature_creatureTypes = [Horror]
      , creature_spec =
          CreatureFacet'
            { creature_cost = manaCost (5, B)
            , creature_power = Power 4
            , creature_toughness = Toughness 4
            , creature_abilities =
                [ swampwalk
                , activated @ 'ZBattlefield \this ->
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
      }

woodlandChasm :: Card OTNLand
woodlandChasm = mkSnowCoveredTapDualLand "Woodland Chasm" Swamp Forest

wrathOfGod :: Card OTNSorcery
wrathOfGod = Card "Wrath of God" $
  yourCardFacet \_you ->
    SorceryFacet
      { sorcery_colors = toColors W
      , sorcery_supertypes = []
      , sorcery_spec =
          ElectCardFacet'
            SorceryFacet'
              { sorcery_cost = manaCost (2, W, W)
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
      }

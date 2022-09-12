{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.Recursive.Show
  ( CardDepth,
    runEnvM,
    showCard,
    showToken,
    showSetCard,
    showSetToken,
  )
where

import safe qualified Control.Monad.State.Strict as State
import safe qualified Data.DList as DList
import safe Data.Inst
  ( Inst10,
    Inst11,
    Inst12,
    Inst2,
    Inst3,
    Inst4,
    Inst5,
    Inst6,
    Inst7,
    Inst8,
    Inst9,
  )
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes)
import safe Data.Proxy (Proxy (Proxy))
import safe Data.String (IsString (..))
import safe Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.CardName (CardName (CardName))
import safe MtgPure.Model.ColoredMana (ColoredMana (..))
import safe MtgPure.Model.ColorlessMana (ColorlessMana (..))
import safe MtgPure.Model.Colors (Colors (..))
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.EffectType (EffectType (OneShot))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana (Mana (..))
import safe MtgPure.Model.ManaCost (ManaCost (..))
import safe MtgPure.Model.ManaPool (ManaPool (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectId (ObjectId (ObjectId))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectN.Type
  ( ON1,
    ON10,
    ON11,
    ON12,
    ON2,
    ON3,
    ON4,
    ON5,
    ON6,
    ON7,
    ON8,
    ON9,
  )
import safe MtgPure.Model.ObjectType (OT1, OT2, OT3, OT4, OT5, OT6, ObjectType (..))
import safe MtgPure.Model.ObjectType.Any (IsAnyType, WAny (..))
import safe MtgPure.Model.ObjectType.Card (IsCardType, WCard (..))
import safe MtgPure.Model.ObjectType.NonCreatureCard
  ( IsNonCreatureCardType,
    WNonCreatureCard (..),
  )
import safe MtgPure.Model.ObjectType.Permanent
  ( IsPermanentType,
    WPermanent (..),
  )
import safe MtgPure.Model.ObjectType.Spell (IsSpellType, WSpell (..))
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Recursive
  ( Ability (..),
    Card (..),
    CardTypeDef (..),
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    Else (..),
    Event,
    EventListener,
    EventListener' (..),
    IsOT,
    IsZO,
    Requirement (..),
    SetCard (SetCard),
    SetToken (SetToken),
    StaticAbility (..),
    Token (..),
    TriggeredAbility (..),
    WithLinkedObject (..),
    WithMaskedObject (..),
    WithThis (..),
  )
import safe MtgPure.Model.Selection (Selection (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (Variable (ReifiedVariable))
import safe MtgPure.Model.VisitObjectN
  ( KnownObjectN (..),
    VisitObjectN (..),
  )
import safe MtgPure.Model.Zone (IsZone (..), SZone (..))
import safe MtgPure.Model.ZoneObject
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OPermanent,
    OPlayerPlaneswalker,
    OSpell,
    ZO,
    ZoneObject (..),
  )

----------------------------------------

defaultDepthLimit :: Maybe Int
defaultDepthLimit = Nothing

instance Show (Ability ot) where
  show = runEnvM defaultDepthLimit . showAbility

instance Show (Card ot) where
  show = runEnvM defaultDepthLimit . showCard

instance Show (CardTypeDef t ot) where
  show = runEnvM defaultDepthLimit . showCardTypeDef

instance Show Condition where
  show = runEnvM defaultDepthLimit . showCondition

instance Show (Cost ot) where
  show = runEnvM defaultDepthLimit . showCost

instance Show (Effect e) where
  show = runEnvM defaultDepthLimit . showEffect

instance Show (Elect e ot) where
  show = runEnvM defaultDepthLimit . showElect

instance Show EventListener where
  show = runEnvM defaultDepthLimit . showEventListener

instance Show (Requirement zone ot) where
  show = runEnvM defaultDepthLimit . showRequirement

instance Show (SetCard ot) where
  show = runEnvM defaultDepthLimit . showSetCard

instance Show (SetToken ot) where
  show = runEnvM defaultDepthLimit . showSetToken

instance Show (StaticAbility ot) where
  show = runEnvM defaultDepthLimit . showStaticAbility

instance Show (Token ot) where
  show = runEnvM defaultDepthLimit . showToken

instance Show (TriggeredAbility ot) where
  show = runEnvM defaultDepthLimit . showTriggeredAbility

instance IsZO zone ot => Show (WithMaskedObject zone (Elect e ot)) where
  show = runEnvM defaultDepthLimit . showWithMaskedObject showElect "obj"

----------------------------------------

class LiteralMana a where
  literalMana :: a -> Maybe Int

instance LiteralMana (ColoredMana a) where
  literalMana = \case
    ColoredMana' _ x -> Just x
    VariableColoredMana {} -> Nothing
    SumColoredMana {} -> Nothing

instance LiteralMana ColorlessMana where
  literalMana = \case
    ColorlessMana' x -> Just x
    VariableColorlessMana {} -> Nothing
    SumColorlessMana {} -> Nothing

instance LiteralMana GenericMana where
  literalMana = \case
    GenericMana' x -> Just x
    VariableGenericMana {} -> Nothing
    SumGenericMana {} -> Nothing

instance LiteralMana (Mana a) where
  literalMana = \case
    WhiteMana x -> literalMana x
    BlueMana x -> literalMana x
    BlackMana x -> literalMana x
    RedMana x -> literalMana x
    GreenMana x -> literalMana x
    ColorlessMana x -> literalMana x
    GenericMana x -> literalMana x

----------------------------------------

data Item :: Type where
  StringItem :: String -> Item
  ObjectItem :: ObjectId -> Generation -> Item
  VariableItem :: VariableId -> Item
  deriving (Show)

instance IsString Item where
  fromString = StringItem

type Items = DList.DList Item

data Paren = NeedsParen | DoesntNeedParen

type ParenItems = (Paren, Items)

parens :: ParenItems -> Items
parens (p, s) = case p of
  NeedsParen -> pure "(" <> s <> pure ")"
  DoesntNeedParen -> s

dollar :: ParenItems -> Items
dollar (p, s) = case p of
  NeedsParen -> pure " $ " <> s
  DoesntNeedParen -> pure " " <> s

dropParens :: ParenItems -> Items
dropParens = snd

noParens :: EnvM Items -> EnvM ParenItems
noParens = fmap $ (,) DoesntNeedParen

yesParens :: EnvM Items -> EnvM ParenItems
yesParens = fmap $ (,) NeedsParen

----------------------------------------

type CardDepth = Maybe Int

type VariableId = Int

type Generation = Int

data Env = Env
  { nextObjectId :: ObjectId,
    nextVariableId :: VariableId,
    originalObjectRep :: Map.Map ObjectId TypeRep,
    currentGeneration :: Generation,
    objectGenerations :: Map.Map ObjectId Generation,
    objectNames :: Map.Map ObjectId String,
    cardDepth :: CardDepth
  }

mkEnv :: CardDepth -> Env
mkEnv depth =
  Env
    { nextObjectId = ObjectId 1,
      nextVariableId = 0,
      originalObjectRep = mempty,
      currentGeneration = 0,
      objectGenerations = mempty,
      objectNames = mempty,
      cardDepth = max 0 <$> depth
    }

type EnvM = State.State Env

runEnvM :: CardDepth -> EnvM ParenItems -> String
runEnvM depth m = concat $ State.evalState strsM $ mkEnv depth
  where
    itemsM = DList.toList . dropParens <$> m
    strsM =
      itemsM >>= \items -> do
        let used = getUsed items
        mapM (showItem used) items
    showItem used = \case
      StringItem s -> pure s
      ObjectItem i@(ObjectId n) g -> do
        prefix <- getObjectNamePrefix i
        let name = prefix ++ show n
        pure $ case Map.findWithDefault False (i, g) (usedObjects used) of
          False -> "_" ++ name
          True -> name
      VariableItem i -> do
        let name = varNames !! i
        pure $ case Map.findWithDefault False i (usedVariables used) of
          False -> "_" ++ name
          True -> name

type UsedObjects = Map.Map (ObjectId, Generation) Bool

type UsedVariables = Map.Map VariableId Bool

data Used = Used
  { usedObjects :: UsedObjects,
    usedVariables :: UsedVariables
  }

getUsed :: [Item] -> Used
getUsed = flip foldr empty $ \item used -> case item of
  StringItem {} -> used
  ObjectItem i g ->
    used
      { usedObjects = Map.insertWith (\_ _ -> True) (i, g) False $ usedObjects used
      }
  VariableItem var ->
    used
      { usedVariables = Map.insertWith (\_ _ -> True) var False $ usedVariables used
      }
  where
    empty = Used mempty mempty

-- TODO: Make this better now that variables can be procured through various abstract means.
varNames :: [String]
varNames = "x" : "y" : "z" : map f [0 ..]
  where
    f :: Int -> String
    f n = "var" ++ show n

getVarName :: Variable -> Item
getVarName = \case
  ReifiedVariable n -> VariableItem n

getObjectName :: Object a -> EnvM Item
getObjectName (Object _ i) = do
  gens <- State.gets objectGenerations
  case Map.lookup i gens of
    Nothing -> error "impossible"
    Just g -> pure $ ObjectItem i g

newtype ObjectIdState = ObjectIdState ObjectId

newObject :: forall a. IsObjectType a => String -> EnvM (Object a, ObjectIdState)
newObject name = do
  i@(ObjectId raw) <- State.gets nextObjectId
  let obj = idToObject @a i
  State.modify' $ \st ->
    st
      { nextObjectId = ObjectId $ raw + 1,
        originalObjectRep = Map.insert i (typeOf obj) $ originalObjectRep st,
        currentGeneration = currentGeneration st + 1,
        objectGenerations = Map.insert i (currentGeneration st) $ objectGenerations st,
        objectNames = Map.insert i name $ objectNames st
      }
  pure (obj, ObjectIdState i)

newObjectN ::
  forall a ot.
  (Typeable (ObjectN ot), IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  String ->
  EnvM (ObjectN ot, ObjectIdState)
newObjectN make name = do
  (obj, snap) <- newObject @a name
  let i = objectToId obj
      objN = make obj
  State.modify' $ \st ->
    st
      { originalObjectRep = Map.insert i (typeOf objN) $ originalObjectRep st
      }
  pure (objN, snap)

restoreObject :: ObjectIdState -> EnvM ()
--restoreObject (ObjectIdState i) = State.modify' $ \st -> st {nextObjectId = i}
restoreObject _ = pure ()

getObjectNamePrefix :: ObjectId -> EnvM String
getObjectNamePrefix i = State.gets (Map.findWithDefault "impossible" i . objectNames)

selectionMemo :: Selection -> String
selectionMemo = \case
  Choose {} -> "choice"
  Target {} -> "target"

showListM :: (a -> EnvM ParenItems) -> [a] -> EnvM ParenItems
showListM f xs = noParens $ do
  ss <- mapM (fmap dropParens . f) xs
  pure $ pure "[" <> DList.intercalate (pure ", ") ss <> pure "]"

toZone :: forall zone ot. IsZone zone => ObjectN ot -> ZO zone ot
toZone = case singZone (Proxy @zone) of
  SBattlefield -> ZOBattlefield SBattlefield
  SLibrary -> ZOLibrary SLibrary

----------------------------------------

showAbility :: Ability ot -> EnvM ParenItems
showAbility = \case
  Activated cost oneShot -> yesParens $ do
    sCost <- parens <$> showElect cost
    sOneShot <- dollar <$> showElect oneShot
    pure $ pure "Activated" <> sCost <> sOneShot
  Static ability -> yesParens $ (pure "Static" <>) . dollar <$> showStaticAbility ability
  Triggered ability -> yesParens $ (pure "Triggered" <>) . dollar <$> showTriggeredAbility ability

showAbilities :: [Ability ot] -> EnvM ParenItems
showAbilities = showListM showAbility

showBasicLandType :: BasicLandType -> EnvM ParenItems
showBasicLandType = noParens . pure . pure . fromString . show

showCard :: Card ot -> EnvM ParenItems
showCard = \case
  Card name wCard def ->
    showCard' "Card" name wCard def
  TribalCard name wCard def ->
    showCard' "TribalCard" name wCard def
  --
  ArtifactCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "ArtifactCard" <> sCard
  CreatureCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "CreatureCard" <> sCard
  EnchantmentCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "EnchantmentCard" <> sCard
  InstantCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "InstantCard" <> sCard
  LandCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "LandCard" <> sCard
  PlaneswalkerCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "PlaneswalkerCard" <> sCard
  SorceryCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "SorceryCard" <> sCard
  where
    showCard' consName (CardName name) wCard withCardTypeDef = yesParens $ do
      depth <- State.gets cardDepth
      State.modify' $ \st -> st {cardDepth = subtract 1 <$> depth}
      let sName = pure (fromString $ show name)
      case depth of
        Just 0 -> pure $ pure consName <> pure " " <> sName <> pure " ..."
        _ -> do
          sWCard <- parens <$> showWCard wCard
          sWithCardTypeDef <- dollar <$> showWithThis showCardTypeDef "this" withCardTypeDef
          pure $ pure consName <> pure " " <> sName <> pure " " <> sWCard <> sWithCardTypeDef

showCardTypeDef :: CardTypeDef tribal ot -> EnvM ParenItems
showCardTypeDef = \case
  ArtifactDef colors cost abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showElect cost
    sAbilities <- dollar <$> showAbilities abilities
    pure $ pure "ArtifactDef " <> sColors <> pure " " <> sCost <> sAbilities
  ArtifactCreatureDef colors cost creatureTypes power toughness abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showElect cost
    sCreatureTypes <- parens <$> showCreatureTypes creatureTypes
    sPower <- parens <$> showPower power
    sToughness <- parens <$> showToughness toughness
    sAbilities <- dollar <$> showAbilities abilities
    pure $
      pure "ArtifactCreatureDef "
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sCreatureTypes
        <> pure " "
        <> sPower
        <> pure " "
        <> sToughness
        <> sAbilities
  CreatureDef colors cost creatureTypes power toughness abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showElect cost
    sCreatureTypes <- parens <$> showCreatureTypes creatureTypes
    sPower <- parens <$> showPower power
    sToughness <- parens <$> showToughness toughness
    sAbilities <- dollar <$> showAbilities abilities
    pure $
      pure "CreatureDef "
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sCreatureTypes
        <> pure " "
        <> sPower
        <> pure " "
        <> sToughness
        <> sAbilities
  EnchantmentDef colors cost abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showElect cost
    sAbilities <- dollar <$> showAbilities abilities
    pure $ pure "EnchantmentDef " <> sColors <> pure " " <> sCost <> sAbilities
  InstantDef colors cost abilities electOneShot -> do
    showOneShot "InstantDef " colors cost abilities electOneShot
  LandDef landTypes abilities -> yesParens $ do
    sLandTypes <- parens <$> showListM showLandType landTypes
    sAbilities <- dollar <$> showAbilities abilities
    pure $ pure "LandDef " <> sLandTypes <> sAbilities
  PlaneswalkerDef colors cost loyalty abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showElect cost
    sLoyalty <- parens <$> showLoyalty loyalty
    sAbilities <- dollar <$> showAbilities abilities
    pure $
      pure "PlaneswalkerDef "
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sLoyalty
        <> sAbilities
  SorceryDef colors cost abilities electOneShot -> do
    showOneShot "SorceryDef " colors cost abilities electOneShot
  TribalDef creatureTypes nonCreature cardDef -> yesParens $ do
    sCreatureTypes <- parens <$> showCreatureTypes creatureTypes
    sNonCreature <- parens <$> showWNonCreatureCard nonCreature
    sCardDef <- dollar <$> showCardTypeDef cardDef
    pure $ pure "TribalDef " <> sCreatureTypes <> pure " " <> sNonCreature <> sCardDef
  VariableDef contCardDef -> yesParens $ do
    i <- State.gets nextVariableId
    State.modify' $ \st -> st {nextVariableId = i + 1}
    let var = ReifiedVariable i
        varName = getVarName var
        cardDef = contCardDef var
    sCardDef <- dropParens <$> showCardTypeDef cardDef
    pure $ pure "VariableDef $ \\" <> pure varName <> pure " -> " <> sCardDef
  where
    showOneShot ::
      forall a.
      IsObjectType a =>
      Item ->
      Colors ->
      Elect (Cost (OT1 a)) (OT1 a) ->
      [Ability (OT1 a)] ->
      Elect (Effect 'OneShot) (OT1 a) ->
      EnvM ParenItems
    showOneShot def colors cost abilities oneShot = yesParens $ do
      sColors <- parens <$> showColors colors
      sCost <- parens <$> showElect cost
      sAbilities <- parens <$> showAbilities abilities
      sElect <- dollar <$> showElect oneShot
      pure $
        pure def <> sColors <> pure " " <> sCost <> pure " " <> sAbilities <> sElect

showColors :: Colors -> EnvM ParenItems
showColors colors = yesParens $ do
  pure $ pure "toColors " <> sOpen <> sSyms <> sClose
  where
    syms = case colors of
      Colors w u b r g ->
        List.intercalate "," $
          catMaybes
            [ show <$> w,
              show <$> u,
              show <$> b,
              show <$> r,
              show <$> g
            ]
    sSyms = pure $ fromString syms
    (sOpen, sClose) = case syms of
      [_] -> (pure "", pure "")
      _ -> (pure "(", pure ")")

showColorlessMana :: ColorlessMana -> EnvM ParenItems
showColorlessMana =
  yesParens . \case
    x@ColorlessMana' {} -> pure $ pure $ fromString $ show x
    VariableColorlessMana var -> do
      let sVar = pure $ getVarName var
      pure $ pure "VariableColorlessMana " <> sVar
    SumColorlessMana x y -> do
      sX <- parens <$> showColorlessMana x
      sY <- parens <$> showColorlessMana y
      pure $ pure "SumColorlessMana " <> sX <> pure " " <> sY

showColoredMana :: ColoredMana a -> EnvM ParenItems
showColoredMana =
  yesParens . \case
    x@ColoredMana' {} -> pure $ pure $ fromString $ show x
    VariableColoredMana sym var -> do
      let sSym = pure $ fromString $ show sym
          sVar = pure $ getVarName var
      pure $ pure "VariableColoredMana " <> sSym <> pure " " <> sVar
    SumColoredMana sym x y -> do
      let sSym = pure $ fromString $ show sym
      sX <- parens <$> showColoredMana x
      sY <- parens <$> showColoredMana y
      pure $ pure "SumColoredMana " <> sSym <> pure " " <> sX <> pure " " <> sY

showCondition :: Condition -> EnvM ParenItems
showCondition = \case
  CAnd conds -> yesParens $ do
    sConds <- parens <$> showConditions conds
    pure $ pure "CAnd " <> sConds
  CNot cond -> yesParens $ do
    sCond <- parens <$> showCondition cond
    pure $ pure "CNot " <> sCond
  COr conds -> yesParens $ do
    sConds <- parens <$> showConditions conds
    pure $ pure "COr " <> sConds
  Satisfies wAny objN reqs -> yesParens $ do
    sWAny <- parens <$> showWAny wAny
    sObjN <- parens <$> showZoneObject objN
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Satisfies " <> sWAny <> pure " " <> sObjN <> sReqs

showConditions :: [Condition] -> EnvM ParenItems
showConditions = showListM showCondition

showCost :: Cost ot -> EnvM ParenItems
showCost = \case
  AndCosts costs -> yesParens $ do
    sCosts <- parens <$> showListM showCost costs
    pure $ pure "AndCosts " <> sCosts
  DiscardRandomCost amount -> yesParens $ do
    let sAmount = pure $ fromString $ show amount
    pure $ pure "DiscardRandomCost " <> sAmount
  LoyaltyCost loyalty -> yesParens $ do
    sLoyalty <- dollar <$> showLoyalty loyalty
    pure $ pure "LoyaltyCost " <> sLoyalty
  ManaCost cost -> yesParens $ do
    sCost <- dollar <$> showManaCost cost
    pure $ pure (fromString "ManaCost") <> sCost
  OrCosts costs -> yesParens $ do
    sCosts <- parens <$> showListM showCost costs
    pure $ pure "OrCosts " <> sCosts
  PayLife amount -> yesParens $ do
    let sAmount = pure $ fromString $ show amount
    pure $ pure "PayLife " <> sAmount
  SacrificeCost perm reqs -> yesParens $ do
    sPerm <- parens <$> showWPermanent perm
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "SacrificeCost " <> sPerm <> sReqs
  TapCost obj -> yesParens $ do
    sObj <- dollar <$> showOPermanent obj
    pure $ pure "TapCost" <> sObj

showCreatureTypes :: [CreatureType] -> EnvM ParenItems
showCreatureTypes = noParens . pure . pure . fromString . show

showDamage :: Damage -> EnvM ParenItems
showDamage =
  yesParens . \case
    Damage n -> do
      pure $ pure $ fromString $ "Damage " ++ show n
    VariableDamage var -> do
      let varName = getVarName var
      pure $ DList.fromList [fromString "VariableDamage ", varName]

showEffect :: Effect e -> EnvM ParenItems
showEffect = \case
  AddMana player mana -> yesParens $ do
    sPlayer <- parens <$> showZoneObject player
    sMana <- dollar <$> showManaPool mana
    pure $ pure "AddMana " <> sPlayer <> sMana
  AddToBattlefield perm player token -> yesParens $ do
    sPerm <- parens <$> showWPermanent perm
    sPlayer <- parens <$> showZoneObject player
    sCard <- dollar <$> showToken token
    pure $ pure "AddToBattlefield " <> sPlayer <> pure " " <> sPerm <> sCard
  ChangeTo perm before after -> yesParens $ do
    sPerm <- parens <$> showWPermanent perm
    sBefore <- parens <$> showOPermanent before
    sAfter <- dollar <$> showCard after
    pure $ pure "ChangeTo " <> sPerm <> pure " " <> sBefore <> sAfter
  CounterAbility obj -> yesParens $ do
    sObj <- dollar <$> showOActivatedOrTriggeredAbility obj
    pure $ pure "CounterAbility" <> sObj
  CounterSpell obj -> yesParens $ do
    sObj <- dollar <$> showOSpell obj
    pure $ pure "CounterSpell" <> sObj
  DealDamage source victim damage -> yesParens $ do
    sSource <- parens <$> showODamageSource source
    sVictim <- parens <$> showOCreaturePlayerPlaneswalker victim
    sDamage <- dollar <$> showDamage damage
    pure $ pure "DealDamage " <> sSource <> pure " " <> sVictim <> sDamage
  Destroy obj -> yesParens $ do
    sObj <- dollar <$> showOPermanent obj
    pure $ pure "Destroy" <> sObj
  DrawCards player n -> yesParens $ do
    sPlayer <- parens <$> showZoneObject player
    let amount = fromString $ show n
    pure $ pure "DrawCards " <> sPlayer <> pure " " <> pure amount
  EffectContinuous effect -> yesParens $ do
    sEffect <- dollar <$> showEffect effect
    pure $ pure "EffectContinuous" <> sEffect
  EOr effects -> yesParens $ do
    sEffects <- dollar <$> showEffects effects
    pure $ pure "EOr" <> sEffects
  Gain wAny obj ability -> yesParens $ do
    sWAny <- parens <$> showWAny wAny
    sObj <- parens <$> showZoneObject obj
    sAbility <- dollar <$> showAbility ability
    pure $ pure "Gain " <> sWAny <> pure " " <> sObj <> sAbility
  Lose wAny obj ability -> yesParens $ do
    sWAny <- parens <$> showWAny wAny
    sObj <- parens <$> showZoneObject obj
    sAbility <- dollar <$> showAbility ability
    pure $ pure "Lose " <> sWAny <> pure " " <> sObj <> sAbility
  PutOntoBattlefield wPerm player obj -> yesParens $ do
    sWPerm <- parens <$> showWPermanent wPerm
    sPlayer <- parens <$> showZoneObject player
    sCard <- dollar <$> showZoneObject obj
    pure $ pure "PutOntOBattlefield " <> sWPerm <> pure " " <> sPlayer <> sCard
  Sacrifice perm player reqs -> yesParens $ do
    sPerm <- parens <$> showWPermanent perm
    sPlayer <- parens <$> showZoneObject player
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Sacrifice " <> sPerm <> pure " " <> sPlayer <> sReqs
  SearchLibrary wCard player withCard -> yesParens $ do
    sWCard <- parens <$> showWCard wCard
    sPlayer <- parens <$> showZoneObject player
    sWithCard <- dollar <$> showWithLinkedObject showElect "card" withCard
    pure $ pure "SearchLibrary " <> sWCard <> pure " " <> sPlayer <> sWithCard
  StatDelta creature power toughness -> yesParens $ do
    sCreature <- parens <$> showZoneObject creature
    sPower <- parens <$> showPower power
    sToughness <- dollar <$> showToughness toughness
    pure $ pure "StatsDelta " <> sCreature <> pure " " <> sPower <> sToughness
  Until electEvent effect -> yesParens $ do
    sElectEvent <- parens <$> showElect electEvent
    sEffect <- dollar <$> showEffect effect
    pure $ pure "Until " <> sElectEvent <> sEffect

showEffects :: [Effect e] -> EnvM ParenItems
showEffects = showListM showEffect

showElect :: Elect e ot -> EnvM ParenItems
showElect = \case
  A sel player withObject -> yesParens $ do
    sSel <- parens <$> showSelection sel
    sPlayer <- parens <$> showZoneObject player
    sWithObject <- dollar <$> showWithMaskedObject showElect (selectionMemo sel) withObject
    pure $ pure "A " <> sSel <> pure " " <> sPlayer <> sWithObject
  ActivePlayer contElect -> yesParens $ do
    (active', snap) <- newObjectN @'OTPlayer O "active"
    let active = ZOBattlefield SBattlefield active'
    sActive <- parens <$> showZoneObject active
    let elect = contElect active
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $ pure "ActivePlayer $ \\" <> sActive <> pure " -> " <> sElect
  All withObject -> yesParens $ do
    sWithObject <- dollar <$> showWithMaskedObject showElect "all" withObject
    pure $ pure "All" <> sWithObject
  Condition cond -> yesParens $ do
    sCond <- dollar <$> showCondition cond
    pure $ pure "Condition" <> sCond
  ControllerOf obj contElect -> yesParens $ do
    objPrefix <- getObjectNamePrefix $
      visitObjectN' objectToId $ case obj of
        ZOBattlefield _ o -> o
    (controller', snap) <- newObjectN @'OTPlayer O $ case objPrefix == "this" of
      True -> "you"
      False -> "controller"
    let controller = ZOBattlefield SBattlefield controller'
    sController <- parens <$> showZoneObject controller
    sObj <- parens <$> showOAny obj
    let elect = contElect controller
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $ pure "ControllerOf " <> sObj <> pure " $ \\" <> sController <> pure " -> " <> sElect
  Cost cost -> yesParens $ do
    sCost <- dollar <$> showCost cost
    pure $ pure "Cost" <> sCost
  Effect effect -> yesParens $ do
    sEffect <- dollar <$> showEffects effect
    pure $ pure "Effect" <> sEffect
  Event event -> yesParens $ do
    sEvent <- dollar <$> showEvent event
    pure $ pure "Event" <> sEvent
  If cond then_ else_ -> yesParens $ do
    sCond <- parens <$> showCondition cond
    sThen <- parens <$> showElect then_
    sElse <- dollar <$> showElse else_
    pure $ pure "If " <> sCond <> pure " " <> sThen <> sElse
  Listen listener -> yesParens $ do
    sListener <- dollar <$> showEventListener listener
    pure $ pure "Listen" <> sListener
  Random withObject -> yesParens $ do
    sWithObject <- dollar <$> showWithMaskedObject showElect "rand" withObject
    pure $ pure "Random" <> sWithObject
  VariableFromPower creature varToElect -> yesParens $ do
    sCreature <- parens <$> showZoneObject creature
    i <- State.gets nextVariableId
    State.modify' $ \st -> st {nextVariableId = i + 1}
    let var = ReifiedVariable i
        varName = getVarName var
        elect = varToElect var
    sElect <- dropParens <$> showElect elect
    pure $ pure "VariableFromPower " <> sCreature <> pure " $ \\" <> pure varName <> pure " -> " <> sElect

showElse :: Else e ot -> EnvM ParenItems
showElse = \case
  ElseCost elect -> yesParens $ do
    sElect <- dollar <$> showElect elect
    pure $ pure "ElseCost" <> sElect
  ElseEffect elect -> yesParens $ do
    sElect <- dollar <$> showElect elect
    pure $ pure "ElseEffect" <> sElect
  ElseEvent -> noParens $ do
    pure $ pure "ElseEvent"

showEvent :: Event -> EnvM ParenItems
showEvent = showEventListener' $ \Proxy -> noParens $ pure $ pure "Proxy"

showEventListener :: EventListener -> EnvM ParenItems
showEventListener = showEventListener' showElect

showEventListener' :: (forall ot. x ot -> EnvM ParenItems) -> EventListener' x -> EnvM ParenItems
showEventListener' showX = \case
  BecomesTapped perm withObject -> yesParens $ do
    sPerm <- parens <$> showWPermanent perm
    sWithObject <- dollar <$> showWithLinkedObject showX "perm" withObject
    pure $ pure "BecomesTapped " <> sPerm <> sWithObject
  Events listeners -> yesParens $ do
    sListeners <- dollar <$> showListM (showEventListener' showX) listeners
    pure $ pure "Evenets" <> sListeners
  SpellIsCast spell withObject -> yesParens $ do
    sSpell <- parens <$> showWSpell spell
    sWithObject <- dollar <$> showWithLinkedObject showX "spell" withObject
    pure $ pure "SpellIsCast " <> sSpell <> sWithObject
  TimePoint timePoint oneShot -> yesParens $ do
    sTimePoint <- parens <$> showTimePoint timePoint
    sOneShot <- dollar <$> showX oneShot
    pure $ pure "TimePoint " <> sTimePoint <> sOneShot

showGenericMana :: GenericMana -> EnvM ParenItems
showGenericMana =
  yesParens . \case
    x@GenericMana' {} -> pure $ pure $ fromString $ show x
    VariableGenericMana var -> do
      let sVar = pure $ getVarName var
      pure $ pure "VariableGenericMana " <> sVar
    SumGenericMana x y -> do
      sX <- parens <$> showGenericMana x
      sY <- parens <$> showGenericMana y
      pure $ pure "SumGenericMana " <> sX <> pure " " <> sY

showLandType :: LandType -> EnvM ParenItems
showLandType landType = case landType of
  BasicLand basic -> yesParens $ do
    sBasic <- dollar <$> showBasicLandType basic
    pure $ pure "BasicLand" <> sBasic
  Desert -> sLandType
  Gate -> sLandType
  Lair -> sLandType
  Locus -> sLandType
  Mine -> sLandType
  PowerPlant -> sLandType
  Tower -> sLandType
  Urzas -> sLandType
  where
    sLandType = noParens $ pure $ pure $ fromString $ show landType

showLoyalty :: Loyalty -> EnvM ParenItems
showLoyalty = yesParens . pure . pure . fromString . show

showMana :: Mana a -> EnvM ParenItems
showMana =
  yesParens . \case
    WhiteMana m -> (pure "WhiteMana" <>) . dollar <$> showColoredMana m
    BlueMana m -> (pure "BlueMana" <>) . dollar <$> showColoredMana m
    BlackMana m -> (pure "BlackMana" <>) . dollar <$> showColoredMana m
    RedMana m -> (pure "RedMana" <>) . dollar <$> showColoredMana m
    GreenMana m -> (pure "GreenMana" <>) . dollar <$> showColoredMana m
    ColorlessMana m -> (pure "ColorlessMana" <>) . dollar <$> showColorlessMana m
    GenericMana m -> (pure "GenericMana" <>) . dollar <$> showGenericMana m

showManaCost :: ManaCost -> EnvM ParenItems
showManaCost cost = yesParens $ do
  let ManaCost'
        { costWhite = w,
          costBlue = u,
          costBlack = b,
          costRed = r,
          costGreen = g,
          costColorless = c,
          costGeneric = x
        } = cost
      lits =
        sequence
          [ literalMana w,
            literalMana u,
            literalMana b,
            literalMana r,
            literalMana g,
            literalMana c,
            literalMana x
          ]
  case lits of
    Just [litW, litU, litB, litR, litG, litC, litX] -> do
      let numW = (W, litW)
          numU = (U, litU)
          numB = (B, litB)
          numR = (R, litR)
          numG = (G, litG)
          numC = (C, litC)
          numX = litX
          go (sym, num) = case num of
            0 -> Nothing
            1 -> Just $ show sym
            _ -> Just $ "(" ++ show sym ++ "," ++ show num ++ ")"
          go' num = case num of
            0 -> Nothing
            _ -> Just $ show num
          manas' = [go' numX, go numW, go numU, go numB, go numR, go numG, go numC]
          manas = catMaybes manas'
          sManas = case manas of
            [] -> "0"
            [m] -> m
            _ -> "(" ++ List.intercalate "," manas ++ ")"
      pure $ pure $ fromString $ "toManaCost " ++ sManas
    _ -> do
      sW <- parens <$> showMana w
      sU <- parens <$> showMana u
      sB <- parens <$> showMana b
      sR <- parens <$> showMana r
      sG <- parens <$> showMana g
      sC <- parens <$> showMana c
      sX <- parens <$> showMana x
      pure $
        pure (fromString "ManaCost' ")
          <> sW
          <> pure " "
          <> sU
          <> pure " "
          <> sB
          <> pure " "
          <> sR
          <> pure " "
          <> sG
          <> pure " "
          <> sC
          <> pure " "
          <> sX

showManaPool :: ManaPool -> EnvM ParenItems
showManaPool pool = yesParens $ do
  let ManaPool
        { poolWhite = w,
          poolBlue = u,
          poolBlack = b,
          poolRed = r,
          poolGreen = g,
          poolColorless = c
        } = pool
      lits =
        sequence
          [ literalMana w,
            literalMana u,
            literalMana b,
            literalMana r,
            literalMana g,
            literalMana c
          ]
  case lits of
    Just [litW, litU, litB, litR, litG, litC] -> do
      let numW = (W, litW)
          numU = (U, litU)
          numB = (B, litB)
          numR = (R, litR)
          numG = (G, litG)
          numC = (C, litC)
          go (sym, num) = case num of
            0 -> Nothing
            1 -> Just $ show sym
            _ -> Just $ "(" ++ show sym ++ "," ++ show num ++ ")"
          manas' = [go numW, go numU, go numB, go numR, go numG, go numC]
          manas = catMaybes manas'
          sManas = case manas of
            [m] -> m
            _ -> "(" ++ List.intercalate "," manas ++ ")"
      pure $ pure $ fromString $ "toManaPool " ++ sManas
    _ -> do
      sW <- parens <$> showMana w
      sU <- parens <$> showMana u
      sB <- parens <$> showMana b
      sR <- parens <$> showMana r
      sG <- parens <$> showMana g
      sC <- parens <$> showMana c
      pure $
        pure (fromString "ManaPool ")
          <> sW
          <> pure " "
          <> sU
          <> pure " "
          <> sB
          <> pure " "
          <> sR
          <> pure " "
          <> sG
          <> pure " "
          <> sC

-- showNonProxy :: NonProxy x -> EnvM ParenItems
-- showNonProxy = \case
--   NonProxyElectEffectOneShot -> noParens $ do
--     pure $ pure "NonProxyElectEffectOneShot"

showO1 ::
  (IsObjectType a) =>
  IsOT (OT1 a) =>
  (z -> EnvM ParenItems) ->
  String ->
  (ON1 a -> z) ->
  EnvM ParenItems
showO1 = showONImpl O

showO2 ::
  (Inst2 IsObjectType a b) =>
  (z -> EnvM ParenItems) ->
  String ->
  (ON2 a b -> z) ->
  EnvM ParenItems
showO2 = showONImpl O2a

showO3 ::
  (Inst3 IsObjectType a b c) =>
  (z -> EnvM ParenItems) ->
  String ->
  (ON3 a b c -> z) ->
  EnvM ParenItems
showO3 = showONImpl O3a

showO4 ::
  (Inst4 IsObjectType a b c d) =>
  (z -> EnvM ParenItems) ->
  String ->
  (ON4 a b c d -> z) ->
  EnvM ParenItems
showO4 = showONImpl O4a

showO5 ::
  (Inst5 IsObjectType a b c d e) =>
  (z -> EnvM ParenItems) ->
  String ->
  (ON5 a b c d e -> z) ->
  EnvM ParenItems
showO5 = showONImpl O5a

showONImpl ::
  forall z a ot.
  (IsOT ot, IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  (z -> EnvM ParenItems) ->
  String ->
  (ObjectN ot -> z) ->
  EnvM ParenItems
showONImpl fromObject showM memo cont = yesParens $ do
  (objN, snap) <- newObjectN @a fromObject memo
  objName <- parens <$> showObjectN objN
  let elect = cont objN
  sElect <- dropParens <$> showM elect
  restoreObject snap
  pure $ pure "\\" <> objName <> pure " -> " <> sElect

showObject :: Object a -> EnvM Items
showObject = fmap pure . getObjectName

showObjectNImpl :: IsObjectType a => TypeRep -> Item -> Object a -> EnvM ParenItems
showObjectNImpl objNRef prefix obj = do
  let i = objectToId obj
  sObj <- showObject obj
  State.gets (Map.lookup i . originalObjectRep) >>= \case
    Nothing -> error "impossible"
    Just originalRep -> case originalRep == objNRef of
      False -> yesParens $ pure $ pure prefix <> pure " " <> sObj
      True -> noParens $ pure sObj

showObject1 :: IsObjectType a => ON1 a -> EnvM ParenItems
showObject1 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "toZO1"

showObject2 :: Inst2 IsObjectType a b => ON2 a b -> EnvM ParenItems
showObject2 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OCreaturePlaneswalker) -> "asCreaturePlaneswalker"
            | rep == typeRep (Proxy @OCreaturePlayer) -> "asCreaturePlayer"
            | rep == typeRep (Proxy @OPlayerPlaneswalker) -> "asPlayerPlaneswalker"
            | otherwise -> "toZO2"

showObject3 :: Inst3 IsObjectType a b c => ON3 a b c -> EnvM ParenItems
showObject3 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OCreaturePlayerPlaneswalker) -> "asCreaturePlayerPlaneswalker"
            | otherwise -> "toZO3"

showObject4 :: Inst4 IsObjectType a b c d => ON4 a b c d -> EnvM ParenItems
showObject4 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "toZO4"

showObject5 :: Inst5 IsObjectType a b c d e => ON5 a b c d e -> EnvM ParenItems
showObject5 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OPermanent) -> "asPermanent"
            | otherwise -> "toZO5"

showObject6 :: Inst6 IsObjectType a b c d e f => ON6 a b c d e f -> EnvM ParenItems
showObject6 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OSpell) -> "asSpell"
            | otherwise -> "toZO6"

showObject7 :: Inst7 IsObjectType a b c d e f g => ON7 a b c d e f g -> EnvM ParenItems
showObject7 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "toZO7"

showObject8 :: Inst8 IsObjectType a b c d e f g h => ON8 a b c d e f g h -> EnvM ParenItems
showObject8 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @ODamageSource) -> "asDamageSource"
            | otherwise -> "toZO8"

showObject9 :: Inst9 IsObjectType a b c d e f g h i => ON9 a b c d e f g h i -> EnvM ParenItems
showObject9 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "toZO9"

showObject10 :: Inst10 IsObjectType a b c d e f g h i j => ON10 a b c d e f g h i j -> EnvM ParenItems
showObject10 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "toZO10"

showObject11 :: Inst11 IsObjectType a b c d e f g h i j k => ON11 a b c d e f g h i j k -> EnvM ParenItems
showObject11 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "toZO11"

showObject12 :: Inst12 IsObjectType a b c d e f g h i j k l => ON12 a b c d e f g h i j k l -> EnvM ParenItems
showObject12 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OAny) -> "asAny"
            | otherwise -> "toZO12"

showObjectN :: VisitObjectN ot => ObjectN ot -> EnvM ParenItems
showObjectN objN = case knownObjectN objN of
  KO1 obj1 -> showObject1 obj1
  KO2 obj2 -> showObject2 obj2
  KO3 obj3 -> showObject3 obj3
  KO4 obj4 -> showObject4 obj4
  KO5 obj5 -> showObject5 obj5
  KO6 obj6 -> showObject6 obj6
  KO7 obj7 -> showObject7 obj7
  KO8 obj8 -> showObject8 obj8
  KO9 obj9 -> showObject9 obj9
  KO10 obj10 -> showObject10 obj10
  KO11 obj11 -> showObject11 obj11
  KO12 obj12 -> showObject12 obj12

showOActivatedOrTriggeredAbility :: OActivatedOrTriggeredAbility -> EnvM ParenItems
showOActivatedOrTriggeredAbility = showZoneObject

showOAny :: OAny -> EnvM ParenItems
showOAny = showZoneObject

showOCreaturePlayerPlaneswalker :: OCreaturePlayerPlaneswalker -> EnvM ParenItems
showOCreaturePlayerPlaneswalker = showZoneObject

showODamageSource :: ODamageSource -> EnvM ParenItems
showODamageSource = showZoneObject

showOPermanent :: OPermanent -> EnvM ParenItems
showOPermanent = showZoneObject

showOSpell :: OSpell -> EnvM ParenItems
showOSpell = showZoneObject

showPower :: Power -> EnvM ParenItems
showPower = yesParens . pure . pure . fromString . show

showRequirement :: Requirement zone ot -> EnvM ParenItems
showRequirement = \case
  ControlledBy obj -> yesParens $ do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "ControlledBy" <> sObj
  HasAbility ability -> yesParens $ do
    sAbility <- dollar <$> showWithThis showAbility "this" ability
    pure $ pure "HasAbility" <> sAbility
  HasLandType landType -> yesParens $ do
    sLandType <- dollar <$> showLandType landType
    pure $ pure "HasLandType" <> sLandType
  Is wAny objN -> yesParens $ do
    sWAny <- parens <$> showWAny wAny
    sObjN <- dollar <$> showZoneObject objN
    pure $ pure "Is " <> sWAny <> sObjN
  Not req -> yesParens $ do
    sReq <- dollar <$> showRequirement req
    pure $ pure "Not" <> sReq
  OfColors colors -> yesParens $ do
    pure $ pure $ fromString $ "OfColors $ " ++ show colors
  OwnedBy obj -> yesParens $ do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "OwnedBy" <> sObj
  PlayerPays cost -> yesParens $ do
    sCost <- dollar <$> showCost cost
    pure $ pure "PlayerPays" <> sCost
  Tapped perm -> yesParens $ do
    pure $ pure $ fromString $ "Tapped " ++ show perm
  RAnd reqs -> yesParens $ do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "RAnd" <> sReqs
  ROr reqs -> yesParens $ do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "ROr" <> sReqs
  R2 reqsA reqsB -> yesParens $ do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- dollar <$> showRequirements reqsB
    pure $ pure "R2 " <> sReqsA <> sReqsB
  R3 reqsA reqsB reqsC -> yesParens $ do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- dollar <$> showRequirements reqsC
    pure $ pure "R3 " <> sReqsA <> pure " " <> sReqsB <> sReqsC
  R4 reqsA reqsB reqsC reqsD -> yesParens $ do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- parens <$> showRequirements reqsC
    sReqsD <- dollar <$> showRequirements reqsD
    pure $ pure "R4 " <> sReqsA <> pure " " <> sReqsB <> pure " " <> sReqsC <> sReqsD
  R5 reqsA reqsB reqsC reqsD reqsE -> yesParens $ do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- parens <$> showRequirements reqsC
    sReqsD <- parens <$> showRequirements reqsD
    sReqsE <- dollar <$> showRequirements reqsE
    pure $ pure "R4 " <> sReqsA <> pure " " <> sReqsB <> pure " " <> sReqsC <> pure " " <> sReqsD <> sReqsE

showRequirements :: [Requirement zone ot] -> EnvM ParenItems
showRequirements = showListM showRequirement

showSelection :: Selection -> EnvM ParenItems
showSelection = \case
  Choose -> noParens $ do
    pure $ pure "Choose"
  Target -> noParens $ do
    pure $ pure "Target"

showSetCard :: SetCard ot -> EnvM ParenItems
showSetCard (SetCard set rarity card) = yesParens $ do
  sCard <- dollar <$> showCard card
  pure $ pure (fromString $ "SetCard " ++ show set ++ " " ++ show rarity) <> sCard

showSetToken :: SetToken ot -> EnvM ParenItems
showSetToken (SetToken set rarity token) = yesParens $ do
  sToken <- dollar <$> showToken token
  pure $ pure (fromString $ "SetToken " ++ show set ++ " " ++ show rarity) <> sToken

showStaticAbility :: StaticAbility a -> EnvM ParenItems
showStaticAbility = \case
  As electListener -> yesParens $ do
    sWithObject <- dollar <$> showElect electListener
    pure $ pure "As" <> sWithObject
  StaticContinuous continuous -> yesParens $ do
    sContinuous <- dollar <$> showElect continuous
    pure $ pure "StaticContinuous" <> sContinuous
  FirstStrike -> noParens $ do
    pure $ pure "FirstStrike"
  Flying -> noParens $ do
    pure $ pure "Flying"
  Haste -> noParens $ do
    pure $ pure "Haste"
  Suspend time cost -> yesParens $ do
    let sTime = pure $ fromString $ show time
    sCost <- dollar <$> showElect cost
    pure $ pure "Suspend " <> sTime <> sCost

showTimePoint :: TimePoint p -> EnvM ParenItems
showTimePoint = yesParens . pure . pure . fromString . show

showToken :: Token ot -> EnvM ParenItems
showToken = \case
  Token wPerm card -> yesParens $ do
    sWPerm <- parens <$> showWPermanent wPerm
    sCard <- dollar <$> showCard card
    pure $ pure "Token " <> sWPerm <> sCard
  ArtifactToken token -> yesParens $ do
    sToken <- dollar <$> showToken token
    pure $ pure "ArtifactToken" <> sToken
  CreatureToken token -> yesParens $ do
    sToken <- dollar <$> showToken token
    pure $ pure "CreatureToken" <> sToken
  EnchantmentToken token -> yesParens $ do
    sToken <- dollar <$> showToken token
    pure $ pure "EnchantmentToken" <> sToken
  LandToken token -> yesParens $ do
    sToken <- dollar <$> showToken token
    pure $ pure "LandToken" <> sToken
  PlaneswalkerToken token -> yesParens $ do
    sToken <- dollar <$> showToken token
    pure $ pure "PlaneswalkerToken" <> sToken

showToughness :: Toughness -> EnvM ParenItems
showToughness = yesParens . pure . pure . fromString . show

showTriggeredAbility :: TriggeredAbility ot -> EnvM ParenItems
showTriggeredAbility = \case
  When listener -> go "When" listener
  where
    go consName eventListener = yesParens $ do
      sEventListener <- dollar <$> showElect eventListener
      pure $ pure (fromString consName) <> sEventListener

showTypeOf :: forall a. PrettyType a => Proxy a -> EnvM ParenItems
showTypeOf _ = conditionalParens $ do
  pure $ pure $ fromString name
  where
    name = prettyType (Proxy @a)
    conditionalParens = case ' ' `elem` name of
      True -> yesParens
      False -> noParens

showWithLinkedObject ::
  forall zone x ot.
  IsZO zone ot =>
  (forall ot'. x ot' -> EnvM ParenItems) ->
  String ->
  WithLinkedObject zone x ot ->
  EnvM ParenItems
showWithLinkedObject showM memo = \case
  LProxy reqs -> yesParens $ do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "LProxy" <> sReqs
  L1 nonProxy reqs cont -> let ty = getType reqs in go ty nonProxy reqs $ showO1 showM memo (cont . toZone)
  L2 nonProxy reqs cont -> let ty = getType reqs in go ty nonProxy reqs $ showO2 showM memo (cont . toZone)
  L3 nonProxy reqs cont -> let ty = getType reqs in go ty nonProxy reqs $ showO3 showM memo (cont . toZone)
  L4 nonProxy reqs cont -> let ty = getType reqs in go ty nonProxy reqs $ showO4 showM memo (cont . toZone)
  L5 nonProxy reqs cont -> let ty = getType reqs in go ty nonProxy reqs $ showO5 showM memo (cont . toZone)
  where
    getType :: [Requirement zone ot] -> Proxy ot
    getType _ = Proxy

    go ty _nonProxy reqs sCont = yesParens $ do
      sTy <- parens <$> showTypeOf ty
      sReqs <- parens <$> showRequirements reqs
      sCont' <- dollar <$> sCont
      pure $ pure "linked @" <> sTy <> pure " " <> sReqs <> sCont'

showWithMaskedObject ::
  IsZone zone =>
  (z -> EnvM ParenItems) ->
  String ->
  WithMaskedObject zone z ->
  EnvM ParenItems
showWithMaskedObject showM memo = \case
  M1 reqs cont -> let ty = getType reqs in go ty reqs $ showO1 showM memo (cont . toZone)
  M2 reqs cont -> let ty = getType reqs in go ty reqs $ showO2 showM memo (cont . toZone)
  M3 reqs cont -> let ty = getType reqs in go ty reqs $ showO3 showM memo (cont . toZone)
  M4 reqs cont -> let ty = getType reqs in go ty reqs $ showO4 showM memo (cont . toZone)
  M5 reqs cont -> let ty = getType reqs in go ty reqs $ showO5 showM memo (cont . toZone)
  where
    getType :: [Requirement zone ot] -> Proxy ot
    getType _ = Proxy

    go ty reqs sCont = yesParens $ do
      sTy <- parens <$> showTypeOf ty
      sReqs <- parens <$> showRequirements reqs
      sCont' <- dollar <$> sCont
      pure $ pure "masked @" <> sTy <> pure " " <> sReqs <> sCont'

showWithThis ::
  forall zone x ot.
  IsZO zone ot =>
  PrettyType (ZO zone ot) =>
  (forall ot'. x ot' -> EnvM ParenItems) ->
  String ->
  WithThis zone x ot ->
  EnvM ParenItems
showWithThis showM memo = \case
  T1 cont -> let ty = getType cont in go ty $ showO1 showM memo (cont . toZone)
  T2 cont -> let ty = getType cont in go ty $ showO2 showM memo (cont . toZone)
  T3 cont -> let ty = getType cont in go ty $ showO3 showM memo (cont . toZone)
  T4 cont -> let ty = getType cont in go ty $ showO4 showM memo (cont . toZone)
  T5 cont -> let ty = getType cont in go ty $ showO5 showM memo (cont . toZone)
  where
    getType :: (ZO zone ot -> x ot) -> Proxy ot
    getType _ = Proxy

    go ty sCont = yesParens $ do
      sTy <- parens <$> showTypeOf ty
      sCont' <- dollar <$> sCont
      pure $ pure "thisObject @" <> sTy <> sCont'

showWAny :: WAny ot -> EnvM ParenItems
showWAny any' = case any' of
  WAnyArtifact -> noParens sAny
  WAnyCreature -> noParens sAny
  WAnyEnchantment -> noParens sAny
  WAnyInstant -> noParens sAny
  WAnyLand -> noParens sAny
  WAnyPlaneswalker -> noParens sAny
  WAnyPlayer -> noParens sAny
  WAnySorcery -> noParens sAny
  WAny -> noParens sAny
  WAny2 -> yesParens $ do
    let go :: forall a b. Inst2 IsAnyType a b => WAny (OT2 a b) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT2 a b))
    pure $ pure "WAny2 :: @" <> pure (go any')
  WAny3 -> yesParens $ do
    let go :: forall a b c. Inst3 IsAnyType a b c => WAny (OT3 a b c) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT3 a b c))
    pure $ pure "WAny3 :: @" <> pure (go any')
  WAny4 -> yesParens $ do
    let go :: forall a b c d. Inst4 IsAnyType a b c d => WAny (OT4 a b c d) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT4 a b c d))
    pure $ pure "WAny4 :: @" <> pure (go any')
  WAny5 -> yesParens $ do
    let go :: forall a b c d e. Inst5 IsAnyType a b c d e => WAny (OT5 a b c d e) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT5 a b c d e))
    pure $ pure "WAny5 :: @" <> pure (go any')
  WAny6 -> yesParens $ do
    let go :: forall a b c d e f. Inst6 IsAnyType a b c d e f => WAny (OT6 a b c d e f) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT6 a b c d e f))
    pure $ pure "WAny6 :: @" <> pure (go any')
  where
    sAny :: EnvM Items
    sAny = pure $ pure $ fromString $ show any'

showWCard :: WCard ot -> EnvM ParenItems
showWCard card = case card of
  WCardArtifact -> noParens sCard
  WCardCreature -> noParens sCard
  WCardEnchantment -> noParens sCard
  WCardInstant -> noParens sCard
  WCardLand -> noParens sCard
  WCardPlaneswalker -> noParens sCard
  WCardSorcery -> noParens sCard
  WCard -> noParens sCard
  WCard2 -> yesParens $ do
    let go :: forall a b. Inst2 IsCardType a b => WCard (OT2 a b) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT2 a b))
    pure $ pure "WCard2 :: @" <> pure (go card)
  WCard3 -> yesParens $ do
    let go :: forall a b c. Inst3 IsCardType a b c => WCard (OT3 a b c) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT3 a b c))
    pure $ pure "WCard3 :: @" <> pure (go card)
  where
    sCard :: EnvM Items
    sCard = pure $ pure $ fromString $ show card

showWNonCreatureCard :: WNonCreatureCard ot -> EnvM ParenItems
showWNonCreatureCard nonCreature = case nonCreature of
  WNonCreatureArtifact -> noParens sNonCreature
  WNonCreatureEnchantment -> noParens sNonCreature
  WNonCreatureInstant -> noParens sNonCreature
  WNonCreatureLand -> noParens sNonCreature
  WNonCreaturePlaneswalker -> noParens sNonCreature
  WNonCreatureSorcery -> noParens sNonCreature
  WNonCreatureCard -> noParens sNonCreature
  WNonCreatureCard2 -> yesParens $ do
    let go :: forall a b. Inst2 IsNonCreatureCardType a b => WNonCreatureCard (OT2 a b) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT2 a b))
    pure $ pure "WNonCreatureCard2 :: @" <> pure (go nonCreature)
  WNonCreatureCard3 -> yesParens $ do
    let go :: forall a b c. Inst3 IsNonCreatureCardType a b c => WNonCreatureCard (OT3 a b c) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT3 a b c))
    pure $ pure "WNonCreatureCard3 :: @" <> pure (go nonCreature)
  where
    sNonCreature :: EnvM Items
    sNonCreature = pure $ pure $ fromString $ show nonCreature

showWPermanent :: WPermanent ot -> EnvM ParenItems
showWPermanent permanent = case permanent of
  WPermanentArtifact -> noParens sPermanent
  WPermanentCreature -> noParens sPermanent
  WPermanentEnchantment -> noParens sPermanent
  WPermanentLand -> noParens sPermanent
  WPermanentPlaneswalker -> noParens sPermanent
  WPermanent -> noParens sPermanent
  WPermanent2 -> yesParens $ do
    let go :: forall a b. Inst2 IsPermanentType a b => WPermanent (OT2 a b) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT2 a b))
    pure $ pure "WPermanent2 :: @" <> pure (go permanent)
  WPermanent3 -> yesParens $ do
    let go :: forall a b c. Inst3 IsPermanentType a b c => WPermanent (OT3 a b c) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT3 a b c))
    pure $ pure "WPermanent3 :: @" <> pure (go permanent)
  WPermanent4 -> yesParens $ do
    let go :: forall a b c d. Inst4 IsPermanentType a b c d => WPermanent (OT4 a b c d) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT4 a b c d))
    pure $ pure "WPermanent4 :: @" <> pure (go permanent)
  where
    sPermanent :: EnvM Items
    sPermanent = pure $ pure $ fromString $ show permanent

showWSpell :: WSpell ot -> EnvM ParenItems
showWSpell spell = case spell of
  WSpellArtifact -> noParens sSpell
  WSpellCreature -> noParens sSpell
  WSpellEnchantment -> noParens sSpell
  WSpellInstant -> noParens sSpell
  WSpellPlaneswalker -> noParens sSpell
  WSpellSorcery -> noParens sSpell
  WSpell -> noParens sSpell
  WSpell2 -> yesParens $ do
    let go :: forall a b. Inst2 IsSpellType a b => WSpell (OT2 a b) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT2 a b))
    pure $ pure "WSpell2 :: @" <> pure (go spell)
  WSpell3 -> yesParens $ do
    let go :: forall a b c. Inst3 IsSpellType a b c => WSpell (OT3 a b c) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT3 a b c))
    pure $ pure "WSpell3 :: @" <> pure (go spell)
  WSpell4 -> yesParens $ do
    let go :: forall a b c d. Inst4 IsSpellType a b c d => WSpell (OT4 a b c d) -> Item
        go _ = fromString $ prettyType (Proxy :: Proxy (OT4 a b c d))
    pure $ pure "WSpell4 :: @" <> pure (go spell)
  where
    sSpell :: EnvM Items
    sSpell = pure $ pure $ fromString $ show spell

showZoneObject :: IsZO zone ot => ZO zone ot -> EnvM ParenItems
showZoneObject = \case
  ZOBattlefield _ objN -> showObjectN objN
  ZOLibrary _ objN -> showObjectN objN

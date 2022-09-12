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
import safe Data.Inst (Inst12, Inst2, Inst3, Inst4, Inst5, Inst6, Inst8)
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe Data.Proxy (Proxy (Proxy))
import safe Data.String (IsString (..))
import safe Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import safe MtgPure.Model.CardName (CardName (CardName))
import safe MtgPure.Model.ColoredMana (ColoredMana (..))
import safe MtgPure.Model.ColorlessMana (ColorlessMana (..))
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage (..))
import safe MtgPure.Model.EffectType (EffectType (OneShot))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana (Mana (..))
import safe MtgPure.Model.ManaCost (ManaCost (..))
import safe MtgPure.Model.ManaPool (ManaPool (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectId (ObjectId (ObjectId))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OPermanent,
    OPlayerPlaneswalker,
    OSpell,
  )
import MtgPure.Model.ObjectType (OT, ObjectType (..))
import safe MtgPure.Model.ObjectType.Any (WAny (..))
import safe MtgPure.Model.ObjectType.NonCreatureCard
  ( IsNonCreatureCardType,
    WNonCreatureCard (..),
  )
import safe MtgPure.Model.ObjectType.Permanent
  ( IsPermanentType,
    WPermanent (..),
  )
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrettyObjectName (PrettyObjectName (..))
import safe MtgPure.Model.Recursive
  ( Ability (..),
    Card (..),
    CardTypeDef (..),
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    EventListener (..),
    Requirement (..),
    SetCard (SetCard),
    SetToken (SetToken),
    StaticAbility (..),
    Token (..),
    TriggeredAbility (..),
    WithObject (..),
  )
import safe MtgPure.Model.Selection (Selection (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (Variable (ReifiedVariable))
import safe MtgPure.Model.VisitObjectN
  ( VisitObjectN (visitObjectN'),
  )

defaultDepthLimit :: Maybe Int
defaultDepthLimit = Nothing

instance Show (Ability ot) where
  show = runEnvM defaultDepthLimit . showAbility

instance Show (Card a) where
  show = runEnvM defaultDepthLimit . showCard

instance Show (CardTypeDef t ot) where
  show = runEnvM defaultDepthLimit . showCardTypeDef

instance Show Condition where
  show = runEnvM defaultDepthLimit . showCondition

instance Show (Cost ot) where
  show = runEnvM defaultDepthLimit . showCost

instance Show (Effect e) where
  show = runEnvM defaultDepthLimit . showEffect

instance Show (Elect e a) where
  show = runEnvM defaultDepthLimit . showElect

instance Show (EventListener ot) where
  show = runEnvM defaultDepthLimit . showEventListener

instance Show (Requirement ot) where
  show = runEnvM defaultDepthLimit . showRequirement

instance Show (SetCard a) where
  show = runEnvM defaultDepthLimit . showSetCard

instance Show (SetToken a) where
  show = runEnvM defaultDepthLimit . showSetToken

instance Show (StaticAbility a) where
  show = runEnvM defaultDepthLimit . showStaticAbility

instance Show (Token a) where
  show = runEnvM defaultDepthLimit . showToken

instance Show (TriggeredAbility ot) where
  show = runEnvM defaultDepthLimit . showTriggeredAbility

instance Show (WithObject (Elect e) o) where
  show = runEnvM defaultDepthLimit . showWithObject showElect "obj"

instance Show (WithObject EventListener o) where
  show = runEnvM defaultDepthLimit . showWithObject showEventListener "obj"

type VariableId = Int

type Generation = Int

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

type CardDepth = Maybe Int

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

showObject :: Object a -> EnvM Items
showObject = fmap pure . getObjectName

showObjectType :: forall a. PrettyObjectName a => a -> EnvM ParenItems
showObjectType _ = yesParens $ do
  let name = fromString $ prettyObjectName (Proxy @a)
  pure $ pure name

showObjectNImpl :: IsObjectType a => TypeRep -> Item -> Object a -> EnvM ParenItems
showObjectNImpl objNRef prefix obj = do
  let i = objectToId obj
  sObj <- showObject obj
  State.gets (Map.lookup i . originalObjectRep) >>= \case
    Nothing -> error "impossible"
    Just originalRep -> case originalRep == objNRef of
      False -> yesParens $ pure $ pure prefix <> pure " " <> sObj
      True -> noParens $ pure sObj

showObject1 :: IsObjectType a => ObjectN '(OT, a) -> EnvM ParenItems
showObject1 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "O"

showObject2 :: Inst2 IsObjectType a b => ObjectN '(OT, a, b) -> EnvM ParenItems
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
            | otherwise -> "toObject2"

showObject3 :: Inst3 IsObjectType a b c => ObjectN '(OT, a, b, c) -> EnvM ParenItems
showObject3 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OCreaturePlayerPlaneswalker) -> "asCreaturePlayerPlaneswalker"
            | otherwise -> "toObject3"

showObject4 :: Inst4 IsObjectType a b c d => ObjectN '(OT, a, b, c, d) -> EnvM ParenItems
showObject4 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | otherwise -> "toObject4"

showObject5 :: Inst5 IsObjectType a b c d e => ObjectN '(OT, a, b, c, d, e) -> EnvM ParenItems
showObject5 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OPermanent) -> "asPermanent"
            | otherwise -> "toObject5"

showObject6 :: Inst6 IsObjectType a b c d e f => ObjectN '(OT, a, b, c, d, e, f) -> EnvM ParenItems
showObject6 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OSpell) -> "asSpell"
            | otherwise -> "toObject6"

showObject8 :: Inst8 IsObjectType a b c d e f g h => ObjectN '(OT, a, b, c, d, e, f, g, h) -> EnvM ParenItems
showObject8 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @ODamageSource) -> "asDamageSource"
            | otherwise -> "toObject8"

showObject12 :: Inst12 IsObjectType a b c d e f g h i j k l => ObjectN '(OT, a, b, c, d, e, f, g, h, i, j, k, l) -> EnvM ParenItems
showObject12 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OAny) -> "asAny"
            | otherwise -> "toObject12"

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

showObjectDecl :: PrettyObjectName a => (a -> EnvM ParenItems) -> a -> EnvM ParenItems
showObjectDecl showName obj = yesParens $ do
  sName <- dropParens <$> showName obj
  sType <- dropParens <$> showObjectType obj
  pure $ sName <> pure " :: " <> sType

showOCreaturePlayerPlaneswalker :: OCreaturePlayerPlaneswalker -> EnvM ParenItems
showOCreaturePlayerPlaneswalker = showObject3

showOPermanent :: OPermanent -> EnvM ParenItems
showOPermanent = showObject5

showOActivatedOrTriggeredAbility :: OActivatedOrTriggeredAbility -> EnvM ParenItems
showOActivatedOrTriggeredAbility = showObject2

showOSpell :: OSpell -> EnvM ParenItems
showOSpell = showObject6

showODamageSource :: ODamageSource -> EnvM ParenItems
showODamageSource = showObject8

showOAny :: OAny -> EnvM ParenItems
showOAny = showObject12

showSetToken :: SetToken a -> EnvM ParenItems
showSetToken (SetToken set rarity token) = yesParens $ do
  sToken <- dollar <$> showToken token
  pure $ pure (fromString $ "SetToken " ++ show set ++ " " ++ show rarity) <> sToken

showSetCard :: SetCard a -> EnvM ParenItems
showSetCard (SetCard set rarity card) = yesParens $ do
  sCard <- dollar <$> showCard card
  pure $ pure (fromString $ "SetCard " ++ show set ++ " " ++ show rarity) <> sCard

runEnvM :: CardDepth -> EnvM ParenItems -> String
runEnvM depth m = concat $ State.evalState strsM $ mkEnv depth
  where
    itemsM = DList.toList . dropParens <$> m
    strsM =
      itemsM >>= \items -> do
        let used = getUsed items
        mapM (showItem used) items

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

getObjectNamePrefix :: ObjectId -> EnvM String
getObjectNamePrefix i = State.gets (Map.findWithDefault "impossible" i . objectNames)

showItem :: Used -> Item -> EnvM String
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

showToken :: forall x. Token x -> EnvM ParenItems
showToken (Token card) = yesParens $ do
  sCard <- dollar <$> showCard card
  pure $ pure "Token" <> sCard

showCard :: Card x -> EnvM ParenItems
showCard = \case
  card@Card1 {} ->
    let go :: forall a. Card '(OT, a) -> EnvM ParenItems
        go = \case
          Card1 (CardName name) cont -> yesParens $ do
            depth <- State.gets cardDepth
            State.modify' $ \st -> st {cardDepth = subtract 1 <$> depth}
            let sName = pure (fromString $ show name)
            case depth of
              Just 0 -> pure $ pure "Card " <> sName <> pure " ..."
              _ -> do
                (this, snap) <- newObjectN @a O "this"
                sThis <- parens <$> showObject1 this
                let def = cont this
                sDef <- dropParens <$> showCardTypeDef def
                restoreObject snap
                pure $ pure "Card " <> sName <> pure " $ \\" <> sThis <> pure " -> " <> sDef
     in go card
  card@Card2 {} ->
    let go :: forall a b. Card '(OT, a, b) -> EnvM ParenItems
        go = \case
          Card2 (CardName name) cont -> yesParens $ do
            depth <- State.gets cardDepth
            State.modify' $ \st -> st {cardDepth = subtract 1 <$> depth}
            let sName = pure (fromString $ show name)
            case depth of
              Just 0 -> pure $ pure "Card2 " <> sName <> pure " ..."
              _ -> do
                (this, snap) <- newObjectN @a O2a "this"
                sThis <- parens <$> showObject2 this
                let def = cont this
                sDef <- dropParens <$> showCardTypeDef def
                restoreObject snap
                pure $ pure "Card2 " <> sName <> pure " $ \\" <> sThis <> pure " -> " <> sDef
     in go card
  ArtifactCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "Artifact" <> sCard
  CreatureCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "Creature" <> sCard
  EnchantmentCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "Enchantment" <> sCard
  InstantCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "Instant" <> sCard
  LandCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "Land" <> sCard
  PlaneswalkerCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "Planeswalker" <> sCard
  SorceryCard card -> yesParens $ do
    sCard <- dollar <$> showCard card
    pure $ pure "Sorcery" <> sCard

showCardTypeDef :: CardTypeDef t a -> EnvM ParenItems
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
    sAbilities <- parens <$> showAbilities abilities
    pure $ pure "EnchantmentDef " <> sColors <> pure " " <> sCost <> pure " " <> sAbilities
  InstantDef colors cost abilities electOneShot -> do
    showOneShot "InstantDef " colors cost abilities electOneShot
  LandDef abilities -> yesParens $ do
    sAbilities <- parens <$> showAbilities abilities
    pure $ pure "LandDef " <> sAbilities
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
    sNonCreature <- parens <$> showNonCreatureCard nonCreature
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
      Elect (Cost '(OT, a)) '(OT, a) ->
      [Ability '(OT, a)] ->
      Elect (Effect 'OneShot) '(OT, a) ->
      EnvM ParenItems
    showOneShot def colors cost abilities oneShot = yesParens $ do
      sColors <- parens <$> showColors colors
      sCost <- parens <$> showElect cost
      sAbilities <- parens <$> showAbilities abilities
      sElect <- dollar <$> showElect oneShot
      pure $
        pure def <> sColors <> pure " " <> sCost <> pure " " <> sAbilities <> sElect

showCreatureTypes :: [CreatureType] -> EnvM ParenItems
showCreatureTypes = noParens . pure . pure . fromString . show

showLoyalty :: Loyalty -> EnvM ParenItems
showLoyalty = yesParens . pure . pure . fromString . show

showPower :: Power -> EnvM ParenItems
showPower = yesParens . pure . pure . fromString . show

showToughness :: Toughness -> EnvM ParenItems
showToughness = yesParens . pure . pure . fromString . show

showColors :: Colors -> EnvM ParenItems
showColors = yesParens . pure . pure . fromString . show

selectionMemo :: Selection -> String
selectionMemo = \case
  Choose {} -> "choice"
  Target {} -> "target"
  Random -> "random"

showSelection :: Selection -> EnvM ParenItems
showSelection = \case
  Choose player -> yesParens $ do
    sPlayer <- dollar <$> showObject1 player
    pure $ pure "Choose" <> sPlayer
  Target player -> yesParens $ do
    sPlayer <- dollar <$> showObject1 player
    pure $ pure "Target" <> sPlayer
  Random -> noParens $ pure $ pure "Random"

showListM :: (a -> EnvM ParenItems) -> [a] -> EnvM ParenItems
showListM f xs = noParens $ do
  ss <- mapM (fmap dropParens . f) xs
  pure $ pure "[" <> DList.intercalate (pure ", ") ss <> pure "]"

showRequirements :: [Requirement a] -> EnvM ParenItems
showRequirements = showListM showRequirement

showRequirement :: forall a. Requirement a -> EnvM ParenItems
showRequirement = \case
  ControlledBy obj -> yesParens $ do
    sObj <- dollar <$> showObject1 obj
    pure $ pure "ControlledBy " <> sObj
  HasAbility ability -> yesParens $ do
    sAbility <- dollar <$> showAbility ability
    pure $ pure "HasAbility" <> sAbility
  HasBasicLandType basic -> yesParens $ do
    pure $ pure $ fromString $ "HasBasicLandType " ++ show basic
  Impossible -> noParens $ do
    pure $ pure "Impossible"
  Is wAny objN -> yesParens $ do
    sWAny <- parens <$> showWAny wAny
    sObjN <- dollar <$> showAnyN wAny objN
    pure $ pure "Is " <> sWAny <> sObjN
  Not req -> yesParens $ do
    sReq <- dollar <$> showRequirement req
    pure $ pure "Not" <> sReq
  OfColors colors -> yesParens $ do
    pure $ pure $ fromString $ "OfColors $ " ++ show colors
  OwnedBy obj -> yesParens $ do
    sObj <- dollar <$> showObject1 obj
    pure $ pure "OwnedBy " <> sObj
  PlayerPays cost -> yesParens $ do
    sCost <- dollar <$> showCost cost
    pure $ pure "PlayerPays" <> sCost
  Tapped perm -> yesParens $ do
    pure $ pure $ fromString $ "Tapped " ++ show perm
  RAnd reqs -> yesParens $ do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "RAnd " <> sReqs
  ROr reqs -> yesParens $ do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "ROr " <> sReqs
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

showAbilities :: [Ability a] -> EnvM ParenItems
showAbilities = showListM showAbility

showAbility :: Ability a -> EnvM ParenItems
showAbility = \case
  Activated cost oneShot -> yesParens $ do
    sCost <- parens <$> showElect cost
    sOneShot <- dollar <$> showElect oneShot
    pure $ pure "Activated" <> sCost <> sOneShot
  Static ability -> yesParens $ (pure "Static" <>) . dollar <$> showStaticAbility ability
  Triggered ability -> yesParens $ (pure "Triggered" <>) . dollar <$> showTriggeredAbility ability

showCost :: Cost a -> EnvM ParenItems
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
    sPerm <- parens <$> showPermanent perm
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "SacrificeCost " <> sPerm <> sReqs
  TapCost obj -> yesParens $ do
    sObj <- dollar <$> showOPermanent obj
    pure $ pure "TapCost" <> sObj

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
          tuple = (numW, numU, numB, numR, numG, numC, numX)
      pure $ pure $ fromString $ "toManaCost " ++ show tuple
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
          tuple = (numW, numU, numB, numR, numG, numC)
      pure $ pure $ fromString $ "toManaPool " ++ show tuple
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

showDamage :: Damage -> EnvM ParenItems
showDamage =
  yesParens . \case
    Damage n -> pure $ pure $ fromString $ "Damage " ++ show n
    DamageFromPower obj -> do
      sObj <- dollar <$> showObject1 obj
      pure $ pure "DamageFromPower " <> sObj
    VariableDamage var -> do
      let varName = getVarName var
      pure $ DList.fromList [fromString "VariableDamage ", varName]

showPermanent :: WPermanent x -> EnvM ParenItems
showPermanent permanent = case permanent of
  WPermanentArtifact -> noParens sPermanent
  WPermanentCreature -> noParens sPermanent
  WPermanentEnchantment -> noParens sPermanent
  WPermanentLand -> noParens sPermanent
  WPermanentPlaneswalker -> noParens sPermanent
  WPermanent -> noParens sPermanent
  WPermanent2 -> yesParens $ do
    let go :: forall a b. Inst2 IsPermanentType a b => WPermanent '(OT, a, b) -> Item
        go _ = fromString $ prettyObjectName (Proxy :: Proxy '(OT, a, b))
    pure $ pure "WPermanent2 :: @" <> pure (go permanent)
  WPermanent3 -> yesParens $ do
    let go :: forall a b c. Inst3 IsPermanentType a b c => WPermanent '(OT, a, b, c) -> Item
        go _ = fromString $ prettyObjectName (Proxy :: Proxy '(OT, a, b, c))
    pure $ pure "WPermanent3 :: @" <> pure (go permanent)
  WPermanent4 -> yesParens $ do
    let go :: forall a b c d. Inst4 IsPermanentType a b c d => WPermanent '(OT, a, b, c, d) -> Item
        go _ = fromString $ prettyObjectName (Proxy :: Proxy '(OT, a, b, c, d))
    pure $ pure "WPermanent4 :: @" <> pure (go permanent)
  where
    sPermanent :: EnvM Items
    sPermanent = pure $ pure $ fromString $ show permanent

showNonCreatureCard :: WNonCreatureCard a -> EnvM ParenItems
showNonCreatureCard nonCreature = case nonCreature of
  WNonCreatureArtifact -> noParens sNonCreature
  WNonCreatureEnchantment -> noParens sNonCreature
  WNonCreatureInstant -> noParens sNonCreature
  WNonCreatureLand -> noParens sNonCreature
  WNonCreaturePlaneswalker -> noParens sNonCreature
  WNonCreatureSorcery -> noParens sNonCreature
  WNonCreatureCard -> noParens sNonCreature
  WNonCreatureCard2 -> yesParens $ do
    let go :: forall a b. Inst2 IsNonCreatureCardType a b => WNonCreatureCard '(OT, a, b) -> Item
        go _ = fromString $ prettyObjectName (Proxy :: Proxy '(OT, a, b))
    pure $ pure "WNonCreatureCard2 :: @" <> pure (go nonCreature)
  WNonCreatureCard3 -> yesParens $ do
    let go :: forall a b c. Inst3 IsNonCreatureCardType a b c => WNonCreatureCard '(OT, a, b, c) -> Item
        go _ = fromString $ prettyObjectName (Proxy :: Proxy '(OT, a, b, c))
    pure $ pure "WNonCreatureCard3 :: @" <> pure (go nonCreature)
  where
    sNonCreature :: EnvM Items
    sNonCreature = pure $ pure $ fromString $ show nonCreature

showTriggeredAbility :: TriggeredAbility a -> EnvM ParenItems
showTriggeredAbility = \case
  When listener -> go "When" listener
  where
    go consName eventListener = yesParens $ do
      sEventListener <- dollar <$> showElect eventListener
      pure $ pure (fromString consName) <> sEventListener

showConditions :: [Condition] -> EnvM ParenItems
showConditions = showListM showCondition

showCondition :: Condition -> EnvM ParenItems
showCondition = \case
  CAnd conds -> yesParens $ do
    sConds <- parens <$> showConditions conds
    pure $ pure "CAnd " <> sConds
  COr conds -> yesParens $ do
    sConds <- parens <$> showConditions conds
    pure $ pure "COr " <> sConds
  Satisfies wAny objN reqs -> yesParens $ do
    sWAny <- parens <$> showWAny wAny
    sObjN <- parens <$> showAnyN wAny objN
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Satisfies " <> sWAny <> pure " " <> sObjN <> sReqs

showWAny :: WAny a -> EnvM ParenItems
showWAny = \case
  WAnyInstant -> noParens $ pure $ pure "WAnyInstant"
  WAnySorcery -> noParens $ pure $ pure "WAnySorcery"
  WAnyPlayer -> noParens $ pure $ pure "WAnyPlayer"
  WAnyPermanent perm -> yesParens $ do
    sPerm <- dollar <$> showPermanent perm
    pure $ pure "WAnyPermanent" <> sPerm

showAnyN :: WAny a -> ObjectN a -> EnvM ParenItems
showAnyN wAny objN = case wAny of
  WAnyInstant -> yesParens $ do
    let O obj = objN
    showObject obj
  WAnySorcery -> yesParens $ do
    let O obj = objN
    showObject obj
  WAnyPlayer -> yesParens $ do
    let O obj = objN
    showObject obj
  WAnyPermanent perm -> do
    showPermanentN perm objN

showEventListener :: EventListener a -> EnvM ParenItems
showEventListener = \case
  Events listeners -> yesParens $ do
    sListeners <- dollar <$> showListM showEventListener listeners
    pure $ pure "Evenets" <> sListeners
  SpellIsCast withObject -> yesParens $ do
    sWithObject <- dollar <$> showWithObject showElect "spell" withObject
    pure $ pure "SpellIsCast" <> sWithObject
  TimePoint timePoint oneShot -> yesParens $ do
    sTimePoint <- parens <$> showTimePoint timePoint
    sOneShot <- dollar <$> showElect oneShot
    pure $ pure "TimePoint " <> sTimePoint <> sOneShot

showTimePoint :: TimePoint p -> EnvM ParenItems
showTimePoint = yesParens . pure . pure . fromString . show

showStaticAbility :: StaticAbility a -> EnvM ParenItems
showStaticAbility = \case
  As withObject -> yesParens $ do
    sWithObject <- dollar <$> showWithObject showEventListener "this" withObject
    pure $ pure "As" <> sWithObject
  ContinuousEffect continuous -> yesParens $ do
    sContinuous <- dollar <$> showElect continuous
    pure $ pure "ContinuousEffect" <> sContinuous
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

showElect :: Elect e a -> EnvM ParenItems
showElect = \case
  A sel withObject -> yesParens $ do
    sSel <- parens <$> showSelection sel
    sWithObject <- dollar <$> showWithObject showElect (selectionMemo sel) withObject
    pure $ pure "A " <> sSel <> sWithObject
  ActivePlayer contElect -> yesParens $ do
    (active, snap) <- newObjectN @'OTPlayer O "active"
    sActive <- parens <$> showObject1 active
    let elect = contElect active
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $ pure "ActivePlayer $ \\" <> sActive <> pure " -> " <> sElect
  All withObject -> yesParens $ do
    sWithObject <- dollar <$> showWithObject showElect "all" withObject
    pure $ pure "All" <> sWithObject
  Condition cond -> yesParens $ do
    sCond <- dollar <$> showCondition cond
    pure $ pure "Condition" <> sCond
  ControllerOf obj contElect -> yesParens $ do
    objPrefix <- getObjectNamePrefix $ visitObjectN' objectToId obj
    (controller, snap) <- newObjectN @'OTPlayer O $ case objPrefix == "this" of
      True -> "you"
      False -> "controller"
    sController <- parens <$> showObject1 controller
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
  Event listener -> yesParens $ do
    sListener <- dollar <$> showEventListener listener
    pure $ pure "Event" <> sListener
  If cond then_ else_ -> yesParens $ do
    sCond <- parens <$> showCondition cond
    sThen <- parens <$> showElect then_
    sElse <- dollar <$> showElect else_
    pure $ pure "If " <> sCond <> pure " " <> sThen <> sElse

showWithObject :: (forall a. x a -> EnvM ParenItems) -> String -> WithObject x b -> EnvM ParenItems
showWithObject showM memo = \case
  O1 reqs cont -> showO1 showM memo reqs cont
  O2 reqs cont -> showO2 showM memo reqs cont
  O3 reqs cont -> showO3 showM memo reqs cont
  O4 reqs cont -> showO4 showM memo reqs cont
  O5 reqs cont -> showO5 showM memo reqs cont

showO1 ::
  forall a b x.
  IsObjectType b =>
  (x a -> EnvM ParenItems) ->
  String ->
  [Requirement '(OT, b)] ->
  (ObjectN '(OT, b) -> x a) ->
  EnvM ParenItems
showO1 showM memo reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirements reqs
  (obj, snap) <- newObjectN @b O memo
  objName <- parens <$> showObjectDecl showObject1 obj
  let elect = cont obj
  sElect <- dropParens <$> showM elect
  restoreObject snap
  pure $
    pure "O1 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showO2 ::
  forall a b c.
  Inst2 IsObjectType b c =>
  (a -> EnvM ParenItems) ->
  String ->
  [Requirement '(OT, b, c)] ->
  (ObjectN '(OT, b, c) -> a) ->
  EnvM ParenItems
showO2 showM memo reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirements reqs
  (obj, snap) <- newObjectN @b O2a memo
  objName <- parens <$> showObjectDecl showObject2 obj
  let elect = cont obj
  sElect <- dropParens <$> showM elect
  restoreObject snap
  pure $
    pure "O2 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showO3 ::
  forall a b c d.
  Inst3 IsObjectType b c d =>
  (a -> EnvM ParenItems) ->
  String ->
  [Requirement '(OT, b, c, d)] ->
  (ObjectN '(OT, b, c, d) -> a) ->
  EnvM ParenItems
showO3 showM memo reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirements reqs
  (obj, snap) <- newObjectN @b O3a memo
  objName <- parens <$> showObjectDecl showObject3 obj
  let elect = cont obj
  sElect <- dropParens <$> showM elect
  restoreObject snap
  pure $
    pure "O3 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showO4 ::
  forall a b c d e.
  Inst4 IsObjectType b c d e =>
  (a -> EnvM ParenItems) ->
  String ->
  [Requirement '(OT, b, c, d, e)] ->
  (ObjectN '(OT, b, c, d, e) -> a) ->
  EnvM ParenItems
showO4 showM memo reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirements reqs
  (obj, snap) <- newObjectN @b O4a memo
  objName <- parens <$> showObjectDecl showObject4 obj
  let elect = cont obj
  sElect <- dropParens <$> showM elect
  restoreObject snap
  pure $
    pure "O4 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showO5 ::
  forall a b c d e f.
  Inst5 IsObjectType b c d e f =>
  (a -> EnvM ParenItems) ->
  String ->
  [Requirement '(OT, b, c, d, e, f)] ->
  (ObjectN '(OT, b, c, d, e, f) -> a) ->
  EnvM ParenItems
showO5 showM memo reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirements reqs
  (obj, snap) <- newObjectN @b O5a memo
  objName <- parens <$> showObjectDecl showObject5 obj
  let elect = cont obj
  sElect <- dropParens <$> showM elect
  restoreObject snap
  pure $
    pure "O5 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showPermanentN :: WPermanent a -> ObjectN a -> EnvM ParenItems
showPermanentN perm obj = case perm of
  WPermanentArtifact -> yesParens $ do
    sObj <- visitObjectN' showObject obj
    pure $ pure "WPermanentArtifact " <> sObj
  WPermanentCreature -> yesParens $ do
    sObj <- visitObjectN' showObject obj
    pure $ pure "WPermanentCreature " <> sObj
  WPermanentEnchantment -> yesParens $ do
    sObj <- visitObjectN' showObject obj
    pure $ pure "WPermanentEnchantment " <> sObj
  WPermanentLand -> yesParens $ do
    sObj <- visitObjectN' showObject obj
    pure $ pure "WPermanentLand " <> sObj
  WPermanentPlaneswalker -> yesParens $ do
    sObj <- visitObjectN' showObject obj
    pure $ pure "WPermanentPlaneswalker " <> sObj
  WPermanent -> yesParens $ do
    sObj <- dollar <$> showOPermanent obj
    pure $ pure "WPermanent" <> sObj
  WPermanent2 -> yesParens $ do
    sObj <- dollar <$> showObject2 obj
    pure $ pure "WPermanent2" <> sObj
  WPermanent3 -> yesParens $ do
    sObj <- dollar <$> showObject3 obj
    pure $ pure "WPermanent3" <> sObj
  WPermanent4 -> yesParens $ do
    sObj <- dollar <$> showObject4 obj
    pure $ pure "WPermanent4" <> sObj

showEffect :: Effect e -> EnvM ParenItems
showEffect = \case
  AddMana mana player -> yesParens $ do
    sMana <- parens <$> showManaPool mana
    sPlayer <- dollar <$> showObject1 player
    pure $ pure "AddMana " <> sMana <> sPlayer
  AddToBattlefield perm player token -> yesParens $ do
    sPerm <- parens <$> showPermanent perm
    sPlayer <- parens <$> showObject1 player
    sCard <- dollar <$> showToken token
    pure $ pure "AddToBattlefield " <> sPlayer <> pure " " <> sPerm <> sCard
  ChangeTo perm before after -> yesParens $ do
    sPerm <- parens <$> showPermanent perm
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
    sPlayer <- parens <$> showObject1 player
    let amount = fromString $ show n
    pure $ pure "DrawCards " <> sPlayer <> pure " " <> pure amount
  Sacrifice perm player reqs -> yesParens $ do
    sPerm <- parens <$> showPermanent perm
    sPlayer <- parens <$> showObject1 player
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Sacrifice " <> sPerm <> pure " " <> sPlayer <> sReqs

showEffects :: [Effect e] -> EnvM ParenItems
showEffects = showListM showEffect
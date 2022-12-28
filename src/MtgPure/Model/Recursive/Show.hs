{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.Recursive.Show (
  CardDepth,
  runEnvM,
  showCard,
  showToken,
  showSetCard,
  showSetToken,
) where

import safe qualified Control.Monad.State.Strict as State
import safe qualified Data.DList as DList
import safe Data.Inst (
  Inst10,
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
import safe Data.Nat (Fin (..), NatList (..))
import safe Data.Proxy (Proxy (Proxy))
import safe Data.String (IsString (..))
import safe Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import safe MtgPure.Model.ArtifactType (ArtifactType)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.CardName (CardName (CardName))
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColoredMana (ColoredMana (..))
import safe MtgPure.Model.ColorlessMana (ColorlessMana (..))
import safe MtgPure.Model.Colors (Colors (..))
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana (Mana (..))
import safe MtgPure.Model.ManaCost (ManaCost (..))
import safe MtgPure.Model.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTKind (
  OTAny,
  OTCreaturePlaneswalker,
  OTCreaturePlayer,
  OTCreaturePlayerPlaneswalker,
  OTDamageSource,
  OTPermanent,
  OTPlayerPlaneswalker,
  OTSpell,
 )
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (ObjectId),
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (
  ON0,
  ON1,
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
  ObjectN (..),
 )
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Any (WAny (..))
import safe MtgPure.Model.Object.Singleton.Card (WCard (..))
import safe MtgPure.Model.Object.Singleton.Permanent (WPermanent (..))
import safe MtgPure.Model.Object.Singleton.Spell (WSpell (..))
import safe MtgPure.Model.Object.VisitObjectN (KnownObjectN (..), VisitObjectN (..))
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  CardFacet (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  Else (..),
  Enchant (..),
  EnchantmentType (..),
  Event,
  EventListener,
  EventListener' (..),
  IsUser (..),
  List (..),
  Requirement (..),
  SetCard (SetCard),
  SetToken (SetToken),
  StaticAbility (..),
  Token (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisActivated,
  WithThisOneShot,
  WithThisTriggered,
  YourCard (..),
 )
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (
  Variable (..),
  VariableId,
  VariableId' (..),
  getVariableId,
 )
import safe MtgPure.Model.Zone (IsZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOT,
  IsZO,
  ZO,
  ZoneObject (..),
  toZone,
 )
import safe Prelude hiding (showList)

----------------------------------------

defaultDepthLimit :: Maybe Int
defaultDepthLimit = Nothing

instance Show (Ability ot) where
  show = runEnvM defaultDepthLimit . showAbility

instance Show (ActivatedAbility zone ot) where
  show = runEnvM defaultDepthLimit . showActivatedAbility

instance Show AnyCard where
  show = runEnvM defaultDepthLimit . showAnyCard

instance Show AnyToken where
  show = runEnvM defaultDepthLimit . showAnyToken

instance Show (Card ot) where
  show = runEnvM defaultDepthLimit . showCard

instance Show (CardFacet ot) where
  show = runEnvM defaultDepthLimit . showCardFacet

instance Show CompleteManaPool where
  show = runEnvM defaultDepthLimit . showCompleteManaPool

instance Show Condition where
  show = runEnvM defaultDepthLimit . showCondition

instance Show (Cost ot) where
  show = runEnvM defaultDepthLimit . showCost

instance Show (Effect ef) where
  show = runEnvM defaultDepthLimit . showEffect

instance Show (Elect p el ot) where
  show = runEnvM defaultDepthLimit . showElect

instance Show EventListener where
  show = runEnvM defaultDepthLimit . showEventListener

instance Show (ManaCost var) where
  show = runEnvM defaultDepthLimit . showManaCost

instance Show (ManaPool snow) where
  show = runEnvM defaultDepthLimit . showManaPool

instance Show (Requirement zone ot) where
  show = runEnvM defaultDepthLimit . showRequirement

instance Show (SetCard ot) where
  show = runEnvM defaultDepthLimit . showSetCard

instance Show (SetToken ot) where
  show = runEnvM defaultDepthLimit . showSetToken

instance Show (StaticAbility zone ot) where
  show = runEnvM defaultDepthLimit . showStaticAbility

instance Show (Token ot) where
  show = runEnvM defaultDepthLimit . showToken

instance Show (TriggeredAbility zone ot) where
  show = runEnvM defaultDepthLimit . showTriggeredAbility

instance IsZO zone ot => Show (WithMaskedObject zone (Elect p e ot)) where
  show = runEnvM defaultDepthLimit . showWithMaskedObject showElect "obj"

instance IsZO zone ot => Show (WithThisActivated zone ot) where
  show = runEnvM defaultDepthLimit . showWithThis showElect "this"

instance IsZO 'ZStack ot => Show (WithThisOneShot ot) where
  show = runEnvM defaultDepthLimit . showWithThis showElect "this"

instance IsZO zone ot => Show (WithThisTriggered zone ot) where
  show = runEnvM defaultDepthLimit . showWithThis showTriggeredAbility "this"

----------------------------------------

class LiteralMana mana where
  literalMana :: mana -> Maybe Int

instance LiteralMana (ColoredMana var mt) where
  literalMana = \case
    ColoredMana' _ x -> Just x
    VariableColoredMana{} -> Nothing
    SumColoredMana{} -> Nothing

instance LiteralMana (ColorlessMana var) where
  literalMana = \case
    ColorlessMana' x -> Just x
    VariableColorlessMana{} -> Nothing
    SumColorlessMana{} -> Nothing

instance LiteralMana (GenericMana var) where
  literalMana = \case
    GenericMana' x -> Just x
    VariableGenericMana{} -> Nothing
    SumGenericMana{} -> Nothing

instance LiteralMana (Mana var snow mt) where
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

type Generation = Int

data Env = Env
  { nextObjectId :: ObjectId
  , nextVariableId :: VariableId
  , originalObjectRep :: Map.Map ObjectId TypeRep
  , currentGeneration :: Generation
  , objectGenerations :: Map.Map ObjectId Generation
  , objectNames :: Map.Map ObjectId String
  , cardDepth :: CardDepth
  }

mkEnv :: CardDepth -> Env
mkEnv depth =
  Env
    { nextObjectId = ObjectId 1
    , nextVariableId = VariableId 0
    , originalObjectRep = mempty
    , currentGeneration = 0
    , objectGenerations = mempty
    , objectNames = mempty
    , cardDepth = max 0 <$> depth
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
      pure case Map.findWithDefault False (i, g) (usedObjects used) of
        False -> "_" ++ name
        True -> name
    VariableItem vid@(VariableId i) -> do
      let name = varNames !! i
      pure case Map.findWithDefault False vid (usedVariables used) of
        False -> "_" ++ name
        True -> name

type UsedObjects = Map.Map (ObjectId, Generation) Bool

type UsedVariables = Map.Map VariableId Bool

data Used = Used
  { usedObjects :: UsedObjects
  , usedVariables :: UsedVariables
  }

getUsed :: [Item] -> Used
getUsed = flip foldr empty \item used -> case item of
  StringItem{} -> used
  ObjectItem i g ->
    used
      { usedObjects =
          Map.insertWith (\_ _ -> True) (i, g) False $
            usedObjects used
      }
  VariableItem var ->
    used
      { usedVariables =
          Map.insertWith (\_ _ -> True) var False $
            usedVariables used
      }
 where
  empty = Used mempty mempty

-- TODO: Make this better now that variables can be procured through various abstract means.
varNames :: [String]
varNames = "x" : "y" : "z" : map f [0 ..]
 where
  f :: Int -> String
  f n = "var" ++ show n

getVarName :: Variable a -> Item
getVarName = VariableItem . getVariableId

getObjectName :: Object a -> EnvM Item
getObjectName (Object _ (UntypedObject _ i)) = do
  gens <- State.gets objectGenerations
  case Map.lookup i gens of
    Nothing -> error "impossible"
    Just g -> pure $ ObjectItem i g

newtype ObjectIdState = ObjectIdState ObjectId

newObject ::
  forall a. IsObjectType a => String -> EnvM (Object a, ObjectIdState)
newObject name = do
  i@(ObjectId raw) <- State.gets nextObjectId
  let obj = idToObject @a $ UntypedObject DefaultObjectDiscriminant i
  State.modify' \st ->
    st
      { nextObjectId = ObjectId $ raw + 1
      , originalObjectRep = Map.insert i (typeOf obj) $ originalObjectRep st
      , currentGeneration = currentGeneration st + 1
      , objectGenerations =
          Map.insert i (currentGeneration st) $
            objectGenerations st
      , objectNames = Map.insert i name $ objectNames st
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
  State.modify' \st ->
    st
      { originalObjectRep = Map.insert i (typeOf objN) $ originalObjectRep st
      }
  pure (objN, snap)

restoreObject :: ObjectIdState -> EnvM ()
--restoreObject (ObjectIdState i) = State.modify' $ \st -> st {nextObjectId = i}
restoreObject _ = pure ()

data Plurality = Singular | Plural

pluralize :: EnvM ParenItems -> EnvM ParenItems
pluralize m = do
  (p, items) <- m
  pure (p, items <> pure "s")

lenseList :: List x -> x
lenseList = \case
  List [x] -> x
  _ -> error "logic error: should not happen by construction"

getObjectNamePrefix :: ObjectId -> EnvM String
getObjectNamePrefix i =
  State.gets (Map.findWithDefault "impossible" i . objectNames)

showListM :: (a -> EnvM ParenItems) -> [a] -> EnvM ParenItems
showListM f xs = noParens do
  ss <- mapM (fmap dropParens . f) xs
  pure $ pure "[" <> DList.intercalate (pure ", ") ss <> pure "]"

----------------------------------------

showAbility :: Ability ot -> EnvM ParenItems
showAbility = \case
  Activated ability -> yesParens do
    sAbility <- dollar <$> showWithThis showElect "this" ability
    pure $ pure "Activated" <> sAbility
  Static ability -> yesParens do
    sAbility <- dollar <$> showStaticAbility ability
    pure $ pure "Static" <> sAbility
  Triggered ability -> yesParens do
    sAbility <- dollar <$> showWithThis showTriggeredAbility "this" ability
    pure $ pure "Triggered" <> sAbility

showAbilities :: [Ability ot] -> EnvM ParenItems
showAbilities = showListM showAbility

showAnyCard :: AnyCard -> EnvM ParenItems
showAnyCard = \case
  AnyCard card -> showCard card

showAnyToken :: AnyToken -> EnvM ParenItems
showAnyToken = \case
  AnyToken card -> showToken card

showActivatedAbility :: ActivatedAbility zone ot -> EnvM ParenItems
showActivatedAbility = \case
  Ability cost effect -> yesParens do
    sCost <- parens <$> showCost cost
    sEffect <- dollar <$> showElect effect
    pure $ pure "Ability " <> sCost <> sEffect

showArtifactType :: ArtifactType -> EnvM ParenItems
showArtifactType = noParens . pure . pure . fromString . show

showArtifactTypes :: [ArtifactType] -> EnvM ParenItems
showArtifactTypes = showListM showArtifactType

showBasicLandType :: BasicLandType -> EnvM ParenItems
showBasicLandType = noParens . pure . pure . fromString . show

showCard :: Card ot -> EnvM ParenItems
showCard = \case
  Card (CardName name) yourCard -> yesParens do
    depth <- State.gets cardDepth
    State.modify' \st -> st{cardDepth = subtract 1 <$> depth}
    let sName = pure (fromString $ show name)
    case depth of
      Just 0 -> pure $ pure "Card " <> sName <> pure " ..."
      _ -> do
        sYourCard <- dollar <$> showYourCard yourCard
        pure $
          pure "Card "
            <> sName
            <> sYourCard

showCardFacet :: CardFacet ot -> EnvM ParenItems
showCardFacet = \case
  ArtifactFacet colors cost artTypes creatTypes abilities -> yesParens do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showCost cost
    sArtTypes <- parens <$> showArtifactTypes artTypes
    sCreatTypes <- parens <$> showCreatureTypes creatTypes
    sAbilities <- dollar <$> showAbilities abilities
    pure $
      pure "ArtifactFacet "
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sArtTypes
        <> pure " "
        <> sCreatTypes
        <> sAbilities
  ArtifactCreatureFacet colors cost artTypes creatTypes power toughness artAbils creatAbils ->
    yesParens do
      sColors <- parens <$> showColors colors
      sCost <- parens <$> showCost cost
      sArtTypes <- parens <$> showArtifactTypes artTypes
      sCreatTypes <- parens <$> showCreatureTypes creatTypes
      sPower <- parens <$> showPower power
      sToughness <- parens <$> showToughness toughness
      sArtAbils <- parens <$> showAbilities artAbils
      sCreatAbils <- dollar <$> showAbilities creatAbils
      pure $
        pure "ArtifactCreatureFacet "
          <> sColors
          <> pure " "
          <> sCost
          <> pure " "
          <> sArtTypes
          <> pure " "
          <> sCreatTypes
          <> pure " "
          <> sPower
          <> pure " "
          <> sToughness
          <> pure " "
          <> sArtAbils
          <> sCreatAbils
  ArtifactLandFacet artTypes creatTypes landTypes artAbils landAbils ->
    yesParens do
      sArtTypes <- parens <$> showArtifactTypes artTypes
      sCreatTypes <- parens <$> showCreatureTypes creatTypes
      sLandTypes <- parens <$> showLandTypes landTypes
      sArtAbils <- parens <$> showAbilities artAbils
      sLandAbils <- dollar <$> showAbilities landAbils
      pure $
        pure "ArtifactLandFacet "
          <> sArtTypes
          <> pure " "
          <> sCreatTypes
          <> pure " "
          <> sLandTypes
          <> pure " "
          <> sArtAbils
          <> sLandAbils
  CreatureFacet colors cost creatureTypes power toughness abilities ->
    yesParens do
      sColors <- parens <$> showColors colors
      sCost <- parens <$> showCost cost
      sCreatureTypes <- parens <$> showCreatureTypes creatureTypes
      sPower <- parens <$> showPower power
      sToughness <- parens <$> showToughness toughness
      sAbilities <- dollar <$> showAbilities abilities
      pure $
        pure "CreatureFacet "
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
  EnchantmentFacet colors cost creatTypes enchantTypes abilities -> yesParens do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showCost cost
    sCreatTypes <- parens <$> showCreatureTypes creatTypes
    sEnchantTypes <- parens <$> showEnchantmentTypes enchantTypes
    sAbilities <- dollar <$> showAbilities abilities
    pure $
      pure "EnchantmentFacet " <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sCreatTypes
        <> pure " "
        <> sEnchantTypes
        <> sAbilities
  EnchantmentCreatureFacet colors cost creatTypes power toughness creatAbils enchAbils bothAbils ->
    yesParens do
      sColors <- parens <$> showColors colors
      sCost <- parens <$> showCost cost
      sCreatTypes <- parens <$> showCreatureTypes creatTypes
      sPower <- parens <$> showPower power
      sToughness <- parens <$> showToughness toughness
      sCreatAbils <- parens <$> showAbilities creatAbils
      sEnchAbils <- parens <$> showAbilities enchAbils
      sBothAbils <- dollar <$> showAbilities bothAbils
      pure $
        pure "EnchantmentCreatureFacet "
          <> sColors
          <> pure " "
          <> sCost
          <> pure " "
          <> sCreatTypes
          <> pure " "
          <> sPower
          <> pure " "
          <> sToughness
          <> pure " "
          <> sCreatAbils
          <> pure " "
          <> sEnchAbils
          <> sBothAbils
  InstantFacet colors cost creatTypes abilities oneShot -> do
    showOneShot "InstantFacet " colors cost creatTypes abilities oneShot
  LandFacet creatTypes landTypes abilities -> yesParens do
    sCreatTypes <- parens <$> showCreatureTypes creatTypes
    sLandTypes <- parens <$> showLandTypes landTypes
    sAbilities <- dollar <$> showAbilities abilities
    pure $ pure "LandFacet " <> sCreatTypes <> pure " " <> sLandTypes <> sAbilities
  PlaneswalkerFacet colors cost loyalty abilities -> yesParens do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showCost cost
    sLoyalty <- parens <$> showLoyalty loyalty
    sAbilities <- dollar <$> showAbilities abilities
    pure $
      pure "PlaneswalkerFacet "
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sLoyalty
        <> sAbilities
  SorceryFacet colors cost creatTypes abilities oneShot -> do
    showOneShot "SorceryFacet " colors cost creatTypes abilities oneShot
 where
  showOneShot ::
    forall a ot.
    IsObjectType a =>
    ot ~ OT1 a =>
    Item ->
    Colors ->
    Cost ot ->
    [CreatureType] ->
    [Ability ot] ->
    WithThisOneShot ot ->
    EnvM ParenItems
  showOneShot def colors cost creatTypes abilities oneShot = yesParens do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showCost cost
    sCreatTypes <- parens <$> showCreatureTypes creatTypes
    sAbilities <- parens <$> showAbilities abilities
    sOneShot <- dollar <$> showWithThis showElect "this" oneShot
    pure $
      pure def
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sCreatTypes
        <> pure " "
        <> sAbilities
        <> sOneShot

showCase :: (x -> EnvM ParenItems) -> Case x -> EnvM ParenItems
showCase showX = \case
  CaseFin fin natList -> yesParens do
    let sFin = pure $ getVarName fin
    sNatList <- dollar <$> showNatList showX natList
    pure $ pure "CaseFin " <> sFin <> sNatList

showColor :: Color -> EnvM ParenItems
showColor = noParens . pure . pure . fromString . show

class ShowColors colors where
  showColors :: colors -> EnvM ParenItems

instance ShowColors [Color] where
  showColors = showListM showColor

instance ShowColors Colors where
  showColors colors = yesParens do
    pure $ pure "toColors " <> sOpen <> sSyms <> sClose
   where
    syms = case colors of
      Colors w u b r g ->
        List.intercalate "," $
          catMaybes
            [show <$> w, show <$> u, show <$> b, show <$> r, show <$> g]
    sSyms = pure $ fromString syms
    (sOpen, sClose) = case syms of
      [_] -> (pure "", pure "")
      _ -> (pure "(", pure ")")

showColorlessMana :: ColorlessMana var -> EnvM ParenItems
showColorlessMana =
  yesParens . \case
    x@ColorlessMana'{} -> pure $ pure $ fromString $ show x
    VariableColorlessMana var -> do
      let sVar = pure $ getVarName var
      pure $ pure "VariableColorlessMana " <> sVar
    SumColorlessMana x y -> do
      sX <- parens <$> showColorlessMana x
      sY <- parens <$> showColorlessMana y
      pure $ pure "SumColorlessMana " <> sX <> pure " " <> sY

showColoredMana :: ColoredMana var a -> EnvM ParenItems
showColoredMana =
  yesParens . \case
    x@ColoredMana'{} -> pure $ pure $ fromString $ show x
    VariableColoredMana sym var -> do
      let sSym = pure $ fromString $ show sym
          sVar = pure $ getVarName var
      pure $ pure "VariableColoredMana " <> sSym <> pure " " <> sVar
    SumColoredMana sym x y -> do
      let sSym = pure $ fromString $ show sym
      sX <- parens <$> showColoredMana x
      sY <- parens <$> showColoredMana y
      pure $ pure "SumColoredMana " <> sSym <> pure " " <> sX <> pure " " <> sY

showCompleteManaPool :: CompleteManaPool -> EnvM ParenItems
showCompleteManaPool complete = yesParens do
  sSnow <- parens <$> showManaPool snow
  sNonSnow <- dollar <$> showManaPool nonSnow
  pure $ pure "CompleteManaPool " <> sSnow <> sNonSnow
 where
  CompleteManaPool
    { poolSnow = snow
    , poolNonSnow = nonSnow
    } = complete

showCondition :: Condition -> EnvM ParenItems
showCondition = \case
  CAnd conds -> yesParens do
    sConds <- parens <$> showConditions conds
    pure $ pure "CAnd " <> sConds
  CNot cond -> yesParens do
    sCond <- parens <$> showCondition cond
    pure $ pure "CNot " <> sCond
  COr conds -> yesParens do
    sConds <- parens <$> showConditions conds
    pure $ pure "COr " <> sConds
  Satisfies wAny objN reqs -> yesParens do
    sWAny <- parens <$> showWAny wAny
    sObjN <- parens <$> showZoneObject objN
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Satisfies " <> sWAny <> pure " " <> sObjN <> sReqs

showConditions :: [Condition] -> EnvM ParenItems
showConditions = showListM showCondition

showCost :: Cost ot -> EnvM ParenItems
showCost = \case
  AndCosts costs -> yesParens do
    sCosts <- dollar <$> showListM showCost costs
    pure $ pure "AndCosts" <> sCosts
  CostCase case_ -> yesParens do
    sCase <- dollar <$> showCase showCost case_
    pure $ pure "CostCase" <> sCase
  DiscardRandomCost amount -> yesParens do
    let sAmount = pure $ fromString $ show amount
    pure $ pure "DiscardRandomCost " <> sAmount
  LoyaltyCost loyalty -> yesParens do
    sLoyalty <- dollar <$> showLoyalty loyalty
    pure $ pure "LoyaltyCost " <> sLoyalty
  ManaCost cost -> yesParens do
    sCost <- dollar <$> showManaCost cost
    pure $ pure (fromString "ManaCost") <> sCost
  OrCosts costs -> yesParens do
    sCosts <- parens <$> showListM showCost costs
    pure $ pure "OrCosts " <> sCosts
  PayLife amount -> yesParens do
    let sAmount = pure $ fromString $ show amount
    pure $ pure "PayLife " <> sAmount
  SacrificeCost perm reqs -> yesParens do
    sPerm <- parens <$> showWPermanent perm
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "SacrificeCost " <> sPerm <> sReqs
  TapCost reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "TapCost" <> sReqs

showCreatureType :: CreatureType -> EnvM ParenItems
showCreatureType = noParens . pure . pure . fromString . show

showCreatureTypes :: [CreatureType] -> EnvM ParenItems
showCreatureTypes = showListM showCreatureType

showDamage :: Damage var -> EnvM ParenItems
showDamage =
  yesParens . \case
    Damage n -> do
      pure $ pure $ fromString $ "Damage " ++ show n
    VariableDamage var -> do
      let varName = getVarName var
      pure $ DList.fromList [fromString "VariableDamage ", varName]

showEffect :: Effect e -> EnvM ParenItems
showEffect = \case
  AddMana player mana -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sMana <- dollar <$> showManaPool mana
    pure $ pure "AddMana " <> sPlayer <> sMana
  AddToBattlefield perm player token -> yesParens do
    sPerm <- parens <$> showWPermanent perm
    sPlayer <- parens <$> showZoneObject player
    sCard <- dollar <$> showToken token
    pure $ pure "AddToBattlefield " <> sPerm <> pure " " <> sPlayer <> sCard
  CantBeRegenerated creature -> yesParens do
    sCreature <- dollar <$> showZoneObject creature
    pure $ pure "CantBeRegenerated" <> sCreature
  ChangeTo perm before after -> yesParens do
    sPerm <- parens <$> showWPermanent perm
    sBefore <- parens <$> showZoneObject before
    sAfter <- dollar <$> showCard after
    pure $ pure "ChangeTo " <> sPerm <> pure " " <> sBefore <> sAfter
  CounterAbility obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "CounterAbility" <> sObj
  CounterSpell obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "CounterSpell" <> sObj
  DealDamage source victim damage -> yesParens do
    sSource <- parens <$> showZoneObject source
    sVictim <- parens <$> showZoneObject victim
    sDamage <- dollar <$> showDamage damage
    pure $ pure "DealDamage " <> sSource <> pure " " <> sVictim <> sDamage
  Destroy obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "Destroy" <> sObj
  DrawCards player n -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    let amount = fromString $ show n
    pure $ pure "DrawCards " <> sPlayer <> pure " " <> pure amount
  EffectCase case_ -> yesParens do
    sCase <- dollar <$> showCase showEffect case_
    pure $ pure "EffectCase" <> sCase
  EffectContinuous effect -> yesParens do
    sEffect <- dollar <$> showEffect effect
    pure $ pure "EffectContinuous" <> sEffect
  Gain wAny obj ability -> yesParens do
    sWAny <- parens <$> showWAny wAny
    sObj <- parens <$> showZoneObject obj
    sAbility <- dollar <$> showAbility ability
    pure $ pure "Gain " <> sWAny <> pure " " <> sObj <> sAbility
  Lose wAny obj ability -> yesParens do
    sWAny <- parens <$> showWAny wAny
    sObj <- parens <$> showZoneObject obj
    sAbility <- dollar <$> showAbility ability
    pure $ pure "Lose " <> sWAny <> pure " " <> sObj <> sAbility
  PutOntoBattlefield wPerm player obj -> yesParens do
    sWPerm <- parens <$> showWPermanent wPerm
    sPlayer <- parens <$> showZoneObject player
    sCard <- dollar <$> showZoneObject obj
    pure $ pure "PutOntoBattlefield " <> sWPerm <> pure " " <> sPlayer <> sCard
  Sacrifice perm player reqs -> yesParens do
    sPerm <- parens <$> showWPermanent perm
    sPlayer <- parens <$> showZoneObject player
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Sacrifice " <> sPerm <> pure " " <> sPlayer <> sReqs
  SearchLibrary wCard player withCard -> yesParens do
    sWCard <- parens <$> showWCard wCard
    sPlayer <- parens <$> showZoneObject player
    sWithCard <- dollar <$> showWithLinkedObject showElect "card" withCard
    pure $ pure "SearchLibrary " <> sWCard <> pure " " <> sPlayer <> sWithCard
  Sequence effects -> yesParens do
    sEffects <- dollar <$> showEffects effects
    pure $ pure "Sequence" <> sEffects
  ShuffleLibrary player -> yesParens do
    sPlayer <- dollar <$> showZoneObject player
    pure $ pure "ShuffleLibrary" <> sPlayer
  StatDelta creature power toughness -> yesParens do
    sCreature <- parens <$> showZoneObject creature
    sPower <- parens <$> showPower power
    sToughness <- dollar <$> showToughness toughness
    pure $ pure "StatDelta " <> sCreature <> pure " " <> sPower <> sToughness
  Tap obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "Tap" <> sObj
  Untap obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "Untap" <> sObj
  Until electEvent effect -> yesParens do
    sElectEvent <- parens <$> showElect electEvent
    sEffect <- dollar <$> showEffect effect
    pure $ pure "Until " <> sElectEvent <> sEffect
  WithList withList -> showWithList showEffect withList

showEffects :: [Effect e] -> EnvM ParenItems
showEffects = showListM showEffect

showElect :: Elect p e ot -> EnvM ParenItems
showElect = \case
  ActivePlayer contElect -> yesParens do
    (active', snap) <- newObjectN @ 'OTPlayer O1 "active"
    let active = toZone active'
        elect = contElect active
    sActive <- parens <$> showZoneObject active
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $ pure "ActivePlayer $ \\" <> sActive <> pure " -> " <> sElect
  All withObjects -> yesParens do
    sWithObjects <- dollar <$> showWithMaskedObjects showElect "obj" withObjects
    pure $ pure "All" <> sWithObjects
  Choose player withObject -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sWithObject <- dollar <$> showWithMaskedObject showElect "choose" withObject
    pure $ pure "Choose " <> sPlayer <> sWithObject
  ChooseOption player natList varToElect -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sNatList <- parens <$> showNatList showCondition natList
    discr <- State.gets nextVariableId
    State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
    let var = ReifiedVariable discr FZ
        varName = getVarName var
        elect = varToElect var
    sElect <- dropParens <$> showElect elect
    pure $
      pure "ChooseOption " <> sPlayer <> pure " "
        <> sNatList
        <> pure " $ \\"
        <> pure varName
        <> pure " -> "
        <> sElect
  Condition cond -> yesParens do
    sCond <- dollar <$> showCondition cond
    pure $ pure "Condition" <> sCond
  ControllerOf zObj contElect -> yesParens do
    objPrefix <-
      getObjectNamePrefix
        let objN :: ObjectN OTAny
            objN = case zObj of
              ZO _ o -> o
         in visitObjectN' objectToId objN
    (controller', snap) <-
      newObjectN @ 'OTPlayer O1 case objPrefix == "this" of
        True -> "you"
        False -> "controller"
    let controller = toZone controller'
    sController <- parens <$> showZoneObject controller
    sZObj <- parens <$> showZoneObject zObj
    let elect = contElect controller
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $
      pure "ControllerOf "
        <> sZObj
        <> pure " $ \\"
        <> sController
        <> pure " -> "
        <> sElect
  Cost cost -> yesParens do
    sCost <- dollar <$> showCost cost
    pure $ pure "Cost" <> sCost
  Effect effect -> yesParens do
    sEffect <- dollar <$> showEffects effect
    pure $ pure "Effect" <> sEffect
  Elect elect -> yesParens do
    sElect <- dollar <$> showElect elect
    pure $ pure "Elect" <> sElect
  ElectActivated activated -> yesParens do
    sPost <- dollar <$> showActivatedAbility activated
    pure $ pure "ElectActivated" <> sPost
  ElectCard post -> yesParens do
    sPost <- dollar <$> showCardFacet post
    pure $ pure "ElectCard" <> sPost
  ElectCase case_ -> yesParens do
    sCase <- dollar <$> showCase showElect case_
    pure $ pure "ElectCase" <> sCase
  Event event -> yesParens do
    sEvent <- dollar <$> showEvent event
    pure $ pure "Event" <> sEvent
  If cond then_ else_ -> yesParens do
    sCond <- parens <$> showCondition cond
    sThen <- parens <$> showElect then_
    sElse <- dollar <$> showElse else_
    pure $ pure "If " <> sCond <> pure " " <> sThen <> sElse
  Listen listener -> yesParens do
    sListener <- dollar <$> showEventListener listener
    pure $ pure "Listen" <> sListener
  Random withObject -> yesParens do
    sWithObject <- dollar <$> showWithMaskedObject showElect "rand" withObject
    pure $ pure "Random" <> sWithObject
  Target player withObject -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sWithObject <- dollar <$> showWithMaskedObject showElect "target" withObject
    pure $ pure "Target " <> sPlayer <> sWithObject
  VariableFromPower creature varToElect -> yesParens do
    sCreature <- parens <$> showZoneObject creature
    discr <- State.gets nextVariableId
    State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
    let var = ReifiedVariable discr 0
        varName = getVarName var
        elect = varToElect var
    sElect <- dropParens <$> showElect elect
    pure $
      pure "VariableFromPower "
        <> sCreature
        <> pure " $ \\"
        <> pure varName
        <> pure " -> "
        <> sElect
  VariableInt contElect -> yesParens do
    discr <- State.gets nextVariableId
    State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
    let var = ReifiedVariable discr 0
        varName = getVarName var
        elect = contElect var
    sElect <- dropParens <$> showElect elect
    pure $ pure "VariableInt $ \\" <> pure varName <> pure " -> " <> sElect

showElse :: Else e ot -> EnvM ParenItems
showElse = \case
  ElseCost elect -> yesParens do
    sElect <- dollar <$> showElect elect
    pure $ pure "ElseCost" <> sElect
  ElseEffect elect -> yesParens do
    sElect <- dollar <$> showElect elect
    pure $ pure "ElseEffect" <> sElect
  ElseEvent -> noParens do
    pure $ pure "ElseEvent"

showEnchant :: Enchant zone ot -> EnvM ParenItems
showEnchant = \case
  Enchant withObj -> yesParens do
    sWithObj <- dollar <$> showWithLinkedObject showElect "enchanted" withObj
    pure $ pure "Enchant" <> sWithObj

showEnchantmentType :: EnchantmentType ot -> EnvM ParenItems
showEnchantmentType = \case
  Aura enchant -> yesParens do
    sEnchant <- dollar <$> showEnchant enchant
    pure $ pure "Aura" <> sEnchant

showEnchantmentTypes :: [EnchantmentType ot] -> EnvM ParenItems
showEnchantmentTypes = showListM showEnchantmentType

showEvent :: Event -> EnvM ParenItems
showEvent = showEventListener' \Proxy -> noParens $ pure $ pure "Proxy"

showEventListener :: EventListener -> EnvM ParenItems
showEventListener = showEventListener' showElect

showEventListener' ::
  (forall ot. x ot -> EnvM ParenItems) ->
  EventListener' x ->
  EnvM ParenItems
showEventListener' showX = \case
  BecomesTapped perm withObject -> yesParens do
    sPerm <- parens <$> showWPermanent perm
    sWithObject <- dollar <$> showWithLinkedObject showX "perm" withObject
    pure $ pure "BecomesTapped " <> sPerm <> sWithObject
  Events listeners -> yesParens do
    sListeners <- dollar <$> showListM (showEventListener' showX) listeners
    pure $ pure "Evenets" <> sListeners
  SpellIsCast spell withObject -> yesParens do
    sSpell <- parens <$> showWSpell spell
    sWithObject <- dollar <$> showWithLinkedObject showX "spell" withObject
    pure $ pure "SpellIsCast " <> sSpell <> sWithObject
  TimePoint timePoint oneShot -> yesParens do
    sTimePoint <- parens <$> showTimePoint timePoint
    sOneShot <- dollar <$> showX oneShot
    pure $ pure "TimePoint " <> sTimePoint <> sOneShot

showGenericMana :: GenericMana var -> EnvM ParenItems
showGenericMana =
  yesParens . \case
    x@GenericMana'{} -> pure $ pure $ fromString $ show x
    VariableGenericMana var -> do
      let sVar = pure $ getVarName var
      pure $ pure "VariableGenericMana " <> sVar
    SumGenericMana x y -> do
      sX <- parens <$> showGenericMana x
      sY <- parens <$> showGenericMana y
      pure $ pure "SumGenericMana " <> sX <> pure " " <> sY

showLandType :: LandType -> EnvM ParenItems
showLandType landType = case landType of
  BasicLand basic -> yesParens do
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

showLandTypes :: [LandType] -> EnvM ParenItems
showLandTypes = showListM showLandType

showLoyalty :: Loyalty -> EnvM ParenItems
showLoyalty = yesParens . pure . pure . fromString . show

showMana :: Mana var snow a -> EnvM ParenItems
showMana =
  yesParens . \case
    WhiteMana m -> (pure "WhiteMana" <>) . dollar <$> showColoredMana m
    BlueMana m -> (pure "BlueMana" <>) . dollar <$> showColoredMana m
    BlackMana m -> (pure "BlackMana" <>) . dollar <$> showColoredMana m
    RedMana m -> (pure "RedMana" <>) . dollar <$> showColoredMana m
    GreenMana m -> (pure "GreenMana" <>) . dollar <$> showColoredMana m
    ColorlessMana m -> (pure "ColorlessMana" <>) . dollar <$> showColorlessMana m
    GenericMana m -> (pure "GenericMana" <>) . dollar <$> showGenericMana m

showManaCost :: ManaCost var -> EnvM ParenItems
showManaCost cost = yesParens do
  let ManaCost'
        { costWhite = w
        , costBlue = u
        , costBlack = b
        , costRed = r
        , costGreen = g
        , costColorless = c
        , costGeneric = x
        } = cost
      lits =
        sequence
          [ literalMana w
          , literalMana u
          , literalMana b
          , literalMana r
          , literalMana g
          , literalMana c
          , literalMana x
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
          manas' =
            [go' numX, go numW, go numU, go numB, go numR, go numG, go numC]
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

showManaPool :: ManaPool snow -> EnvM ParenItems
showManaPool pool = yesParens do
  let ManaPool
        { poolWhite = w
        , poolBlue = u
        , poolBlack = b
        , poolRed = r
        , poolGreen = g
        , poolColorless = c
        } = pool
      lits =
        sequence
          [ literalMana w
          , literalMana u
          , literalMana b
          , literalMana r
          , literalMana g
          , literalMana c
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

showNatList :: forall u n x. IsUser u => (x -> EnvM ParenItems) -> NatList u n x -> EnvM ParenItems
showNatList showX = \case
  LZ x -> yesParens do
    sType <-
      parens <$> do
        let u = showUserType @u
            grouping = case words u of
              [_] -> noParens
              _ -> yesParens
        grouping $ pure $ pure (fromString u)
    sX <- dollar <$> showX x
    pure $ pure "LZ @" <> sType <> sX
  LS x xs -> yesParens do
    sX <- parens <$> showX x
    sXs <- dollar <$> showNatList showX xs
    pure $ pure "LS " <> sX <> sXs

-- showNonProxy :: NonProxy x -> EnvM ParenItems
-- showNonProxy = \case
--   NonProxyElectEffectOneShot -> noParens do
--     pure $ pure "NonProxyElectEffectOneShot"

showO1 ::
  forall zone a z.
  (IsZone zone, IsObjectType a) =>
  IsOT (OT1 a) =>
  Plurality ->
  (z -> EnvM ParenItems) ->
  String ->
  (ON1 a -> z) ->
  EnvM ParenItems
showO1 = showONImpl @zone O1

showO2 ::
  forall zone a b z.
  (IsZone zone, Inst2 IsObjectType a b) =>
  Plurality ->
  (z -> EnvM ParenItems) ->
  String ->
  (ON2 a b -> z) ->
  EnvM ParenItems
showO2 = showONImpl @zone O2a

showO3 ::
  forall zone a b c z.
  (IsZone zone, Inst3 IsObjectType a b c) =>
  Plurality ->
  (z -> EnvM ParenItems) ->
  String ->
  (ON3 a b c -> z) ->
  EnvM ParenItems
showO3 = showONImpl @zone O3a

showO4 ::
  forall zone a b c d z.
  (IsZone zone, Inst4 IsObjectType a b c d) =>
  Plurality ->
  (z -> EnvM ParenItems) ->
  String ->
  (ON4 a b c d -> z) ->
  EnvM ParenItems
showO4 = showONImpl @zone O4a

showO5 ::
  forall zone a b c d e z.
  (IsZone zone, Inst5 IsObjectType a b c d e) =>
  Plurality ->
  (z -> EnvM ParenItems) ->
  String ->
  (ON5 a b c d e -> z) ->
  EnvM ParenItems
showO5 = showONImpl @zone O5a

showO6 ::
  forall zone a b c d e f z.
  (IsZone zone, Inst6 IsObjectType a b c d e f) =>
  Plurality ->
  (z -> EnvM ParenItems) ->
  String ->
  (ON6 a b c d e f -> z) ->
  EnvM ParenItems
showO6 = showONImpl @zone O6a

showONImpl ::
  forall zone z a ot.
  (IsZone zone, IsOT ot, IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  Plurality ->
  (z -> EnvM ParenItems) ->
  String ->
  (ObjectN ot -> z) ->
  EnvM ParenItems
showONImpl fromObject plurality showM memo cont = yesParens do
  (objN, snap) <- newObjectN @a fromObject memo
  objName <-
    parens <$> do
      let m = showObjectN @zone objN
      case plurality of
        Singular -> m
        Plural -> pluralize m
  let elect = cont objN
  sElect <- dropParens <$> showM elect
  restoreObject snap
  pure $ pure "\\" <> objName <> pure " -> " <> sElect

showObject :: Object a -> EnvM Items
showObject = fmap pure . getObjectName

showObjectNImpl ::
  IsObjectType a => TypeRep -> Item -> Object a -> EnvM ParenItems
showObjectNImpl objNRef prefix obj = do
  let i = objectToId obj
  sObj <- showObject obj
  State.gets (Map.lookup i . originalObjectRep) >>= \case
    Nothing -> error "impossible"
    Just originalRep -> case originalRep == objNRef of
      False -> yesParens $ pure $ pure prefix <> pure " " <> sObj
      True -> noParens $ pure sObj

showObject0 ::
  forall zone.
  (IsZone zone) =>
  ON0 ->
  EnvM ParenItems
showObject0 objN = yesParens do
  pure $ pure $ fromString $ "toZO0 " ++ show i
 where
  i = getObjectId objN

showObject1 ::
  forall zone a.
  (IsZone zone, IsObjectType a) =>
  ON1 a ->
  EnvM ParenItems
showObject1 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | otherwise -> "toZO1"

showObject2 ::
  forall zone a b.
  (IsZone zone, Inst2 IsObjectType a b) =>
  ON2 a b ->
  EnvM ParenItems
showObject2 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | rep == typeRep (Proxy @(ZO zone OTCreaturePlaneswalker)) ->
            "asCreaturePlaneswalker"
          | rep == typeRep (Proxy @(ZO zone OTCreaturePlayer)) ->
            "asCreaturePlayer"
          | rep == typeRep (Proxy @(ZO zone OTPlayerPlaneswalker)) ->
            "asPlayerPlaneswalker"
          | otherwise ->
            "toZO2"

showObject3 ::
  forall zone a b c.
  (IsZone zone, Inst3 IsObjectType a b c) =>
  ON3 a b c ->
  EnvM ParenItems
showObject3 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | rep == typeRep (Proxy @(ZO zone OTCreaturePlayerPlaneswalker)) ->
            "asCreaturePlayerPlaneswalker"
          | otherwise ->
            "toZO3"

showObject4 ::
  forall zone a b c d.
  (IsZone zone, Inst4 IsObjectType a b c d) =>
  ON4 a b c d ->
  EnvM ParenItems
showObject4 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | otherwise -> "toZO4"

showObject5 ::
  forall zone a b c d e.
  (IsZone zone, Inst5 IsObjectType a b c d e) =>
  ON5 a b c d e ->
  EnvM ParenItems
showObject5 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | rep == typeRep (Proxy @(ZO zone OTPermanent)) -> "asPermanent"
          | otherwise -> "toZO5"

showObject6 ::
  forall zone a b c d e f.
  (IsZone zone, Inst6 IsObjectType a b c d e f) =>
  ON6 a b c d e f ->
  EnvM ParenItems
showObject6 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | rep == typeRep (Proxy @(ZO zone OTSpell)) -> "asSpell"
          | otherwise -> "toZO6"

showObject7 ::
  forall zone a b c d e f g.
  (IsZone zone, Inst7 IsObjectType a b c d e f g) =>
  ON7 a b c d e f g ->
  EnvM ParenItems
showObject7 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | otherwise -> "toZO7"

showObject8 ::
  forall zone a b c d e f g h.
  (IsZone zone, Inst8 IsObjectType a b c d e f g h) =>
  ON8 a b c d e f g h ->
  EnvM ParenItems
showObject8 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | rep == typeRep (Proxy @(ZO zone OTDamageSource)) -> "asDamageSource"
          | otherwise -> "toZO8"

showObject9 ::
  forall zone a b c d e f g h i.
  (IsZone zone, Inst9 IsObjectType a b c d e f g h i) =>
  ON9 a b c d e f g h i ->
  EnvM ParenItems
showObject9 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | otherwise -> "toZO9"

showObject10 ::
  forall zone a b c d e f g h i j.
  (IsZone zone, Inst10 IsObjectType a b c d e f g h i j) =>
  ON10 a b c d e f g h i j ->
  EnvM ParenItems
showObject10 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | otherwise -> "toZO10"

showObject11 ::
  forall zone a b c d e f g h i j k.
  (IsZone zone, Inst11 IsObjectType a b c d e f g h i j k) =>
  ON11 a b c d e f g h i j k ->
  EnvM ParenItems
showObject11 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | otherwise -> "toZO11"

showObject12 ::
  forall zone a b c d e f g h i j k l.
  (IsZone zone, Inst12 IsObjectType a b c d e f g h i j k l) =>
  ON12 a b c d e f g h i j k l ->
  EnvM ParenItems
showObject12 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: IsObjectType x => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
          | rep == typeRep (Proxy @(ZO zone OTAny)) -> "asAny"
          | otherwise -> "toZO12"

showObjectN :: forall zone ot. (IsZone zone, VisitObjectN ot) => ObjectN ot -> EnvM ParenItems
showObjectN objN = case knownObjectN objN of
  KO0 obj0 -> showObject0 @zone obj0
  KO1 obj1 -> showObject1 @zone obj1
  KO2 obj2 -> showObject2 @zone obj2
  KO3 obj3 -> showObject3 @zone obj3
  KO4 obj4 -> showObject4 @zone obj4
  KO5 obj5 -> showObject5 @zone obj5
  KO6 obj6 -> showObject6 @zone obj6
  KO7 obj7 -> showObject7 @zone obj7
  KO8 obj8 -> showObject8 @zone obj8
  KO9 obj9 -> showObject9 @zone obj9
  KO10 obj10 -> showObject10 @zone obj10
  KO11 obj11 -> showObject11 @zone obj11
  KO12 obj12 -> showObject12 @zone obj12

showPower :: Power -> EnvM ParenItems
showPower = yesParens . pure . pure . fromString . show

showRequirement :: Requirement zone ot -> EnvM ParenItems
showRequirement = \case
  ControlledBy obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "ControlledBy" <> sObj
  ControlsA req -> yesParens do
    sObj <- dollar <$> showRequirement req
    pure $ pure "ControlsA" <> sObj
  HasAbility ability -> yesParens do
    sAbility <- dollar <$> showWithThis showAbility "this" ability
    pure $ pure "HasAbility" <> sAbility
  HasLandType landType -> yesParens do
    sLandType <- dollar <$> showLandType landType
    pure $ pure "HasLandType" <> sLandType
  Is wAny objN -> yesParens do
    sWAny <- parens <$> showWAny wAny
    sObjN <- dollar <$> showZoneObject objN
    pure $ pure "Is " <> sWAny <> sObjN
  IsTapped perm -> yesParens do
    pure $ pure $ fromString $ "IsTapped " ++ show perm
  Not req -> yesParens do
    sReq <- dollar <$> showRequirement req
    pure $ pure "Not" <> sReq
  OfColors colors -> yesParens do
    pure $ pure $ fromString $ "OfColors $ " ++ show colors
  OwnedBy obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "OwnedBy" <> sObj
  PlayerPays cost -> yesParens do
    sCost <- dollar <$> showCost cost
    pure $ pure "PlayerPays" <> sCost
  RAnd reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "RAnd" <> sReqs
  ROr reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "ROr" <> sReqs
  R2 reqsA reqsB -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- dollar <$> showRequirements reqsB
    pure $ pure "R2 " <> sReqsA <> sReqsB
  R3 reqsA reqsB reqsC -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- dollar <$> showRequirements reqsC
    pure $ pure "R3 " <> sReqsA <> pure " " <> sReqsB <> sReqsC
  R4 reqsA reqsB reqsC reqsD -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- parens <$> showRequirements reqsC
    sReqsD <- dollar <$> showRequirements reqsD
    pure $
      pure "R4 "
        <> sReqsA
        <> pure " "
        <> sReqsB
        <> pure " "
        <> sReqsC
        <> sReqsD
  R5 reqsA reqsB reqsC reqsD reqsE -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- parens <$> showRequirements reqsC
    sReqsD <- parens <$> showRequirements reqsD
    sReqsE <- dollar <$> showRequirements reqsE
    pure $
      pure "R4 "
        <> sReqsA
        <> pure " "
        <> sReqsB
        <> pure " "
        <> sReqsC
        <> pure " "
        <> sReqsD
        <> sReqsE

showRequirements :: [Requirement zone ot] -> EnvM ParenItems
showRequirements = showListM showRequirement

showSetCard :: SetCard ot -> EnvM ParenItems
showSetCard (SetCard set rarity card) = yesParens do
  sCard <- dollar <$> showCard card
  pure $
    pure (fromString $ "SetCard " ++ show set ++ " " ++ show rarity)
      <> sCard

showSetToken :: SetToken ot -> EnvM ParenItems
showSetToken (SetToken set rarity token) = yesParens do
  sToken <- dollar <$> showToken token
  pure $
    pure (fromString $ "SetToken " ++ show set ++ " " ++ show rarity)
      <> sToken

showStaticAbility :: StaticAbility zone ot -> EnvM ParenItems
showStaticAbility = \case
  As electListener -> yesParens do
    sWithObject <- dollar <$> showElect electListener
    pure $ pure "As" <> sWithObject
  Bestow cost enchant -> yesParens do
    sCost <- parens <$> showElect cost
    sEnchant <- dollar <$> showEnchant enchant
    pure $ pure "Bestow " <> sCost <> sEnchant
  FirstStrike -> noParens do
    pure $ pure "FirstStrike"
  Flying -> noParens do
    pure $ pure "Flying"
  Haste -> noParens do
    pure $ pure "Haste"
  StaticContinuous continuous -> yesParens do
    sContinuous <- dollar <$> showElect continuous
    pure $ pure "StaticContinuous" <> sContinuous
  Suspend time cost -> yesParens do
    let sTime = pure $ fromString $ show time
    sCost <- dollar <$> showElect cost
    pure $ pure "Suspend " <> sTime <> sCost

showTimePoint :: TimePoint p -> EnvM ParenItems
showTimePoint = yesParens . pure . pure . fromString . show

showToken :: Token ot -> EnvM ParenItems
showToken = \case
  Token wPerm card -> yesParens do
    sWPerm <- parens <$> showWPermanent wPerm
    sCard <- dollar <$> showCard card
    pure $ pure "Token " <> sWPerm <> sCard

showToughness :: Toughness -> EnvM ParenItems
showToughness = yesParens . pure . pure . fromString . show

showTriggeredAbility :: TriggeredAbility zone ot -> EnvM ParenItems
showTriggeredAbility = \case
  When listener -> go "When" listener
 where
  go consName eventListener = yesParens do
    sEventListener <- dollar <$> showElect eventListener
    pure $ pure (fromString consName) <> sEventListener

showTypeOf :: forall a. PrettyType a => Proxy a -> EnvM ParenItems
showTypeOf _ = conditionalParens do
  pure $ pure $ fromString name
 where
  name = prettyType @a
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
  LProxy reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "LProxy" <> sReqs
  L1 nonProxy reqs cont ->
    let ty = getType reqs
     in go ty nonProxy reqs $ showO1 @zone p showM memo (cont . toZone)
  L2 nonProxy reqs cont ->
    let ty = getType reqs
     in go ty nonProxy reqs $ showO2 @zone p showM memo (cont . toZone)
  L3 nonProxy reqs cont ->
    let ty = getType reqs
     in go ty nonProxy reqs $ showO3 @zone p showM memo (cont . toZone)
  L4 nonProxy reqs cont ->
    let ty = getType reqs
     in go ty nonProxy reqs $ showO4 @zone p showM memo (cont . toZone)
  L5 nonProxy reqs cont ->
    let ty = getType reqs
     in go ty nonProxy reqs $ showO5 @zone p showM memo (cont . toZone)
 where
  p = Singular

  getType :: [Requirement zone ot] -> Proxy ot
  getType _ = Proxy

  go ty _nonProxy reqs sCont = yesParens do
    sTy <- parens <$> showTypeOf ty
    sReqs <- parens <$> showRequirements reqs
    sCont' <- dollar <$> sCont
    pure $ pure "linked @" <> sTy <> pure " " <> sReqs <> sCont'

showWithList :: (ret -> EnvM ParenItems) -> WithList ret zone ot -> EnvM ParenItems
showWithList showRet = \case
  CountOf zos cont -> yesParens do
    sZos <- parens <$> showZoneObjects zos
    discr <- State.gets nextVariableId
    State.modify' $ \st -> st{nextVariableId = (1 +) <$> discr}
    let var = ReifiedVariable discr 0
        varName = getVarName var
        ret = cont var
    sRet <- dropParens <$> showRet ret
    pure $ pure "CountOf " <> sZos <> pure " $ \\" <> pure varName <> pure " -> " <> sRet
  Each zos cont -> yesParens do
    sZos <- parens <$> showZoneObjects zos
    let zo = lenseList zos
        ret = cont zo
    sZo <- parens <$> showZoneObject zo
    sRet <- dropParens <$> showRet ret
    pure $ pure "Each " <> sZos <> pure " $ \\" <> sZo <> pure " -> " <> sRet
  SuchThat reqs withList -> yesParens do
    sReqs <- parens <$> showRequirements reqs
    sWithList <- dollar <$> showWithList showRet withList
    pure $ pure "SuchThat " <> sReqs <> sWithList

showWithMaskedObject ::
  forall zone z.
  IsZone zone =>
  (z -> EnvM ParenItems) ->
  String ->
  WithMaskedObject zone z ->
  EnvM ParenItems
showWithMaskedObject showM memo = \case
  M1 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO1 @zone p showM memo (cont . toZone)
  M2 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO2 @zone p showM memo (cont . toZone)
  M3 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO3 @zone p showM memo (cont . toZone)
  M4 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO4 @zone p showM memo (cont . toZone)
  M5 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO5 @zone p showM memo (cont . toZone)
  M6 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO6 @zone p showM memo (cont . toZone)
 where
  p = Singular

  getType :: [Requirement zone ot] -> Proxy ot
  getType _ = Proxy

  go ty reqs sCont = yesParens do
    sTy <- parens <$> showTypeOf ty
    sReqs <- parens <$> showRequirements reqs
    sCont' <- dollar <$> sCont
    pure $ pure "masked @" <> sTy <> pure " " <> sReqs <> sCont'

showWithMaskedObjects ::
  forall zone z.
  IsZone zone =>
  (z -> EnvM ParenItems) ->
  String ->
  WithMaskedObjects zone z ->
  EnvM ParenItems
showWithMaskedObjects showM memo = \case
  M1s reqs cont ->
    let ty = getType reqs in go ty reqs $ showO1 @zone p showM memo (cont . pure . toZone)
  M2s reqs cont ->
    let ty = getType reqs in go ty reqs $ showO2 @zone p showM memo (cont . pure . toZone)
  M3s reqs cont ->
    let ty = getType reqs in go ty reqs $ showO3 @zone p showM memo (cont . pure . toZone)
  M4s reqs cont ->
    let ty = getType reqs in go ty reqs $ showO4 @zone p showM memo (cont . pure . toZone)
  M5s reqs cont ->
    let ty = getType reqs in go ty reqs $ showO5 @zone p showM memo (cont . pure . toZone)
  M6s reqs cont ->
    let ty = getType reqs in go ty reqs $ showO6 @zone p showM memo (cont . pure . toZone)
 where
  p = Plural

  getType :: [Requirement zone ot] -> Proxy ot
  getType _ = Proxy

  go ty reqs sCont = yesParens do
    sTy <- parens <$> showTypeOf ty
    sReqs <- parens <$> showRequirements reqs
    sCont' <- dollar <$> sCont
    pure $ pure "maskeds @" <> sTy <> pure " " <> sReqs <> sCont'

showWithThis ::
  forall zone liftOT ot.
  IsZO zone ot =>
  PrettyType (ZO zone ot) =>
  (forall ot'. liftOT ot' -> EnvM ParenItems) ->
  String ->
  WithThis zone liftOT ot ->
  EnvM ParenItems
showWithThis showM memo = \case
  T1 cont ->
    let go = yesParens do
          sTy <- parens <$> showTypeOf (Proxy @ot)
          sCont <- dollar <$> showO1 @zone Singular showM memo (cont . toZone)
          pure $ pure "thisObject @" <> sTy <> sCont
     in go
  T2 cont ->
    let go ::
          forall a b.
          (IsOT (OT2 a b), Inst2 IsObjectType a b) =>
          ((ZO zone (OT1 a), ZO zone (OT1 b)) -> liftOT (OT2 a b)) ->
          EnvM ParenItems
        go cont' = yesParens do
          sTy <- parens <$> showTypeOf (Proxy @ot)
          (objNa, snap) <- newObjectN @a O1 memo
          (objNb, _) <- newObjectN @b O1 memo
          sObjNa <- parens <$> showObjectN @zone objNa
          sObjNb <- parens <$> showObjectN @zone objNb
          let elect = cont' (toZone objNa, toZone objNb)
          sElect <- dropParens <$> showM elect
          restoreObject snap
          pure $
            pure "thisObject @"
              <> sTy
              <> pure " $ \\("
              <> sObjNa
              <> pure ", "
              <> sObjNb
              <> pure ") -> "
              <> sElect
     in go cont

showW2 :: forall wit a b. Inst2 IsObjectType a b => Item -> wit (OT2 a b) -> EnvM ParenItems
showW2 tyName _ = yesParens do
  sTy <- parens <$> showTypeOf (Proxy @(OT2 a b))
  pure $ pure "tyAp @" <> sTy <> pure " " <> pure tyName <> pure "2"

showW3 :: forall wit a b c. Inst3 IsObjectType a b c => Item -> wit (OT3 a b c) -> EnvM ParenItems
showW3 tyName _ = yesParens do
  sTy <- parens <$> showTypeOf (Proxy @(OT3 a b c))
  pure $ pure "tyAp @" <> sTy <> pure " " <> pure tyName <> pure "3"

showW4 :: forall wit a b c d. Inst4 IsObjectType a b c d => Item -> wit (OT4 a b c d) -> EnvM ParenItems
showW4 tyName _ = yesParens do
  sTy <- parens <$> showTypeOf (Proxy @(OT4 a b c d))
  pure $ pure "tyAp @" <> sTy <> pure " " <> pure tyName <> pure "4"

showW5 :: forall wit a b c d e. Inst5 IsObjectType a b c d e => Item -> wit (OT5 a b c d e) -> EnvM ParenItems
showW5 tyName _ = yesParens do
  sTy <- parens <$> showTypeOf (Proxy @(OT5 a b c d e))
  pure $ pure "tyAp @" <> sTy <> pure " " <> pure tyName <> pure "5"

showW6 :: forall wit a b c d e f. Inst6 IsObjectType a b c d e f => Item -> wit (OT6 a b c d e f) -> EnvM ParenItems
showW6 tyName _ = yesParens do
  sTy <- parens <$> showTypeOf (Proxy @(OT6 a b c d e f))
  pure $ pure "tyAp @" <> sTy <> pure " " <> pure tyName <> pure "6"

showWAny :: WAny ot -> EnvM ParenItems
showWAny wit = case wit of
  WAnyArtifact -> noParens sWit
  WAnyCreature -> noParens sWit
  WAnyEnchantment -> noParens sWit
  WAnyInstant -> noParens sWit
  WAnyLand -> noParens sWit
  WAnyPlaneswalker -> noParens sWit
  WAnyPlayer -> noParens sWit
  WAnySorcery -> noParens sWit
  WAny -> noParens sWit
  WAny2 -> showW2 tyName wit
  WAny3 -> showW3 tyName wit
  WAny4 -> showW4 tyName wit
  WAny5 -> showW5 tyName wit
  WAny6 -> showW6 tyName wit
 where
  tyName :: Item
  tyName = "WAny"
  sWit :: EnvM Items
  sWit = pure $ pure $ fromString $ show wit

showWCard :: WCard ot -> EnvM ParenItems
showWCard wit = case wit of
  WCardArtifact -> noParens sWit
  WCardCreature -> noParens sWit
  WCardEnchantment -> noParens sWit
  WCardInstant -> noParens sWit
  WCardLand -> noParens sWit
  WCardPlaneswalker -> noParens sWit
  WCardSorcery -> noParens sWit
  WCard -> noParens sWit
  WCard2 -> showW2 tyName wit
  WCard3 -> showW3 tyName wit
 where
  tyName :: Item
  tyName = "WCard"
  sWit :: EnvM Items
  sWit = pure $ pure $ fromString $ show wit

-- showWNonCreatureCard :: WNonCreatureCard ot -> EnvM ParenItems
-- showWNonCreatureCard wit = case wit of
--   WNonCreatureArtifact -> noParens sWit
--   WNonCreatureEnchantment -> noParens sWit
--   WNonCreatureInstant -> noParens sWit
--   WNonCreatureLand -> noParens sWit
--   WNonCreaturePlaneswalker -> noParens sWit
--   WNonCreatureSorcery -> noParens sWit
--   WNonCreatureCard -> noParens sWit
--   WNonCreatureCard2 -> showW2 tyName wit
--   WNonCreatureCard3 -> showW3 tyName wit
--  where
--   tyName :: Item
--   tyName = "WNonCreatureCard"
--   sWit :: EnvM Items
--   sWit = pure $ pure $ fromString $ show wit

showWPermanent :: WPermanent ot -> EnvM ParenItems
showWPermanent wit = case wit of
  WPermanentArtifact -> noParens sWit
  WPermanentCreature -> noParens sWit
  WPermanentEnchantment -> noParens sWit
  WPermanentLand -> noParens sWit
  WPermanentPlaneswalker -> noParens sWit
  WPermanent -> noParens sWit
  WPermanent2 -> showW2 tyName wit
  WPermanent3 -> showW3 tyName wit
  WPermanent4 -> showW4 tyName wit
 where
  tyName :: Item
  tyName = "WPermanent"
  sWit :: EnvM Items
  sWit = pure $ pure $ fromString $ show wit

showWSpell :: WSpell ot -> EnvM ParenItems
showWSpell wit = case wit of
  WSpellArtifact -> noParens sWit
  WSpellCreature -> noParens sWit
  WSpellEnchantment -> noParens sWit
  WSpellInstant -> noParens sWit
  WSpellPlaneswalker -> noParens sWit
  WSpellSorcery -> noParens sWit
  WSpell -> noParens sWit
  WSpell2 -> showW2 tyName wit
  WSpell3 -> showW3 tyName wit
  WSpell4 -> showW4 tyName wit
 where
  tyName :: Item
  tyName = "WSpell"
  sWit :: EnvM Items
  sWit = pure $ pure $ fromString $ show wit

showYourCard :: YourCard ot -> EnvM ParenItems
showYourCard = \case
  YourArtifact cont -> goPerm cont "YourArtifact"
  YourArtifactCreature cont -> goPerm cont "YourArtifactCreature"
  YourArtifactLand cont -> goPerm cont "YourArtifactLand"
  YourCreature cont -> goPerm cont "YourCreature"
  YourEnchantment cont -> goPerm cont "YourEnchantment"
  YourEnchantmentCreature cont -> goPerm cont "YourEnchantmentCreature"
  YourLand cont -> goPerm cont "YourLand"
  YourPlaneswalker cont -> goPerm cont "YourPlaneswalker"
  --
  YourInstant cont -> goSpell cont "YourInstant"
  YourSorcery cont -> goSpell cont "YourSorcery"
 where
  withYou action = do
    (you', snap) <- newObjectN @ 'OTPlayer O1 "you"
    let you = toZone you'
    x <- action you
    restoreObject snap
    pure x
  goPerm cont consName = withYou $ \you -> yesParens do
    sYou <- parens <$> showZoneObject you
    let facet = cont you
    sFacet <- dollar <$> showCardFacet facet
    pure $ pure consName <> pure " $ \\" <> sYou <> pure " -> " <> sFacet
  goSpell cont consName = withYou $ \you -> yesParens do
    sYou <- parens <$> showZoneObject you
    let electFacet = cont you
    sFacet <- dollar <$> showElect electFacet
    pure $ pure consName <> pure " $ \\" <> sYou <> pure " -> " <> sFacet

showZoneObject :: forall zone ot. IsZO zone ot => ZO zone ot -> EnvM ParenItems
showZoneObject = \case
  ZO _ objN -> showObjectN @zone objN

-- showZoneObject0 :: forall zone. IsZone zone => ZO zone OT0 -> EnvM ParenItems
-- showZoneObject0 = \case
--   ZO _ objN -> showObject0 @zone objN

showZoneObjects :: forall zone ot. IsZO zone ot => List (ZO zone ot) -> EnvM ParenItems
showZoneObjects (lenseList -> zo) = pluralize $ showZoneObject zo

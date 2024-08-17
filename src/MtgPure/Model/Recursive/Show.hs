{-# LANGUAGE Safe #-}
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

import safe qualified Control.Monad as M
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
import safe MtgPure.Model.CardName (CardName (CardName), HasCardName (..))
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Colors (Colors (..))
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaCost (
  DynamicManaCost (..),
  HybridManaCost (..),
  ManaCost (..),
  PhyrexianManaCost (..),
  isOnlyGeneric,
 )
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (OT1, OT2, OT3, OT4, OT5, OTN (..))
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNCreaturePlaneswalker,
  OTNCreaturePlayer,
  OTNCreaturePlayerPlaneswalker,
  OTNDamageSource,
  OTNPermanent,
  OTNPlayerPlaneswalker,
  OTNSpell,
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
import safe MtgPure.Model.Object.ViewObjectN (viewOTN')
import safe MtgPure.Model.Object.VisitObjectN (visitObjectN')
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  CardCharacteristic (..),
  CardSpec (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectOT (unElectOT),
  Else (..),
  Enchant (..),
  EnchantmentType (..),
  EntersStatic (..),
  Event,
  EventListener,
  EventListener' (..),
  IsUser (..),
  List (..),
  Requirement (..),
  SetCard (SetCard),
  SetToken (SetToken),
  SomeZone (..),
  StaticAbility (..),
  Token (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisAbility (..),
  WithThisActivated,
  WithThisOneShot,
  WithThisStatic,
  WithThisTriggered,
  WithThisZ (..),
 )
import safe MtgPure.Model.Supertype (Supertype (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (
  Var (..),
  Variable (..),
  VariableId,
  VariableId' (..),
  getVariableId,
 )
import safe MtgPure.Model.Zone (IsZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  ZoneObject (..),
  toZone,
 )
import safe Prelude hiding (showList)

----------------------------------------

defaultDepthLimit :: Maybe Int
defaultDepthLimit = Nothing

instance Show (Ability zone ot) where
  show = runEnvM defaultDepthLimit . showAbility

instance Show (ActivatedAbility zone ot) where
  show = runEnvM defaultDepthLimit . showActivatedAbility

instance Show AnyCard where
  show = runEnvM defaultDepthLimit . showAnyCard

instance Show AnyToken where
  show = runEnvM defaultDepthLimit . showAnyToken

instance Show (Card ot) where
  show = runEnvM defaultDepthLimit . showCard

instance Show (CardCharacteristic ot) where
  show = runEnvM defaultDepthLimit . showCardCharacteristic

instance Show (CardSpec ot) where
  show = runEnvM defaultDepthLimit . showCardSpec

instance Show CompleteManaPool where
  show = runEnvM defaultDepthLimit . showCompleteManaPool

instance Show Condition where
  show = runEnvM defaultDepthLimit . showCondition

instance Show Cost where
  show = runEnvM defaultDepthLimit . showCost

instance Show (DynamicManaCost var) where
  show = runEnvM defaultDepthLimit . showDynamicManaCost

instance Show (Effect ef) where
  show = runEnvM defaultDepthLimit . showEffect

instance Show (Elect s el ot) where
  show = runEnvM defaultDepthLimit . showElect

instance Show (EnchantmentType ot) where
  show = runEnvM defaultDepthLimit . showEnchantmentType

instance Show EventListener where
  show = runEnvM defaultDepthLimit . showEventListener

instance Show (HybridManaCost var) where
  show = runEnvM defaultDepthLimit . showHybridManaCost

instance Show (ManaCost var) where
  show = runEnvM defaultDepthLimit . showManaCost

instance Show (ManaPool snow) where
  show = runEnvM defaultDepthLimit . showManaPool

instance Show (PhyrexianManaCost var) where
  show = runEnvM defaultDepthLimit . showPhyrexianManaCost

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

instance (IsZO zone ot) => Show (WithMaskedObject (Elect s e) zone ot) where
  show = runEnvM defaultDepthLimit . showWithMaskedObject showElect "obj"

instance (IsOTN ot) => Show (SomeZone WithThisAbility ot) where
  show = runEnvM defaultDepthLimit . showSomeZone (showWithThisAbility "this")

instance (IsOTN ot) => Show (SomeZone (WithThisZ ActivatedAbility) ot) where
  show = runEnvM defaultDepthLimit . showSomeZone (showWithThisZ showActivatedAbility "this")

instance (IsOTN ot) => Show (SomeZone (WithThisZ StaticAbility) ot) where
  show = runEnvM defaultDepthLimit . showSomeZone (showWithThisZ showStaticAbility "this")

instance (IsOTN ot) => Show (SomeZone (WithThisZ TriggeredAbility) ot) where
  show = runEnvM defaultDepthLimit . showSomeZone (showWithThisZ showTriggeredAbility "this")

instance (IsZO zone ot) => Show (WithThis (Ability zone) zone ot) where
  show = runEnvM defaultDepthLimit . showWithThis showAbility "this"

instance (IsZO zone ot) => Show (WithThisAbility zone ot) where
  show = runEnvM defaultDepthLimit . showWithThisAbility "this"

instance (IsZO zone ot) => Show (WithThisActivated zone ot) where
  show = runEnvM defaultDepthLimit . showWithThis (showElect . unElectOT) "this"

instance (IsZO 'ZStack ot) => Show (WithThisOneShot ot) where
  show = runEnvM defaultDepthLimit . showWithThis showElect "this"

instance (IsZO zone ot) => Show (WithThisStatic zone ot) where
  show = runEnvM defaultDepthLimit . showWithThis showStaticAbility "this"

instance (IsZO zone ot) => Show (WithThisTriggered zone ot) where
  show = runEnvM defaultDepthLimit . showWithThis showTriggeredAbility "this"

----------------------------------------

tryLitMana :: Mana var snow mt -> Maybe Int
tryLitMana = \case
  Mana x -> Just x
  VariableMana{} -> Nothing
  SumMana{} -> Nothing

litMana :: Mana 'NoVar snow mt -> Int
litMana = \case
  Mana x -> x

----------------------------------------

data Item :: Type where
  StringItem :: String -> Item
  ObjectItem :: ObjectId -> Generation -> Item
  VariableItem :: VariableId -> Item
  deriving (Show)

instance IsString Item where
  fromString = StringItem

type Items = DList.DList Item

data Paren = NeedsParen | DoesNotNeedParam

type ParenItems = (Paren, Items)

parens :: ParenItems -> Items
parens (p, s) = case p of
  NeedsParen -> pure "(" <> s <> pure ")"
  DoesNotNeedParam -> s

dollar :: ParenItems -> Items
dollar (p, s) = case p of
  NeedsParen -> pure " $ " <> s
  DoesNotNeedParam -> pure " " <> s

dropParens :: ParenItems -> Items
dropParens = snd

noParens :: EnvM Items -> EnvM ParenItems
noParens = fmap $ (,) DoesNotNeedParam

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

newtype EnvM a = EnvM {unEnvM :: State.State Env a}
  deriving (Functor)

instance Applicative EnvM where
  pure = EnvM . pure
  EnvM f <*> EnvM a = EnvM $ f <*> a

instance Monad EnvM where
  EnvM a >>= f = EnvM $ a >>= unEnvM . f

runEnvM :: CardDepth -> EnvM ParenItems -> String
runEnvM depth m = concat $ State.evalState strsM $ mkEnv depth
 where
  itemsM = DList.toList . dropParens <$> m
  EnvM strsM =
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
getObjectName (Object _ (UntypedObject _ i)) = EnvM do
  gens <- State.gets objectGenerations
  case Map.lookup i gens of
    Nothing -> pure $ ObjectItem i (-1) -- Object is an unbound variable. Can happen when walking past a variable binding before showing the rest of the tree.
    Just g -> pure $ ObjectItem i g

newtype ObjectIdState = ObjectIdState ObjectId

newObject ::
  forall a. (IsObjectType a) => String -> EnvM (Object a, ObjectIdState)
newObject name = EnvM do
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
  EnvM $ State.modify' \st ->
    st
      { originalObjectRep = Map.insert i (typeOf objN) $ originalObjectRep st
      }
  pure (objN, snap)

restoreObject :: ObjectIdState -> EnvM ()
-- restoreObject (ObjectIdState i) = State.modify' \st -> st {nextObjectId = i}
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
getObjectNamePrefix i = EnvM do
  State.gets (Map.findWithDefault "unboundVariable" i . objectNames)

showListM :: (a -> EnvM ParenItems) -> [a] -> EnvM ParenItems
showListM f xs = noParens do
  ss <- mapM (fmap dropParens . f) xs
  pure $ pure "[" <> DList.intercalate (pure ", ") ss <> pure "]"

----------------------------------------

showAbility :: Ability zone ot -> EnvM ParenItems
showAbility = \case
  Activated ability -> yesParens do
    sAbility <- dollar <$> showElect ability
    pure $ pure "Activated" <> sAbility
  Static ability -> yesParens do
    sAbility <- dollar <$> showStaticAbility ability
    pure $ pure "Static" <> sAbility
  Triggered ability -> yesParens do
    sAbility <- dollar <$> showTriggeredAbility ability
    pure $ pure "Triggered" <> sAbility

showAnyCard :: AnyCard -> EnvM ParenItems
showAnyCard = \case
  AnyCard1 card -> yesParens do
    sCard <- dollar <$> showCard card
    pure $ pure "AnyCard1" <> sCard
  AnyCard2 card -> yesParens do
    sCard <- dollar <$> showCard card
    pure $ pure "AnyCard2" <> sCard

showAnyToken :: AnyToken -> EnvM ParenItems
showAnyToken = \case
  AnyToken token -> yesParens do
    sToken <- dollar <$> showToken token
    pure $ pure "AnyToken" <> sToken

showActivatedAbility :: ActivatedAbility zone ot -> EnvM ParenItems
showActivatedAbility = \case
  Ability cost effect -> yesParens do
    sCost <- parens <$> showCost cost
    sEffect <- dollar <$> showElect effect
    pure $ pure "Ability " <> sCost <> sEffect
  Cycling cost -> yesParens do
    sCost <- parens <$> showCost cost
    pure $ pure "Cycling " <> sCost

showArtifactType :: ArtifactType -> EnvM ParenItems
showArtifactType = noParens . pure . pure . fromString . show

showArtifactTypes :: [ArtifactType] -> EnvM ParenItems
showArtifactTypes = showListM showArtifactType

showBasicLandType :: BasicLandType -> EnvM ParenItems
showBasicLandType = noParens . pure . pure . fromString . show

showCard :: Card ot -> EnvM ParenItems
showCard card = case card of
  Card name yourCard -> showCardImpl "Card" card do
    let sName = pure $ fromString $ show name
    sYourCard <- dollar <$> showElect yourCard
    pure $
      pure "Card "
        <> sName
        <> sYourCard
  DoubleSidedCard card1 card2 -> showCardImpl "DoubleSidedCard" card do
    sCard1 <- parens <$> showCard card1
    sCard2 <- dollar <$> showCard card2
    pure $
      pure "DoubleSidedCard "
        <> sCard1
        <> sCard2
  SplitCard card1 card2 splitAbilities -> showCardImpl "SplitCard" card do
    sCard1 <- parens <$> showCard card1
    sCard2 <- parens <$> showCard card2
    sSplitAbilities <- dollar <$> showListM (showSomeZone showAbility) splitAbilities
    pure $
      pure "SplitCard "
        <> sCard1
        <> pure " "
        <> sCard2
        <> sSplitAbilities

showCardImpl :: (HasCardName name) => Item -> name -> EnvM Items -> EnvM ParenItems
showCardImpl consName (getCardName -> CardName name) cont = yesParens do
  depth <- EnvM $ State.gets cardDepth
  EnvM $ State.modify' \st -> st{cardDepth = subtract 1 <$> depth}
  let sName = pure $ fromString $ show name
  case depth of
    Just 0 -> pure $ pure consName <> pure " " <> sName <> pure " ..."
    _ -> cont

showCardCharacteristic :: CardCharacteristic ot -> EnvM ParenItems
showCardCharacteristic = \case
  ArtifactCharacteristic colors sups artTypes spec -> yesParens do
    sColors <- parens <$> showColors colors
    sSups <- parens <$> showSupertypes sups
    sArtTypes <- parens <$> showArtifactTypes artTypes
    sSpec <- dollar <$> showCardSpec spec
    pure $
      pure "ArtifactCharacteristic "
        <> sColors
        <> pure " "
        <> sSups
        <> pure " "
        <> sArtTypes
        <> sSpec
  ArtifactCreatureCharacteristic colors sups artTypes creatTypes power toughness spec ->
    yesParens do
      sColors <- parens <$> showColors colors
      sSups <- parens <$> showSupertypes sups
      sArtTypes <- parens <$> showArtifactTypes artTypes
      sCreatTypes <- parens <$> showCreatureTypes creatTypes
      sPower <- parens <$> showPower power
      sToughness <- parens <$> showToughness toughness
      sSpec <- dollar <$> showCardSpec spec
      pure $
        pure "ArtifactCreatureCharacteristic "
          <> sColors
          <> pure " "
          <> sSups
          <> pure " "
          <> sArtTypes
          <> pure " "
          <> sCreatTypes
          <> pure " "
          <> sPower
          <> pure " "
          <> sToughness
          <> sSpec
  ArtifactLandCharacteristic sups artTypes landTypes spec ->
    yesParens do
      sSups <- parens <$> showSupertypes sups
      sArtTypes <- parens <$> showArtifactTypes artTypes
      sLandTypes <- parens <$> showLandTypes landTypes
      sSpec <- dollar <$> showCardSpec spec
      pure $
        pure "ArtifactLandCharacteristic "
          <> sSups
          <> pure " "
          <> sArtTypes
          <> pure " "
          <> sLandTypes
          <> sSpec
  CreatureCharacteristic colors sups creatureTypes power toughness spec ->
    yesParens do
      sColors <- parens <$> showColors colors
      sSups <- parens <$> showSupertypes sups
      sCreatureTypes <- parens <$> showCreatureTypes creatureTypes
      sPower <- parens <$> showPower power
      sToughness <- parens <$> showToughness toughness
      sSpec <- dollar <$> showCardSpec spec
      pure $
        pure "CreatureCharacteristic "
          <> sColors
          <> pure " "
          <> sSups
          <> pure " "
          <> sCreatureTypes
          <> pure " "
          <> sPower
          <> pure " "
          <> sToughness
          <> sSpec
  EnchantmentCharacteristic colors sups enchantTypes spec -> yesParens do
    sColors <- parens <$> showColors colors
    sSups <- parens <$> showSupertypes sups
    sEnchantTypes <- parens <$> showEnchantmentTypes enchantTypes
    sSpec <- dollar <$> showCardSpec spec
    pure $
      pure "EnchantmentCharacteristic "
        <> sColors
        <> pure " "
        <> sSups
        <> pure " "
        <> sEnchantTypes
        <> sSpec
  EnchantmentCreatureCharacteristic colors sups creatTypes enchantTypes power toughness spec ->
    yesParens do
      sColors <- parens <$> showColors colors
      sSups <- parens <$> showSupertypes sups
      sCreatTypes <- parens <$> showCreatureTypes creatTypes
      sEnchantTypes <- parens <$> showEnchantmentTypes enchantTypes
      sPower <- parens <$> showPower power
      sToughness <- parens <$> showToughness toughness
      sSpec <- dollar <$> showCardSpec spec
      pure $
        pure "EnchantmentCreatureCharacteristic "
          <> sColors
          <> pure " "
          <> sSups
          <> pure " "
          <> sCreatTypes
          <> pure " "
          <> sEnchantTypes
          <> pure " "
          <> sPower
          <> pure " "
          <> sToughness
          <> sSpec
  InstantCharacteristic colors sups spec -> yesParens do
    sColors <- parens <$> showColors colors
    sSups <- parens <$> showSupertypes sups
    sSpec <- dollar <$> showElect spec
    pure $
      pure "InstantCharacteristic "
        <> sColors
        <> pure " "
        <> sSups
        <> sSpec
  LandCharacteristic sups landTypes spec -> yesParens do
    sSups <- parens <$> showSupertypes sups
    sLandTypes <- parens <$> showLandTypes landTypes
    sSpec <- dollar <$> showCardSpec spec
    pure $
      pure "LandCharacteristic "
        <> sSups
        <> pure " "
        <> sLandTypes
        <> sSpec
  PlaneswalkerCharacteristic colors sups spec -> yesParens do
    sColors <- parens <$> showColors colors
    sSups <- parens <$> showSupertypes sups
    sSpec <- dollar <$> showCardSpec spec
    pure $
      pure "PlaneswalkerCharacteristic "
        <> sColors
        <> pure " "
        <> sSups
        <> sSpec
  SorceryCharacteristic colors sups spec -> yesParens do
    sColors <- parens <$> showColors colors
    sSups <- parens <$> showSupertypes sups
    sSpec <- dollar <$> showElect spec
    pure $
      pure "SorceryCharacteristic "
        <> sColors
        <> pure " "
        <> sSups
        <> sSpec

showCardSpec :: CardSpec ot -> EnvM ParenItems
showCardSpec = \case
  ArtifactSpec cost abilities -> yesParens do
    sCost <- parens <$> showCost cost
    sAbilities <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) abilities
    pure $
      pure "ArtifactSpec "
        <> sCost
        <> sAbilities
  ArtifactCreatureSpec cost artAbils creatAbils bothAbils ->
    yesParens do
      sCost <- parens <$> showCost cost
      sArtAbils <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) artAbils
      sCreatAbils <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) creatAbils
      sBothAbils <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) bothAbils
      pure $
        pure "ArtifactCreatureSpec "
          <> sCost
          <> pure " "
          <> sArtAbils
          <> pure " "
          <> sCreatAbils
          <> sBothAbils
  ArtifactLandSpec artAbils landAbils bothAbils ->
    yesParens do
      sArtAbils <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) artAbils
      sLandAbils <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) landAbils
      sBothAbils <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) bothAbils
      pure $
        pure "ArtifactLandSpec "
          <> sArtAbils
          <> pure " "
          <> sLandAbils
          <> sBothAbils
  CreatureSpec cost abilities ->
    yesParens do
      sCost <- parens <$> showCost cost
      sAbilities <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) abilities
      pure $
        pure "CreatureSpec "
          <> sCost
          <> sAbilities
  EnchantmentSpec cost abilities -> yesParens do
    sCost <- parens <$> showCost cost
    sAbilities <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) abilities
    pure $
      pure "EnchantmentSpec "
        <> sCost
        <> sAbilities
  EnchantmentCreatureSpec cost creatAbils enchAbils bothAbils ->
    yesParens do
      sCost <- parens <$> showCost cost
      sCreatAbils <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) creatAbils
      sEnchAbils <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) enchAbils
      sBothAbils <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) bothAbils
      pure $
        pure "EnchantmentCreatureSpec "
          <> sCost
          <> pure " "
          <> sCreatAbils
          <> pure " "
          <> sEnchAbils
          <> sBothAbils
  InstantSpec cost abilities oneShot -> yesParens do
    sCost <- parens <$> showCost cost
    sAbilities <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) abilities
    sOneShot <- dollar <$> showWithThis showElect "this" oneShot
    pure $
      pure "InstantSpec "
        <> sCost
        <> sAbilities
        <> sOneShot
  LandSpec abilities -> yesParens do
    sAbilities <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) abilities
    pure $
      pure "LandSpec"
        <> sAbilities
  PlaneswalkerSpec cost loyalty abilities -> yesParens do
    sCost <- parens <$> showCost cost
    sLoyalty <- parens <$> showLoyalty loyalty
    sAbilities <- dollar <$> showListM (showSomeZone (showWithThisAbility "this")) abilities
    pure $
      pure "PlaneswalkerSpec "
        <> sCost
        <> pure " "
        <> sLoyalty
        <> sAbilities
  SorcerySpec cost abilities oneShot -> yesParens do
    sCost <- parens <$> showCost cost
    sAbilities <- parens <$> showListM (showSomeZone (showWithThisAbility "this")) abilities
    sOneShot <- dollar <$> showWithThis showElect "this" oneShot
    pure $
      pure "SorcerySpec "
        <> sCost
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
  Satisfies objN reqs -> yesParens do
    sObjN <- parens <$> showZoneObject objN
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Satisfies " <> pure " " <> sObjN <> sReqs

showConditions :: [Condition] -> EnvM ParenItems
showConditions = showListM showCondition

showCost :: Cost -> EnvM ParenItems
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
  ExileCost reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "ExileCost" <> sReqs
  LoyaltyCost zoPlaneswalker loyalty -> yesParens do
    sPlaneswalker <- parens <$> showZoneObject zoPlaneswalker
    sLoyalty <- dollar <$> showLoyalty loyalty
    pure $ pure "LoyaltyCost " <> sPlaneswalker <> sLoyalty
  ManaCost cost -> yesParens do
    sCost <- dollar <$> showManaCost cost
    pure $ pure (fromString "ManaCost") <> sCost
  OrCosts costs -> yesParens do
    sCosts <- parens <$> showListM showCost costs
    pure $ pure "OrCosts " <> sCosts
  PayLife amount -> yesParens do
    let sAmount = pure $ fromString $ show amount
    pure $ pure "PayLife " <> sAmount
  SacrificeCost reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "SacrificeCost" <> sReqs
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
  AddToBattlefield player token -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sCard <- dollar <$> showToken token
    pure $ pure "AddToBattlefield " <> sPlayer <> sCard
  CantBeRegenerated creature -> yesParens do
    sCreature <- dollar <$> showZoneObject creature
    pure $ pure "CantBeRegenerated" <> sCreature
  ChangeTo before after -> yesParens do
    sBefore <- parens <$> showZoneObject before
    sAfter <- dollar <$> showCard after
    pure $ pure "ChangeTo " <> sBefore <> sAfter
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
  EndTheTurn -> yesParens do
    pure $ pure "EndTheTurn"
  Exile obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "Exile" <> sObj
  GainAbility obj ability -> yesParens do
    sObj <- parens <$> showZoneObject obj
    sAbility <- dollar <$> showWithThisAbility "this" ability
    pure $ pure "GainAbility " <> sObj <> sAbility
  GainControl player obj -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "GainControl " <> sPlayer <> sObj
  GainLife player n -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    let amount = fromString $ show n
    pure $ pure "GainLife " <> sPlayer <> pure " " <> pure amount
  LoseAbility obj ability -> yesParens do
    sObj <- parens <$> showZoneObject obj
    sAbility <- dollar <$> showWithThisAbility "this" ability
    pure $ pure "LoseAbility " <> sObj <> sAbility
  LoseLife player n -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    let amount = fromString $ show n
    pure $ pure "LoseLife " <> sPlayer <> pure " " <> pure amount
  PutOntoBattlefield player obj -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sCard <- dollar <$> showZoneObject obj
    pure $ pure "PutOntoBattlefield " <> sPlayer <> sCard
  Sacrifice player reqs -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Sacrifice " <> sPlayer <> sReqs
  SearchLibrary searcher searchee withCard -> yesParens do
    sSearcher <- parens <$> showZoneObject searcher
    sSearchee <- parens <$> showZoneObject searchee
    sWithCard <- dollar <$> showWithLinkedObject showElect "card" withCard
    pure $ pure "SearchLibrary " <> sSearcher <> pure " " <> sSearchee <> sWithCard
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
  WithList withList -> yesParens do
    sWithList <- dollar <$> showWithList showEffect withList
    pure $ pure "WithList" <> sWithList

showEffects :: [Effect e] -> EnvM ParenItems
showEffects = showListM showEffect

showElect :: Elect s e ot -> EnvM ParenItems
showElect = \case
  ActivePlayer contElect -> yesParens do
    (active', snap) <- newObjectN @'OTPlayer O1 "active"
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
    discr <- EnvM $ State.gets nextVariableId
    EnvM $ State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
    let var = ReifiedVariable discr FZ
        varName = getVarName var
        elect = varToElect var
    sElect <- dropParens <$> showElect elect
    pure $
      pure "ChooseOption "
        <> sPlayer
        <> pure " "
        <> sNatList
        <> pure " $ \\"
        <> pure varName
        <> pure " -> "
        <> sElect
  Condition cond -> yesParens do
    sCond <- dollar <$> showCondition cond
    pure $ pure "Condition" <> sCond
  ControllerOf zObj contElect -> do
    goPlayerOf1 "ControllerOf" "controller" zObj contElect
  Cost cost -> yesParens do
    sCost <- dollar <$> showCost cost
    pure $ pure "Cost" <> sCost
  Effect effect -> yesParens do
    sEffect <- dollar <$> showEffects effect
    pure $ pure "Effect" <> sEffect
  ElectActivated activated -> yesParens do
    sPost <- dollar <$> showActivatedAbility activated
    pure $ pure "ElectActivated" <> sPost
  ElectCardFacet post -> yesParens do
    sPost <- dollar <$> showCardCharacteristic post
    pure $ pure "ElectCardFacet" <> sPost
  ElectCardSpec post -> yesParens do
    sPost <- dollar <$> showCardSpec post
    pure $ pure "ElectCardSpec" <> sPost
  ElectCase case_ -> yesParens do
    sCase <- dollar <$> showCase showElect case_
    pure $ pure "ElectCase" <> sCase
  EndTargets elect -> yesParens do
    sElect <- dollar <$> showElect elect
    pure $ pure "EndTargets" <> sElect
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
  OwnerOf zObj contElect -> do
    goPlayerOf1 "OwnerOf" "owner" zObj contElect
  PlayerPays oPlayer cost contElect -> yesParens do
    discr <- EnvM $ State.gets nextVariableId
    EnvM $ State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
    let var = ReifiedVariable discr FZ
        varName = getVarName var
        elect = contElect var
    sPlayer <- parens <$> showZoneObject oPlayer
    sCost <- dollar <$> showCost cost
    sElect <- dropParens <$> showElect elect
    pure $
      pure "PlayerPays "
        <> sPlayer
        <> pure " "
        <> sCost
        <> pure " $ \\"
        <> pure varName
        <> pure " -> "
        <> sElect
  Random withObject -> yesParens do
    sWithObject <- dollar <$> showWithMaskedObject showElect "rand" withObject
    pure $ pure "Random" <> sWithObject
  Target player withObject -> yesParens do
    sPlayer <- parens <$> showZoneObject player
    sWithObject <- dollar <$> showWithMaskedObject showElect "target" withObject
    pure $ pure "Target " <> sPlayer <> sWithObject
  VariableFromPower creature varToElect -> yesParens do
    sCreature <- parens <$> showZoneObject creature
    discr <- EnvM $ State.gets nextVariableId
    EnvM $ State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
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
    discr <- EnvM $ State.gets nextVariableId
    EnvM $ State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
    let var = ReifiedVariable discr 0
        varName = getVarName var
        elect = contElect var
    sElect <- dropParens <$> showElect elect
    pure $ pure "VariableInt $ \\" <> pure varName <> pure " -> " <> sElect
  Your contElect -> do
    goPlayerOf0 "Your" "you" contElect
 where
  goPlayerOf0 consName varName contElect = yesParens do
    (player', snap) <- newObjectN @'OTPlayer O1 varName
    let player = toZone player'
    sPlayer <- parens <$> showZoneObject player
    let elect = contElect player
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $
      pure consName
        <> pure " $ \\"
        <> sPlayer
        <> pure " -> "
        <> sElect
  goPlayerOf1 consName varName zObj contElect = yesParens do
    objPrefix <-
      getObjectNamePrefix
        let objN :: ObjectN OTNAny
            objN = case zObj of
              ZO _ o -> o
         in visitObjectN' objectToId objN
    (player', snap) <-
      newObjectN @'OTPlayer O1 case objPrefix == "this" of
        True -> "you"
        False -> varName
    let player = toZone player'
    sPlayer <- parens <$> showZoneObject player
    sZObj <- parens <$> showZoneObject zObj
    let elect = contElect player
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $
      pure consName
        <> pure " "
        <> sZObj
        <> pure " $ \\"
        <> sPlayer
        <> pure " -> "
        <> sElect

showElse :: Else s e ot -> EnvM ParenItems
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

showEntersStatic :: EntersStatic zone ot -> EnvM ParenItems
showEntersStatic = \case
  EntersTapped -> noParens do
    pure $ pure "EntersTapped"

showEvent :: Event -> EnvM ParenItems
showEvent = showEventListener' \Proxy -> noParens $ pure $ pure "Proxy"

showEventListener :: EventListener -> EnvM ParenItems
showEventListener = showEventListener' showElect

showEventListener' ::
  (forall ot. x ot -> EnvM ParenItems) ->
  EventListener' x ->
  EnvM ParenItems
showEventListener' showX = \case
  BecomesTapped withObject -> yesParens do
    sWithObject <- dollar <$> showWithLinkedObject showX "perm" withObject
    pure $ pure "BecomesTapped" <> sWithObject
  EntersBattlefield withObject -> yesParens do
    sWithObject <- dollar <$> showWithLinkedObject showX "perm" withObject
    pure $ pure "EntersBattlefield" <> sWithObject
  EntersNonBattlefield withObject -> yesParens do
    sWithObject <- dollar <$> showWithLinkedObject showX "perm" withObject
    pure $ pure "EntersNonBattlefield" <> sWithObject
  Events listeners -> yesParens do
    sListeners <- dollar <$> showListM (showEventListener' showX) listeners
    pure $ pure "Events" <> sListeners
  SpellIsCast withObject -> yesParens do
    sWithObject <- dollar <$> showWithLinkedObject showX "spell" withObject
    pure $ pure "SpellIsCast" <> sWithObject
  TimePoint timePoint oneShot -> yesParens do
    sTimePoint <- parens <$> showTimePoint timePoint
    sOneShot <- dollar <$> showX oneShot
    pure $ pure "TimePoint " <> sTimePoint <> sOneShot

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
    Mana x -> pure $ pure $ fromString $ show x
    VariableMana var -> do
      let sVar = pure $ getVarName var
      pure $ pure "VariableMana " <> sVar
    SumMana x y -> do
      sX <- parens <$> showMana x
      sY <- parens <$> showMana y
      pure $ pure "SumMana " <> sX <> pure " " <> sY

showPhyrexianManaCost :: PhyrexianManaCost var -> EnvM ParenItems
showPhyrexianManaCost cost = noParens do
  let PhyrexianManaCost
        { phyrexianW = w
        , phyrexianU = u
        , phyrexianB = b
        , phyrexianR = r
        , phyrexianG = g
        , phyrexianC = c
        } = cost
  case cost == mempty of
    True -> pure $ pure "mempty"
    False -> do
      sW <- parens <$> showMana w
      sU <- parens <$> showMana u
      sB <- parens <$> showMana b
      sR <- parens <$> showMana r
      sG <- parens <$> showMana g
      sC <- dollar <$> showMana c
      pure $
        pure "PhyrexianManaCost "
          <> sW
          <> pure " "
          <> sU
          <> pure " "
          <> sB
          <> pure " "
          <> sR
          <> pure " "
          <> sG
          <> sC

showHybridManaCost :: HybridManaCost var -> EnvM ParenItems
showHybridManaCost cost = noParens do
  let HybridManaCost
        { hybridBG = bg
        } = cost
  case cost == mempty of
    True -> pure $ pure "mempty"
    False -> do
      sBG <- parens <$> showMana bg
      pure $ pure "HybridManaCost " <> sBG

showDynamicManaCost :: DynamicManaCost var -> EnvM ParenItems
showDynamicManaCost cost = noParens do
  let DynamicManaCost
        { costSnow = s
        , costGeneric = x
        , costHybrid = hy
        , costPhyrexian = phy
        } = cost
  case cost == mempty of
    True -> pure $ pure "mempty"
    False -> case isOnlyGeneric cost of
      True -> dropParens <$> showMana x
      False -> do
        sS <- parens <$> showMana s
        sX <- parens <$> showMana x
        sHy <- parens <$> showHybridManaCost hy
        sPhy <- dollar <$> showPhyrexianManaCost phy
        pure $ pure "DynamicManaCost " <> sS <> pure " " <> sX <> pure " " <> sHy <> sPhy

-- FIXME
showManaCost :: ManaCost var -> EnvM ParenItems
showManaCost cost = yesParens do
  let ManaCost'
        { costW = w
        , costU = u
        , costB = b
        , costR = r
        , costG = g
        , costC = c
        , costDynamic = dyn
        } = cost
      DynamicManaCost
        { costGeneric = x
        , costSnow = s
        , costHybrid = hybrid
        , costPhyrexian = phyrexian
        } = dyn
      HybridManaCost
        { hybridWU = wu
        , hybridUB = ub
        , hybridBR = br
        , hybridRG = rg
        , hybridGW = gw
        , hybridWB = wb
        , hybridUR = ur
        , hybridBG = bg
        , hybridRW = rw
        , hybridGU = gu
        , hybridW2 = w2
        , hybridU2 = u2
        , hybridB2 = b2
        , hybridR2 = r2
        , hybridG2 = g2
        , hybridC2 = c2
        } = hybrid
      PhyrexianManaCost
        { phyrexianW = pw
        , phyrexianU = pu
        , phyrexianB = pb
        , phyrexianR = pr
        , phyrexianG = pg
        , phyrexianC = pc
        } = phyrexian
      lits =
        sequence
          [ tryLitMana x
          , tryLitMana w
          , tryLitMana u
          , tryLitMana b
          , tryLitMana r
          , tryLitMana g
          , tryLitMana c
          , tryLitMana s
          , tryLitMana wu
          , tryLitMana ub
          , tryLitMana br
          , tryLitMana rg
          , tryLitMana gw
          , tryLitMana wb
          , tryLitMana ur
          , tryLitMana bg
          , tryLitMana rw
          , tryLitMana gu
          , tryLitMana w2
          , tryLitMana u2
          , tryLitMana b2
          , tryLitMana r2
          , tryLitMana g2
          , tryLitMana c2
          , tryLitMana pw
          , tryLitMana pu
          , tryLitMana pb
          , tryLitMana pr
          , tryLitMana pg
          , tryLitMana pc
          ]
  case lits of
    Just
      [ litX
        , litW
        , litU
        , litB
        , litR
        , litG
        , litC
        , litS
        , litWU
        , litUB
        , litBR
        , litRG
        , litGW
        , litWB
        , litUR
        , litBG
        , litRW
        , litGU
        , litW2
        , litU2
        , litB2
        , litR2
        , litG2
        , litC2
        , litPW
        , litPU
        , litPB
        , litPR
        , litPG
        , litPC
        ] -> do
        let numX = litX
            numW = (W, litW)
            numU = (U, litU)
            numB = (B, litB)
            numR = (R, litR)
            numG = (G, litG)
            numC = (C, litC)
            numS = (S, litS)
            numWU = (WU, litWU)
            numUB = (UB, litUB)
            numBR = (BR, litBR)
            numRG = (RG, litRG)
            numGW = (GW, litGW)
            numWB = (WB, litWB)
            numUR = (UR, litUR)
            numBG = (BG, litBG)
            numRW = (RW, litRW)
            numGU = (GU, litGU)
            numW2 = (W2, litW2)
            numU2 = (U2, litU2)
            numB2 = (B2, litB2)
            numR2 = (R2, litR2)
            numG2 = (G2, litG2)
            numC2 = (C2, litC2)
            numPW = (PW, litPW)
            numPU = (PU, litPU)
            numPB = (PB, litPB)
            numPR = (PR, litPR)
            numPG = (PG, litPG)
            numPC = (PC, litPC)
            go (sym, num)
              | num == 0 = []
              | num < 10 = replicate num $ show sym
              | otherwise = ["(" ++ show sym ++ "," ++ show num ++ ")"]
            go' num = case num of
              0 -> []
              _ -> [show num]
            manas' =
              [ go' numX
              , go numW
              , go numU
              , go numB
              , go numR
              , go numG
              , go numC
              , go numS
              , go numWU
              , go numUB
              , go numBR
              , go numRG
              , go numGW
              , go numWB
              , go numUR
              , go numBG
              , go numRW
              , go numGU
              , go numW2
              , go numU2
              , go numB2
              , go numR2
              , go numG2
              , go numC2
              , go numPW
              , go numPU
              , go numPB
              , go numPR
              , go numPG
              , go numPC
              ]
            manas = M.join manas'
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
      sDyn <- dollar <$> showDynamicManaCost dyn
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
          <> sDyn

showManaPool :: ManaPool snow -> EnvM ParenItems
showManaPool pool = yesParens do
  let ManaPool
        { poolW = w
        , poolU = u
        , poolB = b
        , poolR = r
        , poolG = g
        , poolC = c
        } = pool
      lits =
        ( litMana w
        , litMana u
        , litMana b
        , litMana r
        , litMana g
        , litMana c
        )
  case lits of
    (litW, litU, litB, litR, litG, litC) -> do
      let numW = (W, litW)
          numU = (U, litU)
          numB = (B, litB)
          numR = (R, litR)
          numG = (G, litG)
          numC = (C, litC)
          go (sym, num)
            | num == 0 = []
            | num < 10 = replicate num $ show sym
            | otherwise = ["(" ++ show sym ++ "," ++ show num ++ ")"]
          manas' = [go numW, go numU, go numB, go numR, go numG, go numC]
          manas = M.join manas'
          sManas = case manas of
            [m] -> m
            _ -> "(" ++ List.intercalate "," manas ++ ")"
      pure $ pure $ fromString $ "toManaPool " ++ sManas

showNatList :: forall u n x. (IsUser u) => (x -> EnvM ParenItems) -> NatList u n x -> EnvM ParenItems
showNatList showX = \case
  LZ u x -> yesParens do
    let sU = pure $ fromString $ show u
    sX <- dollar <$> showX x
    pure $ pure "LZ (" <> sU <> pure ")" <> sX
  LS u x xs -> yesParens do
    let sU = pure $ fromString $ show u
    sX <- parens <$> showX x
    sXs <- dollar <$> showNatList showX xs
    pure $ pure "LS (" <> sU <> pure ") " <> sX <> sXs

showO1 ::
  forall zone a z.
  (IsZone zone, IsObjectType a) =>
  (IsOTN (OT1 a)) =>
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
  (IsZone zone, IsOTN ot, IsObjectType a) =>
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
  (IsObjectType a) => TypeRep -> Item -> Object a -> EnvM ParenItems
showObjectNImpl objNRef prefix obj = do
  let i = objectToId obj
  sObj <- showObject obj
  EnvM (State.gets $ Map.lookup i . originalObjectRep) >>= \case
    Nothing -> noParens $ pure sObj -- Object is an unbound variable. Can happen when walking past a variable binding before showing the rest of the tree.
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
        | rep == typeRep (Proxy @(ObjectN OTNCreaturePlaneswalker)) ->
            "asCreaturePlaneswalker"
        | rep == typeRep (Proxy @(ObjectN OTNCreaturePlayer)) ->
            "asCreaturePlayer"
        | rep == typeRep (Proxy @(ObjectN OTNPlayerPlaneswalker)) ->
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
        | rep == typeRep (Proxy @(ObjectN OTNCreaturePlayerPlaneswalker)) ->
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
        | rep == typeRep (Proxy @(ObjectN OTNPermanent)) -> "asPermanent"
        | otherwise -> "toZO5"

showObject6 ::
  forall zone a b c d e f.
  (IsZone zone, Inst6 IsObjectType a b c d e f) =>
  ON6 a b c d e f ->
  EnvM ParenItems
showObject6 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
        | rep == typeRep (Proxy @(ObjectN OTNSpell)) -> "asSpell"
        | otherwise -> "toZO6"

showObject7 ::
  forall zone a b c d e f g.
  (IsZone zone, Inst7 IsObjectType a b c d e f g) =>
  ON7 a b c d e f g ->
  EnvM ParenItems
showObject7 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
        | rep == typeRep (Proxy @(ObjectN OTNDamageSource)) -> "asDamageSource"
        | otherwise -> "toZO8"

showObject9 ::
  forall zone a b c d e f g h i.
  (IsZone zone, Inst9 IsObjectType a b c d e f g h i) =>
  ON9 a b c d e f g h i ->
  EnvM ParenItems
showObject9 objN = visitObjectN' visit objN
 where
  rep = typeOf objN
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
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
  visit :: (IsObjectType x) => Object x -> EnvM ParenItems
  visit =
    showObjectNImpl rep $
      if
        | rep == typeRep (Proxy @(ObjectN OTNAny)) -> "asAny"
        | otherwise -> "toZO12"

showObjectN :: forall zone ot. (IsZO zone ot) => ObjectN ot -> EnvM ParenItems
showObjectN objN' = viewOTN' objN' go
 where
  go :: ObjectN (OTN otk) -> OTN otk -> EnvM ParenItems
  go objN = \case
    OT0 -> showObject0 @zone objN
    OT1 -> showObject1 @zone objN
    OT2 -> showObject2 @zone objN
    OT3 -> showObject3 @zone objN
    OT4 -> showObject4 @zone objN
    OT5 -> showObject5 @zone objN
    OT6 -> showObject6 @zone objN
    OT7 -> showObject7 @zone objN
    OT8 -> showObject8 @zone objN
    OT9 -> showObject9 @zone objN
    OT10 -> showObject10 @zone objN
    OT11 -> showObject11 @zone objN
    OT12 -> showObject12 @zone objN

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
    sAbility <- dollar <$> showSomeZone (showWithThisAbility "this") ability
    pure $ pure "HasAbility" <> sAbility
  HasLandType landType -> yesParens do
    sLandType <- dollar <$> showLandType landType
    pure $ pure "HasLandType" <> sLandType
  Is objN -> yesParens do
    sObjN <- dollar <$> showZoneObject objN
    pure $ pure "Is" <> sObjN
  IsOpponentOf player -> yesParens do
    sPlayer <- dollar <$> showZoneObject player
    pure $ pure "IsOpponentOf" <> sPlayer
  IsTapped -> yesParens do
    pure $ pure "IsTapped"
  Not req -> yesParens do
    sReq <- dollar <$> showRequirement req
    pure $ pure "Not" <> sReq
  OfColors colors -> yesParens do
    pure $ pure $ fromString $ "OfColors $ " ++ show colors
  OwnedBy obj -> yesParens do
    sObj <- dollar <$> showZoneObject obj
    pure $ pure "OwnedBy" <> sObj
  RAnd reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "RAnd" <> sReqs
  ROr reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "ROr" <> sReqs
  Req2 reqsA reqsB -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- dollar <$> showRequirements reqsB
    pure $ pure "Req2 " <> sReqsA <> sReqsB
  Req3 reqsA reqsB reqsC -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- dollar <$> showRequirements reqsC
    pure $ pure "Req3 " <> sReqsA <> pure " " <> sReqsB <> sReqsC
  Req4 reqsA reqsB reqsC reqsD -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- parens <$> showRequirements reqsC
    sReqsD <- dollar <$> showRequirements reqsD
    pure $
      pure "Req4 "
        <> sReqsA
        <> pure " "
        <> sReqsB
        <> pure " "
        <> sReqsC
        <> sReqsD
  Req5 reqsA reqsB reqsC reqsD reqsE -> yesParens do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- parens <$> showRequirements reqsC
    sReqsD <- parens <$> showRequirements reqsD
    sReqsE <- dollar <$> showRequirements reqsE
    pure $
      pure "Req5 "
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

showSomeZone ::
  forall liftZOT ot.
  (forall zone. liftZOT zone ot -> EnvM ParenItems) ->
  SomeZone liftZOT ot ->
  EnvM ParenItems
showSomeZone showM = \case
  SomeZone x -> yesParens do
    sX <- dollar <$> showM x
    pure $ pure "SomeZone" <> sX
  SomeZone2 x -> yesParens do
    sX <- dollar <$> showM x
    pure $ pure "SomeZone2" <> sX

showStaticAbility :: StaticAbility zone ot -> EnvM ParenItems
showStaticAbility = \case
  As electListener -> yesParens do
    sWithObject <- dollar <$> showElect electListener
    pure $ pure "As" <> sWithObject
  Bestow cost enchant -> yesParens do
    sCost <- parens <$> showElect cost
    sEnchant <- dollar <$> showEnchant enchant
    pure $ pure "Bestow " <> sCost <> sEnchant
  CantBlock -> noParens do
    pure $ pure "CantBlock"
  Defender -> noParens do
    pure $ pure "Defender"
  Enters entersStatic -> yesParens do
    sEntersStatic <- dollar <$> showEntersStatic entersStatic
    pure $ pure "Enters" <> sEntersStatic
  FirstStrike -> noParens do
    pure $ pure "FirstStrike"
  Flying -> noParens do
    pure $ pure "Flying"
  Fuse -> noParens do
    pure $ pure "Fuse"
  Haste -> noParens do
    pure $ pure "Haste"
  Landwalk reqs -> yesParens do
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Landwalk" <> sReqs
  Phasing -> noParens do
    pure $ pure "Phasing"
  StaticContinuous continuous -> yesParens do
    sContinuous <- dollar <$> showElect continuous
    pure $ pure "StaticContinuous" <> sContinuous
  Suspend time cost -> yesParens do
    let sTime = pure $ fromString $ show time
    sCost <- dollar <$> showElect cost
    pure $ pure "Suspend " <> sTime <> sCost
  Trample -> noParens do
    pure $ pure "Trample"

showSupertypes :: [Supertype ot] -> EnvM ParenItems
showSupertypes = showListM showSupertype

showSupertype :: Supertype ot -> EnvM ParenItems
showSupertype = \case
  Basic -> noParens do
    pure $ pure "Basic"
  Legendary -> noParens do
    pure $ pure "Legendary"
  Snow -> noParens do
    pure $ pure "Snow"
  Tribal tys -> yesParens do
    sTys <- dollar <$> showCreatureTypes tys
    pure $ pure "Tribal" <> sTys
  World -> noParens do
    pure $ pure "World"

showTimePoint :: TimePoint p -> EnvM ParenItems
showTimePoint = yesParens . pure . pure . fromString . show

showToken :: Token ot -> EnvM ParenItems
showToken = \case
  Token card -> yesParens do
    sCard <- dollar <$> showCard card
    pure $ pure "Token" <> sCard

showToughness :: Toughness -> EnvM ParenItems
showToughness = yesParens . pure . pure . fromString . show

showTriggeredAbility :: TriggeredAbility zone ot -> EnvM ParenItems
showTriggeredAbility = \case
  When listener -> go "When" listener
 where
  go consName eventListener = yesParens do
    sEventListener <- dollar <$> showElect eventListener
    pure $ pure (fromString consName) <> sEventListener

showTypeOf :: forall a. (PrettyType a) => Proxy a -> EnvM ParenItems
showTypeOf _ = conditionalParens do
  pure $ pure $ fromString name
 where
  name = prettyType @a
  conditionalParens = case ' ' `elem` name of
    True -> yesParens
    False -> noParens

showWithLinkedObject ::
  forall liftOT zone ot.
  (IsZO zone ot) =>
  (forall ot'. liftOT ot' -> EnvM ParenItems) ->
  String ->
  WithLinkedObject liftOT zone ot ->
  EnvM ParenItems
showWithLinkedObject showM memo = \case
  Linked1 reqs cont ->
    let ty = getType reqs
     in go ty reqs $ showO1 @zone p showM memo (cont . toZone)
  Linked2 reqs cont ->
    let ty = getType reqs
     in go ty reqs $ showO2 @zone p showM memo (cont . toZone)
  Linked3 reqs cont ->
    let ty = getType reqs
     in go ty reqs $ showO3 @zone p showM memo (cont . toZone)
  Linked4 reqs cont ->
    let ty = getType reqs
     in go ty reqs $ showO4 @zone p showM memo (cont . toZone)
  Linked5 reqs cont ->
    let ty = getType reqs
     in go ty reqs $ showO5 @zone p showM memo (cont . toZone)
 where
  p = Singular

  getType :: [Requirement zone ot] -> Proxy ot
  getType _ = Proxy

  go ty reqs sCont = yesParens do
    sTy <- parens <$> showTypeOf ty
    sReqs <- parens <$> showRequirements reqs
    sCont' <- dollar <$> sCont
    pure $ pure "linked @" <> sTy <> pure " " <> sReqs <> sCont'

showWithList :: (ret -> EnvM ParenItems) -> WithList ret zone ot -> EnvM ParenItems
showWithList showRet = \case
  CountOf zos cont -> yesParens do
    sZos <- parens <$> showZoneObjects zos
    discr <- EnvM $ State.gets nextVariableId
    EnvM $ State.modify' \st -> st{nextVariableId = (1 +) <$> discr}
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
  forall liftOT zone ot.
  (IsZone zone) =>
  (liftOT ot -> EnvM ParenItems) ->
  String ->
  WithMaskedObject liftOT zone ot ->
  EnvM ParenItems
showWithMaskedObject showM memo = \case
  Masked1 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO1 @zone p showM memo (cont . toZone)
  Masked2 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO2 @zone p showM memo (cont . toZone)
  Masked3 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO3 @zone p showM memo (cont . toZone)
  Masked4 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO4 @zone p showM memo (cont . toZone)
  Masked5 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO5 @zone p showM memo (cont . toZone)
  Masked6 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO6 @zone p showM memo (cont . toZone)
 where
  p = Singular

  getType :: [Requirement zone ot'] -> Proxy ot'
  getType _ = Proxy

  go ty reqs sCont = yesParens do
    sTy <- parens <$> showTypeOf ty
    sReqs <- parens <$> showRequirements reqs
    sCont' <- dollar <$> sCont
    pure $ pure "masked @" <> sTy <> pure " " <> sReqs <> sCont'

showWithMaskedObjects ::
  forall liftOT zone ot.
  (IsZone zone) =>
  (liftOT ot -> EnvM ParenItems) ->
  String ->
  WithMaskedObjects liftOT zone ot ->
  EnvM ParenItems
showWithMaskedObjects showM memo = \case
  Maskeds1 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO1 @zone p showM memo (cont . pure . toZone)
  Maskeds2 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO2 @zone p showM memo (cont . pure . toZone)
  Maskeds3 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO3 @zone p showM memo (cont . pure . toZone)
  Maskeds4 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO4 @zone p showM memo (cont . pure . toZone)
  Maskeds5 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO5 @zone p showM memo (cont . pure . toZone)
  Maskeds6 reqs cont ->
    let ty = getType reqs in go ty reqs $ showO6 @zone p showM memo (cont . pure . toZone)
 where
  p = Plural

  getType :: [Requirement zone ot'] -> Proxy ot'
  getType _ = Proxy

  go ty reqs sCont = yesParens do
    sTy <- parens <$> showTypeOf ty
    sReqs <- parens <$> showRequirements reqs
    sCont' <- dollar <$> sCont
    pure $ pure "maskeds @" <> sTy <> pure " " <> sReqs <> sCont'

showWithThis ::
  forall liftOT zone ot.
  (IsZO zone ot) =>
  (PrettyType (ZO zone ot)) =>
  (forall ot'. liftOT ot' -> EnvM ParenItems) ->
  String ->
  WithThis liftOT zone ot ->
  EnvM ParenItems
showWithThis showM memo = \case
  This1 cont ->
    let go = yesParens do
          sTy <- parens <$> showTypeOf (Proxy @ot)
          sCont <- dollar <$> showO1 @zone Singular showM memo (cont . toZone)
          pure $ pure "thisObject @" <> sTy <> sCont
     in go
  This2 cont ->
    let go ::
          forall a b.
          (IsOTN (OT2 a b), Inst2 IsObjectType a b) =>
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
  This3 cont ->
    let go ::
          forall a b c.
          (IsOTN (OT3 a b c), Inst3 IsObjectType a b c) =>
          ((ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c)) -> liftOT (OT3 a b c)) ->
          EnvM ParenItems
        go cont' = yesParens do
          sTy <- parens <$> showTypeOf (Proxy @ot)
          (objNa, snap) <- newObjectN @a O1 memo
          (objNb, _) <- newObjectN @b O1 memo
          (objNc, _) <- newObjectN @c O1 memo
          sObjNa <- parens <$> showObjectN @zone objNa
          sObjNb <- parens <$> showObjectN @zone objNb
          sObjNc <- parens <$> showObjectN @zone objNc
          let elect = cont' (toZone objNa, toZone objNb, toZone objNc)
          sElect <- dropParens <$> showM elect
          restoreObject snap
          pure $
            pure "thisObject @"
              <> sTy
              <> pure " $ \\("
              <> sObjNa
              <> pure ", "
              <> sObjNb
              <> pure ", "
              <> sObjNc
              <> pure ") -> "
              <> sElect
     in go cont
  This4 cont ->
    let go ::
          forall a b c d.
          (IsOTN (OT4 a b c d), Inst4 IsObjectType a b c d) =>
          ((ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d)) -> liftOT (OT4 a b c d)) ->
          EnvM ParenItems
        go cont' = yesParens do
          sTy <- parens <$> showTypeOf (Proxy @ot)
          (objNa, snap) <- newObjectN @a O1 memo
          (objNb, _) <- newObjectN @b O1 memo
          (objNc, _) <- newObjectN @c O1 memo
          (objNd, _) <- newObjectN @d O1 memo
          sObjNa <- parens <$> showObjectN @zone objNa
          sObjNb <- parens <$> showObjectN @zone objNb
          sObjNc <- parens <$> showObjectN @zone objNc
          sObjNd <- parens <$> showObjectN @zone objNd
          let elect = cont' (toZone objNa, toZone objNb, toZone objNc, toZone objNd)
          sElect <- dropParens <$> showM elect
          restoreObject snap
          pure $
            pure "thisObject @"
              <> sTy
              <> pure " $ \\("
              <> sObjNa
              <> pure ", "
              <> sObjNb
              <> pure ", "
              <> sObjNc
              <> pure ", "
              <> sObjNd
              <> pure ") -> "
              <> sElect
     in go cont
  This5 cont ->
    let go ::
          forall a b c d e.
          (IsOTN (OT5 a b c d e), Inst5 IsObjectType a b c d e) =>
          ((ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d), ZO zone (OT1 e)) -> liftOT (OT5 a b c d e)) ->
          EnvM ParenItems
        go cont' = yesParens do
          sTy <- parens <$> showTypeOf (Proxy @ot)
          (objNa, snap) <- newObjectN @a O1 memo
          (objNb, _) <- newObjectN @b O1 memo
          (objNc, _) <- newObjectN @c O1 memo
          (objNd, _) <- newObjectN @d O1 memo
          (objNe, _) <- newObjectN @e O1 memo
          sObjNa <- parens <$> showObjectN @zone objNa
          sObjNb <- parens <$> showObjectN @zone objNb
          sObjNc <- parens <$> showObjectN @zone objNc
          sObjNd <- parens <$> showObjectN @zone objNd
          sObjNe <- parens <$> showObjectN @zone objNe
          let elect = cont' (toZone objNa, toZone objNb, toZone objNc, toZone objNd, toZone objNe)
          sElect <- dropParens <$> showM elect
          restoreObject snap
          pure $
            pure "thisObject @"
              <> sTy
              <> pure " $ \\("
              <> sObjNa
              <> pure ", "
              <> sObjNb
              <> pure ", "
              <> sObjNc
              <> pure ", "
              <> sObjNd
              <> pure ", "
              <> sObjNe
              <> pure ") -> "
              <> sElect
     in go cont

showWithThisAbility ::
  forall zone ot.
  String ->
  WithThisAbility zone ot ->
  EnvM ParenItems
showWithThisAbility memo = \case
  WithThisActivated withThis -> yesParens do
    sWithThis <- dollar <$> showWithThis (showElect . unElectOT) memo withThis
    pure $ pure "WithThisActivated" <> sWithThis
  WithThisStatic withThis -> yesParens do
    sWithThis <- dollar <$> showWithThis showStaticAbility memo withThis
    pure $ pure "WithThisStatic" <> sWithThis
  WithThisTriggered withThis -> yesParens do
    sWithThis <- dollar <$> showWithThis showTriggeredAbility memo withThis
    pure $ pure "WithThisTriggered" <> sWithThis

showWithThisZ ::
  forall liftZOT zone ot.
  (forall ot'. liftZOT zone ot' -> EnvM ParenItems) ->
  String ->
  WithThisZ liftZOT zone ot ->
  EnvM ParenItems
showWithThisZ showM memo = \case
  WithThisZ withThis -> yesParens do
    sWithThis <- dollar <$> showWithThis showM memo withThis
    pure $ pure "WithThisZ" <> sWithThis

showZoneObject :: forall zone ot. (IsZO zone ot) => ZO zone ot -> EnvM ParenItems
showZoneObject = \case
  ZO _ objN -> showObjectN @zone objN

-- showZoneObject0 :: forall zone. IsZone zone => ZO zone OT0 -> EnvM ParenItems
-- showZoneObject0 = \case
--   ZO _ objN -> showObject0 @zone objN

showZoneObjects :: forall zone ot. (IsZO zone ot) => List (ZO zone ot) -> EnvM ParenItems
showZoneObjects (lenseList -> zo) = pluralize $ showZoneObject zo

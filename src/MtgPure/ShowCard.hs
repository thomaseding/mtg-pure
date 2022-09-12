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

module MtgPure.ShowCard
  ( showCard,
    showToken,
  )
where

import qualified Control.Monad.State.Strict as State
import qualified Data.DList as DList
import Data.Inst (Inst2, Inst3, Inst5, Inst8)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (..))
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import MtgPure.Model
import MtgPure.Model.Internal.IsObjectType (IsObjectType (..))
import MtgPure.Model.Internal.Object (Object (..))
import MtgPure.Model.Internal.ObjectId (ObjectId (ObjectId))
import MtgPure.Model.Internal.Variable (Variable (ReifiedVariable))

instance Show Card where
  show = showCard

instance Show Token where
  show = showToken

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

data Env = Env
  { nextObjectId :: ObjectId,
    nextVariableId :: VariableId,
    originalObjectRep :: Map.Map ObjectId TypeRep,
    currentGeneration :: Generation,
    objectGenerations :: Map.Map ObjectId Generation,
    objectNames :: Map.Map ObjectId String
  }

newEnv :: Env
newEnv =
  Env
    { nextObjectId = ObjectId 1,
      nextVariableId = 0,
      originalObjectRep = mempty,
      currentGeneration = 0,
      objectGenerations = mempty,
      objectNames = mempty
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

showObjectName :: Object a -> EnvM Items
showObjectName = fmap pure . getObjectName

showObjectType :: forall a. PrettyObjectName a => a -> EnvM ParenItems
showObjectType _ = yesParens $ do
  let name = fromString $ prettyObjectName (Proxy @a)
  pure $ pure name

showObjectNImpl :: IsObjectType a => TypeRep -> Item -> Object a -> EnvM ParenItems
showObjectNImpl objNRef prefix obj = do
  let i = objectToId obj
  sObj <- showObjectName obj
  State.gets (Map.lookup i . originalObjectRep) >>= \case
    Nothing -> error "impossible"
    Just originalRep -> case originalRep == objNRef of
      False -> yesParens $ pure $ pure prefix <> sObj
      True -> noParens $ pure sObj

showObject2 :: Inst2 IsObjectType a b => ObjectN '(a, b) -> EnvM ParenItems
showObject2 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OCreaturePlaneswalker) -> "creaturePlaneswalker "
            | rep == typeRep (Proxy @OCreaturePlayer) -> "creaturePlayer "
            | rep == typeRep (Proxy @OPlayerPlaneswalker) -> "playerPlaneswalker "
            | otherwise -> "toObject2 "

showObject3 :: Inst3 IsObjectType a b c => ObjectN '(a, b, c) -> EnvM ParenItems
showObject3 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OCreaturePlayerPlaneswalker) -> "creaturePlayerPlaneswalker "
            | otherwise -> "toObject3 "

showObject5 :: Inst5 IsObjectType a b c d e => ObjectN '(a, b, c, d, e) -> EnvM ParenItems
showObject5 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OPermanent) -> "permanent "
            | otherwise -> "toObject5 "

showObject8 :: Inst8 IsObjectType a b c d e f g h => ObjectN '(a, b, c, d, e, f, g, h) -> EnvM ParenItems
showObject8 objN = visitObjectN' visit objN
  where
    rep = typeOf objN
    visit :: IsObjectType x => Object x -> EnvM ParenItems
    visit =
      showObjectNImpl rep $
        if
            | rep == typeRep (Proxy @OAny) -> "asAny "
            | otherwise -> "toObject8 "

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
  forall a b.
  (Typeable (ObjectN b), IsObjectType a) =>
  (Object a -> ObjectN b) ->
  String ->
  EnvM (ObjectN b, ObjectIdState)
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

showObjectDecl1 :: IsObjectType a => Object a -> EnvM ParenItems
showObjectDecl1 = showObjectDecl (noParens . showObjectName)

showCreaturePlayerPlaneswalker :: OCreaturePlayerPlaneswalker -> EnvM ParenItems
showCreaturePlayerPlaneswalker = showObject3

showPermanentObject :: OPermanent -> EnvM ParenItems
showPermanentObject = showObject5

showAnyObject :: OAny -> EnvM ParenItems
showAnyObject = showObject8

showToken :: Token -> String
showToken (Token card) = "Token $ " ++ show card

showCard :: Card -> String
showCard card = concat $ State.evalState strsM newEnv
  where
    itemsM = DList.toList . dropParens <$> showCardM card
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

showCardM :: Card -> EnvM ParenItems
showCardM = \case
  Card (CardName name) set rarity def -> yesParens $ do
    sDef <- dollar <$> showCardTypeDef def
    pure $
      pure "Card "
        <> pure (fromString $ show name)
        <> pure " "
        <> pure (fromString $ show set)
        <> pure " "
        <> pure (fromString $ show rarity)
        <> sDef

showCardTypeDef :: CardTypeDef a -> EnvM ParenItems
showCardTypeDef = \case
  Variable cont -> yesParens $ do
    i <- State.gets nextVariableId
    State.modify' $ \st -> st {nextVariableId = i + 1}
    let var = ReifiedVariable i
    let varName = getVarName var
    s <- dropParens <$> showCardTypeDef (cont var)
    pure $ pure "Variable $ \\" <> pure varName <> pure " -> " <> s
  ArtifactDef colors cost abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showSpellCost cost
    sAbilities <- parens <$> showAbilities abilities
    pure $ pure "ArtifactDef " <> sColors <> pure " " <> sCost <> pure " " <> sAbilities
  CreatureDef colors cost subTypes power toughness abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showSpellCost cost
    sSubTypes <- parens <$> showCreatureTypes subTypes
    sPower <- parens <$> showPower power
    sToughness <- parens <$> showToughness toughness
    sAbilities <- parens <$> showAbilities abilities
    pure $
      pure "CreatureDef "
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sSubTypes
        <> pure " "
        <> sPower
        <> pure " "
        <> sToughness
        <> pure " "
        <> sAbilities
  EnchantmentDef colors cost abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showSpellCost cost
    sAbilities <- parens <$> showAbilities abilities
    pure $ pure "EnchantmentDef " <> sColors <> pure " " <> sCost <> pure " " <> sAbilities
  InstantDef colors cost abilities cont -> do
    showOneShot "InstantDef " colors cost abilities cont
  LandDef abilities -> yesParens $ do
    sAbilities <- parens <$> showAbilities abilities
    pure $ pure "LandDef " <> sAbilities
  PlaneswalkerDef colors cost loyalty abilities -> yesParens $ do
    sColors <- parens <$> showColors colors
    sCost <- parens <$> showSpellCost cost
    sLoyalty <- parens <$> showLoyalty loyalty
    sAbilities <- parens <$> showAbilities abilities
    pure $
      pure "PlaneswalkerDef "
        <> sColors
        <> pure " "
        <> sCost
        <> pure " "
        <> sLoyalty
        <> pure " "
        <> sAbilities
  SorceryDef colors cost abilities cont -> do
    showOneShot "SorceryDef " colors cost abilities cont
  where
    showOneShot ::
      forall a.
      IsObjectType a =>
      Item ->
      Colors ->
      SpellCost ->
      [Ability a] ->
      (Object a -> Elect 'OneShot a) ->
      EnvM ParenItems
    showOneShot def colors cost abilities cont = yesParens $ do
      sColors <- parens <$> showColors colors
      sCost <- parens <$> showSpellCost cost
      sAbilities <- parens <$> showAbilities abilities
      (this, snap) <- newObject @a "this"
      thisName <- getObjectName this
      let elect = cont this
      sElect <- dropParens <$> showElect elect
      restoreObject snap
      pure $
        pure def <> sColors <> pure " " <> sCost <> pure " " <> sAbilities
          <> pure " $ \\"
          <> pure thisName
          <> pure " -> "
          <> sElect

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

showSelection :: Selection -> EnvM ParenItems
showSelection = \case
  Choose player -> yesParens $ (pure "Choose " <>) <$> showObjectName player
  Target player -> yesParens $ (pure "Target " <>) <$> showObjectName player
  Random -> noParens $ pure $ pure "Random"

showListM :: (a -> EnvM ParenItems) -> [a] -> EnvM ParenItems
showListM f xs = noParens $ do
  ss <- mapM (fmap dropParens . f) xs
  pure $ pure "[" <> DList.intercalate (pure ", ") ss <> pure "]"

showRequirements :: IsObjectType a => [Requirement a] -> EnvM ParenItems
showRequirements = showListM showRequirement

showRequirement :: forall a. IsObjectType a => Requirement a -> EnvM ParenItems
showRequirement = \case
  Impossible -> noParens $ pure $ pure "Impossible"
  OfColors colors -> yesParens $ pure $ pure $ fromString $ "OfColors $ " ++ show colors
  Not req -> yesParens $ (pure "Not" <>) . dollar <$> showRequirement req
  Is obj -> yesParens $ (pure "Is " <>) <$> showObjectName obj
  ControlledBy obj -> yesParens $ (pure "ControlledBy " <>) <$> showObjectName obj
  OwnedBy obj -> yesParens $ (pure "OwnedBy " <>) <$> showObjectName obj
  Tapped perm -> yesParens $ pure $ pure $ fromString $ "Tapped " ++ show perm

showRequirementN :: RequirementN a -> EnvM ParenItems
showRequirementN = \case
  Req2 reqsA reqsB -> yesParens $ do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    pure $ pure "Req2 " <> sReqsA <> pure " " <> sReqsB
  Req3 reqsA reqsB reqsC -> yesParens $ do
    sReqsA <- parens <$> showRequirements reqsA
    sReqsB <- parens <$> showRequirements reqsB
    sReqsC <- parens <$> showRequirements reqsC
    pure $ pure "Req3 " <> sReqsA <> pure " " <> sReqsB <> pure " " <> sReqsC

showAbilities :: IsObjectType a => [Ability a] -> EnvM ParenItems
showAbilities = showListM showAbility

showAbility :: forall a. IsObjectType a => Ability a -> EnvM ParenItems
showAbility = \case
  Activated cost cont ->
    yesParens $
      (pure "Activated " <>) <$> do
        sCost <- parens <$> showActivationCost cost
        (obj, snap) <- newObject @a "this"
        name <- showObjectName obj
        sElect <- dropParens <$> showElect (cont obj)
        restoreObject snap
        pure $ sCost <> pure " $ \\" <> name <> pure " -> " <> sElect
  Static ability -> yesParens $ (pure "Static" <>) . dollar <$> showStatic ability

showActivationCost :: forall a. IsObjectType a => ActivationCost a -> EnvM ParenItems
showActivationCost cont = yesParens $ do
  (obj, snap) <- newObject @a "this"
  (controller, _) <- newObject @OTPlayer "you"
  objName <- showObjectName obj
  controllerName <- showObjectName controller
  let cost = cont obj controller
  sCost <- dropParens <$> showCost cost
  restoreObject snap
  pure $ pure "\\" <> objName <> pure " " <> controllerName <> pure " -> " <> sCost

showSpellCost :: SpellCost -> EnvM ParenItems
showSpellCost cont = yesParens $ do
  (you, snap) <- newObject @OTPlayer "you"
  youName <- showObjectName you
  let cost = cont you
  sCost <- dropParens <$> showCost cost
  restoreObject snap
  pure $ pure "\\" <> youName <> pure " -> " <> sCost

showCost :: Cost -> EnvM ParenItems
showCost = \case
  ManaCostCost cost -> yesParens $ do
    sCost <- dollar <$> showManaCost cost
    pure $ pure (fromString "ManaCostCost") <> sCost
  AndCosts costs -> yesParens $ do
    sCosts <- parens <$> showListM showCost costs
    pure $ pure "AndCosts " <> sCosts
  OrCosts costs -> yesParens $ do
    sCosts <- parens <$> showListM showCost costs
    pure $ pure "OrCosts " <> sCosts
  TapCost obj -> yesParens $ (pure "TapCost" <>) . dollar <$> showPermanentObject obj
  LoyaltyCost walker loyalty -> yesParens $ do
    sWalker <- showObjectName walker
    sLoyalty <- dollar <$> showLoyalty loyalty
    pure $ pure "LoyaltyCost " <> sWalker <> sLoyalty
  DiscardRandomCost player amount -> yesParens $ do
    sPlayer <- showObjectName player
    let sAmount = pure $ fromString $ show amount
    pure $ pure "DiscardRandomCost " <> sPlayer <> pure " " <> sAmount
  PayLife player amount -> yesParens $ do
    sPlayer <- showObjectName player
    let sAmount = pure $ fromString $ show amount
    pure $ pure "PayLife " <> sPlayer <> sAmount
  SacrificeCost perm player reqs -> yesParens $ do
    sPerm <- parens <$> showPermanent perm
    sPlayer <- showObjectName player
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "SacrificeCost " <> sPerm <> sPlayer <> sReqs

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
  let ManaCost
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
        pure (fromString "ManaCost ")
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
    DamageFromPower o -> do
      name <- showObjectName o
      pure $ pure "DamageFromPower " <> name
    VariableDamage var -> do
      let varName = getVarName var
      pure $ DList.fromList [fromString "VariableDamage ", varName]

showPermanent :: Permanent a -> EnvM ParenItems
showPermanent = noParens . pure . pure . fromString . show

showStatic :: forall a. IsObjectType a => StaticAbility a -> EnvM ParenItems
showStatic = \case
  ContinuousEffect cont -> yesParens $ do
    (this, snap) <- newObject @a "this"
    name <- showObjectName this
    let elect = cont this
    sEffect <- dropParens <$> showElect elect
    restoreObject snap
    pure $ pure "ContinuousEffect $ \\" <> name <> pure " -> " <> sEffect
  Haste -> noParens $ pure $ pure "Haste"
  FirstStrike -> noParens $ pure $ pure "FirstStrike"
  Suspend time cost -> yesParens $ do
    let sTime = pure $ fromString $ show time
    sCost <- dollar <$> showSpellCost cost
    pure $ pure "Suspend " <> sTime <> sCost

showElect :: Elect e a -> EnvM ParenItems
showElect = \case
  ControllerOf obj cont -> yesParens $ do
    objPrefix <- getObjectNamePrefix $ visitObjectN' objectToId obj
    (controller, snap) <- newObject @OTPlayer $ case objPrefix == "this" of
      True -> "you"
      False -> "controller"
    controllerName <- showObjectName controller
    objName <- parens <$> showAnyObject obj
    let elect = cont controller
    sElect <- dropParens <$> showElect elect
    restoreObject snap
    pure $ pure "ControllerOf " <> objName <> pure " $ \\" <> controllerName <> pure " -> " <> sElect
  A x -> yesParens $ (pure "A" <>) . dollar <$> showA x
  All xs -> yesParens $ (pure "All" <>) . dollar <$> showAll xs
  Effect effect -> yesParens $ (pure "Effect" <>) . dollar <$> showEffect effect

showA :: A e a -> EnvM ParenItems
showA = \case
  A1 sel reqs cont -> showA1 sel reqs cont
  A2 sel reqs cont -> showA2 sel reqs cont
  A3 sel reqs cont -> showA3 sel reqs cont

selectionMemo :: Selection -> String
selectionMemo = \case
  Choose {} -> "choice"
  Target {} -> "target"
  Random -> "random"

showA1 ::
  forall a b e.
  IsObjectType b =>
  Selection ->
  [Requirement b] ->
  (Object b -> Elect e a) ->
  EnvM ParenItems
showA1 sel reqs cont = yesParens $ do
  sSel <- parens <$> showSelection sel
  sReqs <- parens <$> showRequirements reqs
  (obj, snap) <- newObject @b $ selectionMemo sel
  objName <- parens <$> showObjectDecl1 obj
  let elect = cont obj
  sElect <- dropParens <$> showElect elect
  restoreObject snap
  pure $
    pure "A1 "
      <> sSel
      <> pure " "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showA2 ::
  forall a b c e.
  Inst2 IsObjectType b c =>
  Selection ->
  RequirementN '(b, c) ->
  (ObjectN '(b, c) -> Elect e a) ->
  EnvM ParenItems
showA2 sel reqs cont = yesParens $ do
  sSel <- parens <$> showSelection sel
  sReqs <- parens <$> showRequirementN reqs
  (obj, snap) <- newObjectN @b O2a $ selectionMemo sel
  objName <- parens <$> showObjectDecl showObject2 obj
  let elect = cont obj
  sElect <- dropParens <$> showElect elect
  restoreObject snap
  pure $
    pure "A2 "
      <> sSel
      <> pure " "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showA3 ::
  forall a b c d e.
  Inst3 IsObjectType b c d =>
  Selection ->
  RequirementN '(b, c, d) ->
  (ObjectN '(b, c, d) -> Elect e a) ->
  EnvM ParenItems
showA3 sel reqs cont = yesParens $ do
  sSel <- parens <$> showSelection sel
  sReqs <- parens <$> showRequirementN reqs
  (obj, snap) <- newObjectN @b O3a $ selectionMemo sel
  objName <- parens <$> showObjectDecl showObject3 obj
  let elect = cont obj
  sElect <- dropParens <$> showElect elect
  restoreObject snap
  pure $
    pure "A3 "
      <> sSel
      <> pure " "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showAll :: All e a -> EnvM ParenItems
showAll = \case
  All1 reqs cont -> showAll1 reqs cont
  All2 reqs cont -> showAll2 reqs cont
  All3 reqs cont -> showAll3 reqs cont

showAll1 ::
  forall a b e.
  IsObjectType b =>
  [Requirement b] ->
  (Object b -> Elect e a) ->
  EnvM ParenItems
showAll1 reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirements reqs
  (obj, snap) <- newObject @b "all"
  objName <- parens <$> showObjectDecl1 obj
  let elect = cont obj
  sElect <- dropParens <$> showElect elect
  restoreObject snap
  pure $
    pure "All1 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showAll2 ::
  forall a b c e.
  Inst2 IsObjectType b c =>
  RequirementN '(b, c) ->
  (ObjectN '(b, c) -> Elect e a) ->
  EnvM ParenItems
showAll2 reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirementN reqs
  (obj, snap) <- newObjectN @b O2a "all"
  objName <- parens <$> showObjectDecl showObject2 obj
  let elect = cont obj
  sElect <- dropParens <$> showElect elect
  restoreObject snap
  pure $
    pure "All2 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showAll3 ::
  forall a b c d e.
  Inst3 IsObjectType b c d =>
  RequirementN '(b, c, d) ->
  (ObjectN '(b, c, d) -> Elect e a) ->
  EnvM ParenItems
showAll3 reqs cont = yesParens $ do
  sReqs <- parens <$> showRequirementN reqs
  (obj, snap) <- newObjectN @b O3a "all"
  objName <- parens <$> showObjectDecl showObject3 obj
  let elect = cont obj
  sElect <- dropParens <$> showElect elect
  restoreObject snap
  pure $
    pure "All3 "
      <> sReqs
      <> pure " $ \\"
      <> objName
      <> pure " -> "
      <> sElect

showEffect :: Effect e -> EnvM ParenItems
showEffect = \case
  DoNothing -> noParens $ pure $ pure "DoNothing"
  AddMana mana player -> yesParens $ do
    sMana <- parens <$> showManaPool mana
    name <- showObjectName player
    pure $ pure "AddMana " <> sMana <> pure " " <> name
  DealDamage source victim damage -> yesParens $ do
    sSource <- parens <$> showAnyObject source
    sVictim <- parens <$> showCreaturePlayerPlaneswalker victim
    sDamage <- dollar <$> showDamage damage
    pure $ pure "DealDamage " <> sSource <> pure " " <> sVictim <> sDamage
  Sacrifice perm player reqs -> yesParens $ do
    sPerm <- parens <$> showPermanent perm
    sPlayer <- showObjectName player
    sReqs <- dollar <$> showRequirements reqs
    pure $ pure "Sacrifice " <> sPerm <> pure " " <> sPlayer <> pure " " <> sReqs
  Destroy perm obj -> yesParens $ do
    sPerm <- parens <$> showPermanent perm
    sObj <- showObjectName obj
    pure $ pure "Destroy " <> sPerm <> pure " " <> sObj
  DrawCards player n -> yesParens $ do
    name <- showObjectName player
    let amount = fromString $ show n
    pure $ pure "DrawCards " <> name <> pure " " <> pure amount

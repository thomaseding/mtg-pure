{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Orphans (
  mapManaCost,
  mapManaPool,
) where

import safe Control.Monad.Writer.Strict (Writer, execWriter, tell)
import safe qualified Data.DList as DList
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe MtgPure.Engine.Orphans.ZO ()
import safe MtgPure.Engine.Prompt (
  AnyElected (..),
  Elected (..),
  QueryObjectResult (..),
 )
import safe MtgPure.Engine.State (
  GameResult (..),
  GameState (..),
 )
import safe MtgPure.Model.Artifact (Artifact (..))
import MtgPure.Model.Battle (Battle (..))
import safe MtgPure.Model.CardName (HasCardName (getCardName))
import safe MtgPure.Model.Creature (Creature (..))
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Enchantment (AnyEnchantmentType (..), Enchantment (..))
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Land (Land (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.Mana (IsManaNoVar, Mana (..))
import safe MtgPure.Model.Mana.ManaCost (
  DynamicManaCost (..),
  HybridManaCost (..),
  ManaCost (ManaCost'),
  PhyrexianManaCost (..),
 )
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPayment (..), ManaPool (..))
import MtgPure.Model.Mana.ManaType (ManaType)
import safe MtgPure.Model.Mana.Snow (IsSnow, Snow)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Planeswalker (Planeswalker (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  Ability,
  Card,
  CardCharacteristic,
  SomeOT (..),
  SomeTerm (..),
  SomeZone (..),
  Token,
  WithThisAbility,
 )
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Supertype (Supertype)
import safe MtgPure.Model.Variable (ForceVars (forceVars), Var (NoVar))

newtype Bulleted a = Bulleted a
  deriving (Eq, Ord)

instance (Show a) => Show (Bulleted a) where
  showsPrec :: (Show a) => Int -> Bulleted a -> ShowS
  showsPrec n (Bulleted x) = ("\n  @@ " ++) . showsPrec n x

class AsBulleted a b | a -> b where
  bulleted :: a -> b

instance AsBulleted [a] [Bulleted a] where
  bulleted :: [a] -> [Bulleted a]
  bulleted = map Bulleted

instance (Ord k) => AsBulleted (Map.Map k v) (Map.Map (Bulleted k) v) where
  bulleted :: (Ord k) => Map.Map k v -> Map.Map (Bulleted k) v
  bulleted = Map.mapKeys Bulleted

type DString = DList.DList Char

tellPrint :: (Show a) => a -> Writer DString ()
tellPrint s = tell $ DList.fromList (show s) <> "\n"

tellLine :: DString -> Writer DString ()
tellLine s = tell $ s <> "\n"

class MapManaCost cost where
  mapManaCost ::
    ( forall snow color.
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var' snow color
    ) ->
    cost var ->
    cost var'
  mapManaCost2 ::
    ( forall snow color.
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var snow color ->
      Mana var' snow color
    ) ->
    cost var ->
    cost var ->
    cost var'

instance MapManaCost PhyrexianManaCost where
  mapManaCost ::
    ( forall (snow :: Snow) (color :: ManaType).
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var' snow color
    ) ->
    PhyrexianManaCost var ->
    PhyrexianManaCost var'
  mapManaCost f (PhyrexianManaCost w u b r g c) =
    PhyrexianManaCost (f w) (f u) (f b) (f r) (f g) (f c)

  mapManaCost2 ::
    ( forall (snow :: Snow) (color :: ManaType).
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var snow color ->
      Mana var' snow color
    ) ->
    PhyrexianManaCost var ->
    PhyrexianManaCost var ->
    PhyrexianManaCost var'
  mapManaCost2
    f
    (PhyrexianManaCost w1 u1 b1 r1 g1 c1)
    (PhyrexianManaCost w2 u2 b2 r2 g2 c2) =
      PhyrexianManaCost (f w1 w2) (f u1 u2) (f b1 b2) (f r1 r2) (f g1 g2) (f c1 c2)

instance MapManaCost HybridManaCost where
  mapManaCost ::
    ( forall (snow :: Snow) (color :: ManaType).
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var' snow color
    ) ->
    HybridManaCost var ->
    HybridManaCost var'
  mapManaCost f (HybridManaCost wu ub br rg gw wb ur bg rw gu w2 u2 b2 r2 g2 c2) =
    HybridManaCost
      (f wu)
      (f ub)
      (f br)
      (f rg)
      (f gw)
      (f wb)
      (f ur)
      (f bg)
      (f rw)
      (f gu)
      (f w2)
      (f u2)
      (f b2)
      (f r2)
      (f g2)
      (f c2)

  mapManaCost2 ::
    ( forall (snow :: Snow) (color :: ManaType).
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var snow color ->
      Mana var' snow color
    ) ->
    HybridManaCost var ->
    HybridManaCost var ->
    HybridManaCost var'
  mapManaCost2
    f
    (HybridManaCost wu1 ub1 br1 rg1 gw1 wb1 ur1 bg1 rw1 gu1 w21 u21 b21 r21 g21 c21)
    (HybridManaCost wu2 ub2 br2 rg2 gw2 wb2 ur2 bg2 rw2 gu2 w22 u22 b22 r22 g22 c22) =
      HybridManaCost
        (f wu1 wu2)
        (f ub1 ub2)
        (f br1 br2)
        (f rg1 rg2)
        (f gw1 gw2)
        (f wb1 wb2)
        (f ur1 ur2)
        (f bg1 bg2)
        (f rw1 rw2)
        (f gu1 gu2)
        (f w21 w22)
        (f u21 u22)
        (f b21 b22)
        (f r21 r22)
        (f g21 g22)
        (f c21 c22)

instance MapManaCost ManaCost where
  mapManaCost ::
    ( forall (snow :: Snow) (color :: ManaType).
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var' snow color
    ) ->
    ManaCost var ->
    ManaCost var'
  mapManaCost f (ManaCost' w u b r g c (DynamicManaCost x s hy phy)) =
    ManaCost' (f w) (f u) (f b) (f r) (f g) (f c) $
      DynamicManaCost (f x) (f s) (mapManaCost f hy) (mapManaCost f phy)

  mapManaCost2 ::
    ( forall (snow :: Snow) (color :: ManaType).
      (IsManaNoVar snow color) =>
      Mana var snow color ->
      Mana var snow color ->
      Mana var' snow color
    ) ->
    ManaCost var ->
    ManaCost var ->
    ManaCost var'
  mapManaCost2
    f
    (ManaCost' w1 u1 b1 r1 g1 c1 (DynamicManaCost x1 s1 hy1 phy1))
    (ManaCost' w2 u2 b2 r2 g2 c2 (DynamicManaCost x2 s2 hy2 phy2)) =
      ManaCost'
        (f w1 w2)
        (f u1 u2)
        (f b1 b2)
        (f r1 r2)
        (f g1 g2)
        (f c1 c2)
        $ DynamicManaCost
          (f x1 x2)
          (f s1 s2)
          (mapManaCost2 f hy1 hy2)
          (mapManaCost2 f phy1 phy2)

mapManaPool ::
  (IsSnow snow, IsSnow snow') =>
  ( forall color.
    (IsManaNoVar snow color) =>
    Mana 'NoVar snow color ->
    Mana 'NoVar snow' color
  ) ->
  ManaPool snow ->
  ManaPool snow'
mapManaPool f (ManaPool w u b r g c) =
  ManaPool (f w) (f u) (f b) (f r) (f g) (f c)

mapManaPool2 ::
  (IsSnow snow, IsSnow snow') =>
  ( forall color.
    (IsManaNoVar snow color) =>
    Mana 'NoVar snow color ->
    Mana 'NoVar snow color ->
    Mana 'NoVar snow' color
  ) ->
  ManaPool snow ->
  ManaPool snow ->
  ManaPool snow'
mapManaPool2
  f
  (ManaPool w1 u1 b1 r1 g1 c1)
  (ManaPool w2 u2 b2 r2 g2 c2) =
    ManaPool
      (f w1 w2)
      (f u1 u2)
      (f b1 b2)
      (f r1 r2)
      (f g1 g2)
      (f c1 c2)

mapCompleteManaPool ::
  ( forall snow.
    (IsSnow snow) =>
    ManaPool snow ->
    ManaPool snow
  ) ->
  CompleteManaPool ->
  CompleteManaPool
mapCompleteManaPool f (CompleteManaPool snow nonSnow) =
  CompleteManaPool (f snow) (f nonSnow)

mapCompleteManaPool2 ::
  ( forall snow.
    (IsSnow snow) =>
    ManaPool snow ->
    ManaPool snow ->
    ManaPool snow
  ) ->
  CompleteManaPool ->
  CompleteManaPool ->
  CompleteManaPool
mapCompleteManaPool2
  f
  (CompleteManaPool snow1 nonSnow1)
  (CompleteManaPool snow2 nonSnow2) =
    CompleteManaPool
      (f snow1 snow2)
      (f nonSnow1 nonSnow2)

instance Num CompleteManaPool where
  (+) :: CompleteManaPool -> CompleteManaPool -> CompleteManaPool
  (+) = mapCompleteManaPool2 (+)

  (-) :: CompleteManaPool -> CompleteManaPool -> CompleteManaPool
  (-) = mapCompleteManaPool2 (-)

  (*) :: CompleteManaPool -> CompleteManaPool -> CompleteManaPool
  (*) = mapCompleteManaPool2 (*)

  abs :: CompleteManaPool -> CompleteManaPool
  abs = mapCompleteManaPool abs

  signum :: CompleteManaPool -> CompleteManaPool
  signum = mapCompleteManaPool signum

  negate :: CompleteManaPool -> CompleteManaPool
  negate = mapCompleteManaPool negate

  fromInteger :: Integer -> CompleteManaPool
  fromInteger n = mempty{poolNonSnow = fromInteger n}

instance Num ManaPayment where
  (+) :: ManaPayment -> ManaPayment -> ManaPayment
  p + q = p{paymentMana = paymentMana p + paymentMana q, paymentLife = paymentLife p + paymentLife q}

  (-) :: ManaPayment -> ManaPayment -> ManaPayment
  p - q = p{paymentMana = paymentMana p - paymentMana q, paymentLife = paymentLife p - paymentLife q}

  (*) :: ManaPayment -> ManaPayment -> ManaPayment
  p * q = p{paymentMana = paymentMana p * paymentMana q, paymentLife = paymentLife p * paymentLife q}

  abs :: ManaPayment -> ManaPayment
  abs p = p{paymentMana = abs (paymentMana p), paymentLife = abs (paymentLife p)}

  signum :: ManaPayment -> ManaPayment
  signum p = p{paymentMana = signum (paymentMana p), paymentLife = signum (paymentLife p)}

  negate :: ManaPayment -> ManaPayment
  negate p = p{paymentMana = negate (paymentMana p), paymentLife = negate (paymentLife p)}

  fromInteger :: Integer -> ManaPayment
  fromInteger n = mempty{paymentMana = fromInteger n}

instance Num (Damage 'NoVar) where
  (+) :: Damage 'NoVar -> Damage 'NoVar -> Damage 'NoVar
  (+) (Damage x) (Damage y) = Damage $ x + y

  (-) :: Damage 'NoVar -> Damage 'NoVar -> Damage 'NoVar
  (-) (Damage x) (Damage y) = Damage $ x - y

  (*) :: Damage 'NoVar -> Damage 'NoVar -> Damage 'NoVar
  (*) (Damage x) (Damage y) = Damage $ x * y

  abs :: Damage 'NoVar -> Damage 'NoVar
  abs (Damage x) = Damage $ abs x

  signum :: Damage 'NoVar -> Damage 'NoVar
  signum (Damage x) = Damage $ signum x

  negate :: Damage 'NoVar -> Damage 'NoVar
  negate (Damage x) = Damage $ negate x

  fromInteger :: Integer -> Damage 'NoVar
  fromInteger = Damage . fromInteger

instance Num Life where
  (+) :: Life -> Life -> Life
  Life a + Life b = Life (a + b)

  (-) :: Life -> Life -> Life
  Life a - Life b = Life (a - b)

  (*) :: Life -> Life -> Life
  Life a * Life b = Life (a * b)

  abs :: Life -> Life
  abs (Life a) = Life (abs a)

  signum :: Life -> Life
  signum (Life a) = Life (signum a)

  fromInteger :: Integer -> Life
  fromInteger a = Life (fromInteger a)

instance Num (Mana 'NoVar snow mt) where
  (+) :: Mana 'NoVar snow mt -> Mana 'NoVar snow mt -> Mana 'NoVar snow mt
  (+) (Mana x) (Mana y) = Mana $ x + y

  (-) :: Mana 'NoVar snow mt -> Mana 'NoVar snow mt -> Mana 'NoVar snow mt
  (-) (Mana x) (Mana y) = Mana $ x - y

  (*) :: Mana 'NoVar snow mt -> Mana 'NoVar snow mt -> Mana 'NoVar snow mt
  (*) (Mana x) (Mana y) = Mana $ x * y

  abs :: Mana 'NoVar snow mt -> Mana 'NoVar snow mt
  abs (Mana x) = Mana $ abs x

  signum :: Mana 'NoVar snow mt -> Mana 'NoVar snow mt
  signum (Mana x) = Mana $ signum x

  negate :: Mana 'NoVar snow mt -> Mana 'NoVar snow mt
  negate (Mana x) = Mana $ negate x

  fromInteger :: Integer -> Mana 'NoVar snow mt
  fromInteger = Mana . fromInteger

instance (IsSnow snow) => Num (ManaPool snow) where
  (+) :: (IsSnow snow) => ManaPool snow -> ManaPool snow -> ManaPool snow
  (+) = mapManaPool2 (+)

  (-) :: (IsSnow snow) => ManaPool snow -> ManaPool snow -> ManaPool snow
  (-) = mapManaPool2 (-)

  (*) :: (IsSnow snow) => ManaPool snow -> ManaPool snow -> ManaPool snow
  (*) = mapManaPool2 (*)

  abs :: (IsSnow snow) => ManaPool snow -> ManaPool snow
  abs = mapManaPool abs

  signum :: (IsSnow snow) => ManaPool snow -> ManaPool snow
  signum = mapManaPool signum

  negate :: (IsSnow snow) => ManaPool snow -> ManaPool snow
  negate = mapManaPool negate

  fromInteger :: (IsSnow snow) => Integer -> ManaPool snow
  fromInteger = \case
    0 -> mempty
    -- NOTE: I could support a sound `fromInteger` definition, but it prolly isn't worth
    -- it because it would have to support only Colorless or all the mana types together,
    -- neither of which is particularly desirable.
    _ -> error "(fromInteger :: ManaPool snow) only supports n=0"

instance Num (ManaCost 'NoVar) where
  (+) :: ManaCost 'NoVar -> ManaCost 'NoVar -> ManaCost 'NoVar
  (+) = mapManaCost2 (+)

  (-) :: ManaCost 'NoVar -> ManaCost 'NoVar -> ManaCost 'NoVar
  (-) = mapManaCost2 (-)

  (*) :: ManaCost 'NoVar -> ManaCost 'NoVar -> ManaCost 'NoVar
  (*) = mapManaCost2 (*)

  abs :: ManaCost 'NoVar -> ManaCost 'NoVar
  abs = mapManaCost abs

  signum :: ManaCost 'NoVar -> ManaCost 'NoVar
  signum = mapManaCost signum

  negate :: ManaCost 'NoVar -> ManaCost 'NoVar
  negate = mapManaCost negate

  fromInteger :: Integer -> ManaCost 'NoVar
  fromInteger x = ManaCost' 0 0 0 0 0 0 $ DynamicManaCost (fromInteger x) 0 mempty mempty

instance ForceVars (ManaCost var) (ManaCost 'NoVar) where
  forceVars :: ManaCost var -> ManaCost 'NoVar
  forceVars = mapManaCost forceVars

deriving instance Show (AnyElected s)

deriving instance Show Artifact

deriving instance Show Battle

deriving instance Show Creature

deriving instance Show (Elected s ot)

deriving instance Show Enchantment

deriving instance Show AnyEnchantmentType

deriving instance Show (GameResult m)

deriving instance Show Graveyard

deriving instance Show Hand

deriving instance Show Land

deriving instance Show Library

deriving instance Show ManaPayment

deriving instance Show Permanent

deriving instance Show PhaseStep

deriving instance Show Planeswalker

deriving instance Show Player

deriving instance Show QueryObjectResult

deriving instance Show (SomeOT Card ot)

deriving instance Show (SomeOT CardCharacteristic ot)

deriving instance Show (SomeOT (SomeZone Ability) ot)

deriving instance Show (SomeOT (SomeZone WithThisAbility) ot)

deriving instance Show (SomeOT Supertype ot)

deriving instance Show (SomeOT Token ot)

deriving instance Show (SomeTerm Card ot)

deriving instance Show (SomeTerm CardCharacteristic ot)

deriving instance Show (SomeTerm (SomeZone Ability) ot)

deriving instance Show (SomeTerm (SomeZone WithThisAbility) ot)

deriving instance Show (SomeTerm Supertype ot)

deriving instance Show (SomeTerm Token ot)

deriving instance Show (SomeZone Ability ot)

deriving instance Show Stack

deriving instance Show StackObject

instance Show Deck where
  show :: Deck -> String
  show = \case
    Deck cards -> "Deck" ++ show (map getCardName cards)

instance Show Sideboard where
  show :: Sideboard -> String
  show = \case
    Sideboard cards -> "Sideboard" ++ show (map getCardName cards)

instance Show (GameState m) where
  show :: GameState m -> String
  show st = DList.toList $ execWriter do
    tellLine "<GameState>"
    tellLine ""
    tellPrint ("startingPlayer", startingPlayer)
    tellPrint ("manaBurn", manaBurn)
    tellPrint ("allPlayerIds", allPlayerIds)
    tellLine ""
    tellPrint ("graveMapSize", Map.size graveMap)
    tellPrint ("graveMap", bulleted graveMap)
    tellLine ""
    tellPrint ("handMapSize", Map.size handMap)
    tellPrint ("handMap", bulleted handMap)
    tellLine ""
    tellPrint ("libMapSize", libMapSize)
    tellLine ""
    tellPrint ("stackEntryTargetsMapSize", stackEntryTargetsMapSize)
    tellPrint ("stackEntryElectedMapSize", stackEntryElectedMapSize)
    tellPrint ("targetMapSize", targetMapSize)
    tellLine ""
    tellPrint ("nextDiscr", nextDiscr)
    tellPrint ("nextObjectId", nextObjectId)
    tellLine ""
    tellPrint ("turnOrder", Stream.take aliveCount turnOrder)
    tellPrint ("turn", turn)
    tellPrint ("phaseStep", phaseStep)
    tellPrint ("apnapOrder", Stream.take aliveCount apnapOrder)
    tellPrint ("priorityOrder", priorityOrder)
    tellLine ""
    tellPrint ("stack", bulleted stack)
    tellLine ""
    tellPrint ("allPlayers", bulleted allPlayers) --  TODO: beautify this
    tellLine ""
    tellPrint ("permMap", bulleted permMap) --  TODO: beautify this
    tellLine ""
    tell "</GameState>"
   where
    aliveCount = 2 -- hacky
    allPlayerIds = Map.keys playerMap
    allPlayers = Map.elems playerMap

    GameState
      { magicCurrentTurn = turn
      , magicFwd = _
      , magicGraveyardCards = (fmap getCardName -> graveMap)
      , magicHandCards = (fmap getCardName -> handMap)
      , magicLibraryCards = (Map.size -> libMapSize)
      , magicManaBurn = manaBurn
      , magicNextObjectDiscriminant = nextDiscr
      , magicNextObjectId = nextObjectId
      , magicPermanents = permMap
      , magicPhaseStep = phaseStep
      , magicPlayers = playerMap
      , magicPlayerOrderAPNAP = apnapOrder
      , magicPlayerOrderPriority = priorityOrder
      , magicPlayerOrderTurn = turnOrder
      , magicPrompt = _
      , magicStack = Stack stack
      , magicStackEntryTargetsMap = (Map.size -> stackEntryTargetsMapSize)
      , magicStackEntryElectedMap = (Map.size -> stackEntryElectedMapSize)
      , magicStartingPlayer = startingPlayer
      , magicTargetProperties = (Map.size -> targetMapSize)
      } = st

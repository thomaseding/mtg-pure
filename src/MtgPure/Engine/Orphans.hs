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
  ) where

import safe Control.Monad.Writer.Strict (Writer, execWriter, tell)
import safe qualified Data.DList as DList
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Stream as Stream
import safe MtgPure.Engine.Orphans.ZO ()
import safe MtgPure.Engine.State (
  AnyElected (..),
  Elected (..),
  GameResult (..),
  GameState (..),
 )
import safe MtgPure.Model.Artifact (Artifact (..))
import safe MtgPure.Model.CardName (HasCardName (getCardName))
import safe MtgPure.Model.Creature (Creature (..))
import safe MtgPure.Model.Deck (Deck (..))
import safe MtgPure.Model.Enchantment (AnyEnchantmentType (..), Enchantment (..))
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Land (Land (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Planeswalker (Planeswalker (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  Ability,
  Card,
  CardFacet,
  Some (..),
  SomeTerm (..),
  Token,
 )
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Sideboard (Sideboard (..))
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))

newtype Bulleted a = Bulleted a
  deriving (Eq, Ord)

instance Show a => Show (Bulleted a) where
  showsPrec n (Bulleted x) = ("\n  @@ " ++) . showsPrec n x

class AsBulleted a b | a -> b where
  bulleted :: a -> b

instance AsBulleted [a] [Bulleted a] where
  bulleted = map Bulleted

instance Ord k => AsBulleted (Map.Map k v) (Map.Map (Bulleted k) v) where
  bulleted = Map.mapKeys Bulleted

type DString = DList.DList Char

tellPrint :: Show a => a -> Writer DString ()
tellPrint s = tell $ DList.fromList (show s) <> "\n"

tellLine :: DString -> Writer DString ()
tellLine s = tell $ s <> "\n"

deriving instance Show (AnyElected pEffect)

deriving instance Show Artifact

deriving instance Show Creature

deriving instance Show (Elected pEffect ot)

deriving instance Show Enchantment

deriving instance Show AnyEnchantmentType

deriving instance Show (GameResult m)

deriving instance Show Graveyard

deriving instance Show Hand

deriving instance Show Land

deriving instance Show Library

deriving instance Show Permanent

deriving instance Show PhaseStep

deriving instance Show Planeswalker

deriving instance Show Player

deriving instance Show (Some Ability ot)

deriving instance Show (Some Card ot)

deriving instance Show (Some CardFacet ot)

deriving instance Show (Some Token ot)

deriving instance Show (SomeTerm Ability ot)

deriving instance Show (SomeTerm Card ot)

deriving instance Show (SomeTerm CardFacet ot)

deriving instance Show (SomeTerm Token ot)

deriving instance Show Stack

deriving instance Show StackObject

instance Show Deck where
  show = \case
    Deck cards -> "Deck" ++ show (map getCardName cards)

instance Show Sideboard where
  show = \case
    Sideboard cards -> "Sideboard" ++ show (map getCardName cards)

instance Show (GameState m) where
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

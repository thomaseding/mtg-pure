{-# LANGUAGE Unsafe #-}

-- | XXX: This module is generated by `Script.GenerateLoadAllModules`.
--
-- This module is used to load all modules in the project.
-- Useful for checking that all modules compile.
module LoadAllModules (
  ) where

-- import LoadAllModules () -- can't include self
import Ansi.AnsiString ()
import Ansi.Box ()
import Ansi.Compile ()
import Ansi.Math ()
import Ansi.Old ()
import Ansi.TrueColor.Debug ()
import Ansi.TrueColor.FileFormat ()
import Ansi.TrueColor.FindColoring ()
import Ansi.TrueColor.FindRendering ()
import Ansi.TrueColor.ImageToAnsi ()
import Ansi.TrueColor.RenderedTile ()
import Ansi.TrueColor.TileGrid ()
import Ansi.TrueColor.Types ()
import Ansi.TrueColor.VirtualChar ()
import Ansi.TrueColor.VirtualCharTile ()
import App.AnsiInspector ()
import App.CardGallery ()
import Control.Monad.Access ()
import Control.Monad.Util ()
import Data.Carousel ()
import Data.ConsIndex ()
import Data.Inst ()
import Data.Nat ()
import Data.Read.Sep ()
import Data.Read.Symbol ()
import Data.SafeJson ()
import Data.TupleN ()
import Data.Virtual ()
import Data.Zipper ()
import Demo.AnsiBox ()
import Demo.AnsiChoiceMenu ()
import Demo.AnsiColors ()
import Demo.AnsiMagicBoard ()
import Demo.SerializableMonadApi.ProofOfConcept ()
import Demo.SerializableMonadApi.Variable ()
import Demo.SerializableMonadApi.VariableMonad ()
import MtgPure ()
import MtgPure.AllCards ()
import MtgPure.Cards ()
import MtgPure.Client.Terminal ()
import MtgPure.Client.Terminal.CommandInput ()
import MtgPure.Client.Terminal.Fwd.Api ()
import MtgPure.Client.Terminal.Fwd.Impl ()
import MtgPure.Client.Terminal.Fwd.Type ()
import MtgPure.Client.Terminal.Monad ()
import MtgPure.Client.Terminal.PriorityAction ()
import MtgPure.Client.Terminal.Render ()
import MtgPure.Engine ()
import MtgPure.Engine.ActivateCast ()
import MtgPure.Engine.CaseOf ()
import MtgPure.Engine.Core ()
import MtgPure.Engine.Enact ()
import MtgPure.Engine.Fwd.Api ()
import MtgPure.Engine.Fwd.Impl ()
import MtgPure.Engine.Fwd.Type ()
import MtgPure.Engine.Legality ()
import MtgPure.Engine.Monad ()
import MtgPure.Engine.Orphans ()
import MtgPure.Engine.Orphans.ZO ()
import MtgPure.Engine.Pay ()
import MtgPure.Engine.PayMana ()
import MtgPure.Engine.PerformElections ()
import MtgPure.Engine.PlayGame ()
import MtgPure.Engine.PlayLand ()
import MtgPure.Engine.Priority ()
import MtgPure.Engine.Prompt ()
import MtgPure.Engine.Resolve ()
import MtgPure.Engine.Satisfies ()
import MtgPure.Engine.State ()
import MtgPure.Engine.StateBasedActions ()
import MtgPure.Engine.Turn ()
import MtgPure.Model ()
import MtgPure.Model.AbilityType ()
import MtgPure.Model.Artifact ()
import MtgPure.Model.ArtifactType ()
import MtgPure.Model.BasicLandType ()
import MtgPure.Model.Battlefield ()
import MtgPure.Model.CardName ()
import MtgPure.Model.CardSet ()
import MtgPure.Model.CardType ()
import MtgPure.Model.Color ()
import MtgPure.Model.ColorToManaType ()
import MtgPure.Model.Colors ()
import MtgPure.Model.ColorsLike ()
import MtgPure.Model.Combinators ()
import MtgPure.Model.Creature ()
import MtgPure.Model.CreatureType ()
import MtgPure.Model.Damage ()
import MtgPure.Model.Deck ()
import MtgPure.Model.EffectType ()
import MtgPure.Model.Enchantment ()
import MtgPure.Model.Graveyard ()
import MtgPure.Model.Hand ()
import MtgPure.Model.IsCardList ()
import MtgPure.Model.IsManaAbility ()
import MtgPure.Model.Land ()
import MtgPure.Model.LandType ()
import MtgPure.Model.Library ()
import MtgPure.Model.Life ()
import MtgPure.Model.Linear ()
import MtgPure.Model.Loyalty ()
import MtgPure.Model.Mana.CountMana ()
import MtgPure.Model.Mana.HasManaSymbol ()
import MtgPure.Model.Mana.Mana ()
import MtgPure.Model.Mana.ManaCost ()
import MtgPure.Model.Mana.ManaPool ()
import MtgPure.Model.Mana.ManaSymbol ()
import MtgPure.Model.Mana.ManaType ()
import MtgPure.Model.Mana.ManaTypeToColor ()
import MtgPure.Model.Mana.Snow ()
import MtgPure.Model.Mana.ToMana ()
import MtgPure.Model.Mana.ToManaCost ()
import MtgPure.Model.Mana.ToManaPool ()
import MtgPure.Model.Mulligan ()
import MtgPure.Model.Object.IndexOT ()
import MtgPure.Model.Object.IsObjectType ()
import MtgPure.Model.Object.LitOTN ()
import MtgPure.Model.Object.MapObjectN ()
import MtgPure.Model.Object.OTKN ()
import MtgPure.Model.Object.OTKN_ ()
import MtgPure.Model.Object.OTN ()
import MtgPure.Model.Object.OTNAliases ()
import MtgPure.Model.Object.OTN_ ()
import MtgPure.Model.Object.Object ()
import MtgPure.Model.Object.ObjectId ()
import MtgPure.Model.Object.ObjectN ()
import MtgPure.Model.Object.ObjectN_ ()
import MtgPure.Model.Object.ObjectType ()
import MtgPure.Model.Object.ObjectTypeN ()
import MtgPure.Model.Object.PromoteIdToObjectN ()
import MtgPure.Model.Object.SObjectType ()
import MtgPure.Model.Object.Singleton.Any ()
import MtgPure.Model.Object.Singleton.Card ()
import MtgPure.Model.Object.Singleton.NonCreatureCard ()
import MtgPure.Model.Object.Singleton.Permanent ()
import MtgPure.Model.Object.Singleton.Spell ()
import MtgPure.Model.Object.ToObjectN ()
import MtgPure.Model.Object.ToObjectN.Classes ()
import MtgPure.Model.Object.ToObjectN.CodeGen ()
import MtgPure.Model.Object.ToObjectN.Instances ()
import MtgPure.Model.Object.ViewObjectN ()
import MtgPure.Model.Object.VisitObjectN ()
import MtgPure.Model.Permanent ()
import MtgPure.Model.Phase ()
import MtgPure.Model.PhaseStep ()
import MtgPure.Model.Planeswalker ()
import MtgPure.Model.Player ()
import MtgPure.Model.Power ()
import MtgPure.Model.PrePost ()
import MtgPure.Model.PrettyType ()
import MtgPure.Model.Rarity ()
import MtgPure.Model.Recursive ()
import MtgPure.Model.Recursive.Ord ()
import MtgPure.Model.Recursive.Show ()
import MtgPure.Model.Recursive.Tree ()
import MtgPure.Model.Sideboard ()
import MtgPure.Model.Stack ()
import MtgPure.Model.Step ()
import MtgPure.Model.Supertype ()
import MtgPure.Model.TimePoint ()
import MtgPure.Model.Toughness ()
import MtgPure.Model.Variable ()
import MtgPure.Model.Zone ()
import MtgPure.Model.ZoneObject.Convert ()
import MtgPure.Model.ZoneObject.ZoneObject ()
import Script.GenerateGalleryBatched.Main ()
import Script.GenerateGallerySingle.Args ()
import Script.GenerateGallerySingle.Main ()
import Script.GenerateLoadAllModules ()
import Script.MtgPureConfig ()
import Script.ScryfallDownloader ()
import System.Keyboard ()
import Test.Engine.Unit.MagicCont ()
import Test.Engine.Unit.PayMana ()
import Test.Game.Hybrid ()
import Test.Game.ManaAbility ()
import Test.Game.RagingGoblin ()
import Test.Game.Shock ()
import Test.Game.StoneRain ()
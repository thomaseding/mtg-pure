{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PlayLand (
  playLandImpl,
  askPlayLandImpl,
) where

import safe Control.Exception (assert)
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Trans.Except (throwE)
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Wrap (
  gainPriority,
  getActivePlayer,
  getHasPriority,
  getPlayer,
  newObjectId,
  rewindIllegal,
  setPermanent,
  setPlayer,
 )
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  runMagicCont,
 )
import safe MtgPure.Engine.Prompt (
  InternalLogicError (..),
  InvalidPlayLand (..),
  PlayLand (..),
  Prompt' (..),
 )
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  MagicCont,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.IsCardList (containsCard, removeCard)
import safe MtgPure.Model.Object (Object, ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTLand)
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Card (..))
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..))
import safe MtgPure.Model.ZoneObject (ZO)
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)

data PlayLandReqs = PlayLandReqs
  { playLandReqs_hasPriority :: Bool
  , playLandReqs_isActive :: Bool
  , playLandReqs_isMainPhase :: Bool
  , playLandReqs_stackEmpty :: Bool
  , playLandReqs_atMaxLands :: Bool
  }
  deriving (Eq, Ord, Show, Typeable)

-- Unfortunately pattern synonyms won't contribute to exhaustiveness checking.
pattern PlayLandReqs_Satisfied :: PlayLandReqs
pattern PlayLandReqs_Satisfied =
  PlayLandReqs
    { playLandReqs_hasPriority = True
    , playLandReqs_isActive = True
    , playLandReqs_isMainPhase = True
    , playLandReqs_stackEmpty = True
    , playLandReqs_atMaxLands = False
    }

getPlayLandReqs :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m PlayLandReqs
getPlayLandReqs oPlayer = do
  st <- internalFromPrivate $ fromRO get
  player <- fromRO $ getPlayer oPlayer
  let landsPlayed = playerLandsPlayedThisTurn player
      maxLands = 1
      isMainPhase = magicPhaseStep st `elem` [PSPreCombatMainPhase, PSPostCombatMainPhase]
      Stack stack = magicStack st
  oActive <- fromPublic getActivePlayer
  hasPriority <- fromPublic $ getHasPriority oPlayer
  pure
    PlayLandReqs
      { playLandReqs_hasPriority = hasPriority -- (116.2a)
      , playLandReqs_isActive = oPlayer == oActive -- (116.2a)
      , playLandReqs_isMainPhase = isMainPhase -- (116.2a)
      , playLandReqs_stackEmpty = null stack -- (116.2a)
      , playLandReqs_atMaxLands = landsPlayed >= maxLands -- (305.2)
      }

askPlayLandImpl :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askPlayLandImpl oPlayer = do
  reqs <- lift $ fromRO $ getPlayLandReqs oPlayer
  case reqs of
    PlayLandReqs_Satisfied -> do
      st <- lift $ fromRO get
      let opaque = mkOpaqueGameState st
          prompt = magicPrompt st
      mSpecial <- lift $ lift $ promptPlayLand prompt opaque oPlayer
      case mSpecial of
        Nothing -> pure ()
        Just special -> do
          isLegal <- lift $ rewindIllegal $ playLandImpl oPlayer special
          throwE $ case isLegal of
            True -> gainPriority oPlayer -- (117.3c)
            False -> runMagicCont (either id id) $ askPlayLandImpl oPlayer
    _ -> pure ()

playLandImpl :: Monad m => Object 'OTPlayer -> PlayLand -> Magic 'Private 'RW m Legality
playLandImpl oPlayer (PlayLand oLand) = playLandZO oPlayer oLand

playLandZO ::
  forall m zone.
  (Monad m, IsZone zone) =>
  Object 'OTPlayer ->
  ZO zone OTLand ->
  Magic 'Private 'RW m Legality
playLandZO oPlayer oLand = do
  st <- internalFromPrivate $ fromRO get
  reqs <- fromRO $ getPlayLandReqs oPlayer
  player <- fromRO $ getPlayer oPlayer
  let opaque = mkOpaqueGameState st
      prompt = magicPrompt st
      hand = playerHand player
      --
      invalid :: (ZO zone OTLand -> InvalidPlayLand) -> Magic 'Private 'RW m Legality
      invalid ex = do
        lift $ exceptionInvalidPlayLand prompt opaque oPlayer $ ex oLand
        pure Illegal
      --
      success :: Card () -> Magic 'Private 'RW m ()
      success card = do
        setPlayer
          oPlayer
          player
            { playerLandsPlayedThisTurn = playerLandsPlayedThisTurn player + 1
            , playerHand = case removeCard card hand of
                Nothing -> assert False hand
                Just hand' -> hand'
            }
        i <- newObjectId
        let oLand' = zo0ToPermanent $ toZO0 i
            perm = case cardToPermanent oPlayer card of
              Nothing -> error $ show ExpectedCardToBeAPermanentCard
              Just perm' -> perm'
        setPermanent oLand' perm
  case reqs of
    PlayLandReqs{playLandReqs_hasPriority = False} -> invalid PlayLand_NoPriority
    PlayLandReqs{playLandReqs_isActive = False} -> invalid PlayLand_NotActive
    PlayLandReqs{playLandReqs_isMainPhase = False} -> invalid PlayLand_NotMainPhase
    PlayLandReqs{playLandReqs_stackEmpty = False} -> invalid PlayLand_StackNonEmpty
    PlayLandReqs{playLandReqs_atMaxLands = True} -> invalid PlayLand_AtMaxLands
    PlayLandReqs
      { playLandReqs_hasPriority = True
      , playLandReqs_isActive = True
      , playLandReqs_isMainPhase = True
      , playLandReqs_stackEmpty = True
      , playLandReqs_atMaxLands = False
      } -> assert (reqs == PlayLandReqs_Satisfied) $ case singZone @zone of
        SZBattlefield -> invalid PlayLand_CannotPlayFromZone
        SZExile -> invalid PlayLand_CannotPlayFromZone
        SZLibrary -> invalid PlayLand_CannotPlayFromZone
        SZStack -> invalid PlayLand_CannotPlayFromZone
        SZGraveyard -> invalid PlayLand_CannotPlayFromZone -- TODO: [Crucible of Worlds]
        SZHand -> do
          mCard <- fromRO $ gets $ Map.lookup (toZO0 oLand) . magicHandCards
          case mCard of
            Nothing -> invalid PlayLand_NotInZone
            Just card -> do
              case containsCard card hand of
                False -> invalid PlayLand_NotOwned
                True -> case card of
                  ArtifactCard{} -> invalid PlayLand_NotALand
                  ArtifactCreatureCard{} -> invalid PlayLand_NotALand
                  CreatureCard{} -> invalid PlayLand_NotALand
                  EnchantmentCard{} -> invalid PlayLand_NotALand
                  EnchantmentCreatureCard{} -> invalid PlayLand_NotALand
                  InstantCard{} -> invalid PlayLand_NotALand
                  PlaneswalkerCard{} -> invalid PlayLand_NotALand
                  SorceryCard{} -> invalid PlayLand_NotALand
                  --
                  LandCard{} -> success card >> pure Legal

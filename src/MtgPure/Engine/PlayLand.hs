{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PlayLand (
  playLand,
) where

import safe Control.Exception (assert)
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Data.Functor ((<&>))
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Api (
  getActivePlayer,
  getHasPriority,
  getPlayer,
  putOntoBattlefield,
 )
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromRO,
  get,
  gets,
  internalFromPrivate,
 )
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (
  InvalidPlayLand (..),
  PlayLand,
  Prompt' (..),
  SpecialAction (PlayLand),
 )
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.IsCardList (containsCard)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTNAliases (OTNLand)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Zone (IsZone (..), SingZone (..))
import safe MtgPure.Model.ZoneObject.Convert (asCard, toZO0)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

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
getPlayLandReqs oPlayer = logCall 'getPlayLandReqs do
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

playLand :: Monad m => Object 'OTPlayer -> SpecialAction PlayLand -> Magic 'Private 'RW m Legality
playLand oPlayer (PlayLand oLand) = logCall 'playLand do
  playLandZO oPlayer oLand

playLandZO ::
  forall m zone.
  (Monad m, IsZone zone) =>
  Object 'OTPlayer ->
  ZO zone OTNLand ->
  Magic 'Private 'RW m Legality
playLandZO oPlayer zoLand = logCall 'playLandZO do
  reqs <- fromRO $ getPlayLandReqs oPlayer
  player <- fromRO $ getPlayer oPlayer
  let hand = playerHand player
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
      } -> assert (reqs == PlayLandReqs_Satisfied) case singZone @zone of
        SingZBattlefield -> invalid PlayLand_CannotPlayFromZone
        SingZExile -> invalid PlayLand_CannotPlayFromZone
        SingZLibrary -> invalid PlayLand_CannotPlayFromZone
        SingZStack -> invalid PlayLand_CannotPlayFromZone
        SingZGraveyard -> invalid PlayLand_CannotPlayFromZone -- TODO: [Crucible of Worlds]
        SingZHand -> do
          mCard <- fromRO $ gets $ Map.lookup (toZO0 zoLand) . magicHandCards
          case mCard of
            Nothing -> invalid PlayLand_NotInZone
            Just{} -> do
              case containsCard (asCard zoLand) hand of
                False -> invalid PlayLand_NotOwned
                True ->
                  putOntoBattlefield oPlayer zoLand <&> \case
                    Nothing -> Illegal
                    Just{} -> Legal
 where
  invalid :: (ZO zone OTNLand -> InvalidPlayLand) -> Magic 'Private 'RW m Legality
  invalid ex = do
    st <- internalFromPrivate $ fromRO get
    let opaque = mkOpaqueGameState st
        prompt = magicPrompt st
    lift $ exceptionInvalidPlayLand prompt opaque oPlayer $ ex zoLand
    pure Illegal

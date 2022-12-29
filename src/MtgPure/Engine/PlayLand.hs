{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PlayLand (
  askPlayLand,
  playLand,
) where

import safe Control.Exception (assert)
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (Attempt, Attempt' (..))
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Api (
  gainPriority,
  getActivePlayer,
  getHasPriority,
  getPlayer,
  modifyPlayer,
  newObjectId,
  removeHandCard,
  rewindIllegal,
  setPermanent,
 )
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  liftCont,
  magicCont,
 )
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (
  InternalLogicError (..),
  InvalidPlayLand (..),
  Play (..),
  Prompt' (..),
 )
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  MagicCont,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.IsCardList (containsCard)
import safe MtgPure.Model.Object.OTKind (OTLand)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  AnyCard (..),
  Card (..),
  CardFacet,
  IsSpecificCard,
  YourCard (..),
 )
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..))
import safe MtgPure.Model.ZoneObject.Convert (asCard, oToZO1, toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO, ZOPlayer)

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

askPlayLand :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askPlayLand = logCall 'askPlayLand $ askPlayLand' $ Attempt 0

askPlayLand' :: Monad m => Attempt -> Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askPlayLand' attempt oPlayer = logCall 'askPlayLand' do
  reqs <- liftCont $ fromRO $ getPlayLandReqs oPlayer
  case reqs of
    PlayLandReqs_Satisfied -> do
      st <- liftCont $ fromRO get
      let opaque = mkOpaqueGameState st
          prompt = magicPrompt st
      mSpecial <- liftCont $ lift $ promptPlayLand prompt attempt opaque oPlayer
      case mSpecial of
        Nothing -> pure ()
        Just special -> do
          isLegal <- liftCont $ rewindIllegal $ playLand oPlayer special
          case isLegal of
            True -> magicCont do
              gainPriority oPlayer -- (117.3c)
            False -> do
              askPlayLand' ((1 +) <$> attempt) oPlayer
    _ -> pure ()

playLand :: Monad m => Object 'OTPlayer -> Play OTLand -> Magic 'Private 'RW m Legality
playLand oPlayer (PlayLand oLand) = logCall 'playLand do
  playLandZO oPlayer oLand

playLandZO ::
  forall m zone.
  (Monad m, IsZone zone) =>
  Object 'OTPlayer ->
  ZO zone OTLand ->
  Magic 'Private 'RW m Legality
playLandZO oPlayer zoLand = logCall 'playLandZO do
  let logCall' s = logCall ('playLandZO, s :: String)
  st <- internalFromPrivate $ fromRO get
  reqs <- fromRO $ getPlayLandReqs oPlayer
  player <- fromRO $ getPlayer oPlayer
  let opaque = mkOpaqueGameState st
      prompt = magicPrompt st
      hand = playerHand player
      zoPlayer = oToZO1 oPlayer
      --
      invalid :: (ZO zone OTLand -> InvalidPlayLand) -> Magic 'Private 'RW m Legality
      invalid ex = do
        lift $ exceptionInvalidPlayLand prompt opaque oPlayer $ ex zoLand
        pure Illegal
      --

      goCard :: forall ot. IsSpecificCard ot => Card ot -> Magic 'Private 'RW m Legality
      goCard card@(Card _name yourCard) = logCall' "goCard" case yourCard of
        YourArtifact{} -> invalid PlayLand_NotALand
        YourArtifactCreature{} -> invalid PlayLand_NotALand
        YourCreature{} -> invalid PlayLand_NotALand
        YourEnchantment{} -> invalid PlayLand_NotALand
        YourEnchantmentCreature{} -> invalid PlayLand_NotALand
        YourInstant{} -> invalid PlayLand_NotALand
        YourPlaneswalker{} -> invalid PlayLand_NotALand
        YourSorcery{} -> invalid PlayLand_NotALand
        --
        YourLand cont -> goPlayerToFacet cont
        YourArtifactLand cont -> goPlayerToFacet cont
       where
        goPlayerToFacet :: IsSpecificCard ot => (ZOPlayer -> CardFacet ot) -> Magic 'Private 'RW m Legality
        goPlayerToFacet playerToFacet = logCall' "goPlayerToFacet" do
          let facet = playerToFacet zoPlayer
          goFacet facet

        goFacet :: CardFacet ot -> Magic 'Private 'RW m Legality
        goFacet facet = logCall' "goFacet" do
          () <- case singZone @zone of
            SZBattlefield -> error $ show CantHappenByConstruction
            SZExile -> error $ show CantHappenByConstruction
            SZLibrary -> error $ show CantHappenByConstruction
            SZStack -> error $ show CantHappenByConstruction
            SZGraveyard -> undefined -- TODO: [Crucible of Worlds]
            SZHand -> do
              removeHandCard oPlayer (asCard zoLand) >>= \case
                Nothing -> error $ show ObjectIdExistsAndAlsoDoesNotExist
                Just{} -> pure ()
          modifyPlayer oPlayer \p -> p{playerLandsPlayedThisTurn = playerLandsPlayedThisTurn p + 1}
          i <- newObjectId
          let oLand' = zo0ToPermanent $ toZO0 i
              perm = case cardToPermanent oPlayer card facet of
                Nothing -> error $ show ExpectedCardToBeAPermanentCard
                Just perm' -> perm'
          setPermanent oLand' $ Just perm
          lift $ promptDebugMessage prompt "successfully played land"
          pure Legal

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
        SZBattlefield -> invalid PlayLand_CannotPlayFromZone
        SZExile -> invalid PlayLand_CannotPlayFromZone
        SZLibrary -> invalid PlayLand_CannotPlayFromZone
        SZStack -> invalid PlayLand_CannotPlayFromZone
        SZGraveyard -> invalid PlayLand_CannotPlayFromZone -- TODO: [Crucible of Worlds]
        SZHand -> do
          mCard <- fromRO $ gets $ Map.lookup (toZO0 zoLand) . magicHandCards
          case mCard of
            Nothing -> invalid PlayLand_NotInZone
            Just anyCard -> do
              case containsCard (asCard zoLand) hand of
                False -> invalid PlayLand_NotOwned
                True -> case anyCard of
                  AnyCard card -> case card of
                    Card{} -> goCard card

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
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Api (
  getActivePlayer,
  getHasPriority,
  getPlayer,
  modifyPlayer,
  newObjectId,
  performElections,
  removeHandCard,
  setPermanent,
 )
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  modify,
 )
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (
  InternalLogicError (..),
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
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.IsCardList (containsCard)
import safe MtgPure.Model.Object.OTNAliases (OTNLand)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  AnyCard (..),
  Card (..),
  CardCharacteristic (..),
  CardSpec,
  Elect,
  IsSpecificCard,
 )
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..))
import safe MtgPure.Model.ZoneObject.Convert (asCard, toZO0, zo0ToPermanent)
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

viewLand :: CardCharacteristic ot -> Maybe (CardSpec ot)
viewLand character = case character of
  ArtifactLandCharacteristic{} -> Just $ artifactLand_spec character
  LandCharacteristic{} -> Just $ land_spec character
  --
  ArtifactCharacteristic{} -> Nothing
  ArtifactCreatureCharacteristic{} -> Nothing
  CreatureCharacteristic{} -> Nothing
  EnchantmentCharacteristic{} -> Nothing
  EnchantmentCreatureCharacteristic{} -> Nothing
  InstantCharacteristic{} -> Nothing
  PlaneswalkerCharacteristic{} -> Nothing
  SorceryCharacteristic{} -> Nothing

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
                  AnyCard1 card -> goCard anyCard card
                  AnyCard2 card -> goCard anyCard card
 where
  logCall' s = logCall ('playLandZO, s :: String)

  invalid :: (ZO zone OTNLand -> InvalidPlayLand) -> Magic 'Private 'RW m Legality
  invalid ex = do
    st <- internalFromPrivate $ fromRO get
    let opaque = mkOpaqueGameState st
        prompt = magicPrompt st
    lift $ exceptionInvalidPlayLand prompt opaque oPlayer $ ex zoLand
    pure Illegal

  goCard :: AnyCard -> Card ot -> Magic 'Private 'RW m Legality
  goCard anyCard card = case card of
    Card _name elect -> goElectIntrinsic anyCard elect
    _ -> undefined

  goElectIntrinsic ::
    IsSpecificCard ot =>
    AnyCard ->
    Elect 'IntrinsicStage (CardCharacteristic ot) ot ->
    Magic 'Private 'RW m Legality
  goElectIntrinsic anyCard electIntrinsic = do
    i <- newObjectId
    -- Yes, lands don't use the stack, but we make a faux stack object anyway
    -- so elections like `Your` work for free. If this ends up being too hokey
    -- the type of `performElections` would need to be changed. Prolly would
    -- change the `ZO 'ZStack OT0` to be the result of a type family for the
    -- non-intrinsic `ElectStage` cases and `ZOPlayer` for `IntrinsicStage`.
    let zoStack0 = toZO0 i
    modify \st' ->
      st'
        { magicControllerMap = Map.insert i oPlayer $ magicControllerMap st'
        , magicOwnerMap = Map.insert i oPlayer $ magicOwnerMap st'
        }
    mCharacteristic <- fromRO $ performElections zoStack0 (pure . Just) electIntrinsic
    case mCharacteristic of
      Nothing -> pure Illegal
      Just character -> do
        result <- goCharacteristic anyCard character
        modify \st' ->
          st'
            { magicControllerMap = Map.delete i $ magicControllerMap st'
            , magicOwnerMap = Map.delete i $ magicOwnerMap st'
            }
        pure result

  goCharacteristic :: AnyCard -> CardCharacteristic ot -> Magic 'Private 'RW m Legality
  goCharacteristic anyCard character = logCall' "goCharacteristic" case viewLand character of
    Nothing -> pure Illegal
    Just spec -> do
      () <- case singZone @zone of
        SZBattlefield -> error $ show CantHappenByConstruction
        SZExile -> error $ show CantHappenByConstruction
        SZLibrary -> error $ show CantHappenByConstruction
        SZStack -> error $ show CantHappenByConstruction
        SZGraveyard -> undefined -- TODO: [Crucible of Worlds]
        SZHand -> do
          removeHandCard oPlayer (asCard zoLand) >>= \case
            Nothing -> error $ show $ ObjectIdExistsAndAlsoDoesNotExist zoLand
            Just{} -> pure ()
      modifyPlayer oPlayer \p -> p{playerLandsPlayedThisTurn = playerLandsPlayedThisTurn p + 1}
      i <- newObjectId
      let oLand' = zo0ToPermanent $ toZO0 i
          perm = case cardToPermanent anyCard character spec of
            Nothing -> error $ show ExpectedCardToBeAPermanentCard
            Just perm' -> perm'
      modify \st' ->
        st'
          { magicControllerMap = Map.insert i oPlayer $ magicControllerMap st'
          , magicOwnerMap = Map.insert i oPlayer $ magicOwnerMap st'
          }
      setPermanent oLand' $ Just perm
      pure Legal

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Resolve (
  resolveTopOfStack,
  resolveManaAbility,
  resolveOneShot,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Api (
  enact,
  gainPriority,
  getActivePlayer,
  newObjectId,
  performElections,
  setPermanent,
 )
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (fromPublicRO, fromRO, get, gets, modify)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (EnactInfo, InternalLogicError (..))
import safe MtgPure.Engine.State (
  AnyElected (..),
  Elected (..),
  GameState (..),
  Magic,
  PendingReady (..),
  logCall,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (AnyCard (..), CardFacet, Effect (..), Elect (..))
import safe MtgPure.Model.Stack (Stack (..), stackObjectToZo0)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO)

resolveTopOfStack :: Monad m => Magic 'Private 'RW m ()
resolveTopOfStack = logCall 'resolveTopOfStack do
  Stack stack <- fromRO $ gets magicStack
  case stack of -- (117.4) (405.5)
    [] -> pure ()
    item : items -> do
      let zoStack = stackObjectToZo0 item
      modify \st -> st{magicStack = Stack items}
      resolveStackObject zoStack
      modify \st ->
        st
          { magicStackEntryTargetsMap = Map.delete zoStack $ magicStackEntryTargetsMap st
          , magicStackEntryElectedMap = Map.delete zoStack $ magicStackEntryElectedMap st
          }
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive

resolveStackObject :: Monad m => ZO 'ZStack OT0 -> Magic 'Private 'RW m ()
resolveStackObject zoStack = logCall 'resolveStackObject do
  st <- fromRO get
  case Map.lookup zoStack $ magicStackEntryElectedMap st of
    Nothing -> error $ show NotSureWhatThisEntails
    Just anyElected -> case anyElected of
      AnyElected elected -> resolveElected zoStack elected

resolveManaAbility :: (IsOTN ot, Monad m) => Elected 'Pre ot -> Magic 'Private 'RW m Legality
resolveManaAbility elected = logCall 'resolveManaAbility do
  let isManaAbility = True -- TODO
  case isManaAbility of
    False -> pure Illegal
    True -> do
      let zoStack = error $ show ManaAbilitiesDontHaveTargetsSoNoZoShouldBeNeeded
      resolveElected zoStack elected
      pure Legal

resolveElected :: forall ot m. (IsOTN ot, Monad m) => ZO 'ZStack OT0 -> Elected 'Pre ot -> Magic 'Private 'RW m ()
resolveElected zoStack elected = logCall 'resolveElected do
  case elected of
    ElectedActivatedAbility{} -> M.void $ resolveOneShot zoStack $ unPending $ electedActivatedAbility_effect elected
    ElectedSpell{} -> case electedSpell_effect elected of
      Just effect -> M.void $ resolveOneShot zoStack $ unPending effect
      Nothing ->
        let electedPerm =
              ElectedPermanent
                { electedPermanent_controller = electedSpell_controller elected
                , electedPermanent_card = electedSpell_card elected
                , electedPermanent_facet = electedSpell_facet elected
                }
         in resolvePermanent electedPerm

resolveOneShot :: Monad m => ZO 'ZStack OT0 -> Elect 'Post (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe EnactInfo)
resolveOneShot zoStack elect = logCall 'resolveOneShot do
  performElections zoStack goEffect elect
 where
  goEffect :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m (Maybe EnactInfo)
  goEffect = fmap Just . enact

data ElectedPermanent (ot :: Type) :: Type where
  ElectedPermanent ::
    { electedPermanent_controller :: Object 'OTPlayer
    , electedPermanent_card :: AnyCard -- Card ot
    , electedPermanent_facet :: CardFacet ot
    } ->
    ElectedPermanent ot
  deriving (Typeable)

resolvePermanent :: (IsOTN ot, Monad m) => ElectedPermanent ot -> Magic 'Private 'RW m ()
resolvePermanent elected = logCall 'resolvePermanent do
  i <- newObjectId
  let oPerm = zo0ToPermanent $ toZO0 i
      perm = case cardToPermanent oPlayer card facet of
        Nothing -> error $ show ExpectedCardToBeAPermanentCard
        Just perm' -> perm'
  setPermanent oPerm $ Just perm
 where
  ElectedPermanent
    { electedPermanent_controller = oPlayer
    , electedPermanent_card = card
    , electedPermanent_facet = facet
    } = elected

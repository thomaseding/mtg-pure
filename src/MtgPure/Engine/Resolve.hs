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
  resolveElected,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Functor ((<&>))
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd.Api (
  enact,
  gainPriority,
  getActivePlayer,
  newObjectId,
  ownerOf,
  performElections,
  pushGraveyardCard,
  setPermanent,
 )
import safe MtgPure.Engine.Monad (fromPublicRO, fromRO, get, gets, liftCont, magicCont, modify)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (
  AnyElected (..),
  Elected (..),
  EnactInfo (..),
  InternalLogicError (..),
  OwnedCard (..),
  PendingReady (..),
  ResolveElected (..),
 )
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  MagicCont,
  logCall,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (AnyCard (..), CardFacet, Effect (..), Elect (..))
import safe MtgPure.Model.Stack (Stack (..), stackObjectToZo0)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO)

resolveTopOfStack :: Monad m => MagicCont 'Private 'RW m () Void
resolveTopOfStack = logCall 'resolveTopOfStack do
  Stack stack <- liftCont $ fromRO $ gets magicStack
  case stack of -- (117.4) (405.5)
    [] -> magicCont $ pure () -- "if the stack is empty, the phase or step ends"
    item : items -> do
      let zoStack = stackObjectToZo0 item
      liftCont $ modify \st -> st{magicStack = Stack items}
      result <- liftCont $ resolveStackObject zoStack
      liftCont $ modify \st ->
        st
          { magicStackEntryTargetsMap = Map.delete zoStack $ magicStackEntryTargetsMap st
          , magicStackEntryElectedMap = Map.delete zoStack $ magicStackEntryElectedMap st
          , magicOwnershipMap = Map.delete (getObjectId zoStack) $ magicOwnershipMap st
          }
      case result of
        ResolvedEffect (enactInfo_endTheTurn -> True) -> endTheTurn
        _ -> do
          oActive <- liftCont $ fromPublicRO getActivePlayer
          magicCont $ gainPriority oActive

endTheTurn :: Monad m => MagicCont 'Private 'RW m () Void
endTheTurn = logCall 'endTheTurn do
  magicCont undefined -- TODO: (721.)

resolveStackObject :: Monad m => ZO 'ZStack OT0 -> Magic 'Private 'RW m ResolveElected
resolveStackObject zoStack = logCall 'resolveStackObject do
  st <- fromRO get
  case Map.lookup zoStack $ magicStackEntryElectedMap st of
    Nothing -> error $ show NotSureWhatThisEntails
    Just anyElected -> case anyElected of
      AnyElected elected -> resolveElected zoStack elected

resolveElected :: forall ot m. (IsOTN ot, Monad m) => ZO 'ZStack OT0 -> Elected 'Pre ot -> Magic 'Private 'RW m ResolveElected
resolveElected zoStack elected = logCall 'resolveElected do
  case elected of
    ElectedActivatedAbility{} -> do
      resolveOneShot zoStack Nothing (unPending $ electedActivatedAbility_effect elected) <&> \case
        Nothing -> Fizzled
        Just enactInfo -> ResolvedEffect enactInfo
    ElectedSpell{} -> do
      owner <- fromRO $ ownerOf zoStack
      let card = electedSpell_card elected
          ownedCard = Just $ OwnedCard owner card
      case electedSpell_effect elected of
        Just effect ->
          resolveOneShot zoStack ownedCard (unPending effect) <&> \case
            Nothing -> Fizzled
            Just enactInfo -> ResolvedEffect enactInfo
        Nothing -> do
          let electedPerm =
                ElectedPermanent
                  { electedPermanent_controller = electedSpell_controller elected
                  , electedPermanent_card = card
                  , electedPermanent_facet = electedSpell_facet elected
                  }
          resolvePermanent electedPerm
          pure PermanentResolved

-- | Returns `Nothing` iff fizzled.
resolveOneShot ::
  Monad m =>
  ZO 'ZStack OT0 ->
  Maybe OwnedCard ->
  Elect 'Post (Effect 'OneShot) ot ->
  Magic 'Private 'RW m (Maybe EnactInfo)
resolveOneShot zoStack mCard elect = logCall 'resolveOneShot do
  result <- performElections zoStack goEffect elect
  case mCard of
    Nothing -> pure ()
    Just (OwnedCard oPlayer card) -> M.void $ pushGraveyardCard oPlayer card
  pure result
 where
  goEffect :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m (Maybe EnactInfo)
  goEffect = fmap Just . enact

data ElectedPermanent (ot :: Type) :: Type where
  ElectedPermanent ::
    { electedPermanent_controller :: Object 'OTPlayer
    , electedPermanent_card :: AnyCard -- TODO: OwnedCard
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

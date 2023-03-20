{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Resolve (
  resolveTopOfStackCont,
  resolveTopOfStack,
  resolveElected,
  endTheTurn,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd.Api (
  bailGainPriority,
  enact,
  getActivePlayer,
  newObjectId,
  ownerOf,
  performResolveElections,
  pushGraveyardCard,
  setPermanent,
 )
import safe MtgPure.Engine.Monad (
  PriorityEnd,
  fromPublicRO,
  fromRO,
  get,
  gets,
  liftCont,
  magicContBail,
  modify,
 )
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (
  AnyElected (..),
  Elected (..),
  ElectionInput (..),
  InternalLogicError (..),
  OwnedCard (..),
  PendingReady (..),
  ResolveElected (..),
  SourceZO (..),
 )
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  MagicCont,
  logCall,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (getObjectId)
import safe MtgPure.Model.Permanent (cardToPermanent)
import safe MtgPure.Model.Recursive (
  AnyCard (..),
  CardCharacteristic,
  CardSpec,
  Effect (..),
  Elect (..),
 )
import safe MtgPure.Model.Stack (Stack (..), stackObjectToZo0)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO)

resolveTopOfStackCont :: Monad m => MagicCont 'Private 'RW PriorityEnd m Void
resolveTopOfStackCont = M.join $ logCall 'resolveTopOfStackCont do
  liftCont resolveTopOfStack >>= \case
    Nothing -> magicContBail $ pure $ Right () -- "if the stack is empty, the phase or step ends"
    Just result -> do
      let _ = result -- TODO: sniff events to do stuff like end the turn in cont monad
      oActive <- liftCont $ fromPublicRO getActivePlayer
      bailGainPriority oActive

resolveTopOfStack :: Monad m => Magic 'Private 'RW m (Maybe ResolveElected)
resolveTopOfStack = logCall 'resolveTopOfStack do
  Stack stack <- fromRO $ gets magicStack
  case stack of -- (117.4) (405.5)
    [] -> pure Nothing
    item : items -> do
      let zoStack = stackObjectToZo0 item
      modify \st -> st{magicStack = Stack items} -- This happens before resolving due to triggered stuff being put onto stack
      result <- resolveStackObject zoStack
      modify \st ->
        st
          { magicStackEntryTargetsMap = Map.delete zoStack $ magicStackEntryTargetsMap st
          , magicStackEntryElectedMap = Map.delete zoStack $ magicStackEntryElectedMap st
          , magicControllerMap = Map.delete (getObjectId zoStack) $ magicControllerMap st
          , magicOwnerMap = Map.delete (getObjectId zoStack) $ magicOwnerMap st
          }
      pure $ Just result

endTheTurn :: Monad m => MagicCont 'Private 'RW Void m Void
endTheTurn = logCall 'endTheTurn do
  magicContBail do
    undefined -- TODO: (721.)

resolveStackObject :: Monad m => ZO 'ZStack OT0 -> Magic 'Private 'RW m ResolveElected
resolveStackObject zoStack = logCall 'resolveStackObject do
  st <- fromRO get
  case Map.lookup zoStack $ magicStackEntryElectedMap st of
    Nothing -> error $ show NotSureWhatThisEntails
    Just anyElected -> case anyElected of
      AnyElected elected -> resolveElected zoStack elected

resolveElected :: forall ot m. (IsOTN ot, Monad m) => ZO 'ZStack OT0 -> Elected 'TargetStage ot -> Magic 'Private 'RW m ResolveElected
resolveElected zoStack elected = logCall 'resolveElected do
  case elected of
    ElectedActivatedAbility{} -> do
      resolveOneShot zoStack Nothing (unPending $ electedActivatedAbility_effect elected)
    ElectedSpell{} -> do
      owner <- fromRO $ ownerOf zoStack
      let card = electedSpell_card elected
          ownedCard = Just $ OwnedCard owner card
      case electedSpell_effect elected of
        Just effect -> do
          resolveOneShot zoStack ownedCard (unPending effect)
        Nothing -> do
          let electedPerm =
                ElectedPermanent
                  { electedPermanent_controller = electedSpell_controller elected
                  , electedPermanent_card = card
                  , electedPermanent_character = electedSpell_character elected
                  , electedPermanent_spec = electedSpell_spec elected
                  }
          resolvePermanent electedPerm
          pure PermanentResolved

resolveOneShot ::
  Monad m =>
  ZO 'ZStack OT0 ->
  -- | `Nothing` is for tokens
  Maybe OwnedCard ->
  Elect 'ResolveStage (Effect 'OneShot) ot ->
  Magic 'Private 'RW m ResolveElected
resolveOneShot zoStack mCard elect = logCall 'resolveOneShot do
  mResult <- performResolveElections (ResolveInput zoStack) goEffect elect
  case mCard of
    Nothing -> pure ()
    Just (OwnedCard oPlayer card) -> do
      M.void $ pushGraveyardCard oPlayer card
  modify \st ->
    st
      { magicControllerMap = Map.delete (getObjectId zoStack) $ magicControllerMap st
      , magicOwnerMap = Map.delete (getObjectId zoStack) $ magicOwnerMap st
      }
  pure case mResult of
    Just result -> result
    Nothing -> undefined -- Impossible? If so, may want the return type of performElections be parametrized by the result Pre/Post
 where
  goEffect :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m (Maybe ResolveElected)
  goEffect effect = do
    evs <- enact (Just $ SourceZO zoStack) effect
    pure $ Just $ ResolvedEffect evs

data ElectedPermanent (ot :: Type) :: Type where
  ElectedPermanent ::
    { electedPermanent_controller :: Object 'OTPlayer
    , electedPermanent_card :: AnyCard -- TODO: OwnedCard
    , electedPermanent_character :: CardCharacteristic ot
    , electedPermanent_spec :: CardSpec ot
    } ->
    ElectedPermanent ot
  deriving (Typeable)

resolvePermanent :: (IsOTN ot, Monad m) => ElectedPermanent ot -> Magic 'Private 'RW m ()
resolvePermanent elected = logCall 'resolvePermanent do
  i <- newObjectId
  let oPerm = zo0ToPermanent $ toZO0 i
      perm = case cardToPermanent card character spec of
        Nothing -> error $ show ExpectedCardToBeAPermanentCard
        Just perm' -> perm'
  modify \st ->
    st
      { magicControllerMap = Map.insert i oPlayer $ magicControllerMap st
      , magicOwnerMap = Map.insert i oPlayer $ magicOwnerMap st
      }
  setPermanent oPerm $ Just perm
 where
  ElectedPermanent
    { electedPermanent_controller = oPlayer
    , electedPermanent_card = card
    , electedPermanent_character = character
    , electedPermanent_spec = spec
    } = elected

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
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Data.Map.Strict as Map
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd.Api (
  enact,
  gainPriority,
  getActivePlayer,
  performElections,
 )
import safe MtgPure.Engine.Monad (fromPublicRO, fromRO, get, gets, modify)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (InternalLogicError (..))
import safe MtgPure.Engine.State (
  AnyElected (..),
  Elected,
  GameState (..),
  Magic,
  PendingReady (..),
  StackEntry (..),
  electedObject_effect,
  logCall,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (Effect (..), Elect (..))
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

resolveTopOfStack :: Monad m => Magic 'Private 'RW m ()
resolveTopOfStack = logCall 'resolveTopOfStack do
  Stack stack <- fromRO $ gets magicStack
  case stack of -- (117.4) (405.5)
    [] -> pure ()
    item : items -> do
      modify $ \st -> st{magicStack = Stack items}
      resolveStackObject item
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive

resolveStackObject :: forall m. Monad m => StackObject -> Magic 'Private 'RW m ()
resolveStackObject = logCall 'resolveStackObject \case
  StackAbility zoAbility -> resolve $ toZO0 zoAbility
  StackSpell zoSpell -> resolve $ toZO0 zoSpell
 where
  resolve :: ZO 'ZStack OT0 -> Magic 'Private 'RW m ()
  resolve zoStack = do
    let go :: AnyElected 'Pre -> Magic 'Private 'RW m ()
        go (AnyElected elected) = resolveElected zoStack elected
    st <- fromRO get
    case Map.lookup zoStack $ magicStackEntryMap st of
      Nothing -> error $ show NotSureWhatThisEntails
      Just entry -> go $ stackEntryElected entry

resolveElected ::
  forall ot m.
  Monad m =>
  ZO 'ZStack OT0 ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m ()
resolveElected zoStack elected = logCall 'resolveElected do
  let goElectEffect :: Elect 'Post (Effect 'OneShot) ot -> Magic 'Private 'RW m ()
      goElectEffect = M.void . performElections zoStack goEffect

      goEffect :: Effect 'OneShot -> Magic 'Private 'RW m (Maybe Void)
      goEffect = fmap (const Nothing) . enact
  goElectEffect $ unPending $ electedObject_effect elected
  pure () -- TODO: GC stack stuff

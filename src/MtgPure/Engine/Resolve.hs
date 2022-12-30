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
  electedObject_effect,
  logCall,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (Effect (..), Elect (..))
import safe MtgPure.Model.Stack (Stack (..), stackObjectToZo0)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

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

resolveStackObject :: forall m. Monad m => ZO 'ZStack OT0 -> Magic 'Private 'RW m ()
resolveStackObject zoStack = logCall 'resolveStackObject do
  st <- fromRO get
  case Map.lookup zoStack $ magicStackEntryElectedMap st of
    Nothing -> error $ show NotSureWhatThisEntails
    Just anyElected -> case anyElected of
      AnyElected elected -> resolveElected zoStack elected

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

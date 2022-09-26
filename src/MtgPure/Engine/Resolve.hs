{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

module MtgPure.Engine.Resolve (
  resolveTopOfStackImpl,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Util (AndLike (..))
import safe qualified Data.Map.Strict as Map
import safe Data.Void (Void)
import safe MtgPure.Engine.Fwd.Wrap (enact, gainPriority, getActivePlayer, performElections)
import safe MtgPure.Engine.Monad (fromPublicRO, fromRO, get, gets, modify)
import safe MtgPure.Engine.Prompt (InternalLogicError (..))
import safe MtgPure.Engine.State (
  AnyElected (..),
  Elected,
  GameState (..),
  Magic,
  PendingReady (..),
  StackEntry (..),
  electedObject_effect,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTActivatedOrTriggeredAbility, OTSpell)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (Effect (..), Elect (..))
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Tribal (IsMaybeTribal, IsTribal, MaybeTribalToOT)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (IsOT, ZO)
import safe MtgPure.Model.ZoneObject.Convert (toZO0)

resolveTopOfStackImpl :: Monad m => Magic 'Private 'RW m ()
resolveTopOfStackImpl = do
  Stack stack <- fromRO $ gets magicStack
  case stack of -- (117.4) (405.5)
    [] -> pure ()
    item : items -> do
      modify $ \st -> st{magicStack = Stack items}
      resolveStackObject item
      oActive <- fromPublicRO getActivePlayer
      gainPriority oActive

resolveStackObject :: forall m. Monad m => StackObject -> Magic 'Private 'RW m ()
resolveStackObject = \case
  StackAbility zoAbility -> resolveAbility zoAbility
  StackSpell zoSpell -> resolveSpell zoSpell
 where
  resolveAbility :: ZO 'ZStack OTActivatedOrTriggeredAbility -> Magic 'Private 'RW m ()
  resolveAbility zoAbility = do
    let go :: AnyElected 'Post 'Pre 'Nothing -> Magic 'Private 'RW m ()
        go (AnyElected elected) = resolveElected zoAbility elected
    st <- fromRO get
    case Map.lookup (toZO0 zoAbility) $ magicStackEntryAbilityMap st of
      Nothing -> error $ show NotSureWhatThisEntails
      Just entry -> go $ stackEntryElected entry

  resolveSpell :: ZO 'ZStack OTSpell -> Magic 'Private 'RW m ()
  resolveSpell zoSpell = do
    let go :: IsTribal tribal => AnyElected 'Post 'Pre ( 'Just tribal) -> Magic 'Private 'RW m ()
        go (AnyElected elected) = resolveElected zoSpell elected
    st <- fromRO get
    case Map.lookup (toZO0 zoSpell) $ magicStackEntryNonTribalMap st of
      Nothing -> case Map.lookup (toZO0 zoSpell) $ magicStackEntryTribalMap st of
        Nothing -> error $ show NotSureWhatThisEntails
        Just entry -> go $ stackEntryElected entry
      Just entry -> go $ stackEntryElected entry

resolveElected ::
  forall mTribal ot m.
  (Monad m, IsMaybeTribal mTribal, IsOT (MaybeTribalToOT mTribal)) =>
  ZO 'ZStack (MaybeTribalToOT mTribal) ->
  Elected 'Post 'Pre mTribal ot ->
  Magic 'Private 'RW m ()
resolveElected zoStack elected = do
  let goElectEffect :: Monad m => Elect 'Post (Effect 'OneShot) ot -> Magic 'Private 'RW m ()
      goElectEffect = M.void . performElections @mTribal andM (toZO0 zoStack) goEffect

      goEffect :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m (Maybe Void)
      goEffect = fmap (const Nothing) . enact
  goElectEffect $ unPending $ electedObject_effect elected
  pure () -- TODO: GC stack stuff

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

module MtgPure.Engine.Satisfies (
  satisfiesImpl,
  zosSatisfyingImpl,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Functor ((<&>))
import safe MtgPure.Engine.Fwd.Wrap (allZOs, findPermanent, satisfies)
import safe MtgPure.Engine.State (Magic)
import safe MtgPure.Model.ObjectId (GetObjectId (..))
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Recursive (Requirement (..))
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Zone (IsZone (..), SZone (SZBattlefield))
import safe MtgPure.Model.ZoneObject (IsZO, OPlayer, ZO)
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)

zosSatisfyingImpl :: (Monad m, IsZO zone ot) => Requirement zone ot -> Magic 'Private 'RO m [ZO zone ot]
zosSatisfyingImpl req = allZOs >>= M.filterM (`satisfies` req)

satisfiesImpl ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
satisfiesImpl zo = \case
  ControlledBy oPlayer -> controlledBy' zo oPlayer
  Is _wAny zo' -> is' zo zo'
  Not req -> not' zo req
  RAnd reqs -> rAnd' zo reqs
  ROr reqs -> rOr' zo reqs
  _ -> undefined

controlledBy' ::
  forall m zone ot.
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  OPlayer ->
  Magic 'Private 'RO m Bool
controlledBy' oAny oPlayer = case singZone @zone of
  SZBattlefield ->
    findPermanent (zo0ToPermanent $ toZO0 oAny) <&> \case
      Nothing -> getObjectId oAny == getObjectId oPlayer -- TODO: [Mindslaver]
      Just perm -> getObjectId (permanentController perm) == getObjectId oPlayer
  _ -> undefined

is' :: (Monad m, IsZO zone ot) => ZO zone ot -> ZO zone ot -> Magic 'Private 'RO m Bool
is' zo zo' = pure $ getObjectId zo == getObjectId zo'

not' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
not' zo = fmap not . satisfies zo

rAnd' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  [Requirement zone ot] ->
  Magic 'Private 'RO m Bool
rAnd' zo = \case
  [] -> pure True
  req : reqs ->
    satisfies zo req >>= \case
      False -> pure False
      True -> rAnd' zo reqs

rOr' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  [Requirement zone ot] ->
  Magic 'Private 'RO m Bool
rOr' zo = \case
  [] -> pure False
  req : reqs ->
    satisfies zo req >>= \case
      True -> pure True
      False -> rOr' zo reqs

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
import safe MtgPure.Engine.Fwd.Wrap (allZOs, findPermanent, getPermanent, satisfies)
import safe MtgPure.Engine.State (Magic)
import safe MtgPure.Model.Land (Land (landTypes))
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.ObjectId (GetObjectId (..))
import safe MtgPure.Model.ObjectType.Kind (OTLand)
import safe MtgPure.Model.Permanent (Permanent (..), Tapped (..))
import safe MtgPure.Model.Recursive (Requirement (..))
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Zone (IsZone (..), SZone (SZBattlefield), Zone (..))
import safe MtgPure.Model.ZoneObject (IsOT, IsZO, ZO, ZOPlayer)
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)

zosSatisfyingImpl :: (Monad m, IsZO zone ot) => Requirement zone ot -> Magic 'Private 'RO m [ZO zone ot]
zosSatisfyingImpl req = allZOs >>= M.filterM (`satisfies` req)

satisfiesImpl ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
satisfiesImpl zo = \case
  ControlledBy zoPlayer -> controlledBy' zo zoPlayer
  HasLandType landType -> hasLandType' zo landType
  Is _wAny zo' -> is' zo zo'
  IsTapped _wPerm -> isTapped' zo
  Not req -> not' zo req
  RAnd reqs -> rAnd' zo reqs
  ROr reqs -> rOr' zo reqs
  _ -> undefined

hasLandType' :: forall zone m. (Monad m, IsZone zone) => ZO zone OTLand -> LandType -> Magic 'Private 'RO m Bool
hasLandType' zo landType = case singZone @zone of
  SZBattlefield -> do
    perm <- getPermanent $ zo0ToPermanent $ toZO0 zo
    pure $ case permanentLand perm of
      Nothing -> False
      Just land -> landType `elem` landTypes land
  _ -> undefined

isTapped' :: (IsOT ot, Monad m) => ZO 'ZBattlefield ot -> Magic 'Private 'RO m Bool
isTapped' zo = do
  perm <- getPermanent $ zo0ToPermanent $ toZO0 zo
  pure $ permanentTapped perm == Tapped

controlledBy' ::
  forall m zone ot.
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  ZOPlayer ->
  Magic 'Private 'RO m Bool
controlledBy' zo zoPlayer = case singZone @zone of
  SZBattlefield ->
    findPermanent (zo0ToPermanent $ toZO0 zo) <&> \case
      Nothing -> getObjectId zo == getObjectId zoPlayer -- TODO: [Mindslaver]
      Just perm -> getObjectId (permanentController perm) == getObjectId zoPlayer
  _ -> undefined

is' :: (Monad m, IsZO zone ot) => ZO zone ot -> ZO zone ot -> Magic 'Private 'RO m Bool
is' zo zo' = pure $ getObjectId zo == getObjectId zo' -- XXX: Also check for ObjectId liveliness?

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

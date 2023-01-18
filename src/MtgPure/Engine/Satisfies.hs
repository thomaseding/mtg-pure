{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Satisfies (
  satisfies,
  zosSatisfying,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Functor ((<&>))
import safe MtgPure.Engine.Fwd.Api (
  allZOs,
  findPermanent,
  getPermanent,
 )
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.State (Magic, logCall)
import safe MtgPure.Model.Land (Land (landTypes))
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.Object.OTNAliases (OTNLand, OTNPlayer)
import safe MtgPure.Model.Object.ObjectId (getObjectId)
import safe MtgPure.Model.Permanent (Permanent (..), Tapped (..))
import safe MtgPure.Model.Recursive (Requirement (..))
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Zone (IsZone (..), SZone (SZBattlefield), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZOPlayer)

zosSatisfying :: (Monad m, IsZO zone ot) => Requirement zone ot -> Magic 'Private 'RO m [ZO zone ot]
zosSatisfying req = allZOs >>= M.filterM (`satisfies` req)

satisfies ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
satisfies zo = logCall 'satisfies \case
  ControlledBy zoPlayer -> controlledBy' zo zoPlayer
  ControlsA{} -> undefined
  HasAbility{} -> undefined
  HasLandType landType -> hasLandType' zo landType
  Is _wAny zo' -> is' zo zo'
  IsOpponentOf zoPlayer -> isOpponentOf' zo zoPlayer
  IsTapped _wPerm -> isTapped' zo
  Not req -> not' zo req
  OfColors{} -> undefined
  OwnedBy{} -> undefined
  PlayerPays{} -> undefined
  RAnd reqs -> rAnd' zo reqs
  ROr reqs -> rOr' zo reqs
  R2{} -> undefined
  R3{} -> undefined
  R4{} -> undefined
  R5{} -> undefined

hasLandType' ::
  forall zone m.
  (Monad m, IsZone zone) =>
  ZO zone OTNLand ->
  LandType ->
  Magic 'Private 'RO m Bool
hasLandType' zo landType = logCall 'hasLandType' case singZone @zone of
  SZBattlefield -> do
    perm <- getPermanent $ zo0ToPermanent $ toZO0 zo
    pure case permanentLand perm of
      Nothing -> False
      Just land -> landType `elem` landTypes land
  _ -> undefined -- XXX: sung zone

isTapped' :: (IsOTN ot, Monad m) => ZO 'ZBattlefield ot -> Magic 'Private 'RO m Bool
isTapped' zo = logCall 'isTapped' do
  perm <- getPermanent $ zo0ToPermanent $ toZO0 zo
  pure $ permanentTapped perm == Tapped

controlledBy' ::
  forall m zone ot.
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  ZOPlayer ->
  Magic 'Private 'RO m Bool
controlledBy' zo zoPlayer = logCall 'controlledBy' case singZone @zone of
  SZBattlefield -> do
    findPermanent (zo0ToPermanent $ toZO0 zo) <&> \case
      Nothing -> getObjectId zo == getObjectId zoPlayer -- TODO: [Mindslaver]
      Just perm -> getObjectId (permanentController perm) == getObjectId zoPlayer
  _ -> undefined -- XXX: sung zone

is' :: (Monad m, IsZO zone ot) => ZO zone ot -> ZO zone ot -> Magic 'Private 'RO m Bool
is' zo zo' = logCall 'is' do
  pure $ getObjectId zo == getObjectId zo' -- XXX: Also check for ObjectId liveliness?

not' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RO m Bool
not' zo = logCall 'not' $ fmap not . satisfies zo

isOpponentOf' ::
  forall m zone.
  (Monad m, IsZO zone OTNPlayer) =>
  ZO zone OTNPlayer ->
  ZOPlayer ->
  Magic 'Private 'RO m Bool
isOpponentOf' candidatePlayer referencePlayer = logCall 'isOpponentOf' do
  -- TODO: multiplayer
  let candidateId = getObjectId candidatePlayer
      referenceId = getObjectId referencePlayer
  pure $ candidateId /= referenceId

rAnd' ::
  (Monad m, IsZO zone ot) =>
  ZO zone ot ->
  [Requirement zone ot] ->
  Magic 'Private 'RO m Bool
rAnd' zo = logCall 'rAnd' \case
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
rOr' zo = logCall 'rOr' \case
  [] -> pure False
  req : reqs ->
    satisfies zo req >>= \case
      True -> pure True
      False -> rOr' zo reqs

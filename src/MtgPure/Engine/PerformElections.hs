{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PerformElections (
  performElections,
  controllerOf,
) where

import safe Control.Exception (assert)
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (AndLike, untilJust)
import safe Data.ConsIndex (consIndex)
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Engine.Fwd.Api (
  caseOf,
  getPermanent,
  zosSatisfying,
 )
import safe MtgPure.Engine.Monad (fromRO, gets, modify)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (InternalLogicError (..), Prompt' (..))
import safe MtgPure.Engine.State (
  AnyRequirement (..),
  GameState (..),
  Magic,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.Object.OTKind (OTAny)
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  Effect (..),
  Elect (..),
  List (List),
  Requirement (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
 )
import safe MtgPure.Model.Variable (Variable (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (
  oToZO1,
  toZO0,
  zo0ToPermanent,
  zo1ToO,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO, ZOPlayer, ZoneObject (ZO))

performElections ::
  forall ot m p el x.
  (Monad m, AndLike (Maybe x)) =>
  ZO 'ZStack OT0 ->
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect p el ot ->
  Magic 'Private 'RW m (Maybe x)
performElections zoStack goTerm = logCall 'performElections \case
  All masked -> electAll goRec masked
  Choose oPlayer thisToElect -> electA Choose' zoStack goRec oPlayer thisToElect
  ControllerOf zo cont -> electControllerOf goRec zo cont
  Cost cost -> goTerm cost
  ElectActivated activated -> goTerm activated
  ElectCard facet -> goTerm facet
  ElectCase case_ -> caseOf goRec case_
  Effect effect -> goTerm $ Sequence effect
  Elect elect -> goTerm elect
  Target oPlayer thisToElect -> electA Target' zoStack goRec oPlayer thisToElect
  VariableInt cont -> electVariableInt goRec cont
  x -> error $ show $ consIndex x
 where
  goRec = performElections zoStack goTerm

electVariableInt ::
  Monad m =>
  (Elect 'Pre el ot -> Magic 'Private 'RW m (Maybe x)) ->
  (Variable Int -> Elect 'Pre el ot) ->
  Magic 'Private 'RW m (Maybe x)
electVariableInt goElect cont = logCall 'electVariableInt do
  let var = ReifiedVariable undefined undefined
  goElect $ cont var

controllerOf ::
  forall zone ot m.
  (IsZO zone ot, Monad m) =>
  ZO zone ot ->
  Magic 'Private 'RO m (Object 'OTPlayer)
controllerOf zo = logCall 'controllerOf case singZone @zone of
  SZBattlefield -> do
    perm <- fromRO $ getPermanent $ zo0ToPermanent $ toZO0 zo
    pure $ permanentController perm
  _ -> undefined

electControllerOf ::
  forall p zone m el ot x.
  (IsZO zone OTAny, Monad m) =>
  (Elect p el ot -> Magic 'Private 'RW m (Maybe x)) ->
  ZO zone OTAny ->
  (ZOPlayer -> Elect p el ot) ->
  Magic 'Private 'RW m (Maybe x)
electControllerOf goElect zo cont = logCall 'electControllerOf do
  controller <- fromRO $ controllerOf zo
  let elect = cont $ oToZO1 controller
  goElect elect

data Selection
  = Choose'
  | Target'

newTarget ::
  forall zone ot m.
  (Monad m, IsZO zone ot) =>
  ZO 'ZStack OT0 ->
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RW m (ZO zone ot)
newTarget zoStack zoTargetBase req = logCall 'newTarget do
  discr <- fromRO $ gets magicNextObjectDiscriminant
  let (ZO sZone objN) = zoTargetBase
      go :: Object a -> Object a
      go = \case
        Object wit (UntypedObject oldDisc i) ->
          assert (oldDisc == DefaultObjectDiscriminant) $ Object wit (UntypedObject discr i)
      objN' = mapObjectN go objN
      zoTarget = ZO sZone objN'
  modify \st ->
    let targetId = getObjectId zoTarget
        propMap = magicTargetProperties st
        propMap' = Map.insert targetId (AnyRequirement req) propMap
        targetIds = case Map.lookup zoStack $ magicStackEntryTargetsMap st of
          Just ts -> ts
          Nothing -> error $ show $ ExpectedStackObjectToExist zoStack
        targetIds' = targetId : targetIds
        targetsMap = magicStackEntryTargetsMap st
        targetsMap' = Map.insert zoStack targetIds' targetsMap
     in st
          { magicNextObjectDiscriminant = (+ 1) <$> discr
          , magicStackEntryTargetsMap = targetsMap'
          , magicTargetProperties = propMap'
          }
  pure zoTarget

electA ::
  forall p zone m el ot x.
  (IsZO zone ot, Monad m) =>
  Selection ->
  ZO 'ZStack OT0 ->
  (Elect p el ot -> Magic 'Private 'RW m (Maybe x)) ->
  ZOPlayer ->
  WithMaskedObject zone (Elect p el ot) ->
  Magic 'Private 'RW m (Maybe x)
electA sel zoStack goElect oPlayer = logCall 'electA \case
  M1 reqs zoToElect -> go reqs zoToElect
  M2 reqs zoToElect -> go reqs zoToElect
  M3 reqs zoToElect -> go reqs zoToElect
  M4 reqs zoToElect -> go reqs zoToElect
  M5 reqs zoToElect -> go reqs zoToElect
  M6 reqs zoToElect -> go reqs zoToElect
 where
  go ::
    (IsZO zone ot', Eq (ZO zone ot')) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> Elect p el ot) ->
    Magic 'Private 'RW m (Maybe x)
  go reqs zoToElect = do
    prompt <- fromRO $ gets magicPrompt
    fromRO (zosSatisfying (RAnd reqs)) >>= \case
      [] -> pure Nothing
      zos@(zosHead : zosTail) -> do
        opaque <- fromRO $ gets mkOpaqueGameState
        zo <- lift $
          untilJust \attempt -> do
            zo <- promptPickZO prompt attempt opaque (zo1ToO oPlayer) $ zosHead :| zosTail
            pure case zo `elem` zos of
              False -> Nothing
              True -> Just zo
        zo' <- case sel of
          Choose' -> pure zo
          Target' -> newTarget zoStack zo $ RAnd reqs
        let elect = zoToElect zo'
        goElect elect

electAll ::
  forall zone m p el ot x.
  (IsZO zone ot, Monad m, AndLike x) =>
  (Elect p el ot -> Magic 'Private 'RW m x) ->
  WithMaskedObjects zone (Elect p el ot) ->
  Magic 'Private 'RW m x
electAll goElect = logCall 'electAll \case
  M1s reqs zosToElect -> go reqs zosToElect
  M2s reqs zosToElect -> go reqs zosToElect
  M3s reqs zosToElect -> go reqs zosToElect
  M4s reqs zosToElect -> go reqs zosToElect
  M5s reqs zosToElect -> go reqs zosToElect
  M6s reqs zosToElect -> go reqs zosToElect
 where
  go ::
    (IsZO zone ot', Eq (ZO zone ot')) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> Elect p el ot) ->
    Magic 'Private 'RW m x
  go reqs zosToElect = do
    zos <- fromRO $ zosSatisfying $ RAnd reqs
    goElect $ zosToElect $ List zos

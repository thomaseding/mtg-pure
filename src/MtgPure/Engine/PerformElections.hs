{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PerformElections (
  performIntrinsicElections,
  performTargetElections,
  performResolveElections,
  controllerOf,
  ownerOf,
) where

import safe Control.Exception (assert)
import safe Control.Monad.Access (IsReadWrite, ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (untilJust)
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.Map.Strict as Map
import safe Data.Nat (Fin (..), IsNat, NatList (..), finToInt, natListElems)
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Api (
  caseOf,
  isSatisfied,
  newVariableId,
  zosSatisfying,
 )
import safe MtgPure.Engine.Monad (fromPublic, fromRO, gets, modify)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (
  ElectionInput (..),
  InternalLogicError (..),
  PickVariety (..),
  Prompt' (..),
 )
import safe MtgPure.Engine.State (
  AnyRequirement (..),
  GameState (..),
  Magic,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.ElectStage (
  CoNonIntrinsicStage (..),
  ElectStage (..),
  ElectStageRW,
  NonIntrinsicStage (..),
 )
import safe MtgPure.Model.Object.MapObjectN (mapObjectN)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.OTNAliases (OTNAny)
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Recursive (
  Condition,
  Effect (..),
  Elect (..),
  List (List),
  Requirement (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
 )
import safe MtgPure.Model.Variable (Variable (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (
  oToZO1,
  zo1ToO,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO, ZOPlayer, ZoneObject (ZO))

performIntrinsicElections ::
  forall ot m el x.
  Monad m =>
  ElectionInput 'IntrinsicStage ->
  -- | `goTerm` continuation
  (el -> Magic 'Private 'RO m x) ->
  Elect 'IntrinsicStage el ot ->
  Magic 'Private 'RO m x
performIntrinsicElections = logCall 'performIntrinsicElections $ performElections' impossible
 where
  impossible = error "impossible"

-- Returns Nothing when the elections cannot be completed (e.g. no valid targets).
-- This may also return Nothing when the `goTerm` continuation returns Nothing.
performTargetElections ::
  forall ot m el x.
  Monad m =>
  ElectionInput 'TargetStage ->
  -- | `goTerm` continuation
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect 'TargetStage el ot ->
  Magic 'Private 'RW m (Maybe x)
performTargetElections = logCall 'performTargetElections $ performElections' Nothing

performResolveElections ::
  forall ot m el x.
  Monad m =>
  ElectionInput 'ResolveStage ->
  -- | `goTerm` continuation
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect 'ResolveStage el ot ->
  Magic 'Private 'RW m (Maybe x)
performResolveElections = logCall 'performResolveElections $ performElections' Nothing

performElections' ::
  forall ot m s el x.
  (Monad m, IsReadWrite (ElectStageRW s)) =>
  x ->
  ElectionInput s ->
  (el -> Magic 'Private (ElectStageRW s) m x) ->
  Elect s el ot ->
  Magic 'Private (ElectStageRW s) m x
performElections' failureX input goTerm = logCall 'performElections' \case
  ActivePlayer{} -> undefined
  All masked -> electAll goRec masked
  Choose oPlayer thisToElect -> goRW \goRecRW -> electA Choose' zoStack failureX goRecRW oPlayer thisToElect
  ChooseOption zoPlayer choices choiceToElect -> goRW \goRecRW -> chooseOption goRecRW zoPlayer choices choiceToElect
  Condition{} -> undefined
  ControllerOf zo cont -> electControllerOf goRec zo cont
  Cost cost -> goTerm cost
  ElectActivated activated -> goTerm activated
  ElectCardFacet character -> goTerm character
  ElectCardSpec spec -> goTerm spec
  ElectCase case_ -> caseOf goRec case_
  Effect effect -> goTerm $ Sequence effect
  EndTargets elect -> goTerm elect
  Event{} -> undefined
  If{} -> undefined
  Listen{} -> undefined
  OwnerOf zo cont -> electOwnerOf goRec zo cont
  PlayerPays{} -> undefined
  Random{} -> undefined
  Target zoPlayer thisToElect -> electA Target' zoStack failureX goRec zoPlayer thisToElect
  VariableFromPower{} -> undefined
  VariableInt cont -> electVariableInt goRec cont
  Your cont -> goRec $ cont $ oToZO1 @ 'ZBattlefield $ intrinsicYou input
 where
  goRec :: Elect s el ot -> Magic 'Private (ElectStageRW s) m x
  goRec = performElections' failureX input goTerm

  goRW ::
    CoNonIntrinsicStage s =>
    ((Elect s el ot -> Magic 'Private 'RW m x) -> Magic 'Private 'RW m a) ->
    Magic 'Private (ElectStageRW s) m a
  goRW m = case coNonIntrinsicStage @s of
    NonIntrinsicTargetStage -> m goRec
    NonIntrinsicResolveStage -> m goRec

  zoStack :: ZO 'ZStack OT0
  zoStack = case input of
    IntrinsicInput{} -> error "zoStack: IntrinsicInput"
    TargetInput{} -> targetStackZO input
    ResolveInput{} -> resolveStackZO input

chooseOption ::
  (Monad m, IsNat n, Typeable user, Show user) =>
  (Elect s el ot -> Magic 'Private 'RW m x) ->
  ZOPlayer ->
  NatList user n Condition ->
  (Variable (Fin user n) -> Elect s el ot) ->
  Magic 'Private 'RW m x
chooseOption goElect zoPlayer choices cont = logCall 'chooseOption do
  let oPlayer = zo1ToO zoPlayer
  opaque <- fromRO $ gets mkOpaqueGameState
  prompt <- fromRO $ gets magicPrompt
  fin <- fromRO $ untilJust \attempt -> do
    fin <- M.lift $ promptChooseOption prompt attempt opaque oPlayer choices
    let choice = natListElems choices !! finToInt fin
    isSatisfied choice >>= \case
      False -> pure Nothing
      True -> pure $ Just fin
  varId <- newVariableId
  let var = ReifiedVariable varId fin
  goElect $ cont var

electVariableInt ::
  Monad m =>
  (Elect 'TargetStage el ot -> Magic 'Private 'RW m x) ->
  (Variable Int -> Elect 'TargetStage el ot) ->
  Magic 'Private 'RW m x
electVariableInt goElect cont = logCall 'electVariableInt do
  varId <- newVariableId
  let var = ReifiedVariable varId undefined -- TODO: prompt user for reified value
  goElect $ cont var

controllerOf ::
  forall zone ot m.
  (IsZO zone ot, Monad m) =>
  ZO zone ot ->
  Magic 'Private 'RO m (Object 'OTPlayer)
controllerOf zo = logCall 'controllerOf do
  gets (Map.lookup (getObjectId zo) . magicControllerMap) >>= \case
    Nothing -> error $ "controllerOf: no controller for " <> show zo
    Just controller -> pure controller

electControllerOf ::
  forall s zone m el ot x.
  (IsZO zone OTNAny, Monad m, IsReadWrite (ElectStageRW s)) =>
  (Elect s el ot -> Magic 'Private (ElectStageRW s) m x) ->
  ZO zone OTNAny ->
  (ZOPlayer -> Elect s el ot) ->
  Magic 'Private (ElectStageRW s) m x
electControllerOf goElect zo cont = logCall 'electControllerOf do
  controller <- fromRO $ controllerOf zo
  let elect = cont $ oToZO1 controller
  goElect elect

ownerOf ::
  forall zone ot m.
  (IsZO zone ot, Monad m) =>
  ZO zone ot ->
  Magic 'Private 'RO m (Object 'OTPlayer)
ownerOf zo = logCall 'ownerOf do
  gets (Map.lookup (getObjectId zo) . magicOwnerMap) >>= \case
    Nothing -> error $ "ownerOf: no owner for " <> show zo
    Just owner -> pure owner

electOwnerOf ::
  forall s zone m el ot x.
  (IsZO zone OTNAny, Monad m, IsReadWrite (ElectStageRW s)) =>
  (Elect s el ot -> Magic 'Private (ElectStageRW s) m x) ->
  ZO zone OTNAny ->
  (ZOPlayer -> Elect s el ot) ->
  Magic 'Private (ElectStageRW s) m x
electOwnerOf goElect zo cont = logCall 'electOwnerOf do
  owner <- fromRO $ ownerOf zo
  let elect = cont $ oToZO1 owner
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
          { magicNextObjectDiscriminant = (1 +) <$> discr
          , magicStackEntryTargetsMap = targetsMap'
          , magicTargetProperties = propMap'
          }
  pure zoTarget

electA ::
  forall s zone m el ot x.
  (IsZO zone ot, Monad m) =>
  Selection ->
  ZO 'ZStack OT0 ->
  x ->
  (Elect s el ot -> Magic 'Private 'RW m x) ->
  ZOPlayer ->
  WithMaskedObject (Elect s el) zone ot ->
  Magic 'Private 'RW m x
electA sel zoStack failureX goElect oPlayer = logCall 'electA \case
  Masked1 reqs zoToElect -> go reqs zoToElect
  Masked2 reqs zoToElect -> go reqs zoToElect
  Masked3 reqs zoToElect -> go reqs zoToElect
  Masked4 reqs zoToElect -> go reqs zoToElect
  Masked5 reqs zoToElect -> go reqs zoToElect
  Masked6 reqs zoToElect -> go reqs zoToElect
 where
  go ::
    (IsZO zone ot', Eq (ZO zone ot')) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> Elect s el ot) ->
    Magic 'Private 'RW m x
  go reqs zoToElect = do
    prompt <- fromRO $ gets magicPrompt
    fromRO (zosSatisfying (RAnd reqs)) >>= \case
      [] -> pure failureX
      zos@(zosHead : zosTail) -> do
        opaque <- fromRO $ gets mkOpaqueGameState
        zo <- fromPublic $ fromRO do
          untilJust \attempt -> do
            zo <- M.lift $ promptPick prompt attempt opaque (zo1ToO oPlayer) PickZO $ zosHead :| zosTail
            pure case zo `elem` zos of
              False -> Nothing
              True -> Just zo
        zo' <- case sel of
          Choose' -> pure zo
          Target' -> newTarget zoStack zo $ RAnd reqs -- TODO: Add a req to prevent targeting self.
        let elect = zoToElect zo'
        goElect elect

electAll ::
  forall zone m s el ot x.
  (IsZO zone ot, Monad m, IsReadWrite (ElectStageRW s)) =>
  (Elect s el ot -> Magic 'Private (ElectStageRW s) m x) ->
  WithMaskedObjects (Elect s el) zone ot ->
  Magic 'Private (ElectStageRW s) m x
electAll goElect = logCall 'electAll \case
  Maskeds1 reqs zosToElect -> go reqs zosToElect
  Maskeds2 reqs zosToElect -> go reqs zosToElect
  Maskeds3 reqs zosToElect -> go reqs zosToElect
  Maskeds4 reqs zosToElect -> go reqs zosToElect
  Maskeds5 reqs zosToElect -> go reqs zosToElect
  Maskeds6 reqs zosToElect -> go reqs zosToElect
 where
  go ::
    (IsZO zone ot', Eq (ZO zone ot')) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> Elect s el ot) ->
    Magic 'Private (ElectStageRW s) m x
  go reqs zosToElect = do
    zos <- fromRO $ zosSatisfying $ RAnd reqs
    goElect $ zosToElect $ List zos

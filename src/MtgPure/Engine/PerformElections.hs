{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
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

module MtgPure.Engine.PerformElections (
  performElectionsImpl,
) where

import safe Control.Exception (assert)
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (AndLike, untilJust)
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Engine.Fwd.Wrap (caseOf, zosSatisfying)
import safe MtgPure.Engine.Monad (fromRO, gets, modify)
import safe MtgPure.Engine.Prompt (InternalLogicError (..), Prompt' (..))
import safe MtgPure.Engine.State (AnyRequirement (..), GameState (..), Magic, StackEntry (..))
import safe MtgPure.Model.Object (
  OT0,
  Object (..),
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.ObjectId (GetObjectId (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (Effect (..), Elect (..), Requirement (..), WithMaskedObject (..))
import safe MtgPure.Model.Tribal (IsMaybeTribal (..), SMaybeTribal (..))
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, OPlayer, ZO, ZoneObject (ZO))
import safe MtgPure.Model.ZoneObject.Convert (zo1ToO)

performElectionsImpl ::
  forall mTribal ot m p el x.
  (IsMaybeTribal mTribal, Monad m, AndLike (Maybe x)) =>
  SMaybeTribal mTribal ->
  ([Magic 'Private 'RW m (Maybe x)] -> Magic 'Private 'RW m (Maybe x)) ->
  ZO 'ZStack OT0 ->
  (el -> Magic 'Private 'RW m (Maybe x)) ->
  Elect p el ot ->
  Magic 'Private 'RW m (Maybe x)
performElectionsImpl wit seqM zoStack goTerm = \case
  All masked -> electAll seqM goRec masked
  CardTypeDef def -> goTerm def
  Choose oPlayer thisToElect -> electA @mTribal Choose' zoStack goRec oPlayer thisToElect
  Cost cost -> goTerm cost
  ElectCase case_ -> caseOf goRec case_
  Effect effect -> goTerm $ Sequence effect
  Elect elect -> goTerm elect
  Target oPlayer thisToElect -> electA @mTribal Target' zoStack goRec oPlayer thisToElect
  _ -> undefined
 where
  goRec = performElectionsImpl wit seqM zoStack goTerm

data Selection
  = Choose'
  | Target'

newTarget ::
  forall mTribal zone ot m.
  (IsMaybeTribal mTribal, Monad m, IsZO zone ot) =>
  ZO 'ZStack OT0 ->
  ZO zone ot ->
  Requirement zone ot ->
  Magic 'Private 'RW m (ZO zone ot)
newTarget zoStack zoTargetBase req = do
  discr <- fromRO $ gets magicNextObjectDiscriminant
  let (ZO sZone objN) = zoTargetBase
      go :: Object a -> Object a
      go = \case
        Object wit oldDisc i ->
          assert (oldDisc == DefaultObjectDiscriminant) $ Object wit discr i
      objN' = mapObjectN go objN
      zoTarget = ZO sZone objN'
  modify $ \st ->
    let targetId = getObjectId zoTarget
        propMap = magicTargetProperties st
        propMap' = Map.insert targetId (AnyRequirement req) propMap
     in case singMaybeTribal @mTribal of
          SNothingTribal ->
            let entry = case Map.lookup zoStack $ magicStackEntryAbilityMap st of
                  Just x -> x
                  Nothing -> error $ show ExpectedStackObjectToExist
                entry' = entry{stackEntryTargets = targetId : stackEntryTargets entry}
                itemMap = magicStackEntryAbilityMap st
                itemMap' = Map.insert zoStack entry' itemMap
             in st
                  { magicNextObjectDiscriminant = (+ 1) <$> discr
                  , magicStackEntryAbilityMap = itemMap'
                  , magicTargetProperties = propMap'
                  }
          SJustTribal ->
            let entry = case Map.lookup zoStack $ magicStackEntryTribalMap st of
                  Just x -> x
                  Nothing -> error $ show ExpectedStackObjectToExist
                entry' = entry{stackEntryTargets = targetId : stackEntryTargets entry}
                itemMap = magicStackEntryTribalMap st
                itemMap' = Map.insert zoStack entry' itemMap
             in st
                  { magicNextObjectDiscriminant = (+ 1) <$> discr
                  , magicStackEntryTribalMap = itemMap'
                  , magicTargetProperties = propMap'
                  }
          SJustNonTribal ->
            let entry = case Map.lookup zoStack $ magicStackEntryNonTribalMap st of
                  Just x -> x
                  Nothing -> error $ show ExpectedStackObjectToExist
                entry' = entry{stackEntryTargets = targetId : stackEntryTargets entry}
                itemMap = magicStackEntryNonTribalMap st
                itemMap' = Map.insert zoStack entry' itemMap
             in st
                  { magicNextObjectDiscriminant = (+ 1) <$> discr
                  , magicStackEntryNonTribalMap = itemMap'
                  , magicTargetProperties = propMap'
                  }
  pure zoTarget

electA ::
  forall mTribal p zone m el ot x.
  (IsMaybeTribal mTribal, IsZO zone ot, Monad m) =>
  Selection ->
  ZO 'ZStack OT0 ->
  (Elect p el ot -> Magic 'Private 'RW m (Maybe x)) ->
  OPlayer ->
  WithMaskedObject zone (Elect p el ot) ->
  Magic 'Private 'RW m (Maybe x)
electA sel zoStack goElect oPlayer = \case
  M1 reqs zoToElect -> go reqs zoToElect
  M2 reqs zoToElect -> go reqs zoToElect
  M3 reqs zoToElect -> go reqs zoToElect
  M4 reqs zoToElect -> go reqs zoToElect
  M5 reqs zoToElect -> go reqs zoToElect
  _ -> undefined
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
      zos -> do
        zo <- lift $
          untilJust $ do
            zo <- promptPickZO prompt (zo1ToO oPlayer) zos
            pure $ case zo `elem` zos of
              False -> Nothing
              True -> Just zo
        zo' <- case sel of
          Choose' -> pure zo
          Target' -> newTarget @mTribal zoStack zo $ RAnd reqs
        let elect = zoToElect zo'
        goElect elect

electAll ::
  forall zone m el ot x.
  (IsZO zone ot, Monad m, AndLike x) =>
  ([Magic 'Private 'RW m x] -> Magic 'Private 'RW m x) ->
  (Elect 'Post el ot -> Magic 'Private 'RW m x) ->
  WithMaskedObject zone (Elect 'Post el ot) ->
  Magic 'Private 'RW m x
electAll seqM goElect = \case
  M1 reqs zoToElect -> go reqs zoToElect
  M2 reqs zoToElect -> go reqs zoToElect
  M3 reqs zoToElect -> go reqs zoToElect
  M4 reqs zoToElect -> go reqs zoToElect
  M5 reqs zoToElect -> go reqs zoToElect
  _ -> undefined
 where
  go ::
    (IsZO zone ot', Eq (ZO zone ot')) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> Elect 'Post el ot) ->
    Magic 'Private 'RW m x
  go reqs zoToElect = do
    zos <- fromRO $ zosSatisfying $ RAnd reqs
    seqM $ map (goElect . zoToElect) zos

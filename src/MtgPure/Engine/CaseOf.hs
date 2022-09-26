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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.CaseOf (
  caseOfImpl,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe MtgPure.Engine.State (Magic)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Recursive (Case (..))
import safe MtgPure.Model.Variable (readVariable)

caseOfImpl :: Monad m => (x -> Magic 'Private 'RW m a) -> Case x -> Magic 'Private 'RW m a
caseOfImpl cont = \case
  CaseColor (readVariable -> color) w u b r g -> case color of
    White -> cont w
    Blue -> cont u
    Black -> cont b
    Red -> cont r
    Green -> cont g

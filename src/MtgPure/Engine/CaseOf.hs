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
import safe Data.Nat (Fin (..), NatList (..))
import safe MtgPure.Engine.State (Magic)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Recursive (Case (..))
import safe MtgPure.Model.Variable (readVariable)

caseOfImpl :: forall m x a. Monad m => (x -> Magic 'Private 'RW m a) -> Case x -> Magic 'Private 'RW m a
caseOfImpl cont = \case
  CaseColor (readVariable -> color) w u b r g -> case color of
    White -> cont w
    Blue -> cont u
    Black -> cont b
    Red -> cont r
    Green -> cont g
  CaseFin (readVariable -> fin) xs ->
    let go :: Fin n -> NatList n x -> Magic 'Private 'RW m a
        go fin' xs' = case (fin', xs') of
          (FZ, LZ x) -> cont x
          (FZ, LS x _) -> cont x
          (FS fin'', LS _ xs'') -> go fin'' xs''
     in go fin xs

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ManaTypeToColor (
  ManaTypeToColor,
) where

import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ManaType (ManaType (..))

type family ManaTypeToColor (mt :: ManaType) = (c :: Color) | c -> mt where
  ManaTypeToColor 'MTWhite = 'White
  ManaTypeToColor 'MTBlue = 'Blue
  ManaTypeToColor 'MTBlack = 'Black
  ManaTypeToColor 'MTRed = 'Red
  ManaTypeToColor 'MTGreen = 'Green

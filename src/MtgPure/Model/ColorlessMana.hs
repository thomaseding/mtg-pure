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

module MtgPure.Model.ColorlessMana (
  ColorlessMana (..),
) where

import safe Data.Kind (Type)
import safe MtgPure.Model.Variable (Variable)

data ColorlessMana :: Type where
  ColorlessMana' :: Int -> ColorlessMana
  VariableColorlessMana :: Variable -> ColorlessMana
  SumColorlessMana :: ColorlessMana -> ColorlessMana -> ColorlessMana
  deriving (Eq, Ord, Show)

instance Semigroup ColorlessMana where
  (<>) (ColorlessMana' x) (ColorlessMana' y) = ColorlessMana' (x + y)
  (<>) (ColorlessMana' 0) y = y
  (<>) x (ColorlessMana' 0) = x
  (<>) x y = SumColorlessMana x y

instance Monoid ColorlessMana where
  mempty = ColorlessMana' 0

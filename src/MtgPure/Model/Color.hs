{-# LANGUAGE AllowAmbiguousTypes #-}
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

module MtgPure.Model.Color (
  Colorless,
  Color (..),
  SColor (..),
  IsColor (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Colorless :: Type

data Color :: Type where
  White :: Color
  Blue :: Color
  Black :: Color
  Red :: Color
  Green :: Color
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable) -- TODO: orphan some of these

data SColor (color :: Color) :: Type where
  SWhite :: SColor 'White
  SBlue :: SColor 'Blue
  SBlack :: SColor 'Black
  SRed :: SColor 'Red
  SGreen :: SColor 'Green
  deriving (Typeable)

class Typeable color => IsColor (color :: Color) where
  litColor :: Color
  singColor :: SColor color

instance IsColor 'White where
  litColor = White
  singColor = SWhite

instance IsColor 'Blue where
  litColor = Blue
  singColor = SBlue

instance IsColor 'Black where
  litColor = Black
  singColor = SBlack

instance IsColor 'Red where
  litColor = Red
  singColor = SRed

instance IsColor 'Green where
  litColor = Green
  singColor = SGreen

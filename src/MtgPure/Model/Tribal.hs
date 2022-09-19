{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Tribal (
  Tribal (..),
  STribal (..),
  IsTribal (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Tribal
  = Tribal
  | NonTribal
  deriving (Eq, Ord, Show, Typeable)

data STribal (tribal :: Tribal) :: Type where
  STribal :: STribal 'Tribal
  SNonTribal :: STribal 'NonTribal
  deriving (Typeable)

deriving instance Eq (STribal tribal)

deriving instance Ord (STribal tribal)

deriving instance Show (STribal tribal)

class IsTribal (tribal :: Tribal) where
  singTribal :: STribal tribal

instance IsTribal 'Tribal where
  singTribal = STribal

instance IsTribal 'NonTribal where
  singTribal = SNonTribal

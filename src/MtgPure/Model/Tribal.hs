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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Tribal (
  Tribal (..),
  STribal (..),
  IsTribal (..),
  IsMaybeTribal (..),
  SMaybeTribal (..),
  MaybeTribalToOT,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ObjectType.Kind (OTActivatedOrTriggeredAbility, OTSpell)

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

type family MaybeTribalToOT (mTribal :: Maybe Tribal) = (ot :: Type) where
  MaybeTribalToOT 'Nothing = OTActivatedOrTriggeredAbility
  MaybeTribalToOT ( 'Just _) = OTSpell

data SMaybeTribal (mTribal :: Maybe Tribal) :: Type where
  SNothingTribal :: SMaybeTribal 'Nothing
  SJustTribal :: SMaybeTribal ( 'Just 'Tribal)
  SJustNonTribal :: SMaybeTribal ( 'Just 'NonTribal)
  deriving (Typeable)

class IsMaybeTribal (mTribal :: Maybe Tribal) where
  singMaybeTribal :: SMaybeTribal mTribal

instance IsMaybeTribal 'Nothing where
  singMaybeTribal = SNothingTribal

instance IsTribal tribal => IsMaybeTribal ( 'Just tribal) where
  singMaybeTribal = case singTribal @tribal of
    STribal -> SJustTribal
    SNonTribal -> SJustNonTribal

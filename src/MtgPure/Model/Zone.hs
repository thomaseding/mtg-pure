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

module MtgPure.Model.Zone
  ( Zone (..),
    SZone (..),
    IsZone (..),
  )
where

import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)

data Zone :: Type where
  Battlefield :: Zone
  Library :: Zone
  deriving (Eq, Ord, Show, Typeable)

data SZone :: Zone -> Type where
  SBattlefield :: SZone 'Battlefield
  SLibrary :: SZone 'Library
  deriving (Typeable)

deriving instance Eq (SZone zone)

deriving instance Ord (SZone zone)

deriving instance Show (SZone zone)

class Typeable zone => IsZone zone where
  singZone :: Proxy zone -> SZone zone
  litZone :: Proxy zone -> Zone

instance IsZone 'Battlefield where
  singZone _ = SBattlefield
  litZone _ = Battlefield

instance IsZone 'Library where
  singZone _ = SLibrary
  litZone _ = Library

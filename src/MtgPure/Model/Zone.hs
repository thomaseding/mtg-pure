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
  LibraryZone :: Zone
  deriving (Eq, Ord, Show, Typeable)

data SZone :: Zone -> Type where
  SLibraryZone :: SZone 'LibraryZone
  deriving (Typeable)

deriving instance Eq (SZone zone)

deriving instance Ord (SZone zone)

deriving instance Show (SZone zone)

class IsZone zone where
  singZone :: Proxy zone -> SZone zone
  litZone :: Proxy zone -> Zone

instance IsZone 'LibraryZone where
  singZone _ = SLibraryZone
  litZone _ = LibraryZone

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

module MtgPure.Model.Zone (
  Zone (..),
  SZone (..),
  IsZone (..),
) where

import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)

data Zone :: Type where
  ZBattlefield :: Zone
  ZExile :: Zone
  ZGraveyard :: Zone
  ZHand :: Zone
  ZLibrary :: Zone
  ZStack :: Zone
  deriving (Eq, Ord, Show, Typeable)

data SZone :: Zone -> Type where
  SZBattlefield :: SZone 'ZBattlefield
  SZExile :: SZone 'ZExile
  SZGraveyard :: SZone 'ZGraveyard
  SZHand :: SZone 'ZHand
  SZLibrary :: SZone 'ZLibrary
  SZStack :: SZone 'ZStack
  deriving (Typeable)

deriving instance Eq (SZone zone)

deriving instance Ord (SZone zone)

deriving instance Show (SZone zone)

class Typeable zone => IsZone zone where
  singZone :: Proxy zone -> SZone zone
  litZone :: Proxy zone -> Zone

instance IsZone 'ZBattlefield where
  singZone _ = SZBattlefield
  litZone _ = ZBattlefield

instance IsZone 'ZExile where
  singZone _ = SZExile
  litZone _ = ZExile

instance IsZone 'ZGraveyard where
  singZone _ = SZGraveyard
  litZone _ = ZGraveyard

instance IsZone 'ZHand where
  singZone _ = SZHand
  litZone _ = ZHand

instance IsZone 'ZLibrary where
  singZone _ = SZLibrary
  litZone _ = ZLibrary

instance IsZone 'ZStack where
  singZone _ = SZStack
  litZone _ = ZStack

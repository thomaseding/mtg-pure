{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MtgPure.Model.Permanent
  ( Permanent (..),
  )
where

import Data.Kind (Type)
import MtgPure.Model.ObjectType
  ( OTArtifact,
    OTCreature,
    OTEnchantment,
    OTLand,
    OTPlaneswalker,
    ObjectType,
  )

data Permanent :: ObjectType -> Type where
  Artifact :: Permanent OTArtifact
  Creature :: Permanent OTCreature
  Enchantment :: Permanent OTEnchantment
  Land :: Permanent OTLand
  Planeswalker :: Permanent OTPlaneswalker

deriving instance Show (Permanent a)

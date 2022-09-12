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

module MtgPure.Model.ObjectType.Any
  ( WAny (..),
    IsAnyType,
  )
where

import safe Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ObjectN.Type
  ( OAny,
    OArtifact,
    OCreature,
    OEnchantment,
    OInstant,
    OLand,
    ON2,
    ON3,
    ON4,
    ON5,
    ON6,
    OPlaneswalker,
    OPlayer,
    OSorcery,
  )

-- Witness type
data WAny :: Type -> Type where
  WAnyArtifact :: WAny OArtifact
  WAnyCreature :: WAny OCreature
  WAnyEnchantment :: WAny OEnchantment
  WAnyInstant :: WAny OInstant
  WAnyLand :: WAny OLand
  WAnyPlaneswalker :: WAny OPlaneswalker
  WAnyPlayer :: WAny OPlayer
  WAnySorcery :: WAny OSorcery
  WAny :: WAny OAny
  WAny2 :: Inst2 IsAnyType a b => WAny (ON2 a b)
  WAny3 :: Inst3 IsAnyType a b c => WAny (ON3 a b c)
  WAny4 :: Inst4 IsAnyType a b c d => WAny (ON4 a b c d)
  WAny5 :: Inst5 IsAnyType a b c d e => WAny (ON5 a b c d e)
  WAny6 :: Inst6 IsAnyType a b c d e f => WAny (ON6 a b c d e f)

deriving instance Show (WAny ot)

type IsAnyType = IsObjectType

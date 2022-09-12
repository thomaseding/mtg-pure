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
import safe MtgPure.Model.ObjectType (OT)
import safe MtgPure.Model.ObjectType.Kind
  ( OTAny,
    OTArtifact,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTPlayer,
    OTSorcery,
  )

-- Witness type
data WAny :: forall ot. ot -> Type where
  WAnyArtifact :: WAny OTArtifact
  WAnyCreature :: WAny OTCreature
  WAnyEnchantment :: WAny OTEnchantment
  WAnyInstant :: WAny OTInstant
  WAnyLand :: WAny OTLand
  WAnyPlaneswalker :: WAny OTPlaneswalker
  WAnyPlayer :: WAny OTPlayer
  WAnySorcery :: WAny OTSorcery
  WAny :: WAny OTAny
  WAny2 :: Inst2 IsAnyType a b => WAny '(OT, a, b)
  WAny3 :: Inst3 IsAnyType a b c => WAny '(OT, a, b, c)
  WAny4 :: Inst4 IsAnyType a b c d => WAny '(OT, a, b, c, d)
  WAny5 :: Inst5 IsAnyType a b c d e => WAny '(OT, a, b, c, d, e)
  WAny6 :: Inst6 IsAnyType a b c d e f => WAny '(OT, a, b, c, d, e, f)

deriving instance Show (WAny ot)

type IsAnyType = IsObjectType

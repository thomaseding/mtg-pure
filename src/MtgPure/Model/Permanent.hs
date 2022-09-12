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

module MtgPure.Model.Permanent
  ( Permanent (..),
    PermanentType (..),
    IsPermanentType (..),
    PermanentVisitor (..),
    visitPermanent',
  )
where

import Data.Inst (Inst2, Inst3, Inst4)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.ObjectN (OArtifact, OCreature, OEnchantment, OLand, OPlaneswalker, ObjectN)
import MtgPure.Model.ObjectType
  ( OTArtifact,
    OTCreature,
    OTEnchantment,
    OTLand,
    OTPermanent,
    OTPlaneswalker,
  )

data PermanentType
  = PTArtifact
  | PTCreature
  | PTEnchantment
  | PTLand
  | PTPlaneswalker
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Witness type
data Permanent :: forall a. a -> Type where
  PermanentArtifact :: Permanent OTArtifact
  PermanentCreature :: Permanent OTCreature
  PermanentEnchantment :: Permanent OTEnchantment
  PermanentLand :: Permanent OTLand
  PermanentPlaneswalker :: Permanent OTPlaneswalker
  Permanent :: Permanent OTPermanent
  Permanent2 :: Inst2 IsPermanentType a b => Permanent '(a, b)
  Permanent3 :: Inst3 IsPermanentType a b c => Permanent '(a, b, c)
  Permanent4 :: Inst4 IsPermanentType a b c d => Permanent '(a, b, c, d)

deriving instance Show (Permanent a)

data PermanentVisitor a = PermanentVisitor
  { visitPArtifact :: OArtifact -> a,
    visitPCreature :: OCreature -> a,
    visitPEnchantment :: OEnchantment -> a,
    visitPLand :: OLand -> a,
    visitPPlaneswalker :: OPlaneswalker -> a
  }

class IsObjectType a => IsPermanentType a where
  singPermanentType :: Proxy a -> PermanentType
  singPermanent :: Proxy a -> Permanent a
  visitPermanent :: PermanentVisitor b -> Permanent a -> ObjectN a -> b

visitPermanent' :: IsPermanentType a => (forall b. IsPermanentType b => ObjectN b -> x) -> Permanent a -> ObjectN a -> x
visitPermanent' f = visitPermanent $ PermanentVisitor f f f f f

instance IsPermanentType OTArtifact where
  singPermanentType _ = PTArtifact
  singPermanent _ = PermanentArtifact
  visitPermanent v _ = visitPArtifact v

instance IsPermanentType OTCreature where
  singPermanentType _ = PTCreature
  singPermanent _ = PermanentCreature
  visitPermanent v _ = visitPCreature v

instance IsPermanentType OTEnchantment where
  singPermanentType _ = PTEnchantment
  singPermanent _ = PermanentEnchantment
  visitPermanent v _ = visitPEnchantment v

instance IsPermanentType OTLand where
  singPermanentType _ = PTLand
  singPermanent _ = PermanentLand
  visitPermanent v _ = visitPLand v

instance IsPermanentType OTPlaneswalker where
  singPermanentType _ = PTPlaneswalker
  singPermanent _ = PermanentPlaneswalker
  visitPermanent v _ = visitPPlaneswalker v

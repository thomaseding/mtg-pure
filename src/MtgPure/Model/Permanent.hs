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
import MtgPure.Model.Object (OArtifact, OCreature, OEnchantment, OLand, OPlaneswalker, Object)
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

data Permanent :: forall a. a -> Type where
  Artifact :: Permanent OTArtifact
  Creature :: Permanent OTCreature
  Enchantment :: Permanent OTEnchantment
  Land :: Permanent OTLand
  Planeswalker :: Permanent OTPlaneswalker
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
  visitPermanent :: PermanentVisitor b -> Permanent a -> Object a -> b

visitPermanent' :: IsPermanentType a => (forall b. IsPermanentType b => Object b -> x) -> Permanent a -> Object a -> x
visitPermanent' f = visitPermanent $ PermanentVisitor f f f f f

instance IsPermanentType OTArtifact where
  singPermanentType _ = PTArtifact
  visitPermanent v _ = visitPArtifact v

instance IsPermanentType OTCreature where
  singPermanentType _ = PTCreature
  visitPermanent v _ = visitPCreature v

instance IsPermanentType OTEnchantment where
  singPermanentType _ = PTEnchantment
  visitPermanent v _ = visitPEnchantment v

instance IsPermanentType OTLand where
  singPermanentType _ = PTLand
  visitPermanent v _ = visitPLand v

instance IsPermanentType OTPlaneswalker where
  singPermanentType _ = PTPlaneswalker
  visitPermanent v _ = visitPPlaneswalker v

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

module MtgPure.Model.ObjectType.Permanent
  ( WPermanent (..),
    PermanentType (..),
    IsPermanentType (..),
    PermanentVisitor (..),
    visitPermanent',
  )
where

import safe Data.Inst (Inst2, Inst3, Inst4)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.ObjectN.Type
  ( OArtifact,
    OCreature,
    OEnchantment,
    OLand,
    ON1,
    ON2,
    ON3,
    ON4,
    OPermanent,
    OPlaneswalker,
  )
import MtgPure.Model.ObjectType (ObjectType (..))

data PermanentType
  = PTArtifact
  | PTCreature
  | PTEnchantment
  | PTLand
  | PTPlaneswalker
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Witness type
data WPermanent :: Type -> Type where
  WPermanentArtifact :: WPermanent OArtifact
  WPermanentCreature :: WPermanent OCreature
  WPermanentEnchantment :: WPermanent OEnchantment
  WPermanentLand :: WPermanent OLand
  WPermanentPlaneswalker :: WPermanent OPlaneswalker
  WPermanent :: WPermanent OPermanent
  WPermanent2 :: Inst2 IsPermanentType a b => WPermanent (ON2 a b)
  WPermanent3 :: Inst3 IsPermanentType a b c => WPermanent (ON3 a b c)
  WPermanent4 :: Inst4 IsPermanentType a b c d => WPermanent (ON4 a b c d)
  deriving (Typeable)

deriving instance Show (WPermanent a)

data PermanentVisitor a = PermanentVisitor
  { visitPArtifact :: OArtifact -> a,
    visitPCreature :: OCreature -> a,
    visitPEnchantment :: OEnchantment -> a,
    visitPLand :: OLand -> a,
    visitPPlaneswalker :: OPlaneswalker -> a
  }

class IsObjectType a => IsPermanentType a where
  singPermanentType :: Proxy a -> PermanentType
  singPermanent :: Proxy a -> WPermanent (ON1 a)
  visitPermanent :: PermanentVisitor b -> WPermanent (ON1 a) -> ON1 a -> b

visitPermanent' :: IsPermanentType a => (forall b. IsPermanentType b => ON1 b -> x) -> WPermanent (ON1 a) -> ON1 a -> x
visitPermanent' f = visitPermanent $ PermanentVisitor f f f f f

instance IsPermanentType 'OTArtifact where
  singPermanentType _ = PTArtifact
  singPermanent _ = WPermanentArtifact
  visitPermanent v _ = visitPArtifact v

instance IsPermanentType 'OTCreature where
  singPermanentType _ = PTCreature
  singPermanent _ = WPermanentCreature
  visitPermanent v _ = visitPCreature v

instance IsPermanentType 'OTEnchantment where
  singPermanentType _ = PTEnchantment
  singPermanent _ = WPermanentEnchantment
  visitPermanent v _ = visitPEnchantment v

instance IsPermanentType 'OTLand where
  singPermanentType _ = PTLand
  singPermanent _ = WPermanentLand
  visitPermanent v _ = visitPLand v

instance IsPermanentType 'OTPlaneswalker where
  singPermanentType _ = PTPlaneswalker
  singPermanent _ = WPermanentPlaneswalker
  visitPermanent v _ = visitPPlaneswalker v

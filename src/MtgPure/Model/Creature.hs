{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Creature (
  Creature (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.ObjectType.Kind (OTCreature)
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Recursive (Ability)
import safe MtgPure.Model.Toughness (Toughness (..))

data Creature :: Type where
  Creature ::
    { creatureTypes :: [CreatureType]
    , creaturePower :: Power
    , creatureToughness :: Toughness
    , creatureAbilities :: [Ability OTCreature]
    } ->
    Creature
  deriving (Typeable)

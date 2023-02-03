{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Creature (
  Creature (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Toughness (Toughness (..))

data Creature :: Type where
  Creature ::
    { creatureTypes :: [CreatureType]
    , creaturePower :: Power
    , creatureToughness :: Toughness
    } ->
    Creature
  deriving (Typeable)

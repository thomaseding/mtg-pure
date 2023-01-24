{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Supertype (
  Supertype (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OTNAliases (OTNLand)

data Supertype (ot :: Type) :: Type where
  Basic :: Supertype OTNLand
  --Basic :: CoLand ot => Supertype OTNLand
  Legendary :: Supertype ot
  Snow :: Supertype ot
  --Tribal :: CoNonCreature ot => [CreatureType] -> Supertype ot
  World :: Supertype ot
  deriving (Typeable)

deriving instance Eq (Supertype ot)

deriving instance Ord (Supertype ot)

deriving instance Show (Supertype ot)

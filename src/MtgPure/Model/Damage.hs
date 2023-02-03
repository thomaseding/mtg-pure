{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Damage (
  Damage' (..),
  Damage,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Variable (ForceVars (..), Var (..), Variable (..))

data Damage' (v :: Var) (a :: Type) :: Type where
  Damage :: a -> Damage' v a
  VariableDamage :: Variable a -> Damage' 'Var a
  deriving (Typeable) --  TODO: Make some of these orphans

deriving instance Eq a => Eq (Damage' v a)

deriving instance Functor (Damage' v)

deriving instance Ord a => Ord (Damage' v a)

deriving instance Show a => Show (Damage' v a)

type Damage v = Damage' v Int

instance ForceVars (Damage v) (Damage 'NoVar) where
  forceVars = \case
    Damage n -> Damage n
    VariableDamage (ReifiedVariable _ n) -> Damage n

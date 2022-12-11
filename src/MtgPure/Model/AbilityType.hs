{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.AbilityType (
  AbilityType (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.EffectType (EffectType (..))

data AbilityType :: EffectType -> Type where
  ATActivatedAbility :: AbilityType 'OneShot
  ATManaAbility :: AbilityType 'OneShot
  ATStaticAbility :: AbilityType 'Continuous
  ATTriggeredAbility :: AbilityType 'OneShot
  deriving (Typeable)

deriving instance Eq (AbilityType e)

deriving instance Ord (AbilityType e)

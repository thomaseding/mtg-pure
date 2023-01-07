{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Enchantment (
  AnyEnchantmentType (..),
  Enchantment (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Recursive (EnchantmentType)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN)

data AnyEnchantmentType :: Type where
  AnyEnchantmentType :: IsOTN ot => EnchantmentType ot -> AnyEnchantmentType

data Enchantment :: Type where
  Enchantment ::
    { enchantmentTypes :: [AnyEnchantmentType]
    } ->
    Enchantment
  deriving (Typeable)

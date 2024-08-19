{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Enchantment (
  AnyEnchantmentType (..),
  Enchantment (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Recursive (EnchantmentType, Flags)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN)

data AnyEnchantmentType (fs :: Flags) :: Type where
  AnyEnchantmentType :: (IsOTN ot) => EnchantmentType fs ot -> AnyEnchantmentType fs

data Enchantment (fs :: Flags) :: Type where
  Enchantment ::
    { enchantmentTypes :: [AnyEnchantmentType fs]
    } ->
    Enchantment fs
  deriving (Typeable)

{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.EffectType (
  EffectType (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data EffectType :: Type where
  OneShot :: EffectType
  Continuous :: EffectType
  deriving (Eq, Ord, Show, Typeable)

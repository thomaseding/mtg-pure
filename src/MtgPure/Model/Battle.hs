{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Battle (
  Battle (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Recursive (BattleType)

data Battle :: Type where
  Battle ::
    { battleTypes :: [BattleType]
    } ->
    Battle
  deriving (Typeable)

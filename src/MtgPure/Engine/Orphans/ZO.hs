{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Orphans.ZO (
  ) where

import safe MtgPure.Model.Object (Object (Object))
import safe MtgPure.Model.ObjectId (GetObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectType (SObjectType (SLand))
import safe MtgPure.Model.ZoneObject (ZO)

instance GetObjectId (ObjectN ot) => Show (ZO zone ot) where
  show zo = "Z" ++ show o
   where
    o = Object SLand $ getUntypedObject zo

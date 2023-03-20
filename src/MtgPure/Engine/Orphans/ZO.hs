{-# LANGUAGE Safe #-}
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

import safe MtgPure.Model.Object.Object (Object (Object))
import safe MtgPure.Model.Object.ObjectId (GetObjectId (..))
import safe MtgPure.Model.Object.SingOT (SingOT (..))
import safe MtgPure.Model.Object.VisitObjectN ()
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

instance Show (ZO zone ot) where
  show zo = "Z" ++ show o
   where
    arb = SingLand
    o = Object arb $ getUntypedObject zo

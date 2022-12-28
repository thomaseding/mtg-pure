{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Library (
  Library (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsCardList (IsCardList (..))
import safe MtgPure.Model.Object.OTKind (OTCard)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

newtype Library :: Type where
  Library ::
    { unLibrary :: [ZO 'ZLibrary OTCard]
    } ->
    Library
  deriving (Typeable)

instance IsCardList Library (ZO 'ZLibrary OTCard) where
  toCardList = Library
  fromCardList = unLibrary

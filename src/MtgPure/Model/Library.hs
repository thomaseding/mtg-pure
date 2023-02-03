{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Library (
  Library (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsCardList (IsCardList (..))
import safe MtgPure.Model.Object.OTNAliases (OTNCard)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

newtype Library :: Type where
  Library :: {unLibrary :: [ZO 'ZLibrary OTNCard]} -> Library
  deriving (Typeable)

instance IsCardList Library (ZO 'ZLibrary OTNCard) where
  toCardList = Library
  fromCardList = unLibrary

instance Semigroup Library where
  Library a <> Library b = Library (a <> b)

instance Monoid Library where
  mempty = Library mempty

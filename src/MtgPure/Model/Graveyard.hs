{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Graveyard (
  Graveyard (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsCardList (IsCardList (..))
import safe MtgPure.Model.Object.OTNAliases (OTNCard)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

newtype Graveyard :: Type where
  Graveyard :: {unGraveyard :: [ZO 'ZGraveyard OTNCard]} -> Graveyard
  deriving (Typeable)

instance IsCardList Graveyard (ZO 'ZGraveyard OTNCard) where
  toCardList = Graveyard
  fromCardList = unGraveyard

instance Semigroup Graveyard where
  Graveyard a <> Graveyard b = Graveyard (a <> b)

instance Monoid Graveyard where
  mempty = Graveyard mempty

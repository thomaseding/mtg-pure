{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Hand (
  Hand (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsCardList (IsCardList (..))
import safe MtgPure.Model.Object.OTNAliases (OTNCard)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

newtype Hand :: Type where
  Hand ::
    { unHand :: [ZO 'ZHand OTNCard]
    } ->
    Hand
  deriving (Typeable)

instance IsCardList Hand (ZO 'ZHand OTNCard) where
  toCardList = Hand
  fromCardList = unHand

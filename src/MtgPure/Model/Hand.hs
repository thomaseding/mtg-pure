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
import safe MtgPure.Model.ObjectType.Kind (OTCard)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (ZO)

newtype Hand :: Type where
  Hand ::
    { unHand :: [ZO 'ZHand OTCard]
    } ->
    Hand
  deriving (Typeable)

instance IsCardList Hand (ZO 'ZHand OTCard) where
  toCardList = Hand
  fromCardList = unHand

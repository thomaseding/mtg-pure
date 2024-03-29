{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CardName (
  CardName (..),
  HasCardName (..),
) where

import safe Data.String (IsString (..))
import safe Data.Typeable (Typeable)

newtype CardName = CardName
  { unCardName :: String
  }
  deriving (Eq, Ord, Typeable)

instance IsString CardName where
  fromString = CardName

instance Show CardName where
  show = show . unCardName

class HasCardName a where
  getCardName :: a -> CardName

instance HasCardName CardName where
  getCardName = id

instance Semigroup CardName where
  CardName a <> CardName b = CardName $ a ++ b

instance (HasCardName a, HasCardName b) => HasCardName (Either a b) where
  getCardName = either getCardName getCardName

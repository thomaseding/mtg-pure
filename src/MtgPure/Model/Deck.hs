{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Deck (
  Deck (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Recursive (AnyCard)

newtype Deck :: Type where
  Deck :: {unDeck :: [AnyCard]} -> Deck
  deriving (Typeable)

instance Semigroup Deck where
  (<>) :: Deck -> Deck -> Deck
  Deck a <> Deck b = Deck (a <> b)

instance Monoid Deck where
  mempty :: Deck
  mempty = Deck mempty

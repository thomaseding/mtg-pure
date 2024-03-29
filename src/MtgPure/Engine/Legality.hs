{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Legality (
  Legality (..),
  toLegality,
  fromLegality,
  maybeToLegality,
  legalityToMaybe,
) where

import safe Data.Typeable (Typeable)

data Legality
  = Legal
  | Illegal
  deriving (Eq, Ord, Show, Typeable)

toLegality :: Bool -> Legality
toLegality = \case
  True -> Legal
  False -> Illegal

fromLegality :: Legality -> Bool
fromLegality = \case
  Legal -> True
  Illegal -> False

maybeToLegality :: Maybe () -> Legality
maybeToLegality = \case
  Nothing -> Illegal
  Just () -> Legal

legalityToMaybe :: Legality -> Maybe ()
legalityToMaybe = \case
  Illegal -> Nothing
  Legal -> Just ()

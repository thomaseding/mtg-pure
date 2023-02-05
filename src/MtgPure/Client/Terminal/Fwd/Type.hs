{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Client.Terminal.Fwd.Type (
  Fwd' (..),
) where

import safe MtgPure.Engine.State (OpaqueGameState)

data Fwd' m where
  Fwd ::
    { fwd_ :: ()
    , fwd_printGameState :: OpaqueGameState m -> m ()
    } ->
    Fwd' m

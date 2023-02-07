{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Client.Terminal.Fwd.Api (
  printGameState,
) where

import safe MtgPure.Client.Terminal.Fwd.Type (Fwd' (..))
import safe MtgPure.Client.Terminal.Monad (Fwd, Terminal, getsTerminalState, terminal_fwd)
import safe MtgPure.Engine.State (OpaqueGameState)

getFwd :: Terminal Fwd
getFwd = getsTerminalState terminal_fwd

_fwd0 :: (Fwd -> Terminal z) -> Terminal z
_fwd0 go = do
  fwd <- getFwd
  go fwd

_fwd1 :: (Fwd -> (a -> Terminal z)) -> a -> Terminal z
_fwd1 go a = do
  fwd <- getFwd
  go fwd a

fwd2 :: (Fwd -> (a -> b -> Terminal z)) -> a -> b -> Terminal z
fwd2 go a b = do
  fwd <- getFwd
  go fwd a b

_fwd3 :: (Fwd -> (a -> b -> c -> Terminal z)) -> a -> b -> c -> Terminal z
_fwd3 go a b c = do
  fwd <- getFwd
  go fwd a b c

printGameState :: OpaqueGameState Terminal -> Maybe String -> Terminal ()
printGameState = fwd2 fwd_printGameState

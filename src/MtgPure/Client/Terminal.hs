{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Use fromRight" #-}

module MtgPure.Client.Terminal (
  Terminal,
  TerminalInput (..),
  fwdImpl,
  playTerminalGame,
  runTerminal,
) where

import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (
  Terminal,
  TerminalInput (..),
  runTerminal,
 )
import safe MtgPure.Client.Terminal.PriorityAction (playTerminalGame)
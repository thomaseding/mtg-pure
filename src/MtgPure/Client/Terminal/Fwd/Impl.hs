{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Client.Terminal.Fwd.Impl (
  fwdImpl,
) where

import safe MtgPure.Client.Terminal.Fwd.Type (Fwd' (..))
import safe MtgPure.Client.Terminal.Monad (Fwd)
import safe MtgPure.Client.Terminal.Render (printGameState)

fwdImpl :: Fwd
fwdImpl =
  Fwd
    { fwd_ = ()
    , fwd_printGameState = printGameState
    }

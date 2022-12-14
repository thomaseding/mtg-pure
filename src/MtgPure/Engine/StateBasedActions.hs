{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.StateBasedActions (
  performStateBasedActions,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe MtgPure.Engine.State (Magic, logCall)

performStateBasedActions :: Monad m => Magic 'Private 'RW m ()
performStateBasedActions = logCall 'performStateBasedActions do
  pure () -- TODO

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mulligan (
  Mulligan (..),
) where

data Mulligan
  = DisableMulligan
  | ParisMulligan
  | VancouverMulligan
  | LondonMulligan

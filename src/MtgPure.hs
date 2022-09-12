{-# LANGUAGE Safe #-}

-- ghci -hidir .output -odir .output -fobject-code -Wall -Werror MtgPure

module MtgPure
  ( module MtgPure.Cards,
    codeGenToObjectN,
  )
where

import safe MtgPure.Cards
import safe MtgPure.Model.ToObjectN.CodeGen (codeGenToObjectN)

-- ghci -hidir output -odir output -fobject-code -Wall -Werror MtgPure

module MtgPure
  ( module MtgPure.Cards,
    module MtgPure.ShowCard,
    codeGenToObjectN,
  )
where

import MtgPure.Cards
import MtgPure.Model.ToObjectN.CodeGen (codeGenToObjectN)
import MtgPure.ShowCard

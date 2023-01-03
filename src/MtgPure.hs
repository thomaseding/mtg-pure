-- ghci -hidir .output -odir .output -fobject-code -Wall -Werror -XDataKinds MtgPure

module MtgPure (
  module MtgPure.AllCards,
  module MtgPure.Cards,
  module MtgPure.Client.Console,
  module MtgPure.Engine,
  module MtgPure.Model,
  module MtgPure.ModelCombinators,
  NatList (..),
  codeGenToObjectN,
  mainMountainRagingGoblin,
  mainMountainShock,
  mainMountainStoneRain,
) where

import safe Data.Nat (NatList (..))
import safe MtgPure.AllCards
import safe MtgPure.Cards
import safe MtgPure.Client.Console
import safe MtgPure.Engine hiding (
  ActivePlayer,
  ControllerOf,
  Satisfies,
  controllerOf,
  satisfies,
 )
import safe MtgPure.Model
import safe MtgPure.Model.Object.ToObjectN.CodeGen (codeGenToObjectN)
import safe MtgPure.ModelCombinators
import safe MtgPure.Test.Direct ()
import safe MtgPure.Test.MagicContUnit ()
import safe MtgPure.Test.MountainRagingGoblin (mainMountainRagingGoblin)
import safe MtgPure.Test.MountainShock (mainMountainShock)
import safe MtgPure.Test.MountainStoneRain (mainMountainStoneRain)
import safe MtgPure.Test.Variabled ()
import safe MtgPure.Test.VariabledMonad ()

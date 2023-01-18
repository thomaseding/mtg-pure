-- ghci -hidir .output -odir .output -fobject-code -Wall -Werror -XDataKinds MtgPure

module MtgPure (
  module MtgPure.AllCards,
  module MtgPure.Cards,
  module MtgPure.Client.Console,
  module MtgPure.Engine,
  module MtgPure.Model,
  NatList (..),
  mainCodeGenToObjectN,
  mainManaAbility,
  mainMountainRagingGoblin,
  mainMountainShock,
  mainMountainStoneRain,
  mainUnitMagicCont,
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
import safe MtgPure.Model.Object.ToObjectN.CodeGen (mainCodeGenToObjectN)
import safe MtgPure.Test.Engine.Unit.MagicCont (mainUnitMagicCont)
import safe MtgPure.Test.Game.ManaAbility (mainManaAbility)
import safe MtgPure.Test.Game.MountainRagingGoblin (mainMountainRagingGoblin)
import safe MtgPure.Test.Game.MountainShock (mainMountainShock)
import safe MtgPure.Test.Game.MountainStoneRain (mainMountainStoneRain)
import safe MtgPure.Test.SerializableMonadApi.ProofOfConcept ()
import safe MtgPure.Test.SerializableMonadApi.Variabled ()
import safe MtgPure.Test.SerializableMonadApi.VariabledMonad ()

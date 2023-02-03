{-# LANGUAGE Safe #-}

module MtgPure (
  module MtgPure.AllCards,
  module MtgPure.Cards,
  module MtgPure.Client.Terminal,
  module MtgPure.Engine,
  module MtgPure.Model,
  NatList (..),
  mainAnsiBoxExample,
  mainAnsiBoxMagic,
  mainCodeGenToObjectN,
  mainHybrid,
  mainManaAbility,
  mainRagingGoblin,
  mainShock,
  mainStoneRain,
  mainUnitMagicCont,
  mainUnitPayMana,
) where

import safe Data.Nat (NatList (..))
import safe Demo.AnsiBox (mainAnsiBoxExample)
import safe Demo.AnsiMagicBoard (mainAnsiBoxMagic)
import safe Demo.SerializableMonadApi.ProofOfConcept ()
import safe Demo.SerializableMonadApi.Variable ()
import safe Demo.SerializableMonadApi.VariableMonad ()
import safe MtgPure.AllCards
import safe MtgPure.Cards
import safe MtgPure.Client.Terminal
import safe MtgPure.Engine hiding (
  ActivePlayer,
  ControllerOf,
  Satisfies,
  controllerOf,
  satisfies,
 )
import safe MtgPure.Model
import safe MtgPure.Model.Object.ToObjectN.CodeGen (mainCodeGenToObjectN)
import safe Test.Engine.Unit.MagicCont (mainUnitMagicCont)
import safe Test.Engine.Unit.PayMana (mainUnitPayMana)
import safe Test.Game.Hybrid (mainHybrid)
import safe Test.Game.ManaAbility (mainManaAbility)
import safe Test.Game.RagingGoblin (mainRagingGoblin)
import safe Test.Game.Shock (mainShock)
import safe Test.Game.StoneRain (mainStoneRain)

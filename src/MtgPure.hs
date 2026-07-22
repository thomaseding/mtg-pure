{-# LANGUAGE Safe #-}

module MtgPure (
  allCards,
  mainAnsiBoxExample,
  mainAnsiBoxMagic,
  mainDemoGameplay,
  mainHybrid,
  mainManaAbility,
  mainRagingGoblin,
  mainShock,
  mainStoneRain,
  mainUnitMagicCont,
  mainUnitPayMana,
) where

import safe Demo.Ansi.Box (mainAnsiBoxExample)
import safe Demo.Ansi.MagicBoard (mainAnsiBoxMagic)
import safe Demo.MtgPure.Gameplay (mainDemoGameplay)
import safe MtgPure.AllCards (allCards)
import safe Test.Engine.Unit.MagicCont (mainUnitMagicCont)
import safe Test.Engine.Unit.PayMana (mainUnitPayMana)
import safe Test.Game.Play.Hybrid (mainHybrid)
import safe Test.Game.Play.ManaAbility (mainManaAbility)
import safe Test.Game.Play.RagingGoblin (mainRagingGoblin)
import safe Test.Game.Play.Shock (mainShock)
import safe Test.Game.Play.StoneRain (mainStoneRain)

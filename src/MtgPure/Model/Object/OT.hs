-- XXX: Keep these LANGUAGE pragmas in this file for code generation scripts
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

-- NOTE: OT stands for ObjectType
module MtgPure.Model.Object.OT (
  OT (..),
) where

import safe Data.Typeable (Typeable)

-- This generalizes the notion of "object" in MTG (e.g. contains "player")
--
-- Don't bother encoding OTManaAbility; since mana abilities are activated/triggered abilities and cannot be interacted with (605.3b)
--
-- If combinatorial explosion starts to be an issue, just stop the ToObjectN limit to that of 12 or so and also special case those
-- types to directly jump to ToObjectAny. Then it's the client's responsibility to provide any obtuse instances for ToObjectN.
data OT
  = OTActivatedAbility
  | OTArtifact
  | OTBattle
  | OTCreature
  | OTEmblem
  | OTEnchantment
  | OTInstant
  | OTLand
  | OTPlaneswalker
  | OTPlayer
  | OTSorcery
  | OTStaticAbility
  | OTTriggeredAbility
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

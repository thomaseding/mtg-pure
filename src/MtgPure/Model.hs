{-# LANGUAGE Safe #-}

module MtgPure.Model
  ( module MtgPure.Model.CardName,
    module MtgPure.Model.CardSet,
    module MtgPure.Model.CardType,
    module MtgPure.Model.Color,
    module MtgPure.Model.ColorToManaType,
    module MtgPure.Model.ColoredMana,
    module MtgPure.Model.ColorlessMana,
    module MtgPure.Model.Colors,
    module MtgPure.Model.ColorsLike,
    module MtgPure.Model.Cost,
    module MtgPure.Model.CreatureType,
    module MtgPure.Model.Damage,
    module MtgPure.Model.EffectType,
    module MtgPure.Model.GenericMana,
    module MtgPure.Model.HasManaSymbol,
    module MtgPure.Model.IsObjectType,
    module MtgPure.Model.Loyalty,
    module MtgPure.Model.Mana,
    module MtgPure.Model.ManaCost,
    module MtgPure.Model.ManaPool,
    module MtgPure.Model.ManaSymbol,
    module MtgPure.Model.ManaType,
    module MtgPure.Model.ManaTypeToColor,
    module MtgPure.Model.Object,
    module MtgPure.Model.ObjectN,
    module MtgPure.Model.ObjectType,
    module MtgPure.Model.Permanent,
    module MtgPure.Model.Power,
    module MtgPure.Model.PrettyObjectName,
    module MtgPure.Model.Rarity,
    module MtgPure.Model.Recursive,
    module MtgPure.Model.Requirement,
    module MtgPure.Model.Selection,
    module MtgPure.Model.ToMana,
    module MtgPure.Model.ToManaCost,
    module MtgPure.Model.ToManaPool,
    module MtgPure.Model.ToObjectN,
    module MtgPure.Model.Toughness,
    module MtgPure.Model.Variable,
    module MtgPure.Model.VisitObjectN,
  )
where

-- TODO:
-- Don't bother with the Internal model structure as-is.
-- Instead have Model as the master repr and export the corresponding
-- stuff to Model.Authoring and Model.Runtime

import MtgPure.Model.CardName
import MtgPure.Model.CardSet
import MtgPure.Model.CardType
import MtgPure.Model.Color
import MtgPure.Model.ColorToManaType
import MtgPure.Model.ColoredMana
import MtgPure.Model.ColorlessMana
import MtgPure.Model.Colors
import MtgPure.Model.ColorsLike
import MtgPure.Model.Cost
import MtgPure.Model.CreatureType
import MtgPure.Model.Damage
import MtgPure.Model.EffectType
import MtgPure.Model.GenericMana
import MtgPure.Model.HasManaSymbol
import MtgPure.Model.IsObjectType
import MtgPure.Model.Loyalty
import MtgPure.Model.Mana
import MtgPure.Model.ManaCost
import MtgPure.Model.ManaPool
import MtgPure.Model.ManaSymbol
import MtgPure.Model.ManaType
import MtgPure.Model.ManaTypeToColor
import MtgPure.Model.Object
import MtgPure.Model.ObjectN
import MtgPure.Model.ObjectType
import MtgPure.Model.Permanent
import MtgPure.Model.Power
import MtgPure.Model.PrettyObjectName
import MtgPure.Model.Rarity
import MtgPure.Model.Recursive
import MtgPure.Model.Requirement
import MtgPure.Model.Selection
import MtgPure.Model.ToMana
import MtgPure.Model.ToManaCost
import MtgPure.Model.ToManaPool
import MtgPure.Model.ToObjectN
import MtgPure.Model.Toughness
import MtgPure.Model.Variable
import MtgPure.Model.VisitObjectN

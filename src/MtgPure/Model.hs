{-# LANGUAGE Safe #-}

module MtgPure.Model
  ( module MtgPure.Model.AbilityType,
    module MtgPure.Model.BasicLandType,
    module MtgPure.Model.CardName,
    module MtgPure.Model.CardSet,
    module MtgPure.Model.CardType,
    module MtgPure.Model.Color,
    module MtgPure.Model.ColorToManaType,
    module MtgPure.Model.ColoredMana,
    module MtgPure.Model.ColorlessMana,
    module MtgPure.Model.Colors,
    module MtgPure.Model.ColorsLike,
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
    module MtgPure.Model.ObjectId,
    module MtgPure.Model.ObjectN,
    module MtgPure.Model.ObjectN.Type,
    module MtgPure.Model.ObjectType,
    module MtgPure.Model.ObjectType.Any,
    module MtgPure.Model.ObjectType.Kind,
    module MtgPure.Model.ObjectType.NonCreatureCard,
    module MtgPure.Model.ObjectType.Permanent,
    module MtgPure.Model.Phase,
    module MtgPure.Model.Power,
    module MtgPure.Model.PrettyObjectName,
    module MtgPure.Model.Rarity,
    module MtgPure.Model.Recursive,
    module MtgPure.Model.Selection,
    module MtgPure.Model.Step,
    module MtgPure.Model.TimePoint,
    module MtgPure.Model.ToMana,
    module MtgPure.Model.ToManaCost,
    module MtgPure.Model.ToManaPool,
    module MtgPure.Model.ToObjectN,
    module MtgPure.Model.Toughness,
    module MtgPure.Model.Tribal,
    module MtgPure.Model.Variable,
    module MtgPure.Model.VisitObjectN,
  )
where

-- TODO:
-- Don't bother with the Internal model structure as-is.
-- Instead have Model as the master repr and export the corresponding
-- stuff to Model.Authoring and Model.Runtime

import safe MtgPure.Model.AbilityType
import safe MtgPure.Model.BasicLandType
import safe MtgPure.Model.CardName
import safe MtgPure.Model.CardSet
import safe MtgPure.Model.CardType
import safe MtgPure.Model.Color
import safe MtgPure.Model.ColorToManaType
import safe MtgPure.Model.ColoredMana
import safe MtgPure.Model.ColorlessMana
import safe MtgPure.Model.Colors
import safe MtgPure.Model.ColorsLike
import safe MtgPure.Model.CreatureType
import safe MtgPure.Model.Damage
import safe MtgPure.Model.EffectType
import safe MtgPure.Model.GenericMana
import safe MtgPure.Model.HasManaSymbol
import safe MtgPure.Model.IsObjectType
import safe MtgPure.Model.Loyalty
import safe MtgPure.Model.Mana
import safe MtgPure.Model.ManaCost
import safe MtgPure.Model.ManaPool
import safe MtgPure.Model.ManaSymbol
import safe MtgPure.Model.ManaType
import safe MtgPure.Model.ManaTypeToColor
import safe MtgPure.Model.Object
import safe MtgPure.Model.ObjectId
import safe MtgPure.Model.ObjectN
import safe MtgPure.Model.ObjectN.Type
import safe MtgPure.Model.ObjectType
import safe MtgPure.Model.ObjectType.Any
import safe MtgPure.Model.ObjectType.Kind
import safe MtgPure.Model.ObjectType.NonCreatureCard
import safe MtgPure.Model.ObjectType.Permanent
import safe MtgPure.Model.Phase
import safe MtgPure.Model.Power
import safe MtgPure.Model.PrettyObjectName
import safe MtgPure.Model.Rarity
import safe MtgPure.Model.Recursive
import safe MtgPure.Model.Recursive.Eq ()
import safe MtgPure.Model.Recursive.Show ()
import safe MtgPure.Model.Selection
import safe MtgPure.Model.Step
import safe MtgPure.Model.TimePoint
import safe MtgPure.Model.ToMana
import safe MtgPure.Model.ToManaCost
import safe MtgPure.Model.ToManaPool
import safe MtgPure.Model.ToObjectN
import safe MtgPure.Model.Toughness
import safe MtgPure.Model.Tribal
import safe MtgPure.Model.Variable
import safe MtgPure.Model.VisitObjectN

{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.PrettyType (
  PrettyType (..),
) where

import safe Control.Exception (assert)
import safe Data.Inst (
  Inst10,
  Inst11,
  Inst12,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
  Inst7,
  Inst8,
  Inst9,
 )
import safe Data.Proxy (Proxy (Proxy))
import safe Data.Typeable (TypeRep, Typeable, typeRep)
import safe MtgPure.Model.Object.IsObjectType (
  IsObjectType (litObjectType),
 )
import safe MtgPure.Model.Object.OT (
  OT (..),
 )
import safe MtgPure.Model.Object.OTN (
  OT0,
  OT1,
  OT10,
  OT11,
  OT12,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OT8,
  OT9,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNArtifactCreature,
  OTNCreaturePlaneswalker,
  OTNCreaturePlayer,
  OTNCreaturePlayerPlaneswalker,
  OTNDamageSource,
  OTNEnchantmentCreature,
  OTNPermanent,
  OTNPlayerPlaneswalker,
  OTNSpell,
 )

class Typeable ty => PrettyType ty where
  prettyType :: String

instance IsObjectType a => PrettyType a where
  prettyType = show (litObjectType @a)

getRep :: forall a. Typeable a => TypeRep
getRep = typeRep (Proxy @a)

instance PrettyType OT0 where
  prettyType = "OT0"

instance IsObjectType a => PrettyType (OT1 a) where
  prettyType = case litObjectType @a of
    OTActivatedAbility -> "OTActivatedAbility"
    OTArtifact -> "OTArtifact"
    OTCreature -> "OTCreature"
    OTEmblem -> "OTEmblem"
    OTEnchantment -> "OTEnchantment"
    OTInstant -> "OTInstant"
    OTLand -> "OTLand"
    OTPlaneswalker -> "OTPlaneswalker"
    OTPlayer -> "OTPlayer"
    OTSorcery -> "OTSorcery"
    OTStaticAbility -> "OTStaticAbility"
    OTTriggeredAbility -> "OTTriggeredAbility"

instance Inst2 IsObjectType a b => PrettyType (OT2 a b) where
  prettyType =
    if
        | rep == getRep @OTNArtifactCreature ->
          "OTNArtifactCreature"
        | rep == getRep @OTNCreaturePlaneswalker ->
          "OTNCreaturePlaneswalker"
        | rep == getRep @OTNCreaturePlayer ->
          "OTNCreaturePlayer"
        | rep == getRep @OTNEnchantmentCreature ->
          "OTNEnchantmentCreature"
        | rep == getRep @OTNPlayerPlaneswalker ->
          "OTNPlayerPlaneswalker"
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ "]"
   where
    rep = getRep @(OT2 a b)

instance Inst3 IsObjectType a b c => PrettyType (OT3 a b c) where
  prettyType =
    if
        | rep == getRep @OTNCreaturePlayerPlaneswalker ->
          "OTNCreaturePlayerPlaneswalker"
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ "]"
   where
    rep = getRep @(OT3 a b c)

instance Inst4 IsObjectType a b c d => PrettyType (OT4 a b c d) where
  prettyType =
    if
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ "]"
   where
    _rep = getRep @(OT4 a b c d)

instance Inst5 IsObjectType a b c d e => PrettyType (OT5 a b c d e) where
  prettyType =
    if
        | rep == getRep @OTNPermanent ->
          "OTNPermanent"
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ ", "
            ++ show (getRep @e)
            ++ "]"
   where
    rep = getRep @(OT5 a b c d e)

instance Inst6 IsObjectType a b c d e f => PrettyType (OT6 a b c d e f) where
  prettyType =
    if
        | rep == getRep @OTNSpell ->
          "OTNSpell"
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ ", "
            ++ show (getRep @e)
            ++ ", "
            ++ show (getRep @f)
            ++ "]"
   where
    rep = getRep @(OT6 a b c d e f)

instance Inst7 IsObjectType a b c d e f g => PrettyType (OT7 a b c d e f g) where
  prettyType =
    if
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ ", "
            ++ show (getRep @e)
            ++ ", "
            ++ show (getRep @f)
            ++ ", "
            ++ show (getRep @g)
            ++ "]"
   where
    _rep = getRep @(OT7 a b c d e f g)

instance Inst8 IsObjectType a b c d e f g h => PrettyType (OT8 a b c d e f g h) where
  prettyType =
    if
        | rep == getRep @OTNDamageSource ->
          "OTNDamageSource"
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ ", "
            ++ show (getRep @e)
            ++ ", "
            ++ show (getRep @f)
            ++ ", "
            ++ show (getRep @g)
            ++ ", "
            ++ show (getRep @h)
            ++ "]"
   where
    rep = getRep @(OT8 a b c d e f g h)

instance Inst9 IsObjectType a b c d e f g h i => PrettyType (OT9 a b c d e f g h i) where
  prettyType =
    if
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ ", "
            ++ show (getRep @e)
            ++ ", "
            ++ show (getRep @f)
            ++ ", "
            ++ show (getRep @g)
            ++ ", "
            ++ show (getRep @h)
            ++ ","
            ++ show (getRep @i)
            ++ "]"
   where
    _rep = getRep @(OT9 a b c d e f g h i)

instance Inst10 IsObjectType a b c d e f g h i j => PrettyType (OT10 a b c d e f g h i j) where
  prettyType =
    if
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ ", "
            ++ show (getRep @e)
            ++ ", "
            ++ show (getRep @f)
            ++ ", "
            ++ show (getRep @g)
            ++ ", "
            ++ show (getRep @h)
            ++ ","
            ++ show (getRep @i)
            ++ ","
            ++ show (getRep @j)
            ++ "]"
   where
    _rep = getRep @(OT10 a b c d e f g h i j)

instance Inst11 IsObjectType a b c d e f g h i j k => PrettyType (OT11 a b c d e f g h i j k) where
  prettyType =
    if
        | otherwise ->
          "OTN '["
            ++ show (getRep @a)
            ++ ", "
            ++ show (getRep @b)
            ++ ", "
            ++ show (getRep @c)
            ++ ", "
            ++ show (getRep @d)
            ++ ", "
            ++ show (getRep @e)
            ++ ", "
            ++ show (getRep @f)
            ++ ", "
            ++ show (getRep @g)
            ++ ", "
            ++ show (getRep @h)
            ++ ","
            ++ show (getRep @i)
            ++ ","
            ++ show (getRep @j)
            ++ ","
            ++ show (getRep @k)
            ++ "]"
   where
    _rep = getRep @(OT11 a b c d e f g h i j k)

instance Inst12 IsObjectType a b c d e f g h i j k l => PrettyType (OT12 a b c d e f g h i j k l) where
  prettyType =
    if
        | rep == getRep @OTNAny ->
          "OTNAny"
        | otherwise ->
          assert False $ -- should hit OTNAny
            "OTN '["
              ++ show (getRep @a)
              ++ ", "
              ++ show (getRep @b)
              ++ ", "
              ++ show (getRep @c)
              ++ ", "
              ++ show (getRep @d)
              ++ ", "
              ++ show (getRep @e)
              ++ ", "
              ++ show (getRep @f)
              ++ ", "
              ++ show (getRep @g)
              ++ ", "
              ++ show (getRep @h)
              ++ ","
              ++ show (getRep @i)
              ++ ","
              ++ show (getRep @j)
              ++ ","
              ++ show (getRep @k)
              ++ ","
              ++ show (getRep @l)
              ++ "]"
   where
    rep = getRep @(OT12 a b c d e f g h i j k l)

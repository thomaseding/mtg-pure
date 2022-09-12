{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.PrettyType (
  PrettyType (..),
) where

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
import safe Data.Typeable (Typeable, typeRep)
import safe MtgPure.Model.IsObjectType (IsObjectType (litObjectType))
import safe MtgPure.Model.ObjectType (
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
  ObjectType (..),
 )
import safe MtgPure.Model.ObjectType.Kind (
  OTAny,
  OTArtifactCreature,
  OTCreaturePlaneswalker,
  OTCreaturePlayer,
  OTCreaturePlayerPlaneswalker,
  OTDamageSource,
  OTEnchantmentCreature,
  OTPermanent,
  OTPlayerPlaneswalker,
  OTSpell,
 )

class Typeable ty => PrettyType ty where
  prettyType :: Proxy ty -> String

instance IsObjectType a => PrettyType a where
  prettyType = show . litObjectType

instance IsObjectType a => PrettyType (OT1 a) where
  prettyType _ = case litObjectType (Proxy @a) of
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
  prettyType proxy =
    if
        | rep == typeRep (Proxy @OTArtifactCreature) ->
          "OTArtifactCreature"
        | rep == typeRep (Proxy @OTCreaturePlaneswalker) ->
          "OTCreaturePlaneswalker"
        | rep == typeRep (Proxy @OTCreaturePlayer) ->
          "OTCreaturePlayer"
        | rep == typeRep (Proxy @OTEnchantmentCreature) ->
          "OTEnchantmentCreature"
        | rep == typeRep (Proxy @OTPlayerPlaneswalker) ->
          "OTPlayerPlaneswalker"
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ")"
   where
    rep = typeRep proxy

instance Inst3 IsObjectType a b c => PrettyType (OT3 a b c) where
  prettyType proxy =
    if
        | rep == typeRep (Proxy @OTCreaturePlayerPlaneswalker) ->
          "OTCreaturePlayerPlaneswalker"
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ")"
   where
    rep = typeRep proxy

instance Inst4 IsObjectType a b c d => PrettyType (OT4 a b c d) where
  prettyType proxy =
    if
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ")"
   where
    _rep = typeRep proxy

instance Inst5 IsObjectType a b c d e => PrettyType (OT5 a b c d e) where
  prettyType proxy =
    if
        | rep == typeRep (Proxy @OTPermanent) ->
          "OTPermanent"
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ")"
   where
    rep = typeRep proxy

instance Inst6 IsObjectType a b c d e f => PrettyType (OT6 a b c d e f) where
  prettyType proxy =
    if
        | rep == typeRep (Proxy @OTSpell) ->
          "OTSpell"
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ", "
            ++ show (typeRep (Proxy @f))
            ++ ")"
   where
    rep = typeRep proxy

instance Inst7 IsObjectType a b c d e f g => PrettyType (OT7 a b c d e f g) where
  prettyType proxy =
    if
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ", "
            ++ show (typeRep (Proxy @f))
            ++ ", "
            ++ show (typeRep (Proxy @g))
            ++ ")"
   where
    _rep = typeRep proxy

instance Inst8 IsObjectType a b c d e f g h => PrettyType (OT8 a b c d e f g h) where
  prettyType proxy =
    if
        | rep == typeRep (Proxy @OTDamageSource) ->
          "OTDamageSource"
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ", "
            ++ show (typeRep (Proxy @f))
            ++ ", "
            ++ show (typeRep (Proxy @g))
            ++ ", "
            ++ show (typeRep (Proxy @h))
            ++ ")"
   where
    rep = typeRep proxy

instance Inst9 IsObjectType a b c d e f g h i => PrettyType (OT9 a b c d e f g h i) where
  prettyType proxy =
    if
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ", "
            ++ show (typeRep (Proxy @f))
            ++ ", "
            ++ show (typeRep (Proxy @g))
            ++ ", "
            ++ show (typeRep (Proxy @h))
            ++ ","
            ++ show (typeRep (Proxy @i))
            ++ ")"
   where
    _rep = typeRep proxy

instance Inst10 IsObjectType a b c d e f g h i j => PrettyType (OT10 a b c d e f g h i j) where
  prettyType proxy =
    if
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ", "
            ++ show (typeRep (Proxy @f))
            ++ ", "
            ++ show (typeRep (Proxy @g))
            ++ ", "
            ++ show (typeRep (Proxy @h))
            ++ ","
            ++ show (typeRep (Proxy @i))
            ++ ","
            ++ show (typeRep (Proxy @j))
            ++ ")"
   where
    _rep = typeRep proxy

instance Inst11 IsObjectType a b c d e f g h i j k => PrettyType (OT11 a b c d e f g h i j k) where
  prettyType proxy =
    if
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ", "
            ++ show (typeRep (Proxy @f))
            ++ ", "
            ++ show (typeRep (Proxy @g))
            ++ ", "
            ++ show (typeRep (Proxy @h))
            ++ ","
            ++ show (typeRep (Proxy @i))
            ++ ","
            ++ show (typeRep (Proxy @j))
            ++ ","
            ++ show (typeRep (Proxy @k))
            ++ ")"
   where
    _rep = typeRep proxy

instance Inst12 IsObjectType a b c d e f g h i j k l => PrettyType (OT12 a b c d e f g h i j k l) where
  prettyType proxy =
    if
        | rep == typeRep (Proxy @OTAny) ->
          "OTAny"
        | otherwise ->
          "OT '( '(), "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ", "
            ++ show (typeRep (Proxy @f))
            ++ ", "
            ++ show (typeRep (Proxy @g))
            ++ ", "
            ++ show (typeRep (Proxy @h))
            ++ ","
            ++ show (typeRep (Proxy @i))
            ++ ","
            ++ show (typeRep (Proxy @j))
            ++ ","
            ++ show (typeRep (Proxy @k))
            ++ ","
            ++ show (typeRep (Proxy @l))
            ++ ")"
   where
    rep = typeRep proxy

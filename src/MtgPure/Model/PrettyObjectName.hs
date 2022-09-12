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

module MtgPure.Model.PrettyObjectName
  ( PrettyObjectName (..),
  )
where

import Data.Inst
  ( Inst10,
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
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import MtgPure.Model.IsObjectType (IsObjectType (singObjectType))
import MtgPure.Model.ObjectN
  ( OAny,
    OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    OPermanent,
    OPlayerPlaneswalker,
    OSpell,
    ObjectN (..),
  )
import MtgPure.Model.ObjectType (ObjectType (..))
import MtgPure.Model.ObjectType.Kind
  ( OTCreaturePlaneswalker,
    OTCreaturePlayer,
    OTCreaturePlayerPlaneswalker,
    OTPermanent,
    OTPlayerPlaneswalker,
  )

class PrettyObjectName a where
  prettyObjectName :: Proxy a -> String

instance IsObjectType a => PrettyObjectName a where
  prettyObjectName = show . singObjectType

instance IsObjectType a => PrettyObjectName (ObjectN a) where
  prettyObjectName _ = case singObjectType (Proxy @a) of
    OTActivatedAbility -> "OActivatedAbility"
    OTArtifact -> "OArtifact"
    OTCreature -> "OCreature"
    OTEmblem -> "OEmblem"
    OTEnchantment -> "OEnchantment"
    OTInstant -> "OInstant"
    OTLand -> "OLand"
    OTPlaneswalker -> "OPlaneswalker"
    OTPlayer -> "OPlayer"
    OTSorcery -> "OSorcery"
    OTStaticAbility -> "OStaticAbility"
    OTTriggeredAbility -> "OTriggeredAbility"

instance Inst2 IsObjectType a b => PrettyObjectName '(a, b) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OTCreaturePlaneswalker) -> "OTCreaturePlaneswalker"
        | rep == typeRep (Proxy @OTCreaturePlayer) -> "OTCreaturePlayer"
        | rep == typeRep (Proxy @OTPlayerPlaneswalker) -> "OTPlayerPlaneswalker"
        | otherwise ->
          "'( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ")"
    where
      rep = typeRep proxy

instance Inst2 IsObjectType a b => PrettyObjectName (ObjectN '(a, b)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OCreaturePlaneswalker) -> "OCreaturePlaneswalker"
        | rep == typeRep (Proxy @OCreaturePlayer) -> "OCreaturePlayer"
        | rep == typeRep (Proxy @OPlayerPlaneswalker) -> "OPlayerPlaneswalker"
        | otherwise ->
          "ObjectN '( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ")"
    where
      rep = typeRep proxy

instance Inst3 IsObjectType a b c => PrettyObjectName '(a, b, c) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OTCreaturePlayerPlaneswalker) -> "OTCreaturePlayerPlaneswalker"
        | otherwise ->
          "'( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ")"
    where
      rep = typeRep proxy

instance Inst3 IsObjectType a b c => PrettyObjectName (ObjectN '(a, b, c)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OCreaturePlayerPlaneswalker) -> "OCreaturePlayerPlaneswalker"
        | otherwise ->
          "ObjectN '( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ")"
    where
      rep = typeRep proxy

instance Inst4 IsObjectType a b c d => PrettyObjectName '(a, b, c, d) where
  prettyObjectName proxy =
    if
        | otherwise ->
          "'( "
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

instance Inst4 IsObjectType a b c d => PrettyObjectName (ObjectN '(a, b, c, d)) where
  prettyObjectName proxy =
    if
        | otherwise ->
          "ObjectN '( "
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

instance Inst5 IsObjectType a b c d e => PrettyObjectName '(a, b, c, d, e) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OTPermanent) -> "OTPermanent"
        | otherwise ->
          "'( "
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

instance Inst5 IsObjectType a b c d e => PrettyObjectName (ObjectN '(a, b, c, d, e)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OPermanent) -> "OPermanent"
        | otherwise ->
          "ObjectN '( "
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

instance Inst6 IsObjectType a b c d e f => PrettyObjectName (ObjectN '(a, b, c, d, e, f)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OSpell) -> "OSpell"
        | otherwise ->
          "ObjectN '( "
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

instance Inst7 IsObjectType a b c d e f g => PrettyObjectName (ObjectN '(a, b, c, d, e, f, g)) where
  prettyObjectName proxy =
    if
        | otherwise ->
          "ObjectN '( "
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

instance Inst8 IsObjectType a b c d e f g h => PrettyObjectName (ObjectN '(a, b, c, d, e, f, g, h)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OAny) -> "ODamageSource"
        | otherwise ->
          "ObjectN '( "
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

instance Inst9 IsObjectType a b c d e f g h i => PrettyObjectName (ObjectN '(a, b, c, d, e, f, g, h, i)) where
  prettyObjectName proxy =
    if
        | otherwise ->
          "ObjectN '( "
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

instance Inst10 IsObjectType a b c d e f g h i j => PrettyObjectName (ObjectN '(a, b, c, d, e, f, g, h, i, j)) where
  prettyObjectName proxy =
    if
        | otherwise ->
          "ObjectN '( "
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

instance Inst11 IsObjectType a b c d e f g h i j k => PrettyObjectName (ObjectN '(a, b, c, d, e, f, g, h, i, j, k)) where
  prettyObjectName proxy =
    if
        | otherwise ->
          "ObjectN '( "
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

instance Inst12 IsObjectType a b c d e f g h i j k l => PrettyObjectName (ObjectN '(a, b, c, d, e, f, g, h, i, j, k, l)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OAny) -> "OAny"
        | otherwise ->
          "ObjectN '( "
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

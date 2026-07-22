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
  Inst13,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
  Inst7,
  Inst8,
  Inst9,
 )
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy (Proxy))
import safe Data.Typeable (TypeRep, Typeable, typeRep)
import safe GHC.TypeNats (Nat)
import safe MtgPure.Model.Object.IsObjectType (
  IsObjectType (litObjectType),
 )
import safe MtgPure.Model.Object.OT (OT)
import safe MtgPure.Model.Object.OTN (
  OT0,
  OT1,
  OT10,
  OT11,
  OT12,
  OT13,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OT8,
  OT9,
  OTN,
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

class (Typeable ty) => PrettyType ty where
  prettyType :: String

instance (IsObjectType a, Typeable a) => PrettyType a where
  prettyType :: (IsObjectType a) => String
  prettyType = show (litObjectType @a)

getRep :: forall a. (Typeable a) => TypeRep
getRep = typeRep (Proxy @a)

-- | Length of an @OTN@'s promoted object-type list, e.g. @OTKArity (OTKPermanent) ~ 6@.
-- Enumerated rather than recursive: the longest alias (@OTNAny@) has 13 elements,
-- matching the largest @OTn@ instance below, so there's no need to support more.
type family OTKArity (xs :: [OT]) :: Nat where
  OTKArity '[] = 0
  OTKArity '[_] = 1
  OTKArity '[_, _] = 2
  OTKArity '[_, _, _] = 3
  OTKArity '[_, _, _, _] = 4
  OTKArity '[_, _, _, _, _] = 5
  OTKArity '[_, _, _, _, _, _] = 6
  OTKArity '[_, _, _, _, _, _, _] = 7
  OTKArity '[_, _, _, _, _, _, _, _] = 8
  OTKArity '[_, _, _, _, _, _, _, _, _] = 9
  OTKArity '[_, _, _, _, _, _, _, _, _, _] = 10
  OTKArity '[_, _, _, _, _, _, _, _, _, _, _] = 11
  OTKArity '[_, _, _, _, _, _, _, _, _, _, _, _] = 12
  OTKArity '[_, _, _, _, _, _, _, _, _, _, _, _, _] = 13

-- | Arity of an @OTNXxx@ alias, e.g. @OTNArity OTNPermanent ~ 6@.
--
-- Each @PrettyType (OTn ...)@ instance below defines a local @getRepN@ pinned
-- to its own @n@ via this family, so comparing an alias against the wrong
-- instance's @rep@ (e.g. checking a 7-element alias from the @OT6@ instance)
-- is a type error instead of a silently-never-matching runtime comparison.
type family OTNArity (ty :: Type) :: Nat where
  OTNArity (OTN otk) = OTKArity otk

instance PrettyType OT0 where
  prettyType :: String
  prettyType = "OT0"

instance (IsObjectType a) => PrettyType (OT1 a) where
  prettyType :: (IsObjectType a) => String
  prettyType = "OT1 " ++ show (getRep @a)

instance (Inst2 IsObjectType a b) => PrettyType (OT2 a b) where
  prettyType :: (Inst2 IsObjectType a b) => String
  prettyType =
    if
      | rep == getRep2 @OTNArtifactCreature ->
          "OTNArtifactCreature"
      | rep == getRep2 @OTNCreaturePlaneswalker ->
          "OTNCreaturePlaneswalker"
      | rep == getRep2 @OTNCreaturePlayer ->
          "OTNCreaturePlayer"
      | rep == getRep2 @OTNEnchantmentCreature ->
          "OTNEnchantmentCreature"
      | rep == getRep2 @OTNPlayerPlaneswalker ->
          "OTNPlayerPlaneswalker"
      | otherwise ->
          "OT2 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
   where
    rep = getRep @(OT2 a b)
    getRep2 :: forall x. (Typeable x, OTNArity x ~ 2) => TypeRep
    getRep2 = getRep @x

instance (Inst3 IsObjectType a b c) => PrettyType (OT3 a b c) where
  prettyType :: (Inst3 IsObjectType a b c) => String
  prettyType =
    if
      | rep == getRep3 @OTNCreaturePlayerPlaneswalker ->
          "OTNCreaturePlayerPlaneswalker"
      | otherwise ->
          "OT3 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
   where
    rep = getRep @(OT3 a b c)
    getRep3 :: forall x. (Typeable x, OTNArity x ~ 3) => TypeRep
    getRep3 = getRep @x

instance (Inst4 IsObjectType a b c d) => PrettyType (OT4 a b c d) where
  prettyType :: (Inst4 IsObjectType a b c d) => String
  prettyType =
    if
      | otherwise ->
          "OT4 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
   where
    _rep = getRep @(OT4 a b c d)
    _getRep4 :: forall x. (Typeable x, OTNArity x ~ 4) => TypeRep
    _getRep4 = getRep @x

instance (Inst5 IsObjectType a b c d e) => PrettyType (OT5 a b c d e) where
  prettyType :: (Inst5 IsObjectType a b c d e) => String
  prettyType =
    if
      | otherwise ->
          "OT5 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
   where
    _rep = getRep @(OT5 a b c d e)
    _getRep5 :: forall x. (Typeable x, OTNArity x ~ 5) => TypeRep
    _getRep5 = getRep @x

instance (Inst6 IsObjectType a b c d e f) => PrettyType (OT6 a b c d e f) where
  prettyType :: (Inst6 IsObjectType a b c d e f) => String
  prettyType =
    if
      | rep == getRep6 @OTNPermanent ->
          "OTNPermanent"
      | otherwise ->
          "OT6 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
            ++ " "
            ++ show (getRep @f)
   where
    rep = getRep @(OT6 a b c d e f)
    getRep6 :: forall x. (Typeable x, OTNArity x ~ 6) => TypeRep
    getRep6 = getRep @x

instance (Inst7 IsObjectType a b c d e f g) => PrettyType (OT7 a b c d e f g) where
  prettyType :: (Inst7 IsObjectType a b c d e f g) => String
  prettyType =
    if
      | rep == getRep7 @OTNSpell ->
          "OTNSpell"
      | otherwise ->
          "OT7 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
            ++ " "
            ++ show (getRep @f)
            ++ " "
            ++ show (getRep @g)
   where
    rep = getRep @(OT7 a b c d e f g)
    getRep7 :: forall x. (Typeable x, OTNArity x ~ 7) => TypeRep
    getRep7 = getRep @x

instance (Inst8 IsObjectType a b c d e f g h) => PrettyType (OT8 a b c d e f g h) where
  prettyType :: (Inst8 IsObjectType a b c d e f g h) => String
  prettyType =
    if
      | otherwise ->
          "OT8 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
            ++ " "
            ++ show (getRep @f)
            ++ " "
            ++ show (getRep @g)
            ++ " "
            ++ show (getRep @h)
   where
    _rep = getRep @(OT8 a b c d e f g h)
    _getRep8 :: forall x. (Typeable x, OTNArity x ~ 8) => TypeRep
    _getRep8 = getRep @x

instance (Inst9 IsObjectType a b c d e f g h i) => PrettyType (OT9 a b c d e f g h i) where
  prettyType :: (Inst9 IsObjectType a b c d e f g h i) => String
  prettyType =
    if
      | rep == getRep9 @OTNDamageSource ->
          "OTNDamageSource"
      | otherwise ->
          "OT9 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
            ++ " "
            ++ show (getRep @f)
            ++ " "
            ++ show (getRep @g)
            ++ " "
            ++ show (getRep @h)
            ++ " "
            ++ show (getRep @i)
   where
    rep = getRep @(OT9 a b c d e f g h i)
    getRep9 :: forall x. (Typeable x, OTNArity x ~ 9) => TypeRep
    getRep9 = getRep @x

instance (Inst10 IsObjectType a b c d e f g h i j) => PrettyType (OT10 a b c d e f g h i j) where
  prettyType :: (Inst10 IsObjectType a b c d e f g h i j) => String
  prettyType =
    if
      | otherwise ->
          "OT10 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
            ++ " "
            ++ show (getRep @f)
            ++ " "
            ++ show (getRep @g)
            ++ " "
            ++ show (getRep @h)
            ++ " "
            ++ show (getRep @i)
            ++ " "
            ++ show (getRep @j)
   where
    _rep = getRep @(OT10 a b c d e f g h i j)
    _getRep10 :: forall x. (Typeable x, OTNArity x ~ 10) => TypeRep
    _getRep10 = getRep @x

instance (Inst11 IsObjectType a b c d e f g h i j k) => PrettyType (OT11 a b c d e f g h i j k) where
  prettyType :: (Inst11 IsObjectType a b c d e f g h i j k) => String
  prettyType =
    if
      | otherwise ->
          "OT11 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
            ++ " "
            ++ show (getRep @f)
            ++ " "
            ++ show (getRep @g)
            ++ " "
            ++ show (getRep @h)
            ++ " "
            ++ show (getRep @i)
            ++ " "
            ++ show (getRep @j)
            ++ " "
            ++ show (getRep @k)
   where
    _rep = getRep @(OT11 a b c d e f g h i j k)
    _getRep11 :: forall x. (Typeable x, OTNArity x ~ 11) => TypeRep
    _getRep11 = getRep @x

instance (Inst12 IsObjectType a b c d e f g h i j k l) => PrettyType (OT12 a b c d e f g h i j k l) where
  prettyType :: (Inst12 IsObjectType a b c d e f g h i j k l) => String
  prettyType =
    if
      | otherwise ->
          "OT12 "
            ++ show (getRep @a)
            ++ " "
            ++ show (getRep @b)
            ++ " "
            ++ show (getRep @c)
            ++ " "
            ++ show (getRep @d)
            ++ " "
            ++ show (getRep @e)
            ++ " "
            ++ show (getRep @f)
            ++ " "
            ++ show (getRep @g)
            ++ " "
            ++ show (getRep @h)
            ++ " "
            ++ show (getRep @i)
            ++ " "
            ++ show (getRep @j)
            ++ " "
            ++ show (getRep @k)
            ++ " "
            ++ show (getRep @l)
   where
    _rep = getRep @(OT12 a b c d e f g h i j k l)
    _getRep12 :: forall x. (Typeable x, OTNArity x ~ 12) => TypeRep
    _getRep12 = getRep @x

instance (Inst13 IsObjectType a b c d e f g h i j k l m) => PrettyType (OT13 a b c d e f g h i j k l m) where
  prettyType :: (Inst13 IsObjectType a b c d e f g h i j k l m) => String
  prettyType =
    if
      | rep == getRep13 @OTNAny ->
          "OTNAny"
      | otherwise ->
          assert False $ -- should hit OTNAny
            "OT13 "
              ++ show (getRep @a)
              ++ " "
              ++ show (getRep @b)
              ++ " "
              ++ show (getRep @c)
              ++ " "
              ++ show (getRep @d)
              ++ " "
              ++ show (getRep @e)
              ++ " "
              ++ show (getRep @f)
              ++ " "
              ++ show (getRep @g)
              ++ " "
              ++ show (getRep @h)
              ++ " "
              ++ show (getRep @i)
              ++ " "
              ++ show (getRep @j)
              ++ " "
              ++ show (getRep @k)
              ++ " "
              ++ show (getRep @l)
              ++ " "
              ++ show (getRep @m)
   where
    rep = getRep @(OT13 a b c d e f g h i j k l m)
    getRep13 :: forall x. (Typeable x, OTNArity x ~ 13) => TypeRep
    getRep13 = getRep @x

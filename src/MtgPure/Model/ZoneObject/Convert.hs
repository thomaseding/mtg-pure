{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}

module MtgPure.Model.ZoneObject.Convert (
  zo1ToO,
  ToZO0 (..),
  toZO1,
  toZO2,
  toZO3,
  toZO4,
  toZO5,
  toZO6,
  toZO7,
  toZO8,
  toZO9,
  toZO10,
  toZO11,
  toZO12,
  castOToON,
  oToZO1,
  AsActivatedOrTriggeredAbility,
  AsAny,
  AsCard,
  AsCreaturePlayerPlaneswalker,
  AsDamageSource,
  AsPermanent,
  AsSpell',
  AsSpell,
  asActivatedOrTriggeredAbility,
  asAny,
  asCard,
  asCreaturePlayerPlaneswalker,
  asDamageSource,
  asPermanent,
  asSpell,
  zo0ToAny,
  zo0ToCard,
  zo0ToPermanent,
  zo0ToSpell,
) where

import safe Data.Inst (
  Inst1,
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
import safe Data.Kind (Type)
import safe Data.Monoid (First (..))
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.LitOTN (LitOTN (..))
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
  OTNActivatedOrTriggeredAbility,
  OTNAny,
  OTNCard,
  OTNCreaturePlayerPlaneswalker,
  OTNDamageSource,
  OTNPermanent,
  OTNSpell,
 )
import safe MtgPure.Model.Object.OTN_ (OTN' (..))
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  GetObjectId (..),
  ObjectId (..),
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (
  ObjectN,
  mkO1,
  mkO10a,
  mkO10b,
  mkO10c,
  mkO10d,
  mkO10e,
  mkO10f,
  mkO10g,
  mkO10h,
  mkO10i,
  mkO10j,
  mkO11a,
  mkO11b,
  mkO11c,
  mkO11d,
  mkO11e,
  mkO11f,
  mkO11g,
  mkO11h,
  mkO11i,
  mkO11j,
  mkO11k,
  mkO12a,
  mkO12b,
  mkO12c,
  mkO12d,
  mkO12e,
  mkO12f,
  mkO12g,
  mkO12h,
  mkO12i,
  mkO12j,
  mkO12k,
  mkO12l,
  mkO2a,
  mkO2b,
  mkO3a,
  mkO3b,
  mkO3c,
  mkO4a,
  mkO4b,
  mkO4c,
  mkO4d,
  mkO5a,
  mkO5b,
  mkO5c,
  mkO5d,
  mkO5e,
  mkO6a,
  mkO6b,
  mkO6c,
  mkO6d,
  mkO6e,
  mkO6f,
  mkO7a,
  mkO7b,
  mkO7c,
  mkO7d,
  mkO7e,
  mkO7f,
  mkO7g,
  mkO8a,
  mkO8b,
  mkO8c,
  mkO8d,
  mkO8e,
  mkO8f,
  mkO8g,
  mkO8h,
  mkO9a,
  mkO9b,
  mkO9c,
  mkO9d,
  mkO9e,
  mkO9f,
  mkO9g,
  mkO9h,
  mkO9i,
 )
import safe MtgPure.Model.Object.ObjectN_ (ObjectN' (..))
import safe MtgPure.Model.Object.ObjectType (
  ObjectType (..),
 )
import safe MtgPure.Model.Object.SObjectType (
  SObjectType (..),
 )
import safe MtgPure.Model.Object.ToObjectN.Classes (
  ToObject1 (..),
  ToObject10 (..),
  ToObject11 (..),
  ToObject12 (..),
  ToObject2 (..),
  ToObject3 (..),
  ToObject4 (..),
  ToObject5 (..),
  ToObject6 (..),
  ToObject6',
  ToObject7 (..),
  ToObject8 (..),
  ToObject9 (..),
 )
import safe MtgPure.Model.Object.VisitObjectN (visitObjectN')
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, ZO, ZoneObject (..), toZone, zoToObjectN)

zo1ToO :: (IsObjectType a, IsZone zone) => ZO zone (OT1 a) -> Object a
zo1ToO zo = case zoToObjectN zo of
  O1 o -> o

class ToZO0 (zone :: Zone) (object :: Type) where
  toZO0 :: object -> ZO zone OT0

instance IsZone zone => ToZO0 zone ObjectId where
  toZO0 = toZone . O0 . UntypedObject DefaultObjectDiscriminant

instance IsZone zone => ToZO0 zone (Object a) where
  toZO0 = toZO0 . getObjectId

instance IsZone zone => ToZO0 zone (ObjectN ot) where
  toZO0 = toZO0 . visitObjectN' getObjectId

instance IsZone zone => ToZO0 zone (ZO zone ot) where
  toZO0 = toZO0 . zoToObjectN

toZO1 :: ToObject1 ot a => ZO zone ot -> ZO zone (OT1 a)
toZO1 = \case
  ZO sZone o -> ZO sZone $ toObject1 o

toZO2 :: ToObject2 ot a b => ZO zone ot -> ZO zone (OT2 a b)
toZO2 = \case
  ZO sZone o -> ZO sZone $ toObject2 o

toZO3 :: ToObject3 ot a b c => ZO zone ot -> ZO zone (OT3 a b c)
toZO3 = \case
  ZO sZone o -> ZO sZone $ toObject3 o

toZO4 :: ToObject4 ot a b c d => ZO zone ot -> ZO zone (OT4 a b c d)
toZO4 = \case
  ZO sZone o -> ZO sZone $ toObject4 o

toZO5 :: ToObject5 ot a b c d e => ZO zone ot -> ZO zone (OT5 a b c d e)
toZO5 = \case
  ZO sZone o -> ZO sZone $ toObject5 o

toZO6 :: ToObject6 ot a b c d e f => ZO zone ot -> ZO zone (OT6 a b c d e f)
toZO6 = \case
  ZO sZone o -> ZO sZone $ toObject6 o

toZO7 ::
  ToObject7 ot a b c d e f g => ZO zone ot -> ZO zone (OT7 a b c d e f g)
toZO7 = \case
  ZO sZone o -> ZO sZone $ toObject7 o

toZO8 ::
  ToObject8 ot a b c d e f g h => ZO zone ot -> ZO zone (OT8 a b c d e f g h)
toZO8 = \case
  ZO sZone o -> ZO sZone $ toObject8 o

toZO9 ::
  ToObject9 ot a b c d e f g h i =>
  ZO zone ot ->
  ZO zone (OT9 a b c d e f g h i)
toZO9 = \case
  ZO sZone o -> ZO sZone $ toObject9 o

toZO10 ::
  ToObject10 ot a b c d e f g h i j =>
  ZO zone ot ->
  ZO zone (OT10 a b c d e f g h i j)
toZO10 = \case
  ZO sZone o -> ZO sZone $ toObject10 o

toZO11 ::
  ToObject11 ot a b c d e f g h i j k =>
  ZO zone ot ->
  ZO zone (OT11 a b c d e f g h i j k)
toZO11 = \case
  ZO sZone o -> ZO sZone $ toObject11 o

toZO12 ::
  ToObject12 ot a b c d e f g h i j k l =>
  ZO zone ot ->
  ZO zone (OT12 a b c d e f g h i j k l)
toZO12 = \case
  ZO sZone o -> ZO sZone $ toObject12 o

oToZO1 :: (IsZone zone, IsObjectType a) => Object a -> ZO zone (OT1 a)
oToZO1 = toZone . O1

-- NOTE: Wrapper to get higher kinded types to type-check in `mapOTN`.
newtype MaybeObjectN (ot :: Type) = MaybeObjectN {unMaybeObjectN :: Maybe (ObjectN ot)}

castOToON :: forall z ot. (IsObjectType z, IsOTN ot) => Object z -> Maybe (ObjectN ot)
castOToON o = objN
 where
  objN :: Maybe (ObjectN ot)
  objN = unMaybeObjectN $
    mapOTN @ot $ \case
      OT0 -> MaybeObjectN $
        Just $ O0 case o of
          Object _ u -> u
      ot@OT1 ->
        let go :: forall a. Inst1 IsObjectType a => OT1 a -> MaybeObjectN (OT1 a)
            go _ = MaybeObjectN $ goCast $ mkO1 @a
         in go ot
      ot@OT2 ->
        let go :: forall a b. Inst2 IsObjectType a b => OT2 a b -> MaybeObjectN (OT2 a b)
            go _ =
              goConcat
                [ goCast $ mkO2a @a @b
                , goCast $ mkO2b @a @b
                ]
         in go ot
      ot@OT3 ->
        let go :: forall a b c. Inst3 IsObjectType a b c => OT3 a b c -> MaybeObjectN (OT3 a b c)
            go _ =
              goConcat
                [ goCast $ mkO3a @a @b @c
                , goCast $ mkO3b @a @b @c
                , goCast $ mkO3c @a @b @c
                ]
         in go ot
      ot@OT4 ->
        let go :: forall a b c d. Inst4 IsObjectType a b c d => OT4 a b c d -> MaybeObjectN (OT4 a b c d)
            go _ =
              goConcat
                [ goCast $ mkO4a @a @b @c @d
                , goCast $ mkO4b @a @b @c @d
                , goCast $ mkO4c @a @b @c @d
                , goCast $ mkO4d @a @b @c @d
                ]
         in go ot
      ot@OT5 ->
        let go :: forall a b c d e. Inst5 IsObjectType a b c d e => OT5 a b c d e -> MaybeObjectN (OT5 a b c d e)
            go _ =
              goConcat
                [ goCast $ mkO5a @a @b @c @d @e
                , goCast $ mkO5b @a @b @c @d @e
                , goCast $ mkO5c @a @b @c @d @e
                , goCast $ mkO5d @a @b @c @d @e
                , goCast $ mkO5e @a @b @c @d @e
                ]
         in go ot
      ot@OT6 ->
        let go :: forall a b c d e f. Inst6 IsObjectType a b c d e f => OT6 a b c d e f -> MaybeObjectN (OT6 a b c d e f)
            go _ =
              goConcat
                [ goCast $ mkO6a @a @b @c @d @e @f
                , goCast $ mkO6b @a @b @c @d @e @f
                , goCast $ mkO6c @a @b @c @d @e @f
                , goCast $ mkO6d @a @b @c @d @e @f
                , goCast $ mkO6e @a @b @c @d @e @f
                , goCast $ mkO6f @a @b @c @d @e @f
                ]
         in go ot
      ot@OT7 ->
        let go :: forall a b c d e f g. Inst7 IsObjectType a b c d e f g => OT7 a b c d e f g -> MaybeObjectN (OT7 a b c d e f g)
            go _ =
              goConcat
                [ goCast $ mkO7a @a @b @c @d @e @f @g
                , goCast $ mkO7b @a @b @c @d @e @f @g
                , goCast $ mkO7c @a @b @c @d @e @f @g
                , goCast $ mkO7d @a @b @c @d @e @f @g
                , goCast $ mkO7e @a @b @c @d @e @f @g
                , goCast $ mkO7f @a @b @c @d @e @f @g
                , goCast $ mkO7g @a @b @c @d @e @f @g
                ]
         in go ot
      ot@OT8 ->
        let go :: forall a b c d e f g h. Inst8 IsObjectType a b c d e f g h => OT8 a b c d e f g h -> MaybeObjectN (OT8 a b c d e f g h)
            go _ =
              goConcat
                [ goCast $ mkO8a @a @b @c @d @e @f @g @h
                , goCast $ mkO8b @a @b @c @d @e @f @g @h
                , goCast $ mkO8c @a @b @c @d @e @f @g @h
                , goCast $ mkO8d @a @b @c @d @e @f @g @h
                , goCast $ mkO8e @a @b @c @d @e @f @g @h
                , goCast $ mkO8f @a @b @c @d @e @f @g @h
                , goCast $ mkO8g @a @b @c @d @e @f @g @h
                , goCast $ mkO8h @a @b @c @d @e @f @g @h
                ]
         in go ot
      ot@OT9 ->
        let go :: forall a b c d e f g h i. Inst9 IsObjectType a b c d e f g h i => OT9 a b c d e f g h i -> MaybeObjectN (OT9 a b c d e f g h i)
            go _ =
              goConcat
                [ goCast $ mkO9a @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9b @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9c @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9d @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9e @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9f @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9g @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9h @a @b @c @d @e @f @g @h @i
                , goCast $ mkO9i @a @b @c @d @e @f @g @h @i
                ]
         in go ot
      ot@OT10 ->
        let go :: forall a b c d e f g h i j. Inst10 IsObjectType a b c d e f g h i j => OT10 a b c d e f g h i j -> MaybeObjectN (OT10 a b c d e f g h i j)
            go _ =
              goConcat
                [ goCast $ mkO10a @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10b @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10c @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10d @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10e @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10f @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10g @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10h @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10i @a @b @c @d @e @f @g @h @i @j
                , goCast $ mkO10j @a @b @c @d @e @f @g @h @i @j
                ]
         in go ot
      ot@OT11 ->
        let go :: forall a b c d e f g h i j k. Inst11 IsObjectType a b c d e f g h i j k => OT11 a b c d e f g h i j k -> MaybeObjectN (OT11 a b c d e f g h i j k)
            go _ =
              goConcat
                [ goCast $ mkO11a @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11b @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11c @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11d @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11e @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11f @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11g @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11h @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11i @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11j @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ mkO11k @a @b @c @d @e @f @g @h @i @j @k
                ]
         in go ot
      ot@OT12 ->
        let go :: forall a b c d e f g h i j k l. Inst12 IsObjectType a b c d e f g h i j k l => OT12 a b c d e f g h i j k l -> MaybeObjectN (OT12 a b c d e f g h i j k l)
            go _ =
              goConcat
                [ goCast $ mkO12a @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12b @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12c @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12d @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12e @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12f @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12g @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12h @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12i @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12j @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12k @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ mkO12l @a @b @c @d @e @f @g @h @i @j @k @l
                ]
         in go ot
   where
    goConcat = MaybeObjectN . getFirst . mconcat . map First

    goCast :: Typeable z' => (Object z' -> ObjectN ot') -> Maybe (ObjectN ot')
    goCast f = f <$> cast o

type AsActivatedOrTriggeredAbility ot =
  ToObject2 ot 'OTActivatedAbility 'OTTriggeredAbility

type AsCreaturePlayerPlaneswalker ot =
  ToObject3 ot 'OTCreature 'OTPlaneswalker 'OTPlayer

type AsPermanent ot =
  ToObject5
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTLand
    'OTPlaneswalker

type AsSpell' ot =
  ToObject6'
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTPlaneswalker
    'OTSorcery

type AsSpell ot =
  ToObject6
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTPlaneswalker
    'OTSorcery

type AsCard ot =
  ToObject7
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTLand
    'OTPlaneswalker
    'OTSorcery

type AsDamageSource ot =
  ToObject8
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTInstant
    'OTLand
    'OTPlaneswalker
    'OTPlayer
    'OTSorcery

type AsAny ot =
  ToObject12
    ot
    'OTActivatedAbility
    'OTArtifact
    'OTCreature
    'OTEmblem
    'OTEnchantment
    'OTInstant
    'OTLand
    'OTPlaneswalker
    'OTPlayer
    'OTSorcery
    'OTStaticAbility
    'OTTriggeredAbility

asActivatedOrTriggeredAbility ::
  AsActivatedOrTriggeredAbility ot =>
  ZO zone ot ->
  ZO zone OTNActivatedOrTriggeredAbility
asActivatedOrTriggeredAbility = toZO2

asCreaturePlayerPlaneswalker ::
  AsCreaturePlayerPlaneswalker ot =>
  ZO zone ot ->
  ZO zone OTNCreaturePlayerPlaneswalker
asCreaturePlayerPlaneswalker = toZO3

asPermanent :: AsPermanent ot => ZO zone ot -> ZO zone OTNPermanent
asPermanent = toZO5

asSpell :: AsSpell ot => ZO zone ot -> ZO zone OTNSpell
asSpell = toZO6

asCard :: AsCard ot => ZO zone ot -> ZO zone OTNCard
asCard = toZO7

asDamageSource :: AsDamageSource ot => ZO zone ot -> ZO zone OTNDamageSource
asDamageSource = toZO8

asAny :: AsAny ot => ZO zone ot -> ZO zone OTNAny
asAny = toZO12

zo0ToPermanent :: ZO 'ZBattlefield OT0 -> ZO 'ZBattlefield OTNPermanent
zo0ToPermanent = asPermanent . ZO SZBattlefield . O1 . Object SLand . getUntypedObject

zo0ToSpell :: forall zone. IsZone zone => ZO zone OT0 -> ZO zone OTNSpell
zo0ToSpell = asSpell . ZO (singZone @zone) . O1 . Object SArtifact . getUntypedObject

zo0ToCard :: forall zone. IsZone zone => ZO zone OT0 -> ZO zone OTNCard
zo0ToCard = asCard . ZO (singZone @zone) . O1 . Object SLand . getUntypedObject

zo0ToAny :: forall zone. IsZone zone => ZO zone OT0 -> ZO zone OTNAny
zo0ToAny = asAny . ZO (singZone @zone) . O1 . Object SLand . getUntypedObject

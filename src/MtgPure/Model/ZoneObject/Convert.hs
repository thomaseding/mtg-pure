{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use lambda-case" #-}

module MtgPure.Model.ZoneObject.Convert (
  zo1ToO,
  toZO0,
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
import safe MtgPure.Model.Object.LitOT (LitOT (..))
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
  OTActivatedOrTriggeredAbility,
  OTAny,
  OTCard,
  OTCreaturePlayerPlaneswalker,
  OTDamageSource,
  OTPermanent,
  OTSpell,
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
import safe MtgPure.Model.Object.ObjectN (ObjectN (..))
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
import safe MtgPure.Model.Object.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOT, ZO, ZoneObject (..), toZone, zoToObjectN)

zo1ToO :: (IsObjectType a, IsZone zone) => ZO zone (OT1 a) -> Object a
zo1ToO zo = case zoToObjectN zo of
  O1 o -> o

class ToZO0 (zone :: Zone) (object :: Type) where
  toZO0 :: object -> ZO zone OT0

instance IsZone zone => ToZO0 zone ObjectId where
  toZO0 = toZone . O0 . UntypedObject DefaultObjectDiscriminant

instance IsZone zone => ToZO0 zone (Object a) where
  toZO0 = toZO0 . getObjectId

instance (VisitObjectN ot, IsZone zone) => ToZO0 zone (ObjectN ot) where
  toZO0 = toZO0 . visitObjectN' getObjectId

instance (VisitObjectN ot, IsZone zone) => ToZO0 zone (ZO zone ot) where
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

-- NOTE: Wrapper to get higher kinded types to type-check in `mapOT`.
newtype MaybeObjectN (ot :: Type) = MaybeObjectN {unMaybeObjectN :: Maybe (ObjectN ot)}

castOToON :: forall z ot. (IsObjectType z, IsOT ot) => Object z -> Maybe (ObjectN ot)
castOToON o = objN
 where
  objN :: Maybe (ObjectN ot)
  objN = unMaybeObjectN $
    mapOT @ot $ \case
      OT0 -> MaybeObjectN $
        Just $ O0 case o of
          Object _ u -> u
      ot@OT1 ->
        let go :: forall a. Inst1 IsObjectType a => OT1 a -> MaybeObjectN (OT1 a)
            go _ = MaybeObjectN $ goCast $ O1 @a
         in go ot
      ot@OT2 ->
        let go :: forall a b. Inst2 IsObjectType a b => OT2 a b -> MaybeObjectN (OT2 a b)
            go _ =
              goConcat
                [ goCast $ O2a @a @b
                , goCast $ O2b @a @b
                ]
         in go ot
      ot@OT3 ->
        let go :: forall a b c. Inst3 IsObjectType a b c => OT3 a b c -> MaybeObjectN (OT3 a b c)
            go _ =
              goConcat
                [ goCast $ O3a @a @b @c
                , goCast $ O3b @a @b @c
                , goCast $ O3c @a @b @c
                ]
         in go ot
      ot@OT4 ->
        let go :: forall a b c d. Inst4 IsObjectType a b c d => OT4 a b c d -> MaybeObjectN (OT4 a b c d)
            go _ =
              goConcat
                [ goCast $ O4a @a @b @c @d
                , goCast $ O4b @a @b @c @d
                , goCast $ O4c @a @b @c @d
                , goCast $ O4d @a @b @c @d
                ]
         in go ot
      ot@OT5 ->
        let go :: forall a b c d e. Inst5 IsObjectType a b c d e => OT5 a b c d e -> MaybeObjectN (OT5 a b c d e)
            go _ =
              goConcat
                [ goCast $ O5a @a @b @c @d @e
                , goCast $ O5b @a @b @c @d @e
                , goCast $ O5c @a @b @c @d @e
                , goCast $ O5d @a @b @c @d @e
                , goCast $ O5e @a @b @c @d @e
                ]
         in go ot
      ot@OT6 ->
        let go :: forall a b c d e f. Inst6 IsObjectType a b c d e f => OT6 a b c d e f -> MaybeObjectN (OT6 a b c d e f)
            go _ =
              goConcat
                [ goCast $ O6a @a @b @c @d @e @f
                , goCast $ O6b @a @b @c @d @e @f
                , goCast $ O6c @a @b @c @d @e @f
                , goCast $ O6d @a @b @c @d @e @f
                , goCast $ O6e @a @b @c @d @e @f
                , goCast $ O6f @a @b @c @d @e @f
                ]
         in go ot
      ot@OT7 ->
        let go :: forall a b c d e f g. Inst7 IsObjectType a b c d e f g => OT7 a b c d e f g -> MaybeObjectN (OT7 a b c d e f g)
            go _ =
              goConcat
                [ goCast $ O7a @a @b @c @d @e @f @g
                , goCast $ O7b @a @b @c @d @e @f @g
                , goCast $ O7c @a @b @c @d @e @f @g
                , goCast $ O7d @a @b @c @d @e @f @g
                , goCast $ O7e @a @b @c @d @e @f @g
                , goCast $ O7f @a @b @c @d @e @f @g
                , goCast $ O7g @a @b @c @d @e @f @g
                ]
         in go ot
      ot@OT8 ->
        let go :: forall a b c d e f g h. Inst8 IsObjectType a b c d e f g h => OT8 a b c d e f g h -> MaybeObjectN (OT8 a b c d e f g h)
            go _ =
              goConcat
                [ goCast $ O8a @a @b @c @d @e @f @g @h
                , goCast $ O8b @a @b @c @d @e @f @g @h
                , goCast $ O8c @a @b @c @d @e @f @g @h
                , goCast $ O8d @a @b @c @d @e @f @g @h
                , goCast $ O8e @a @b @c @d @e @f @g @h
                , goCast $ O8f @a @b @c @d @e @f @g @h
                , goCast $ O8g @a @b @c @d @e @f @g @h
                , goCast $ O8h @a @b @c @d @e @f @g @h
                ]
         in go ot
      ot@OT9 ->
        let go :: forall a b c d e f g h i. Inst9 IsObjectType a b c d e f g h i => OT9 a b c d e f g h i -> MaybeObjectN (OT9 a b c d e f g h i)
            go _ =
              goConcat
                [ goCast $ O9a @a @b @c @d @e @f @g @h @i
                , goCast $ O9b @a @b @c @d @e @f @g @h @i
                , goCast $ O9c @a @b @c @d @e @f @g @h @i
                , goCast $ O9d @a @b @c @d @e @f @g @h @i
                , goCast $ O9e @a @b @c @d @e @f @g @h @i
                , goCast $ O9f @a @b @c @d @e @f @g @h @i
                , goCast $ O9g @a @b @c @d @e @f @g @h @i
                , goCast $ O9h @a @b @c @d @e @f @g @h @i
                , goCast $ O9i @a @b @c @d @e @f @g @h @i
                ]
         in go ot
      ot@OT10 ->
        let go :: forall a b c d e f g h i j. Inst10 IsObjectType a b c d e f g h i j => OT10 a b c d e f g h i j -> MaybeObjectN (OT10 a b c d e f g h i j)
            go _ =
              goConcat
                [ goCast $ O10a @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10b @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10c @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10d @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10e @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10f @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10g @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10h @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10i @a @b @c @d @e @f @g @h @i @j
                , goCast $ O10j @a @b @c @d @e @f @g @h @i @j
                ]
         in go ot
      ot@OT11 ->
        let go :: forall a b c d e f g h i j k. Inst11 IsObjectType a b c d e f g h i j k => OT11 a b c d e f g h i j k -> MaybeObjectN (OT11 a b c d e f g h i j k)
            go _ =
              goConcat
                [ goCast $ O11a @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11b @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11c @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11d @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11e @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11f @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11g @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11h @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11i @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11j @a @b @c @d @e @f @g @h @i @j @k
                , goCast $ O11k @a @b @c @d @e @f @g @h @i @j @k
                ]
         in go ot
      ot@OT12 ->
        let go :: forall a b c d e f g h i j k l. Inst12 IsObjectType a b c d e f g h i j k l => OT12 a b c d e f g h i j k l -> MaybeObjectN (OT12 a b c d e f g h i j k l)
            go _ =
              goConcat
                [ goCast $ O12a @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12b @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12c @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12d @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12e @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12f @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12g @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12h @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12i @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12j @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12k @a @b @c @d @e @f @g @h @i @j @k @l
                , goCast $ O12l @a @b @c @d @e @f @g @h @i @j @k @l
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
  ZO zone OTActivatedOrTriggeredAbility
asActivatedOrTriggeredAbility = toZO2

asCreaturePlayerPlaneswalker ::
  AsCreaturePlayerPlaneswalker ot =>
  ZO zone ot ->
  ZO zone OTCreaturePlayerPlaneswalker
asCreaturePlayerPlaneswalker = toZO3

asPermanent :: AsPermanent ot => ZO zone ot -> ZO zone OTPermanent
asPermanent = toZO5

asSpell :: AsSpell ot => ZO zone ot -> ZO zone OTSpell
asSpell = toZO6

asCard :: AsCard ot => ZO zone ot -> ZO zone OTCard
asCard = toZO7

asDamageSource :: AsDamageSource ot => ZO zone ot -> ZO zone OTDamageSource
asDamageSource = toZO8

asAny :: AsAny ot => ZO zone ot -> ZO zone OTAny
asAny = toZO12

zo0ToPermanent :: ZO 'ZBattlefield OT0 -> ZO 'ZBattlefield OTPermanent
zo0ToPermanent = asPermanent . ZO SZBattlefield . O1 . Object SLand . getUntypedObject

zo0ToSpell :: forall zone. IsZone zone => ZO zone OT0 -> ZO zone OTSpell
zo0ToSpell = asSpell . ZO (singZone @zone) . O1 . Object SArtifact . getUntypedObject

zo0ToCard :: forall zone. IsZone zone => ZO zone OT0 -> ZO zone OTCard
zo0ToCard = asCard . ZO (singZone @zone) . O1 . Object SLand . getUntypedObject

zo0ToAny :: forall zone. IsZone zone => ZO zone OT0 -> ZO zone OTAny
zo0ToAny = asAny . ZO (singZone @zone) . O1 . Object SLand . getUntypedObject

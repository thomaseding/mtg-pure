{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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
  castOToZO,
  oToZO1,
  AsPermanent,
  asPermanent,
  zo0ToPermanent,
) where

import safe Data.Inst (
  Inst1,
  Inst2,
  Inst3,
 )
import safe Data.Kind (Type)
import safe Data.Monoid (First (..))
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Model.Object (
  IsObjectType,
  LitOT (..),
  OT (..),
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
  Object (..),
  ObjectType (..),
  SObjectType (..),
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.ObjectId (GetObjectId (..), ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Recursive.Ord ()
import safe MtgPure.Model.ToObjectN.Classes (
  ToObject1 (..),
  ToObject10 (..),
  ToObject11 (..),
  ToObject12 (..),
  ToObject2 (..),
  ToObject3 (..),
  ToObject4 (..),
  ToObject5 (..),
  ToObject6 (..),
  ToObject7 (..),
  ToObject8 (..),
  ToObject9 (..),
 )
import safe MtgPure.Model.VisitObjectN (VisitObjectN (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO, ZoneObject (..), zoToObjectN)

zo1ToO :: (IsObjectType a, IsZone zone) => ZO zone (OT1 a) -> Object a
zo1ToO zo = case zoToObjectN zo of
  O1 o -> o

class ToZO0 (zone :: Zone) (object :: Type) where
  toZO0 :: object -> ZO zone OT0

instance IsZone zone => ToZO0 zone ObjectId where
  toZO0 = ZO (singZone @zone) . O0

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

-- NB: Wrapper to get higher kinded types to type-check in `mapOT`.
newtype MaybeObjectN (ot :: Type) = MaybeObjectN {unMaybeObjectN :: Maybe (ObjectN ot)}

oToZO1 :: (IsZone zone, IsObjectType a) => Object a -> ZO zone (OT1 a)
oToZO1 = ZO singZone . O1

castOToZO :: forall z zone ot. (IsObjectType z, IsZO zone ot) => Object z -> Maybe (ZO zone ot)
castOToZO o = ZO (singZone @zone) <$> objN
 where
  objN :: Maybe (ObjectN ot)
  objN = unMaybeObjectN $
    mapOT @ot $ \case
      OT0 -> MaybeObjectN $ Just $ O0 $ getObjectId o
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
      ot@OT4 -> undefined ot
      ot@OT5 -> undefined ot
      ot@OT6 -> undefined ot
      ot@OT7 -> undefined ot
      ot@OT8 -> undefined ot
      ot@OT9 -> undefined ot
      ot@OT10 -> undefined ot
      ot@OT11 -> undefined ot
      ot@OT12 -> undefined ot
   where
    goConcat = MaybeObjectN . getFirst . mconcat . map First

    goCast :: Typeable z' => (Object z' -> ObjectN ot') -> Maybe (ObjectN ot')
    goCast f = f <$> cast o

type AsPermanent ot =
  ToObject5
    ot
    'OTArtifact
    'OTCreature
    'OTEnchantment
    'OTLand
    'OTPlaneswalker

asPermanent :: AsPermanent ot => ZO zone ot -> ZO zone OTPermanent
asPermanent = toZO5

zo0ToPermanent :: ZO 'ZBattlefield OT0 -> ZO 'ZBattlefield OTPermanent
zo0ToPermanent = asPermanent . ZO SZBattlefield . O1 . Object SLand DefaultObjectDiscriminant . getObjectId

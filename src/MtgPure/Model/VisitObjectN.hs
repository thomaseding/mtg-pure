{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.VisitObjectN (
  VisitObjectN (..),
  ObjectVisitorN (..),
  KnownObjectN (..),
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
import safe Data.Proxy (Proxy)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object (Object)
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectN.Type (
  ON1,
  ON10,
  ON11,
  ON12,
  ON2,
  ON3,
  ON4,
  ON5,
  ON6,
  ON7,
  ON8,
  ON9,
 )
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
 )

data KnownObjectTypeN :: Type -> Type where
  KOT1 :: Inst1 IsObjectType a => KnownObjectTypeN (OT1 a)
  KOT2 :: Inst2 IsObjectType a b => KnownObjectTypeN (OT2 a b)
  KOT3 :: Inst3 IsObjectType a b c => KnownObjectTypeN (OT3 a b c)
  KOT4 :: Inst4 IsObjectType a b c d => KnownObjectTypeN (OT4 a b c d)
  KOT5 :: Inst5 IsObjectType a b c d e => KnownObjectTypeN (OT5 a b c d e)
  KOT6 :: Inst6 IsObjectType a b c d e f => KnownObjectTypeN (OT6 a b c d e f)
  KOT7 :: Inst7 IsObjectType a b c d e f g => KnownObjectTypeN (OT7 a b c d e f g)
  KOT8 :: Inst8 IsObjectType a b c d e f g h => KnownObjectTypeN (OT8 a b c d e f g h)
  KOT9 :: Inst9 IsObjectType a b c d e f g h i => KnownObjectTypeN (OT9 a b c d e f g h i)
  KOT10 :: Inst10 IsObjectType a b c d e f g h i j => KnownObjectTypeN (OT10 a b c d e f g h i j)
  KOT11 :: Inst11 IsObjectType a b c d e f g h i j k => KnownObjectTypeN (OT11 a b c d e f g h i j k)
  KOT12 :: Inst12 IsObjectType a b c d e f g h i j k l => KnownObjectTypeN (OT12 a b c d e f g h i j k l)

data KnownObjectN :: Type -> Type where
  KO1 :: Inst1 IsObjectType a => ON1 a -> KnownObjectN (OT1 a)
  KO2 :: Inst2 IsObjectType a b => ON2 a b -> KnownObjectN (OT2 a b)
  KO3 :: Inst3 IsObjectType a b c => ON3 a b c -> KnownObjectN (OT3 a b c)
  KO4 :: Inst4 IsObjectType a b c d => ON4 a b c d -> KnownObjectN (OT4 a b c d)
  KO5 :: Inst5 IsObjectType a b c d e => ON5 a b c d e -> KnownObjectN (OT5 a b c d e)
  KO6 :: Inst6 IsObjectType a b c d e f => ON6 a b c d e f -> KnownObjectN (OT6 a b c d e f)
  KO7 :: Inst7 IsObjectType a b c d e f g => ON7 a b c d e f g -> KnownObjectN (OT7 a b c d e f g)
  KO8 :: Inst8 IsObjectType a b c d e f g h => ON8 a b c d e f g h -> KnownObjectN (OT8 a b c d e f g h)
  KO9 :: Inst9 IsObjectType a b c d e f g h i => ON9 a b c d e f g h i -> KnownObjectN (OT9 a b c d e f g h i)
  KO10 :: Inst10 IsObjectType a b c d e f g h i j => ON10 a b c d e f g h i j -> KnownObjectN (OT10 a b c d e f g h i j)
  KO11 :: Inst11 IsObjectType a b c d e f g h i j k => ON11 a b c d e f g h i j k -> KnownObjectN (OT11 a b c d e f g h i j k)
  KO12 :: Inst12 IsObjectType a b c d e f g h i j k l => ON12 a b c d e f g h i j k l -> KnownObjectN (OT12 a b c d e f g h i j k l)

class Typeable ot => VisitObjectN ot where
  data ObjectVisitorN ot :: Type -> Type
  visitObjectN' :: (forall a. IsObjectType a => Object a -> x) -> ObjectN ot -> x
  visitObjectN :: ObjectVisitorN ot x -> ObjectN ot -> x
  orderObjectN :: Proxy ot -> Int
  knownObjectN :: ObjectN ot -> KnownObjectN ot
  knownObjectTypeN :: Proxy ot -> KnownObjectTypeN ot

vn :: VisitObjectN ot => ObjectVisitorN ot x -> ObjectN ot -> x
vn = visitObjectN

instance Inst1 IsObjectType a => VisitObjectN (OT1 a) where
  data ObjectVisitorN (OT1 a) x = ObjectVisitor1
    { visitObject1 :: Object a -> x
    }

  visitObjectN' f = visitObjectN $ ObjectVisitor1 f
  visitObjectN v = \case
    O x -> visitObject1 v x
  orderObjectN _ = 1
  knownObjectN = KO1
  knownObjectTypeN _ = KOT1

instance Inst2 IsObjectType a b => VisitObjectN (OT2 a b) where
  data ObjectVisitorN (OT2 a b) x = ObjectVisitor2
    { visitObject2a :: Object a -> x
    , visitObject2b :: Object b -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor2 f f
  visitObjectN v = \case
    O2a x -> a x
    O2b x -> b x
    ON2a x -> vn (ObjectVisitor1 b) x
    ON2b x -> vn (ObjectVisitor1 a) x
   where
    a = visitObject2a v
    b = visitObject2b v
  orderObjectN _ = 2
  knownObjectN = KO2
  knownObjectTypeN _ = KOT2

instance Inst3 IsObjectType a b c => VisitObjectN (OT3 a b c) where
  data ObjectVisitorN (OT3 a b c) x = ObjectVisitor3
    { visitObject3a :: Object a -> x
    , visitObject3b :: Object b -> x
    , visitObject3c :: Object c -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor3 f f f
  visitObjectN v = \case
    O3a x -> a x
    O3b x -> b x
    O3c x -> c x
    ON3a x -> vn (ObjectVisitor2 b c) x
    ON3b x -> vn (ObjectVisitor2 a c) x
    ON3c x -> vn (ObjectVisitor2 a b) x
   where
    a = visitObject3a v
    b = visitObject3b v
    c = visitObject3c v
  orderObjectN _ = 3
  knownObjectN = KO3
  knownObjectTypeN _ = KOT3

instance Inst4 IsObjectType a b c d => VisitObjectN (OT4 a b c d) where
  data ObjectVisitorN (OT4 a b c d) x = ObjectVisitor4
    { visitObject4a :: Object a -> x
    , visitObject4b :: Object b -> x
    , visitObject4c :: Object c -> x
    , visitObject4d :: Object d -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor4 f f f f
  visitObjectN v = \case
    O4a x -> visitObject4a v x
    O4b x -> visitObject4b v x
    O4c x -> visitObject4c v x
    O4d x -> visitObject4d v x
    ON4a x -> vn (ObjectVisitor3 b c d) x
    ON4b x -> vn (ObjectVisitor3 a c d) x
    ON4c x -> vn (ObjectVisitor3 a b d) x
    ON4d x -> vn (ObjectVisitor3 a b c) x
   where
    a = visitObject4a v
    b = visitObject4b v
    c = visitObject4c v
    d = visitObject4d v
  orderObjectN _ = 4
  knownObjectN = KO4
  knownObjectTypeN _ = KOT4

instance Inst5 IsObjectType a b c d e => VisitObjectN (OT5 a b c d e) where
  data ObjectVisitorN (OT5 a b c d e) x = ObjectVisitor5
    { visitObject5a :: Object a -> x
    , visitObject5b :: Object b -> x
    , visitObject5c :: Object c -> x
    , visitObject5d :: Object d -> x
    , visitObject5e :: Object e -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor5 f f f f f
  visitObjectN v = \case
    O5a x -> visitObject5a v x
    O5b x -> visitObject5b v x
    O5c x -> visitObject5c v x
    O5d x -> visitObject5d v x
    O5e x -> visitObject5e v x
    ON5a x -> vn (ObjectVisitor4 b c d e) x
    ON5b x -> vn (ObjectVisitor4 a c d e) x
    ON5c x -> vn (ObjectVisitor4 a b d e) x
    ON5d x -> vn (ObjectVisitor4 a b c e) x
    ON5e x -> vn (ObjectVisitor4 a b c d) x
   where
    a = visitObject5a v
    b = visitObject5b v
    c = visitObject5c v
    d = visitObject5d v
    e = visitObject5e v
  orderObjectN _ = 5
  knownObjectN = KO5
  knownObjectTypeN _ = KOT5

instance Inst6 IsObjectType a b c d e f => VisitObjectN (OT6 a b c d e f) where
  data ObjectVisitorN (OT6 a b c d e f) x = ObjectVisitor6
    { visitObject6a :: Object a -> x
    , visitObject6b :: Object b -> x
    , visitObject6c :: Object c -> x
    , visitObject6d :: Object d -> x
    , visitObject6e :: Object e -> x
    , visitObject6f :: Object f -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor6 f f f f f f
  visitObjectN v = \case
    O6a x -> visitObject6a v x
    O6b x -> visitObject6b v x
    O6c x -> visitObject6c v x
    O6d x -> visitObject6d v x
    O6e x -> visitObject6e v x
    O6f x -> visitObject6f v x
    ON6a x -> vn (ObjectVisitor5 b c d e f) x
    ON6b x -> vn (ObjectVisitor5 a c d e f) x
    ON6c x -> vn (ObjectVisitor5 a b d e f) x
    ON6d x -> vn (ObjectVisitor5 a b c e f) x
    ON6e x -> vn (ObjectVisitor5 a b c d f) x
    ON6f x -> vn (ObjectVisitor5 a b c d e) x
   where
    a = visitObject6a v
    b = visitObject6b v
    c = visitObject6c v
    d = visitObject6d v
    e = visitObject6e v
    f = visitObject6f v
  orderObjectN _ = 6
  knownObjectN = KO6
  knownObjectTypeN _ = KOT6

instance Inst7 IsObjectType a b c d e f g => VisitObjectN (OT7 a b c d e f g) where
  data ObjectVisitorN (OT7 a b c d e f g) x = ObjectVisitor7
    { visitObject7a :: Object a -> x
    , visitObject7b :: Object b -> x
    , visitObject7c :: Object c -> x
    , visitObject7d :: Object d -> x
    , visitObject7e :: Object e -> x
    , visitObject7f :: Object f -> x
    , visitObject7g :: Object g -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor7 f f f f f f f
  visitObjectN v = \case
    O7a x -> visitObject7a v x
    O7b x -> visitObject7b v x
    O7c x -> visitObject7c v x
    O7d x -> visitObject7d v x
    O7e x -> visitObject7e v x
    O7f x -> visitObject7f v x
    O7g x -> visitObject7g v x
    ON7a x -> vn (ObjectVisitor6 b c d e f g) x
    ON7b x -> vn (ObjectVisitor6 a c d e f g) x
    ON7c x -> vn (ObjectVisitor6 a b d e f g) x
    ON7d x -> vn (ObjectVisitor6 a b c e f g) x
    ON7e x -> vn (ObjectVisitor6 a b c d f g) x
    ON7f x -> vn (ObjectVisitor6 a b c d e g) x
    ON7g x -> vn (ObjectVisitor6 a b c d e f) x
   where
    a = visitObject7a v
    b = visitObject7b v
    c = visitObject7c v
    d = visitObject7d v
    e = visitObject7e v
    f = visitObject7f v
    g = visitObject7g v
  orderObjectN _ = 7
  knownObjectN = KO7
  knownObjectTypeN _ = KOT7

instance Inst8 IsObjectType a b c d e f g h => VisitObjectN (OT8 a b c d e f g h) where
  data ObjectVisitorN (OT8 a b c d e f g h) x = ObjectVisitor8
    { visitObject8a :: Object a -> x
    , visitObject8b :: Object b -> x
    , visitObject8c :: Object c -> x
    , visitObject8d :: Object d -> x
    , visitObject8e :: Object e -> x
    , visitObject8f :: Object f -> x
    , visitObject8g :: Object g -> x
    , visitObject8h :: Object h -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor8 f f f f f f f f
  visitObjectN v = \case
    O8a x -> visitObject8a v x
    O8b x -> visitObject8b v x
    O8c x -> visitObject8c v x
    O8d x -> visitObject8d v x
    O8e x -> visitObject8e v x
    O8f x -> visitObject8f v x
    O8g x -> visitObject8g v x
    O8h x -> visitObject8h v x
    ON8a x -> vn (ObjectVisitor7 b c d e f g h) x
    ON8b x -> vn (ObjectVisitor7 a c d e f g h) x
    ON8c x -> vn (ObjectVisitor7 a b d e f g h) x
    ON8d x -> vn (ObjectVisitor7 a b c e f g h) x
    ON8e x -> vn (ObjectVisitor7 a b c d f g h) x
    ON8f x -> vn (ObjectVisitor7 a b c d e g h) x
    ON8g x -> vn (ObjectVisitor7 a b c d e f h) x
    ON8h x -> vn (ObjectVisitor7 a b c d e f g) x
   where
    a = visitObject8a v
    b = visitObject8b v
    c = visitObject8c v
    d = visitObject8d v
    e = visitObject8e v
    f = visitObject8f v
    g = visitObject8g v
    h = visitObject8h v
  orderObjectN _ = 8
  knownObjectN = KO8
  knownObjectTypeN _ = KOT8

instance Inst9 IsObjectType a b c d e f g h i => VisitObjectN (OT9 a b c d e f g h i) where
  data ObjectVisitorN (OT9 a b c d e f g h i) x = ObjectVisitor9
    { visitObject9a :: Object a -> x
    , visitObject9b :: Object b -> x
    , visitObject9c :: Object c -> x
    , visitObject9d :: Object d -> x
    , visitObject9e :: Object e -> x
    , visitObject9f :: Object f -> x
    , visitObject9g :: Object g -> x
    , visitObject9h :: Object h -> x
    , visitObject9i :: Object i -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor9 f f f f f f f f f
  visitObjectN v = \case
    O9a x -> visitObject9a v x
    O9b x -> visitObject9b v x
    O9c x -> visitObject9c v x
    O9d x -> visitObject9d v x
    O9e x -> visitObject9e v x
    O9f x -> visitObject9f v x
    O9g x -> visitObject9g v x
    O9h x -> visitObject9h v x
    O9i x -> visitObject9i v x
    ON9a x -> vn (ObjectVisitor8 b c d e f g h i) x
    ON9b x -> vn (ObjectVisitor8 a c d e f g h i) x
    ON9c x -> vn (ObjectVisitor8 a b d e f g h i) x
    ON9d x -> vn (ObjectVisitor8 a b c e f g h i) x
    ON9e x -> vn (ObjectVisitor8 a b c d f g h i) x
    ON9f x -> vn (ObjectVisitor8 a b c d e g h i) x
    ON9g x -> vn (ObjectVisitor8 a b c d e f h i) x
    ON9h x -> vn (ObjectVisitor8 a b c d e f g i) x
    ON9i x -> vn (ObjectVisitor8 a b c d e f g h) x
   where
    a = visitObject9a v
    b = visitObject9b v
    c = visitObject9c v
    d = visitObject9d v
    e = visitObject9e v
    f = visitObject9f v
    g = visitObject9g v
    h = visitObject9h v
    i = visitObject9i v
  orderObjectN _ = 9
  knownObjectN = KO9
  knownObjectTypeN _ = KOT9

instance Inst10 IsObjectType a b c d e f g h i j => VisitObjectN (OT10 a b c d e f g h i j) where
  data ObjectVisitorN (OT10 a b c d e f g h i j) x = ObjectVisitor10
    { visitObject10a :: Object a -> x
    , visitObject10b :: Object b -> x
    , visitObject10c :: Object c -> x
    , visitObject10d :: Object d -> x
    , visitObject10e :: Object e -> x
    , visitObject10f :: Object f -> x
    , visitObject10g :: Object g -> x
    , visitObject10h :: Object h -> x
    , visitObject10i :: Object i -> x
    , visitObject10j :: Object j -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor10 f f f f f f f f f f
  visitObjectN v = \case
    O10a x -> visitObject10a v x
    O10b x -> visitObject10b v x
    O10c x -> visitObject10c v x
    O10d x -> visitObject10d v x
    O10e x -> visitObject10e v x
    O10f x -> visitObject10f v x
    O10g x -> visitObject10g v x
    O10h x -> visitObject10h v x
    O10i x -> visitObject10i v x
    O10j x -> visitObject10j v x
    ON10a x -> vn (ObjectVisitor9 b c d e f g h i j) x
    ON10b x -> vn (ObjectVisitor9 a c d e f g h i j) x
    ON10c x -> vn (ObjectVisitor9 a b d e f g h i j) x
    ON10d x -> vn (ObjectVisitor9 a b c e f g h i j) x
    ON10e x -> vn (ObjectVisitor9 a b c d f g h i j) x
    ON10f x -> vn (ObjectVisitor9 a b c d e g h i j) x
    ON10g x -> vn (ObjectVisitor9 a b c d e f h i j) x
    ON10h x -> vn (ObjectVisitor9 a b c d e f g i j) x
    ON10i x -> vn (ObjectVisitor9 a b c d e f g h j) x
    ON10j x -> vn (ObjectVisitor9 a b c d e f g h i) x
   where
    a = visitObject10a v
    b = visitObject10b v
    c = visitObject10c v
    d = visitObject10d v
    e = visitObject10e v
    f = visitObject10f v
    g = visitObject10g v
    h = visitObject10h v
    i = visitObject10i v
    j = visitObject10j v
  orderObjectN _ = 10
  knownObjectN = KO10
  knownObjectTypeN _ = KOT10

instance Inst11 IsObjectType a b c d e f g h i j k => VisitObjectN (OT11 a b c d e f g h i j k) where
  data ObjectVisitorN (OT11 a b c d e f g h i j k) x = ObjectVisitor11
    { visitObject11a :: Object a -> x
    , visitObject11b :: Object b -> x
    , visitObject11c :: Object c -> x
    , visitObject11d :: Object d -> x
    , visitObject11e :: Object e -> x
    , visitObject11f :: Object f -> x
    , visitObject11g :: Object g -> x
    , visitObject11h :: Object h -> x
    , visitObject11i :: Object i -> x
    , visitObject11j :: Object j -> x
    , visitObject11k :: Object k -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor11 f f f f f f f f f f f
  visitObjectN v = \case
    O11a x -> visitObject11a v x
    O11b x -> visitObject11b v x
    O11c x -> visitObject11c v x
    O11d x -> visitObject11d v x
    O11e x -> visitObject11e v x
    O11f x -> visitObject11f v x
    O11g x -> visitObject11g v x
    O11h x -> visitObject11h v x
    O11i x -> visitObject11i v x
    O11j x -> visitObject11j v x
    O11k x -> visitObject11k v x
    ON11a x -> vn (ObjectVisitor10 b c d e f g h i j k) x
    ON11b x -> vn (ObjectVisitor10 a c d e f g h i j k) x
    ON11c x -> vn (ObjectVisitor10 a b d e f g h i j k) x
    ON11d x -> vn (ObjectVisitor10 a b c e f g h i j k) x
    ON11e x -> vn (ObjectVisitor10 a b c d f g h i j k) x
    ON11f x -> vn (ObjectVisitor10 a b c d e g h i j k) x
    ON11g x -> vn (ObjectVisitor10 a b c d e f h i j k) x
    ON11h x -> vn (ObjectVisitor10 a b c d e f g i j k) x
    ON11i x -> vn (ObjectVisitor10 a b c d e f g h j k) x
    ON11j x -> vn (ObjectVisitor10 a b c d e f g h i k) x
    ON11k x -> vn (ObjectVisitor10 a b c d e f g h i j) x
   where
    a = visitObject11a v
    b = visitObject11b v
    c = visitObject11c v
    d = visitObject11d v
    e = visitObject11e v
    f = visitObject11f v
    g = visitObject11g v
    h = visitObject11h v
    i = visitObject11i v
    j = visitObject11j v
    k = visitObject11k v
  orderObjectN _ = 11
  knownObjectN = KO11
  knownObjectTypeN _ = KOT11

instance Inst12 IsObjectType a b c d e f g h i j k l => VisitObjectN (OT12 a b c d e f g h i j k l) where
  data ObjectVisitorN (OT12 a b c d e f g h i j k l) x = ObjectVisitor12
    { visitObject12a :: Object a -> x
    , visitObject12b :: Object b -> x
    , visitObject12c :: Object c -> x
    , visitObject12d :: Object d -> x
    , visitObject12e :: Object e -> x
    , visitObject12f :: Object f -> x
    , visitObject12g :: Object g -> x
    , visitObject12h :: Object h -> x
    , visitObject12i :: Object i -> x
    , visitObject12j :: Object j -> x
    , visitObject12k :: Object k -> x
    , visitObject12l :: Object l -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor12 f f f f f f f f f f f f
  visitObjectN v = \case
    O12a x -> visitObject12a v x
    O12b x -> visitObject12b v x
    O12c x -> visitObject12c v x
    O12d x -> visitObject12d v x
    O12e x -> visitObject12e v x
    O12f x -> visitObject12f v x
    O12g x -> visitObject12g v x
    O12h x -> visitObject12h v x
    O12i x -> visitObject12i v x
    O12j x -> visitObject12j v x
    O12k x -> visitObject12k v x
    O12l x -> visitObject12l v x
    ON12a x -> vn (ObjectVisitor11 b c d e f g h i j k l) x
    ON12b x -> vn (ObjectVisitor11 a c d e f g h i j k l) x
    ON12c x -> vn (ObjectVisitor11 a b d e f g h i j k l) x
    ON12d x -> vn (ObjectVisitor11 a b c e f g h i j k l) x
    ON12e x -> vn (ObjectVisitor11 a b c d f g h i j k l) x
    ON12f x -> vn (ObjectVisitor11 a b c d e g h i j k l) x
    ON12g x -> vn (ObjectVisitor11 a b c d e f h i j k l) x
    ON12h x -> vn (ObjectVisitor11 a b c d e f g i j k l) x
    ON12i x -> vn (ObjectVisitor11 a b c d e f g h j k l) x
    ON12j x -> vn (ObjectVisitor11 a b c d e f g h i k l) x
    ON12k x -> vn (ObjectVisitor11 a b c d e f g h i j l) x
    ON12l x -> vn (ObjectVisitor11 a b c d e f g h i j k) x
   where
    a = visitObject12a v
    b = visitObject12b v
    c = visitObject12c v
    d = visitObject12d v
    e = visitObject12e v
    f = visitObject12f v
    g = visitObject12g v
    h = visitObject12h v
    i = visitObject12i v
    j = visitObject12j v
    k = visitObject12k v
    l = visitObject12l v
  orderObjectN _ = 12
  knownObjectN = KO12
  knownObjectTypeN _ = KOT12

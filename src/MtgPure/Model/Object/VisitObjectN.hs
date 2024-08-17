{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.VisitObjectN (
  ObjectVisitorN (..),
  visitObjectN,
  visitObjectN',
) where

import safe Data.Kind (Type)
import safe MtgPure.Model.Object.IsObjectType (
  IsObjectType (singObjectType),
 )
import safe MtgPure.Model.Object.OT (OT (OTLand))
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
  OTN (..),
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (GetObjectId (..))
import safe MtgPure.Model.Object.ObjectN (
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
  ObjectN (..),
 )
import safe MtgPure.Model.Object.ViewObjectN (viewOTN, viewOTN')

type OTArbitrary = 'OTLand

instance GetObjectId (ObjectN ot) where
  getUntypedObject = \case
    O0 o -> o
    x -> visitObjectN' getUntypedObject x

data ObjectVisitorN (ret :: Type) (ot :: Type) :: Type where
  ObjectVisitor0 ::
    { visitObject0 :: Object OTArbitrary -> ret
    } ->
    ObjectVisitorN ret OT0
  ObjectVisitor1 ::
    { visitObject1 :: Object a -> ret
    } ->
    ObjectVisitorN ret (OT1 a)
  ObjectVisitor2 ::
    { visitObject2a :: Object a -> ret
    , visitObject2b :: Object b -> ret
    } ->
    ObjectVisitorN ret (OT2 a b)
  ObjectVisitor3 ::
    { visitObject3a :: Object a -> ret
    , visitObject3b :: Object b -> ret
    , visitObject3c :: Object c -> ret
    } ->
    ObjectVisitorN ret (OT3 a b c)
  ObjectVisitor4 ::
    { visitObject4a :: Object a -> ret
    , visitObject4b :: Object b -> ret
    , visitObject4c :: Object c -> ret
    , visitObject4d :: Object d -> ret
    } ->
    ObjectVisitorN ret (OT4 a b c d)
  ObjectVisitor5 ::
    { visitObject5a :: Object a -> ret
    , visitObject5b :: Object b -> ret
    , visitObject5c :: Object c -> ret
    , visitObject5d :: Object d -> ret
    , visitObject5e :: Object e -> ret
    } ->
    ObjectVisitorN ret (OT5 a b c d e)
  ObjectVisitor6 ::
    { visitObject6a :: Object a -> ret
    , visitObject6b :: Object b -> ret
    , visitObject6c :: Object c -> ret
    , visitObject6d :: Object d -> ret
    , visitObject6e :: Object e -> ret
    , visitObject6f :: Object f -> ret
    } ->
    ObjectVisitorN ret (OT6 a b c d e f)
  ObjectVisitor7 ::
    { visitObject7a :: Object a -> ret
    , visitObject7b :: Object b -> ret
    , visitObject7c :: Object c -> ret
    , visitObject7d :: Object d -> ret
    , visitObject7e :: Object e -> ret
    , visitObject7f :: Object f -> ret
    , visitObject7g :: Object g -> ret
    } ->
    ObjectVisitorN ret (OT7 a b c d e f g)
  ObjectVisitor8 ::
    { visitObject8a :: Object a -> ret
    , visitObject8b :: Object b -> ret
    , visitObject8c :: Object c -> ret
    , visitObject8d :: Object d -> ret
    , visitObject8e :: Object e -> ret
    , visitObject8f :: Object f -> ret
    , visitObject8g :: Object g -> ret
    , visitObject8h :: Object h -> ret
    } ->
    ObjectVisitorN ret (OT8 a b c d e f g h)
  ObjectVisitor9 ::
    { visitObject9a :: Object a -> ret
    , visitObject9b :: Object b -> ret
    , visitObject9c :: Object c -> ret
    , visitObject9d :: Object d -> ret
    , visitObject9e :: Object e -> ret
    , visitObject9f :: Object f -> ret
    , visitObject9g :: Object g -> ret
    , visitObject9h :: Object h -> ret
    , visitObject9i :: Object i -> ret
    } ->
    ObjectVisitorN ret (OT9 a b c d e f g h i)
  ObjectVisitor10 ::
    { visitObject10a :: Object a -> ret
    , visitObject10b :: Object b -> ret
    , visitObject10c :: Object c -> ret
    , visitObject10d :: Object d -> ret
    , visitObject10e :: Object e -> ret
    , visitObject10f :: Object f -> ret
    , visitObject10g :: Object g -> ret
    , visitObject10h :: Object h -> ret
    , visitObject10i :: Object i -> ret
    , visitObject10j :: Object j -> ret
    } ->
    ObjectVisitorN ret (OT10 a b c d e f g h i j)
  ObjectVisitor11 ::
    { visitObject11a :: Object a -> ret
    , visitObject11b :: Object b -> ret
    , visitObject11c :: Object c -> ret
    , visitObject11d :: Object d -> ret
    , visitObject11e :: Object e -> ret
    , visitObject11f :: Object f -> ret
    , visitObject11g :: Object g -> ret
    , visitObject11h :: Object h -> ret
    , visitObject11i :: Object i -> ret
    , visitObject11j :: Object j -> ret
    , visitObject11k :: Object k -> ret
    } ->
    ObjectVisitorN ret (OT11 a b c d e f g h i j k)
  ObjectVisitor12 ::
    { visitObject12a :: Object a -> ret
    , visitObject12b :: Object b -> ret
    , visitObject12c :: Object c -> ret
    , visitObject12d :: Object d -> ret
    , visitObject12e :: Object e -> ret
    , visitObject12f :: Object f -> ret
    , visitObject12g :: Object g -> ret
    , visitObject12h :: Object h -> ret
    , visitObject12i :: Object i -> ret
    , visitObject12j :: Object j -> ret
    , visitObject12k :: Object k -> ret
    , visitObject12l :: Object l -> ret
    } ->
    ObjectVisitorN ret (OT12 a b c d e f g h i j k l)

visitObjectN' :: (forall a. (IsObjectType a) => Object a -> ret) -> ObjectN ot -> ret
visitObjectN' f objN' = viewOTN' objN' \objN ot -> case ot of
  OT0 -> visitObjectN (ObjectVisitor0 f) objN
  OT1 -> visitObjectN (ObjectVisitor1 f) objN
  OT2 -> visitObjectN (ObjectVisitor2 f f) objN
  OT3 -> visitObjectN (ObjectVisitor3 f f f) objN
  OT4 -> visitObjectN (ObjectVisitor4 f f f f) objN
  OT5 -> visitObjectN (ObjectVisitor5 f f f f f) objN
  OT6 -> visitObjectN (ObjectVisitor6 f f f f f f) objN
  OT7 -> visitObjectN (ObjectVisitor7 f f f f f f f) objN
  OT8 -> visitObjectN (ObjectVisitor8 f f f f f f f f) objN
  OT9 -> visitObjectN (ObjectVisitor9 f f f f f f f f f) objN
  OT10 -> visitObjectN (ObjectVisitor10 f f f f f f f f f f) objN
  OT11 -> visitObjectN (ObjectVisitor11 f f f f f f f f f f f) objN
  OT12 -> visitObjectN (ObjectVisitor12 f f f f f f f f f f f f) objN

vn :: ObjectVisitorN ret ot -> ObjectN ot -> ret
vn = visitObjectN

data VOUser ret ot = VOUser (ObjectVisitorN ret ot) (ObjectN ot)

visitObjectN :: ObjectVisitorN ret ot -> ObjectN ot -> ret
visitObjectN v objN = viewOTN (VOUser v objN) objN visitObjectNImpl

visitObjectNImpl :: VOUser ret (OTN otk) -> OTN otk -> ret
visitObjectNImpl (VOUser v objN) otn = case otn of
  OT0 -> case objN of
    O0 uo -> visitObject0 v $ Object (singObjectType @OTArbitrary) uo
  OT1 -> case objN of
    O1 o -> visitObject1 v o
  OT2 -> visitObject2 v objN
  OT3 -> visitObject3 v objN
  OT4 -> visitObject4 v objN
  OT5 -> visitObject5 v objN
  OT6 -> visitObject6 v objN
  OT7 -> visitObject7 v objN
  OT8 -> visitObject8 v objN
  OT9 -> visitObject9 v objN
  OT10 -> visitObject10 v objN
  OT11 -> visitObject11 v objN
  OT12 -> visitObject12 v objN

visitObject2 :: ObjectVisitorN ret (OT2 a b) -> ON2 a b -> ret
visitObject2 v objN =
  let a = visitObject2a v
      b = visitObject2b v
   in case objN of
        O2a x -> a x
        O2b x -> b x
        ON2a x -> vn (ObjectVisitor1 b) x
        ON2b x -> vn (ObjectVisitor1 a) x

visitObject3 :: ObjectVisitorN ret (OT3 a b c) -> ON3 a b c -> ret
visitObject3 v objN =
  let a = visitObject3a v
      b = visitObject3b v
      c = visitObject3c v
   in case objN of
        O3a x -> a x
        O3b x -> b x
        O3c x -> c x
        ON3a x -> vn (ObjectVisitor2 b c) x
        ON3b x -> vn (ObjectVisitor2 a c) x
        ON3c x -> vn (ObjectVisitor2 a b) x

visitObject4 :: ObjectVisitorN ret (OT4 a b c d) -> ON4 a b c d -> ret
visitObject4 v objN =
  let a = visitObject4a v
      b = visitObject4b v
      c = visitObject4c v
      d = visitObject4d v
   in case objN of
        O4a x -> a x
        O4b x -> b x
        O4c x -> c x
        O4d x -> d x
        ON4a x -> vn (ObjectVisitor3 b c d) x
        ON4b x -> vn (ObjectVisitor3 a c d) x
        ON4c x -> vn (ObjectVisitor3 a b d) x
        ON4d x -> vn (ObjectVisitor3 a b c) x

visitObject5 :: ObjectVisitorN ret (OT5 a b c d e) -> ON5 a b c d e -> ret
visitObject5 v objN =
  let a = visitObject5a v
      b = visitObject5b v
      c = visitObject5c v
      d = visitObject5d v
      e = visitObject5e v
   in case objN of
        O5a x -> a x
        O5b x -> b x
        O5c x -> c x
        O5d x -> d x
        O5e x -> e x
        ON5a x -> vn (ObjectVisitor4 b c d e) x
        ON5b x -> vn (ObjectVisitor4 a c d e) x
        ON5c x -> vn (ObjectVisitor4 a b d e) x
        ON5d x -> vn (ObjectVisitor4 a b c e) x
        ON5e x -> vn (ObjectVisitor4 a b c d) x

visitObject6 :: ObjectVisitorN ret (OT6 a b c d e f) -> ON6 a b c d e f -> ret
visitObject6 v objN =
  let a = visitObject6a v
      b = visitObject6b v
      c = visitObject6c v
      d = visitObject6d v
      e = visitObject6e v
      f = visitObject6f v
   in case objN of
        O6a x -> a x
        O6b x -> b x
        O6c x -> c x
        O6d x -> d x
        O6e x -> e x
        O6f x -> f x
        ON6a x -> vn (ObjectVisitor5 b c d e f) x
        ON6b x -> vn (ObjectVisitor5 a c d e f) x
        ON6c x -> vn (ObjectVisitor5 a b d e f) x
        ON6d x -> vn (ObjectVisitor5 a b c e f) x
        ON6e x -> vn (ObjectVisitor5 a b c d f) x
        ON6f x -> vn (ObjectVisitor5 a b c d e) x

visitObject7 :: ObjectVisitorN ret (OT7 a b c d e f g) -> ON7 a b c d e f g -> ret
visitObject7 v objN =
  let a = visitObject7a v
      b = visitObject7b v
      c = visitObject7c v
      d = visitObject7d v
      e = visitObject7e v
      f = visitObject7f v
      g = visitObject7g v
   in case objN of
        O7a x -> a x
        O7b x -> b x
        O7c x -> c x
        O7d x -> d x
        O7e x -> e x
        O7f x -> f x
        O7g x -> g x
        ON7a x -> vn (ObjectVisitor6 b c d e f g) x
        ON7b x -> vn (ObjectVisitor6 a c d e f g) x
        ON7c x -> vn (ObjectVisitor6 a b d e f g) x
        ON7d x -> vn (ObjectVisitor6 a b c e f g) x
        ON7e x -> vn (ObjectVisitor6 a b c d f g) x
        ON7f x -> vn (ObjectVisitor6 a b c d e g) x
        ON7g x -> vn (ObjectVisitor6 a b c d e f) x

visitObject8 :: ObjectVisitorN ret (OT8 a b c d e f g h) -> ON8 a b c d e f g h -> ret
visitObject8 v objN =
  let a = visitObject8a v
      b = visitObject8b v
      c = visitObject8c v
      d = visitObject8d v
      e = visitObject8e v
      f = visitObject8f v
      g = visitObject8g v
      h = visitObject8h v
   in case objN of
        O8a x -> a x
        O8b x -> b x
        O8c x -> c x
        O8d x -> d x
        O8e x -> e x
        O8f x -> f x
        O8g x -> g x
        O8h x -> h x
        ON8a x -> vn (ObjectVisitor7 b c d e f g h) x
        ON8b x -> vn (ObjectVisitor7 a c d e f g h) x
        ON8c x -> vn (ObjectVisitor7 a b d e f g h) x
        ON8d x -> vn (ObjectVisitor7 a b c e f g h) x
        ON8e x -> vn (ObjectVisitor7 a b c d f g h) x
        ON8f x -> vn (ObjectVisitor7 a b c d e g h) x
        ON8g x -> vn (ObjectVisitor7 a b c d e f h) x
        ON8h x -> vn (ObjectVisitor7 a b c d e f g) x

visitObject9 :: ObjectVisitorN ret (OT9 a b c d e f g h i) -> ON9 a b c d e f g h i -> ret
visitObject9 v objN =
  let a = visitObject9a v
      b = visitObject9b v
      c = visitObject9c v
      d = visitObject9d v
      e = visitObject9e v
      f = visitObject9f v
      g = visitObject9g v
      h = visitObject9h v
      i = visitObject9i v
   in case objN of
        O9a x -> a x
        O9b x -> b x
        O9c x -> c x
        O9d x -> d x
        O9e x -> e x
        O9f x -> f x
        O9g x -> g x
        O9h x -> h x
        O9i x -> i x
        ON9a x -> vn (ObjectVisitor8 b c d e f g h i) x
        ON9b x -> vn (ObjectVisitor8 a c d e f g h i) x
        ON9c x -> vn (ObjectVisitor8 a b d e f g h i) x
        ON9d x -> vn (ObjectVisitor8 a b c e f g h i) x
        ON9e x -> vn (ObjectVisitor8 a b c d f g h i) x
        ON9f x -> vn (ObjectVisitor8 a b c d e g h i) x
        ON9g x -> vn (ObjectVisitor8 a b c d e f h i) x
        ON9h x -> vn (ObjectVisitor8 a b c d e f g i) x
        ON9i x -> vn (ObjectVisitor8 a b c d e f g h) x

visitObject10 :: ObjectVisitorN ret (OT10 a b c d e f g h i j) -> ON10 a b c d e f g h i j -> ret
visitObject10 v objN =
  let a = visitObject10a v
      b = visitObject10b v
      c = visitObject10c v
      d = visitObject10d v
      e = visitObject10e v
      f = visitObject10f v
      g = visitObject10g v
      h = visitObject10h v
      i = visitObject10i v
      j = visitObject10j v
   in case objN of
        O10a x -> a x
        O10b x -> b x
        O10c x -> c x
        O10d x -> d x
        O10e x -> e x
        O10f x -> f x
        O10g x -> g x
        O10h x -> h x
        O10i x -> i x
        O10j x -> j x
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

visitObject11 :: ObjectVisitorN ret (OT11 a b c d e f g h i j k) -> ON11 a b c d e f g h i j k -> ret
visitObject11 v objN =
  let a = visitObject11a v
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
   in case objN of
        O11a x -> a x
        O11b x -> b x
        O11c x -> c x
        O11d x -> d x
        O11e x -> e x
        O11f x -> f x
        O11g x -> g x
        O11h x -> h x
        O11i x -> i x
        O11j x -> j x
        O11k x -> k x
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

visitObject12 :: ObjectVisitorN ret (OT12 a b c d e f g h i j k l) -> ON12 a b c d e f g h i j k l -> ret
visitObject12 v objN =
  let a = visitObject12a v
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
   in case objN of
        O12a x -> a x
        O12b x -> b x
        O12c x -> c x
        O12d x -> d x
        O12e x -> e x
        O12f x -> f x
        O12g x -> g x
        O12h x -> h x
        O12i x -> i x
        O12j x -> j x
        O12k x -> k x
        O12l x -> l x
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

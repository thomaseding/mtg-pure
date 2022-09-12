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

module MtgPure.Model.VisitObjectN
  ( VisitObjectN (..),
    ObjectVisitorN (..),
  )
where

import Data.Inst (Inst2, Inst3, Inst4, Inst5, Inst6, Inst7, Inst8)
import Data.Kind (Type)
import MtgPure.Model.Internal.IsObjectType (IsObjectType)
import MtgPure.Model.Object (Object)
import MtgPure.Model.ObjectN (ObjectN (..))

class VisitObjectN (a :: k) where
  data ObjectVisitorN a :: Type -> Type
  visitObjectN' :: (forall b. IsObjectType b => Object b -> x) -> ObjectN a -> x
  visitObjectN :: ObjectVisitorN a x -> ObjectN a -> x

vn :: VisitObjectN a => ObjectVisitorN a x -> ObjectN a -> x
vn = visitObjectN

instance Inst2 IsObjectType a b => VisitObjectN '(a, b) where
  data ObjectVisitorN '(a, b) x = ObjectVisitor2
    { visitObject2a :: Object a -> x,
      visitObject2b :: Object b -> x
    }
  visitObjectN' f = visitObjectN $ ObjectVisitor2 f f
  visitObjectN v = \case
    O2a x -> visitObject2a v x
    O2b x -> visitObject2b v x

instance Inst3 IsObjectType a b c => VisitObjectN '(a, b, c) where
  data ObjectVisitorN '(a, b, c) x = ObjectVisitor3
    { visitObject3a :: Object a -> x,
      visitObject3b :: Object b -> x,
      visitObject3c :: Object c -> x
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

instance Inst4 IsObjectType a b c d => VisitObjectN '(a, b, c, d) where
  data ObjectVisitorN '(a, b, c, d) x = ObjectVisitor4
    { visitObject4a :: Object a -> x,
      visitObject4b :: Object b -> x,
      visitObject4c :: Object c -> x,
      visitObject4d :: Object d -> x
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

instance Inst5 IsObjectType a b c d e => VisitObjectN '(a, b, c, d, e) where
  data ObjectVisitorN '(a, b, c, d, e) x = ObjectVisitor5
    { visitObject5a :: Object a -> x,
      visitObject5b :: Object b -> x,
      visitObject5c :: Object c -> x,
      visitObject5d :: Object d -> x,
      visitObject5e :: Object e -> x
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

instance Inst6 IsObjectType a b c d e f => VisitObjectN '(a, b, c, d, e, f) where
  data ObjectVisitorN '(a, b, c, d, e, f) x = ObjectVisitor6
    { visitObject6a :: Object a -> x,
      visitObject6b :: Object b -> x,
      visitObject6c :: Object c -> x,
      visitObject6d :: Object d -> x,
      visitObject6e :: Object e -> x,
      visitObject6f :: Object f -> x
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

instance Inst7 IsObjectType a b c d e f g => VisitObjectN '(a, b, c, d, e, f, g) where
  data ObjectVisitorN '(a, b, c, d, e, f, g) x = ObjectVisitor7
    { visitObject7a :: Object a -> x,
      visitObject7b :: Object b -> x,
      visitObject7c :: Object c -> x,
      visitObject7d :: Object d -> x,
      visitObject7e :: Object e -> x,
      visitObject7f :: Object f -> x,
      visitObject7g :: Object g -> x
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

instance Inst8 IsObjectType a b c d e f g h => VisitObjectN '(a, b, c, d, e, f, g, h) where
  data ObjectVisitorN '(a, b, c, d, e, f, g, h) x = ObjectVisitor8
    { visitObject8a :: Object a -> x,
      visitObject8b :: Object b -> x,
      visitObject8c :: Object c -> x,
      visitObject8d :: Object d -> x,
      visitObject8e :: Object e -> x,
      visitObject8f :: Object f -> x,
      visitObject8g :: Object g -> x,
      visitObject8h :: Object h -> x
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

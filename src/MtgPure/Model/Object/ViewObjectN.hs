{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.Object.ViewObjectN (
  viewOTN,
  viewOTN',
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
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
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
import safe MtgPure.Model.Object.ObjectN (ObjectN (..))

viewOTN' ::
  forall ret otn.
  ObjectN otn ->
  (forall k (otk :: k). (otn ~ OTN otk) => ObjectN (OTN otk) -> OTN otk -> ret) ->
  ret
viewOTN' objN = viewOTN objN objN

viewOTN ::
  forall user ret otn.
  -- | This is useful as to keep some user-level type in lock with the discovered OTN.
  user otn ->
  ObjectN otn ->
  (forall k (otk :: k). (otn ~ OTN otk) => user (OTN otk) -> OTN otk -> ret) ->
  ret
viewOTN user' objN cont = case objN of
  O0{} -> go0 user'
  --
  O1{} -> go1 user'
  --
  O2a{} -> go2 user'
  O2b{} -> go2 user'
  ON2b{} -> go2 user'
  ON2a{} -> go2 user'
  --
  O3b{} -> go3 user'
  O3a{} -> go3 user'
  O3c{} -> go3 user'
  ON3a{} -> go3 user'
  ON3b{} -> go3 user'
  ON3c{} -> go3 user'
  --
  O4a{} -> go4 user'
  O4b{} -> go4 user'
  O4c{} -> go4 user'
  O4d{} -> go4 user'
  ON4a{} -> go4 user'
  ON4b{} -> go4 user'
  ON4c{} -> go4 user'
  ON4d{} -> go4 user'
  --
  O5b{} -> go5 user'
  O5a{} -> go5 user'
  O5c{} -> go5 user'
  O5d{} -> go5 user'
  O5e{} -> go5 user'
  ON5a{} -> go5 user'
  ON5b{} -> go5 user'
  ON5c{} -> go5 user'
  ON5d{} -> go5 user'
  ON5e{} -> go5 user'
  --
  O6a{} -> go6 user'
  O6b{} -> go6 user'
  O6c{} -> go6 user'
  O6d{} -> go6 user'
  O6e{} -> go6 user'
  O6f{} -> go6 user'
  ON6a{} -> go6 user'
  ON6b{} -> go6 user'
  ON6c{} -> go6 user'
  ON6d{} -> go6 user'
  ON6e{} -> go6 user'
  ON6f{} -> go6 user'
  --
  O7a{} -> go7 user'
  O7b{} -> go7 user'
  O7c{} -> go7 user'
  O7d{} -> go7 user'
  O7e{} -> go7 user'
  O7f{} -> go7 user'
  O7g{} -> go7 user'
  ON7a{} -> go7 user'
  ON7b{} -> go7 user'
  ON7c{} -> go7 user'
  ON7d{} -> go7 user'
  ON7e{} -> go7 user'
  ON7f{} -> go7 user'
  ON7g{} -> go7 user'
  --
  O8a{} -> go8 user'
  O8b{} -> go8 user'
  O8c{} -> go8 user'
  O8d{} -> go8 user'
  O8e{} -> go8 user'
  O8f{} -> go8 user'
  O8g{} -> go8 user'
  O8h{} -> go8 user'
  ON8a{} -> go8 user'
  ON8b{} -> go8 user'
  ON8c{} -> go8 user'
  ON8d{} -> go8 user'
  ON8e{} -> go8 user'
  ON8f{} -> go8 user'
  ON8g{} -> go8 user'
  ON8h{} -> go8 user'
  --
  O9a{} -> go9 user'
  O9b{} -> go9 user'
  O9c{} -> go9 user'
  O9d{} -> go9 user'
  O9e{} -> go9 user'
  O9f{} -> go9 user'
  O9g{} -> go9 user'
  O9h{} -> go9 user'
  O9i{} -> go9 user'
  ON9a{} -> go9 user'
  ON9b{} -> go9 user'
  ON9c{} -> go9 user'
  ON9d{} -> go9 user'
  ON9e{} -> go9 user'
  ON9f{} -> go9 user'
  ON9g{} -> go9 user'
  ON9h{} -> go9 user'
  ON9i{} -> go9 user'
  --
  O10b{} -> go10 user'
  O10a{} -> go10 user'
  O10c{} -> go10 user'
  O10d{} -> go10 user'
  O10e{} -> go10 user'
  O10f{} -> go10 user'
  O10g{} -> go10 user'
  O10h{} -> go10 user'
  O10i{} -> go10 user'
  O10j{} -> go10 user'
  ON10a{} -> go10 user'
  ON10b{} -> go10 user'
  ON10c{} -> go10 user'
  ON10d{} -> go10 user'
  ON10e{} -> go10 user'
  ON10f{} -> go10 user'
  ON10g{} -> go10 user'
  ON10h{} -> go10 user'
  ON10i{} -> go10 user'
  ON10j{} -> go10 user'
  --
  O11b{} -> go11 user'
  O11a{} -> go11 user'
  O11c{} -> go11 user'
  O11d{} -> go11 user'
  O11e{} -> go11 user'
  O11f{} -> go11 user'
  O11g{} -> go11 user'
  O11h{} -> go11 user'
  O11i{} -> go11 user'
  O11j{} -> go11 user'
  O11k{} -> go11 user'
  ON11a{} -> go11 user'
  ON11b{} -> go11 user'
  ON11c{} -> go11 user'
  ON11d{} -> go11 user'
  ON11e{} -> go11 user'
  ON11f{} -> go11 user'
  ON11g{} -> go11 user'
  ON11h{} -> go11 user'
  ON11i{} -> go11 user'
  ON11j{} -> go11 user'
  ON11k{} -> go11 user'
  --
  O12b{} -> go12 user'
  O12a{} -> go12 user'
  O12c{} -> go12 user'
  O12d{} -> go12 user'
  O12e{} -> go12 user'
  O12f{} -> go12 user'
  O12g{} -> go12 user'
  O12h{} -> go12 user'
  O12i{} -> go12 user'
  O12j{} -> go12 user'
  O12k{} -> go12 user'
  O12l{} -> go12 user'
  ON12a{} -> go12 user'
  ON12b{} -> go12 user'
  ON12c{} -> go12 user'
  ON12d{} -> go12 user'
  ON12e{} -> go12 user'
  ON12f{} -> go12 user'
  ON12g{} -> go12 user'
  ON12h{} -> go12 user'
  ON12i{} -> go12 user'
  ON12j{} -> go12 user'
  ON12k{} -> go12 user'
  ON12l{} -> go12 user'
 where
  go0 :: (otn ~ OT0) => user OT0 -> ret
  go0 user = cont user OT0

  go1 ::
    forall a.
    (Inst1 IsObjectType a, otn ~ OT1 a) =>
    user (OT1 a) ->
    ret
  go1 user = cont user $ OT1 @a

  go2 ::
    forall a b.
    (Inst2 IsObjectType a b, otn ~ OT2 a b) =>
    user (OT2 a b) ->
    ret
  go2 user = cont user $ OT2 @a @b

  go3 ::
    forall a b c.
    (Inst3 IsObjectType a b c, otn ~ OT3 a b c) =>
    user (OT3 a b c) ->
    ret
  go3 user = cont user $ OT3 @a @b @c

  go4 ::
    forall a b c d.
    (Inst4 IsObjectType a b c d, otn ~ OT4 a b c d) =>
    user (OT4 a b c d) ->
    ret
  go4 user = cont user $ OT4 @a @b @c @d

  go5 ::
    forall a b c d e.
    (Inst5 IsObjectType a b c d e, otn ~ OT5 a b c d e) =>
    user (OT5 a b c d e) ->
    ret
  go5 user = cont user $ OT5 @a @b @c @d @e

  go6 ::
    forall a b c d e f.
    (Inst6 IsObjectType a b c d e f, otn ~ OT6 a b c d e f) =>
    user (OT6 a b c d e f) ->
    ret
  go6 user = cont user $ OT6 @a @b @c @d @e @f

  go7 ::
    forall a b c d e f g.
    (Inst7 IsObjectType a b c d e f g, otn ~ OT7 a b c d e f g) =>
    user (OT7 a b c d e f g) ->
    ret
  go7 user = cont user $ OT7 @a @b @c @d @e @f @g

  go8 ::
    forall a b c d e f g h.
    (Inst8 IsObjectType a b c d e f g h, otn ~ OT8 a b c d e f g h) =>
    user (OT8 a b c d e f g h) ->
    ret
  go8 user = cont user $ OT8 @a @b @c @d @e @f @g @h

  go9 ::
    forall a b c d e f g h i.
    (Inst9 IsObjectType a b c d e f g h i, otn ~ OT9 a b c d e f g h i) =>
    user (OT9 a b c d e f g h i) ->
    ret
  go9 user = cont user $ OT9 @a @b @c @d @e @f @g @h @i

  go10 ::
    forall a b c d e f g h i j.
    (Inst10 IsObjectType a b c d e f g h i j, otn ~ OT10 a b c d e f g h i j) =>
    user (OT10 a b c d e f g h i j) ->
    ret
  go10 user = cont user $ OT10 @a @b @c @d @e @f @g @h @i @j

  go11 ::
    forall a b c d e f g h i j k.
    (Inst11 IsObjectType a b c d e f g h i j k, otn ~ OT11 a b c d e f g h i j k) =>
    user (OT11 a b c d e f g h i j k) ->
    ret
  go11 user = cont user $ OT11 @a @b @c @d @e @f @g @h @i @j @k

  go12 ::
    forall a b c d e f g h i j k l.
    (Inst12 IsObjectType a b c d e f g h i j k l, otn ~ OT12 a b c d e f g h i j k l) =>
    user (OT12 a b c d e f g h i j k l) ->
    ret
  go12 user = cont user $ OT12 @a @b @c @d @e @f @g @h @i @j @k @l

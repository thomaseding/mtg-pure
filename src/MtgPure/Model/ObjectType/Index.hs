{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType.Index (
  IndexOT (..),
) where

import Data.Inst (
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
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.IsObjectType (IsObjectType (..), objectTypeIndex)
import safe MtgPure.Model.ObjectType (
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

oti :: forall ot. IsObjectType ot => Int
oti = objectTypeIndex (Proxy @ot)

class Typeable ot => IndexOT ot where
  indexOT :: Proxy ot -> [Int]

instance IndexOT OT0 where
  indexOT _ = []

instance
  (Inst1 IsObjectType a) =>
  IndexOT (OT1 a)
  where
  indexOT _ = [oti @a]

instance
  (Inst2 IsObjectType a b) =>
  IndexOT (OT2 a b)
  where
  indexOT _ = [oti @a, oti @b]

instance
  (Inst3 IsObjectType a b c) =>
  IndexOT (OT3 a b c)
  where
  indexOT _ = [oti @a, oti @b, oti @c]

instance
  (Inst4 IsObjectType a b c d) =>
  IndexOT (OT4 a b c d)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d]

instance
  (Inst5 IsObjectType a b c d e) =>
  IndexOT (OT5 a b c d e)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d, oti @e]

instance
  (Inst6 IsObjectType a b c d e f) =>
  IndexOT (OT6 a b c d e f)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d, oti @e, oti @f]

instance
  (Inst7 IsObjectType a b c d e f g) =>
  IndexOT (OT7 a b c d e f g)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d, oti @e, oti @f, oti @g]

instance
  (Inst8 IsObjectType a b c d e f g h) =>
  IndexOT (OT8 a b c d e f g h)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d, oti @e, oti @f, oti @g, oti @h]

instance
  (Inst9 IsObjectType a b c d e f g h i) =>
  IndexOT (OT9 a b c d e f g h i)
  where
  indexOT _ =
    [oti @a, oti @b, oti @c, oti @d, oti @e, oti @f, oti @g, oti @h, oti @i]

instance
  (Inst10 IsObjectType a b c d e f g h i j) =>
  IndexOT (OT10 a b c d e f g h i j)
  where
  indexOT _ =
    [ oti @a
    , oti @b
    , oti @c
    , oti @d
    , oti @e
    , oti @f
    , oti @g
    , oti @h
    , oti @i
    , oti @j
    ]

instance
  (Inst11 IsObjectType a b c d e f g h i j k) =>
  IndexOT (OT11 a b c d e f g h i j k)
  where
  indexOT _ =
    [ oti @a
    , oti @b
    , oti @c
    , oti @d
    , oti @e
    , oti @f
    , oti @g
    , oti @h
    , oti @i
    , oti @j
    , oti @k
    ]

instance
  (Inst12 IsObjectType a b c d e f g h i j k l) =>
  IndexOT (OT12 a b c d e f g h i j k l)
  where
  indexOT _ =
    [ oti @a
    , oti @b
    , oti @c
    , oti @d
    , oti @e
    , oti @f
    , oti @g
    , oti @h
    , oti @i
    , oti @j
    , oti @k
    , oti @l
    ]

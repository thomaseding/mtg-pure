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

module MtgPure.Model.ObjectType.Index
  ( IndexOT (..),
  )
where

import Data.Inst
  ( Inst1,
    Inst2,
    Inst3,
    Inst4,
    Inst5,
    Inst6,
  )
import safe Data.Typeable (Proxy (..))
import safe MtgPure.Model.IsObjectType (IsObjectType (..), objectTypeIndex)
import safe MtgPure.Model.ObjectType (OT)

oti :: forall ot. IsObjectType ot => Int
oti = objectTypeIndex (Proxy @ot)

class IndexOT ot where
  indexOT :: Proxy ot -> [Int]

instance
  (Inst1 IsObjectType a) =>
  IndexOT '(OT, a)
  where
  indexOT _ = [oti @a]

instance
  (Inst2 IsObjectType a b) =>
  IndexOT '(OT, a, b)
  where
  indexOT _ = [oti @a, oti @b]

instance
  (Inst3 IsObjectType a b c) =>
  IndexOT '(OT, a, b, c)
  where
  indexOT _ = [oti @a, oti @b, oti @c]

instance
  (Inst4 IsObjectType a b c d) =>
  IndexOT '(OT, a, b, c, d)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d]

instance
  (Inst5 IsObjectType a b c d e) =>
  IndexOT '(OT, a, b, c, d, e)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d, oti @e]

instance
  (Inst6 IsObjectType a b c d e f) =>
  IndexOT '(OT, a, b, c, d, e, f)
  where
  indexOT _ = [oti @a, oti @b, oti @c, oti @d, oti @e, oti @f]

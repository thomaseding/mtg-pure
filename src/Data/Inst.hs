{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.Inst
  ( Inst2,
    Inst3,
    Inst4,
    Inst5,
    Inst6,
    Inst7,
    Inst8,
  )
where

import Data.Kind (Constraint)

type Inst2 (x :: k -> Constraint) a b = (x a, x b)

type Inst3 (x :: k -> Constraint) a b c = (Inst2 x a b, x c)

type Inst4 (x :: k -> Constraint) a b c d = (Inst3 x a b c, x d)

type Inst5 (x :: k -> Constraint) a b c d e = (Inst4 x a b c d, x e)

type Inst6 (x :: k -> Constraint) a b c d e f = (Inst5 x a b c d e, x f)

type Inst7 (x :: k -> Constraint) a b c d e f g = (Inst6 x a b c d e f, x g)

type Inst8 (x :: k -> Constraint) a b c d e f g h = (Inst7 x a b c d e f g, x h)

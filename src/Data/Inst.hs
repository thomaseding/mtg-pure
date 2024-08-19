{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Data.Inst (
  Inst1,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
  Inst7,
  Inst8,
  Inst9,
  Inst10,
  Inst11,
  Inst12,
  Inst13,
) where

import safe Data.Kind (Constraint)

type Inst1 (x :: k -> Constraint) a = x a

type Inst2 (x :: k -> Constraint) a b = (x a, x b)

type Inst3 (x :: k -> Constraint) a b c = (Inst2 x a b, x c)

type Inst4 (x :: k -> Constraint) a b c d = (Inst3 x a b c, x d)

type Inst5 (x :: k -> Constraint) a b c d e = (Inst4 x a b c d, x e)

type Inst6 (x :: k -> Constraint) a b c d e f = (Inst5 x a b c d e, x f)

type Inst7 (x :: k -> Constraint) a b c d e f g = (Inst6 x a b c d e f, x g)

type Inst8 (x :: k -> Constraint) a b c d e f g h =
  (Inst7 x a b c d e f g, x h)

type Inst9 (x :: k -> Constraint) a b c d e f g h i =
  (Inst8 x a b c d e f g h, x i)

type Inst10 (x :: k -> Constraint) a b c d e f g h i j =
  (Inst9 x a b c d e f g h i, x j)

type Inst11 (x :: k' -> Constraint) a b c d e f g h i j k =
  (Inst10 x a b c d e f g h i j, x k)

type Inst12 (x :: k' -> Constraint) a b c d e f g h i j k l =
  (Inst11 x a b c d e f g h i j k, x l)

type Inst13 (x :: k' -> Constraint) a b c d e f g h i j k l m =
  (Inst12 x a b c d e f g h i j k l, x m)

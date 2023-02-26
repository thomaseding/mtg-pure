{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.Object.SmartConstructors (
  mkO0,
  mkO1,
  mkO2a,
  mkO2b,
  mkON2b,
  mkON2a,
  mkO3b,
  mkO3a,
  mkO3c,
  mkON3a,
  mkON3b,
  mkON3c,
  mkO4a,
  mkO4b,
  mkO4c,
  mkO4d,
  mkON4a,
  mkON4b,
  mkON4c,
  mkON4d,
  mkO5b,
  mkO5a,
  mkO5c,
  mkO5d,
  mkO5e,
  mkON5a,
  mkON5b,
  mkON5c,
  mkON5d,
  mkON5e,
  mkO6a,
  mkO6b,
  mkO6c,
  mkO6d,
  mkO6e,
  mkO6f,
  mkON6a,
  mkON6b,
  mkON6c,
  mkON6d,
  mkON6e,
  mkON6f,
  mkO7a,
  mkO7b,
  mkO7c,
  mkO7d,
  mkO7e,
  mkO7f,
  mkO7g,
  mkON7a,
  mkON7b,
  mkON7c,
  mkON7d,
  mkON7e,
  mkON7f,
  mkON7g,
  mkO8a,
  mkO8b,
  mkO8c,
  mkO8d,
  mkO8e,
  mkO8f,
  mkO8g,
  mkO8h,
  mkON8a,
  mkON8b,
  mkON8c,
  mkON8d,
  mkON8e,
  mkON8f,
  mkON8g,
  mkON8h,
  mkO9a,
  mkO9b,
  mkO9c,
  mkO9d,
  mkO9e,
  mkO9f,
  mkO9g,
  mkO9h,
  mkO9i,
  mkON9a,
  mkON9b,
  mkON9c,
  mkON9d,
  mkON9e,
  mkON9f,
  mkON9g,
  mkON9h,
  mkON9i,
  mkO10b,
  mkO10a,
  mkO10c,
  mkO10d,
  mkO10e,
  mkO10f,
  mkO10g,
  mkO10h,
  mkO10i,
  mkO10j,
  mkON10a,
  mkON10b,
  mkON10c,
  mkON10d,
  mkON10e,
  mkON10f,
  mkON10g,
  mkON10h,
  mkON10i,
  mkON10j,
  mkO11b,
  mkO11a,
  mkO11c,
  mkO11d,
  mkO11e,
  mkO11f,
  mkO11g,
  mkO11h,
  mkO11i,
  mkO11j,
  mkO11k,
  mkON11a,
  mkON11b,
  mkON11c,
  mkON11d,
  mkON11e,
  mkON11f,
  mkON11g,
  mkON11h,
  mkON11i,
  mkON11j,
  mkON11k,
  mkO12b,
  mkO12a,
  mkO12c,
  mkO12d,
  mkO12e,
  mkO12f,
  mkO12g,
  mkO12h,
  mkO12i,
  mkO12j,
  mkO12k,
  mkO12l,
  mkON12a,
  mkON12b,
  mkON12c,
  mkON12d,
  mkON12e,
  mkON12f,
  mkON12g,
  mkON12h,
  mkON12i,
  mkON12j,
  mkON12k,
  mkON12l,
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
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (UntypedObject)
import safe MtgPure.Model.Object.ObjectN (
  ON0,
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
  ObjectN (..),
 )

mkO0 :: UntypedObject -> ON0
mkO0 = O0

mkO1 :: Inst1 IsObjectType a => Object a -> ON1 a
mkO1 = O1

mkO2a :: Inst2 IsObjectType a b => Object a -> ON2 a b
mkO2a = O2a

mkO2b :: Inst2 IsObjectType a b => Object b -> ON2 a b
mkO2b = O2b

mkON2b :: Inst2 IsObjectType a b => ON1 a -> ON2 a b
mkON2b = ON2b

mkON2a :: Inst2 IsObjectType a b => ON1 b -> ON2 a b
mkON2a = ON2a

mkO3b :: Inst3 IsObjectType a b c => Object b -> ON3 a b c
mkO3b = O3b

mkO3a :: Inst3 IsObjectType a b c => Object a -> ON3 a b c
mkO3a = O3a

mkO3c :: Inst3 IsObjectType a b c => Object c -> ON3 a b c
mkO3c = O3c

mkON3a :: Inst3 IsObjectType a b c => ON2 b c -> ON3 a b c
mkON3a = ON3a

mkON3b :: Inst3 IsObjectType a b c => ON2 a c -> ON3 a b c
mkON3b = ON3b

mkON3c :: Inst3 IsObjectType a b c => ON2 a b -> ON3 a b c
mkON3c = ON3c

mkO4a :: Inst4 IsObjectType a b c d => Object a -> ON4 a b c d
mkO4a = O4a

mkO4b :: Inst4 IsObjectType a b c d => Object b -> ON4 a b c d
mkO4b = O4b

mkO4c :: Inst4 IsObjectType a b c d => Object c -> ON4 a b c d
mkO4c = O4c

mkO4d :: Inst4 IsObjectType a b c d => Object d -> ON4 a b c d
mkO4d = O4d

mkON4a :: Inst4 IsObjectType a b c d => ON3 b c d -> ON4 a b c d
mkON4a = ON4a

mkON4b :: Inst4 IsObjectType a b c d => ON3 a c d -> ON4 a b c d
mkON4b = ON4b

mkON4c :: Inst4 IsObjectType a b c d => ON3 a b d -> ON4 a b c d
mkON4c = ON4c

mkON4d :: Inst4 IsObjectType a b c d => ON3 a b c -> ON4 a b c d
mkON4d = ON4d

mkO5b :: Inst5 IsObjectType a b c d e => Object b -> ON5 a b c d e
mkO5b = O5b

mkO5a :: Inst5 IsObjectType a b c d e => Object a -> ON5 a b c d e
mkO5a = O5a

mkO5c :: Inst5 IsObjectType a b c d e => Object c -> ON5 a b c d e
mkO5c = O5c

mkO5d :: Inst5 IsObjectType a b c d e => Object d -> ON5 a b c d e
mkO5d = O5d

mkO5e :: Inst5 IsObjectType a b c d e => Object e -> ON5 a b c d e
mkO5e = O5e

mkON5a :: Inst5 IsObjectType a b c d e => ON4 b c d e -> ON5 a b c d e
mkON5a = ON5a

mkON5b :: Inst5 IsObjectType a b c d e => ON4 a c d e -> ON5 a b c d e
mkON5b = ON5b

mkON5c :: Inst5 IsObjectType a b c d e => ON4 a b d e -> ON5 a b c d e
mkON5c = ON5c

mkON5d :: Inst5 IsObjectType a b c d e => ON4 a b c e -> ON5 a b c d e
mkON5d = ON5d

mkON5e :: Inst5 IsObjectType a b c d e => ON4 a b c d -> ON5 a b c d e
mkON5e = ON5e

mkO6a :: Inst6 IsObjectType a b c d e f => Object a -> ON6 a b c d e f
mkO6a = O6a

mkO6b :: Inst6 IsObjectType a b c d e f => Object b -> ON6 a b c d e f
mkO6b = O6b

mkO6c :: Inst6 IsObjectType a b c d e f => Object c -> ON6 a b c d e f
mkO6c = O6c

mkO6d :: Inst6 IsObjectType a b c d e f => Object d -> ON6 a b c d e f
mkO6d = O6d

mkO6e :: Inst6 IsObjectType a b c d e f => Object e -> ON6 a b c d e f
mkO6e = O6e

mkO6f :: Inst6 IsObjectType a b c d e f => Object f -> ON6 a b c d e f
mkO6f = O6f

mkON6a :: Inst6 IsObjectType a b c d e f => ON5 b c d e f -> ON6 a b c d e f
mkON6a = ON6a

mkON6b :: Inst6 IsObjectType a b c d e f => ON5 a c d e f -> ON6 a b c d e f
mkON6b = ON6b

mkON6c :: Inst6 IsObjectType a b c d e f => ON5 a b d e f -> ON6 a b c d e f
mkON6c = ON6c

mkON6d :: Inst6 IsObjectType a b c d e f => ON5 a b c e f -> ON6 a b c d e f
mkON6d = ON6d

mkON6e :: Inst6 IsObjectType a b c d e f => ON5 a b c d f -> ON6 a b c d e f
mkON6e = ON6e

mkON6f :: Inst6 IsObjectType a b c d e f => ON5 a b c d e -> ON6 a b c d e f
mkON6f = ON6f

mkO7a :: Inst7 IsObjectType a b c d e f g => Object a -> ON7 a b c d e f g
mkO7a = O7a

mkO7b :: Inst7 IsObjectType a b c d e f g => Object b -> ON7 a b c d e f g
mkO7b = O7b

mkO7c :: Inst7 IsObjectType a b c d e f g => Object c -> ON7 a b c d e f g
mkO7c = O7c

mkO7d :: Inst7 IsObjectType a b c d e f g => Object d -> ON7 a b c d e f g
mkO7d = O7d

mkO7e :: Inst7 IsObjectType a b c d e f g => Object e -> ON7 a b c d e f g
mkO7e = O7e

mkO7f :: Inst7 IsObjectType a b c d e f g => Object f -> ON7 a b c d e f g
mkO7f = O7f

mkO7g :: Inst7 IsObjectType a b c d e f g => Object g -> ON7 a b c d e f g
mkO7g = O7g

mkON7a :: Inst7 IsObjectType a b c d e f g => ON6 b c d e f g -> ON7 a b c d e f g
mkON7a = ON7a

mkON7b :: Inst7 IsObjectType a b c d e f g => ON6 a c d e f g -> ON7 a b c d e f g
mkON7b = ON7b

mkON7c :: Inst7 IsObjectType a b c d e f g => ON6 a b d e f g -> ON7 a b c d e f g
mkON7c = ON7c

mkON7d :: Inst7 IsObjectType a b c d e f g => ON6 a b c e f g -> ON7 a b c d e f g
mkON7d = ON7d

mkON7e :: Inst7 IsObjectType a b c d e f g => ON6 a b c d f g -> ON7 a b c d e f g
mkON7e = ON7e

mkON7f :: Inst7 IsObjectType a b c d e f g => ON6 a b c d e g -> ON7 a b c d e f g
mkON7f = ON7f

mkON7g :: Inst7 IsObjectType a b c d e f g => ON6 a b c d e f -> ON7 a b c d e f g
mkON7g = ON7g

mkO8a :: Inst8 IsObjectType a b c d e f g h => Object a -> ON8 a b c d e f g h
mkO8a = O8a

mkO8b :: Inst8 IsObjectType a b c d e f g h => Object b -> ON8 a b c d e f g h
mkO8b = O8b

mkO8c :: Inst8 IsObjectType a b c d e f g h => Object c -> ON8 a b c d e f g h
mkO8c = O8c

mkO8d :: Inst8 IsObjectType a b c d e f g h => Object d -> ON8 a b c d e f g h
mkO8d = O8d

mkO8e :: Inst8 IsObjectType a b c d e f g h => Object e -> ON8 a b c d e f g h
mkO8e = O8e

mkO8f :: Inst8 IsObjectType a b c d e f g h => Object f -> ON8 a b c d e f g h
mkO8f = O8f

mkO8g :: Inst8 IsObjectType a b c d e f g h => Object g -> ON8 a b c d e f g h
mkO8g = O8g

mkO8h :: Inst8 IsObjectType a b c d e f g h => Object h -> ON8 a b c d e f g h
mkO8h = O8h

mkON8a :: Inst8 IsObjectType a b c d e f g h => ON7 b c d e f g h -> ON8 a b c d e f g h
mkON8a = ON8a

mkON8b :: Inst8 IsObjectType a b c d e f g h => ON7 a c d e f g h -> ON8 a b c d e f g h
mkON8b = ON8b

mkON8c :: Inst8 IsObjectType a b c d e f g h => ON7 a b d e f g h -> ON8 a b c d e f g h
mkON8c = ON8c

mkON8d :: Inst8 IsObjectType a b c d e f g h => ON7 a b c e f g h -> ON8 a b c d e f g h
mkON8d = ON8d

mkON8e :: Inst8 IsObjectType a b c d e f g h => ON7 a b c d f g h -> ON8 a b c d e f g h
mkON8e = ON8e

mkON8f :: Inst8 IsObjectType a b c d e f g h => ON7 a b c d e g h -> ON8 a b c d e f g h
mkON8f = ON8f

mkON8g :: Inst8 IsObjectType a b c d e f g h => ON7 a b c d e f h -> ON8 a b c d e f g h
mkON8g = ON8g

mkON8h :: Inst8 IsObjectType a b c d e f g h => ON7 a b c d e f g -> ON8 a b c d e f g h
mkON8h = ON8h

mkO9a :: Inst9 IsObjectType a b c d e f g h i => Object a -> ON9 a b c d e f g h i
mkO9a = O9a

mkO9b :: Inst9 IsObjectType a b c d e f g h i => Object b -> ON9 a b c d e f g h i
mkO9b = O9b

mkO9c :: Inst9 IsObjectType a b c d e f g h i => Object c -> ON9 a b c d e f g h i
mkO9c = O9c

mkO9d :: Inst9 IsObjectType a b c d e f g h i => Object d -> ON9 a b c d e f g h i
mkO9d = O9d

mkO9e :: Inst9 IsObjectType a b c d e f g h i => Object e -> ON9 a b c d e f g h i
mkO9e = O9e

mkO9f :: Inst9 IsObjectType a b c d e f g h i => Object f -> ON9 a b c d e f g h i
mkO9f = O9f

mkO9g :: Inst9 IsObjectType a b c d e f g h i => Object g -> ON9 a b c d e f g h i
mkO9g = O9g

mkO9h :: Inst9 IsObjectType a b c d e f g h i => Object h -> ON9 a b c d e f g h i
mkO9h = O9h

mkO9i :: Inst9 IsObjectType a b c d e f g h i => Object i -> ON9 a b c d e f g h i
mkO9i = O9i

mkON9a :: Inst9 IsObjectType a b c d e f g h i => ON8 b c d e f g h i -> ON9 a b c d e f g h i
mkON9a = ON9a

mkON9b :: Inst9 IsObjectType a b c d e f g h i => ON8 a c d e f g h i -> ON9 a b c d e f g h i
mkON9b = ON9b

mkON9c :: Inst9 IsObjectType a b c d e f g h i => ON8 a b d e f g h i -> ON9 a b c d e f g h i
mkON9c = ON9c

mkON9d :: Inst9 IsObjectType a b c d e f g h i => ON8 a b c e f g h i -> ON9 a b c d e f g h i
mkON9d = ON9d

mkON9e :: Inst9 IsObjectType a b c d e f g h i => ON8 a b c d f g h i -> ON9 a b c d e f g h i
mkON9e = ON9e

mkON9f :: Inst9 IsObjectType a b c d e f g h i => ON8 a b c d e g h i -> ON9 a b c d e f g h i
mkON9f = ON9f

mkON9g :: Inst9 IsObjectType a b c d e f g h i => ON8 a b c d e f h i -> ON9 a b c d e f g h i
mkON9g = ON9g

mkON9h :: Inst9 IsObjectType a b c d e f g h i => ON8 a b c d e f g i -> ON9 a b c d e f g h i
mkON9h = ON9h

mkON9i :: Inst9 IsObjectType a b c d e f g h i => ON8 a b c d e f g h -> ON9 a b c d e f g h i
mkON9i = ON9i

mkO10b :: Inst10 IsObjectType a b c d e f g h i j => Object b -> ON10 a b c d e f g h i j
mkO10b = O10b

mkO10a :: Inst10 IsObjectType a b c d e f g h i j => Object a -> ON10 a b c d e f g h i j
mkO10a = O10a

mkO10c :: Inst10 IsObjectType a b c d e f g h i j => Object c -> ON10 a b c d e f g h i j
mkO10c = O10c

mkO10d :: Inst10 IsObjectType a b c d e f g h i j => Object d -> ON10 a b c d e f g h i j
mkO10d = O10d

mkO10e :: Inst10 IsObjectType a b c d e f g h i j => Object e -> ON10 a b c d e f g h i j
mkO10e = O10e

mkO10f :: Inst10 IsObjectType a b c d e f g h i j => Object f -> ON10 a b c d e f g h i j
mkO10f = O10f

mkO10g :: Inst10 IsObjectType a b c d e f g h i j => Object g -> ON10 a b c d e f g h i j
mkO10g = O10g

mkO10h :: Inst10 IsObjectType a b c d e f g h i j => Object h -> ON10 a b c d e f g h i j
mkO10h = O10h

mkO10i :: Inst10 IsObjectType a b c d e f g h i j => Object i -> ON10 a b c d e f g h i j
mkO10i = O10i

mkO10j :: Inst10 IsObjectType a b c d e f g h i j => Object j -> ON10 a b c d e f g h i j
mkO10j = O10j

mkON10a :: Inst10 IsObjectType a b c d e f g h i j => ON9 b c d e f g h i j -> ON10 a b c d e f g h i j
mkON10a = ON10a

mkON10b :: Inst10 IsObjectType a b c d e f g h i j => ON9 a c d e f g h i j -> ON10 a b c d e f g h i j
mkON10b = ON10b

mkON10c :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b d e f g h i j -> ON10 a b c d e f g h i j
mkON10c = ON10c

mkON10d :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b c e f g h i j -> ON10 a b c d e f g h i j
mkON10d = ON10d

mkON10e :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b c d f g h i j -> ON10 a b c d e f g h i j
mkON10e = ON10e

mkON10f :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b c d e g h i j -> ON10 a b c d e f g h i j
mkON10f = ON10f

mkON10g :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b c d e f h i j -> ON10 a b c d e f g h i j
mkON10g = ON10g

mkON10h :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b c d e f g i j -> ON10 a b c d e f g h i j
mkON10h = ON10h

mkON10i :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b c d e f g h j -> ON10 a b c d e f g h i j
mkON10i = ON10i

mkON10j :: Inst10 IsObjectType a b c d e f g h i j => ON9 a b c d e f g h i -> ON10 a b c d e f g h i j
mkON10j = ON10j

mkO11b :: Inst11 IsObjectType a b c d e f g h i j k => Object b -> ON11 a b c d e f g h i j k
mkO11b = O11b

mkO11a :: Inst11 IsObjectType a b c d e f g h i j k => Object a -> ON11 a b c d e f g h i j k
mkO11a = O11a

mkO11c :: Inst11 IsObjectType a b c d e f g h i j k => Object c -> ON11 a b c d e f g h i j k
mkO11c = O11c

mkO11d :: Inst11 IsObjectType a b c d e f g h i j k => Object d -> ON11 a b c d e f g h i j k
mkO11d = O11d

mkO11e :: Inst11 IsObjectType a b c d e f g h i j k => Object e -> ON11 a b c d e f g h i j k
mkO11e = O11e

mkO11f :: Inst11 IsObjectType a b c d e f g h i j k => Object f -> ON11 a b c d e f g h i j k
mkO11f = O11f

mkO11g :: Inst11 IsObjectType a b c d e f g h i j k => Object g -> ON11 a b c d e f g h i j k
mkO11g = O11g

mkO11h :: Inst11 IsObjectType a b c d e f g h i j k => Object h -> ON11 a b c d e f g h i j k
mkO11h = O11h

mkO11i :: Inst11 IsObjectType a b c d e f g h i j k => Object i -> ON11 a b c d e f g h i j k
mkO11i = O11i

mkO11j :: Inst11 IsObjectType a b c d e f g h i j k => Object j -> ON11 a b c d e f g h i j k
mkO11j = O11j

mkO11k :: Inst11 IsObjectType a b c d e f g h i j k => Object k -> ON11 a b c d e f g h i j k
mkO11k = O11k

mkON11a :: Inst11 IsObjectType a b c d e f g h i j k => ON10 b c d e f g h i j k -> ON11 a b c d e f g h i j k
mkON11a = ON11a

mkON11b :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a c d e f g h i j k -> ON11 a b c d e f g h i j k
mkON11b = ON11b

mkON11c :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b d e f g h i j k -> ON11 a b c d e f g h i j k
mkON11c = ON11c

mkON11d :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c e f g h i j k -> ON11 a b c d e f g h i j k
mkON11d = ON11d

mkON11e :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c d f g h i j k -> ON11 a b c d e f g h i j k
mkON11e = ON11e

mkON11f :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c d e g h i j k -> ON11 a b c d e f g h i j k
mkON11f = ON11f

mkON11g :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c d e f h i j k -> ON11 a b c d e f g h i j k
mkON11g = ON11g

mkON11h :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c d e f g i j k -> ON11 a b c d e f g h i j k
mkON11h = ON11h

mkON11i :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c d e f g h j k -> ON11 a b c d e f g h i j k
mkON11i = ON11i

mkON11j :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c d e f g h i k -> ON11 a b c d e f g h i j k
mkON11j = ON11j

mkON11k :: Inst11 IsObjectType a b c d e f g h i j k => ON10 a b c d e f g h i j -> ON11 a b c d e f g h i j k
mkON11k = ON11k

mkO12b :: Inst12 IsObjectType a b c d e f g h i j k l => Object b -> ON12 a b c d e f g h i j k l
mkO12b = O12b

mkO12a :: Inst12 IsObjectType a b c d e f g h i j k l => Object a -> ON12 a b c d e f g h i j k l
mkO12a = O12a

mkO12c :: Inst12 IsObjectType a b c d e f g h i j k l => Object c -> ON12 a b c d e f g h i j k l
mkO12c = O12c

mkO12d :: Inst12 IsObjectType a b c d e f g h i j k l => Object d -> ON12 a b c d e f g h i j k l
mkO12d = O12d

mkO12e :: Inst12 IsObjectType a b c d e f g h i j k l => Object e -> ON12 a b c d e f g h i j k l
mkO12e = O12e

mkO12f :: Inst12 IsObjectType a b c d e f g h i j k l => Object f -> ON12 a b c d e f g h i j k l
mkO12f = O12f

mkO12g :: Inst12 IsObjectType a b c d e f g h i j k l => Object g -> ON12 a b c d e f g h i j k l
mkO12g = O12g

mkO12h :: Inst12 IsObjectType a b c d e f g h i j k l => Object h -> ON12 a b c d e f g h i j k l
mkO12h = O12h

mkO12i :: Inst12 IsObjectType a b c d e f g h i j k l => Object i -> ON12 a b c d e f g h i j k l
mkO12i = O12i

mkO12j :: Inst12 IsObjectType a b c d e f g h i j k l => Object j -> ON12 a b c d e f g h i j k l
mkO12j = O12j

mkO12k :: Inst12 IsObjectType a b c d e f g h i j k l => Object k -> ON12 a b c d e f g h i j k l
mkO12k = O12k

mkO12l :: Inst12 IsObjectType a b c d e f g h i j k l => Object l -> ON12 a b c d e f g h i j k l
mkO12l = O12l

mkON12a :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 b c d e f g h i j k l -> ON12 a b c d e f g h i j k l
mkON12a = ON12a

mkON12b :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a c d e f g h i j k l -> ON12 a b c d e f g h i j k l
mkON12b = ON12b

mkON12c :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b d e f g h i j k l -> ON12 a b c d e f g h i j k l
mkON12c = ON12c

mkON12d :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c e f g h i j k l -> ON12 a b c d e f g h i j k l
mkON12d = ON12d

mkON12e :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d f g h i j k l -> ON12 a b c d e f g h i j k l
mkON12e = ON12e

mkON12f :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d e g h i j k l -> ON12 a b c d e f g h i j k l
mkON12f = ON12f

mkON12g :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d e f h i j k l -> ON12 a b c d e f g h i j k l
mkON12g = ON12g

mkON12h :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d e f g i j k l -> ON12 a b c d e f g h i j k l
mkON12h = ON12h

mkON12i :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d e f g h j k l -> ON12 a b c d e f g h i j k l
mkON12i = ON12i

mkON12j :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d e f g h i k l -> ON12 a b c d e f g h i j k l
mkON12j = ON12j

mkON12k :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d e f g h i j l -> ON12 a b c d e f g h i j k l
mkON12k = ON12k

mkON12l :: Inst12 IsObjectType a b c d e f g h i j k l => ON11 a b c d e f g h i j k -> ON12 a b c d e f g h i j k l
mkON12l = ON12l

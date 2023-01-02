{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.OTKN (
  OTK0,
  OTK1,
  OTK2,
  OTK3,
  OTK4,
  OTK5,
  OTK6,
  OTK7,
  OTK8,
  OTK9,
  OTK10,
  OTK11,
  OTK12,
) where

import safe MtgPure.Model.Object.OTKN_ (
  OTK0',
  OTK1',
  OTK10',
  OTK11',
  OTK12',
  OTK2',
  OTK3',
  OTK4',
  OTK5',
  OTK6',
  OTK7',
  OTK8',
  OTK9',
 )
import safe MtgPure.Model.Object.ObjectType (ObjectType)

type OTK0 = OTK0' ObjectType

type OTK1 a = OTK1' ObjectType a

type OTK2 a b = OTK2' ObjectType a b

type OTK3 a b c = OTK3' ObjectType a b c

type OTK4 a b c d = OTK4' ObjectType a b c d

type OTK5 a b c d e = OTK5' ObjectType a b c d e

type OTK6 a b c d e f = OTK6' ObjectType a b c d e f

type OTK7 a b c d e f g = OTK7' ObjectType a b c d e f g

type OTK8 a b c d e f g h = OTK8' ObjectType a b c d e f g h

type OTK9 a b c d e f g h i = OTK9' ObjectType a b c d e f g h i

type OTK10 a b c d e f g h i j = OTK10' ObjectType a b c d e f g h i j

type OTK11 a b c d e f g h i j k = OTK11' ObjectType a b c d e f g h i j k

type OTK12 a b c d e f g h i j k l = OTK12' ObjectType a b c d e f g h i j k l

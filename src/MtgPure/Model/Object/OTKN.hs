{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

-- NOTE: OTKN stands for ObjectTypeKindN
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

import safe MtgPure.Model.Object.ObjectType (ObjectType)

type OT = ObjectType

type OTK0 = '()

type OTK1 a = '( '(), a :: OT)

type OTK2 a b = '( '(), a :: OT, b :: OT)

type OTK3 a b c = '( '(), a :: OT, b :: OT, c :: OT)

type OTK4 a b c d = '( '(), a :: OT, b :: OT, c :: OT, d :: OT)

type OTK5 a b c d e = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT)

type OTK6 a b c d e f = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT)

type OTK7 a b c d e f g = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT, g :: OT)

type OTK8 a b c d e f g h = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT, g :: OT, h :: OT)

type OTK9 a b c d e f g h i = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT, g :: OT, h :: OT, i :: OT)

type OTK10 a b c d e f g h i j = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT, g :: OT, h :: OT, i :: OT, j :: OT)

type OTK11 a b c d e f g h i j k = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT, g :: OT, h :: OT, i :: OT, j :: OT, k :: OT)

type OTK12 a b c d e f g h i j k l = '( '(), a :: OT, b :: OT, c :: OT, d :: OT, e :: OT, f :: OT, g :: OT, h :: OT, i :: OT, j :: OT, k :: OT, l :: OT)

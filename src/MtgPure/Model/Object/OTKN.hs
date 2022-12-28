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

import safe MtgPure.Model.Object.ObjectType (ObjectType)

type OTK0 = '()

type OTK1 a = '( '(), a :: ObjectType)

type OTK2 a b = '( '(), a :: ObjectType, b :: ObjectType)

type OTK3 a b c = '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType)

type OTK4 a b c d =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType)

type OTK5 a b c d e =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType)

type OTK6 a b c d e f =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType)

type OTK7 a b c d e f g =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType)

type OTK8 a b c d e f g h =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType)

type OTK9 a b c d e f g h i =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType)

type OTK10 a b c d e f g h i j =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType)

type OTK11 a b c d e f g h i j k =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType)

type OTK12 a b c d e f g h i j k l =
  '( '(), a :: ObjectType, b :: ObjectType, c :: ObjectType, d :: ObjectType, e :: ObjectType, f :: ObjectType, g :: ObjectType, h :: ObjectType, i :: ObjectType, j :: ObjectType, k :: ObjectType, l :: ObjectType)

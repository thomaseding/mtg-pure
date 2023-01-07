{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

-- NOTE: OTKN stands for ObjectTypeKindN
module MtgPure.Model.Object.OTKN_ (
  OTK0',
  OTK1',
  OTK2',
  OTK3',
  OTK4',
  OTK5',
  OTK6',
  OTK7',
  OTK8',
  OTK9',
  OTK10',
  OTK11',
  OTK12',
) where

type OTK0' ot = '()

type OTK1' ot a = '( '(), a :: ot)

type OTK2' ot a b = '( '(), a :: ot, b :: ot)

type OTK3' ot a b c = '( '(), a :: ot, b :: ot, c :: ot)

type OTK4' ot a b c d = '( '(), a :: ot, b :: ot, c :: ot, d :: ot)

type OTK5' ot a b c d e = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot)

type OTK6' ot a b c d e f = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot, f :: ot)

type OTK7' ot a b c d e f g = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot, f :: ot, g :: ot)

type OTK8' ot a b c d e f g h = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot, f :: ot, g :: ot, h :: ot)

type OTK9' ot a b c d e f g h i = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot, f :: ot, g :: ot, h :: ot, i :: ot)

type OTK10' ot a b c d e f g h i j = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot, f :: ot, g :: ot, h :: ot, i :: ot, j :: ot)

type OTK11' ot a b c d e f g h i j k = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot, f :: ot, g :: ot, h :: ot, i :: ot, j :: ot, k :: ot)

type OTK12' ot a b c d e f g h i j k l = '( '(), a :: ot, b :: ot, c :: ot, d :: ot, e :: ot, f :: ot, g :: ot, h :: ot, i :: ot, j :: ot, k :: ot, l :: ot)

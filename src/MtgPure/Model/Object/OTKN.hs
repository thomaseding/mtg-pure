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
  OTK13,
) where

import safe MtgPure.Model.Object.OT (OT)

type OTK0 = '[] :: [OT]

type OTK1 a = '[a] :: [OT]

type OTK2 a b = '[a, b] :: [OT]

type OTK3 a b c = '[a, b, c] :: [OT]

type OTK4 a b c d = '[a, b, c, d] :: [OT]

type OTK5 a b c d e = '[a, b, c, d, e] :: [OT]

type OTK6 a b c d e f = '[a, b, c, d, e, f] :: [OT]

type OTK7 a b c d e f g = '[a, b, c, d, e, f, g] :: [OT]

type OTK8 a b c d e f g h = '[a, b, c, d, e, f, g, h] :: [OT]

type OTK9 a b c d e f g h i = '[a, b, c, d, e, f, g, h, i] :: [OT]

type OTK10 a b c d e f g h i j = '[a, b, c, d, e, f, g, h, i, j] :: [OT]

type OTK11 a b c d e f g h i j k = '[a, b, c, d, e, f, g, h, i, j, k] :: [OT]

type OTK12 a b c d e f g h i j k l = '[a, b, c, d, e, f, g, h, i, j, k, l] :: [OT]

type OTK13 a b c d e f g h i j k l m = '[a, b, c, d, e, f, g, h, i, j, k, l, m] :: [OT]

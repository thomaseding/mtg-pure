{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Object.IndexOT (
  IndexOT (..),
  areObjectTypesSatisfied,
) where

import safe Data.Inst (
  Inst1,
  Inst10,
  Inst11,
  Inst12,
  Inst13,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
  Inst7,
  Inst8,
  Inst9,
 )
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OT (OT)
import safe MtgPure.Model.Object.OTN (
  OT0,
  OT1,
  OT10,
  OT11,
  OT12,
  OT13,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OT7,
  OT8,
  OT9,
 )

idx :: forall ot. (IsObjectType ot) => OT
idx = litObjectType @ot

class (Typeable ot) => IndexOT ot where
  -- | Prerequisite: Inner lists may not contain duplicate entries. Inner lists are also sorted.
  indexOT :: [[OT]]

instance (Inst2 IndexOT ot1 ot2) => IndexOT (ot1, ot2) where
  indexOT :: (Inst2 IndexOT ot1 ot2) => [[OT]]
  indexOT = indexOT @ot1 ++ indexOT @ot2

instance (Inst3 IndexOT ot1 ot2 ot3) => IndexOT (ot1, ot2, ot3) where
  indexOT :: (Inst3 IndexOT ot1 ot2 ot3) => [[OT]]
  indexOT = indexOT @ot1 ++ indexOT @ot2 ++ indexOT @ot3

instance IndexOT OT0 where
  indexOT :: [[OT]]
  indexOT = []

instance
  (Inst1 IsObjectType a) =>
  IndexOT (OT1 a)
  where
  indexOT :: (Inst1 IsObjectType a) => [[OT]]
  indexOT = [[idx @a]]

instance
  (Inst2 IsObjectType a b) =>
  IndexOT (OT2 a b)
  where
  indexOT :: (Inst2 IsObjectType a b) => [[OT]]
  indexOT = [[idx @a, idx @b]]

instance
  (Inst3 IsObjectType a b c) =>
  IndexOT (OT3 a b c)
  where
  indexOT :: (Inst3 IsObjectType a b c) => [[OT]]
  indexOT = [[idx @a, idx @b, idx @c]]

instance
  (Inst4 IsObjectType a b c d) =>
  IndexOT (OT4 a b c d)
  where
  indexOT :: (Inst4 IsObjectType a b c d) => [[OT]]
  indexOT = [[idx @a, idx @b, idx @c, idx @d]]

instance
  (Inst5 IsObjectType a b c d e) =>
  IndexOT (OT5 a b c d e)
  where
  indexOT :: (Inst5 IsObjectType a b c d e) => [[OT]]
  indexOT = [[idx @a, idx @b, idx @c, idx @d, idx @e]]

instance
  (Inst6 IsObjectType a b c d e f) =>
  IndexOT (OT6 a b c d e f)
  where
  indexOT :: (Inst6 IsObjectType a b c d e f) => [[OT]]
  indexOT = [[idx @a, idx @b, idx @c, idx @d, idx @e, idx @f]]

instance
  (Inst7 IsObjectType a b c d e f g) =>
  IndexOT (OT7 a b c d e f g)
  where
  indexOT :: (Inst7 IsObjectType a b c d e f g) => [[OT]]
  indexOT = [[idx @a, idx @b, idx @c, idx @d, idx @e, idx @f, idx @g]]

instance
  (Inst8 IsObjectType a b c d e f g h) =>
  IndexOT (OT8 a b c d e f g h)
  where
  indexOT :: (Inst8 IsObjectType a b c d e f g h) => [[OT]]
  indexOT = [[idx @a, idx @b, idx @c, idx @d, idx @e, idx @f, idx @g, idx @h]]

instance
  (Inst9 IsObjectType a b c d e f g h i) =>
  IndexOT (OT9 a b c d e f g h i)
  where
  indexOT :: (Inst9 IsObjectType a b c d e f g h i) => [[OT]]
  indexOT =
    [[idx @a, idx @b, idx @c, idx @d, idx @e, idx @f, idx @g, idx @h, idx @i]]

instance
  (Inst10 IsObjectType a b c d e f g h i j) =>
  IndexOT (OT10 a b c d e f g h i j)
  where
  indexOT :: (Inst10 IsObjectType a b c d e f g h i j) => [[OT]]
  indexOT =
    [
      [ idx @a
      , idx @b
      , idx @c
      , idx @d
      , idx @e
      , idx @f
      , idx @g
      , idx @h
      , idx @i
      , idx @j
      ]
    ]

instance
  (Inst11 IsObjectType a b c d e f g h i j k) =>
  IndexOT (OT11 a b c d e f g h i j k)
  where
  indexOT :: (Inst11 IsObjectType a b c d e f g h i j k) => [[OT]]
  indexOT =
    [
      [ idx @a
      , idx @b
      , idx @c
      , idx @d
      , idx @e
      , idx @f
      , idx @g
      , idx @h
      , idx @i
      , idx @j
      , idx @k
      ]
    ]

instance
  (Inst12 IsObjectType a b c d e f g h i j k l) =>
  IndexOT (OT12 a b c d e f g h i j k l)
  where
  indexOT :: (Inst12 IsObjectType a b c d e f g h i j k l) => [[OT]]
  indexOT =
    [
      [ idx @a
      , idx @b
      , idx @c
      , idx @d
      , idx @e
      , idx @f
      , idx @g
      , idx @h
      , idx @i
      , idx @j
      , idx @k
      , idx @l
      ]
    ]

instance
  (Inst13 IsObjectType a b c d e f g h i j k l m) =>
  IndexOT (OT13 a b c d e f g h i j k l m)
  where
  indexOT :: (Inst13 IsObjectType a b c d e f g h i j k l m) => [[OT]]
  indexOT =
    [
      [ idx @a
      , idx @b
      , idx @c
      , idx @d
      , idx @e
      , idx @f
      , idx @g
      , idx @h
      , idx @i
      , idx @j
      , idx @k
      , idx @l
      , idx @m
      ]
    ]

-- | Examples:
--  * `areObjectTypesSatisfied \@OTNArtifact \@OTNArtifact => True`
--  * `areObjectTypesSatisfied \@OTNArtifact \@OTNCreature => False`
--  * `areObjectTypesSatisfied \@OTNArtifact \@OTNArtifactCreature => True`
--  * `areObjectTypesSatisfied \@OTNArtifactCreature \@OTNArtifact => False`
--  * `areObjectTypesSatisfied \@OTNArtifactCreature \@OTNArtifactCreature => True`
areObjectTypesSatisfied :: forall ot ot'. (IndexOT ot, IndexOT ot') => Bool
areObjectTypesSatisfied = gos (indexOT @ot) (indexOT @ot')
 where
  gos :: [[OT]] -> [[OT]] -> Bool
  gos [] [] = True
  gos [] _ = False
  gos _ [] = False
  gos (ot : ots) (ot' : ots') = case go ot ot' of
    True -> gos ots ots'
    False -> False

  go :: [OT] -> [OT] -> Bool
  go [] _ = True
  go _ [] = False
  go (ot : ots) (ot' : ots') = case ot == ot' of
    True -> go ots ots'
    False -> go (ot : ots) ots'

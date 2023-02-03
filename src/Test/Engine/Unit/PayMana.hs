{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module Test.Engine.Unit.PayMana (
  main,
  mainUnitPayMana,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Engine.PayMana (CanPayManaCost (..), playerCanPayManaCost, possiblePayments)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.ManaCost (ManaCost (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool)
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ToManaCost (ToManaCost (..))
import safe MtgPure.Model.Mana.ToManaPool (ToCompleteManaPool (..))
import safe MtgPure.Model.Player (Player (..), emptyPlayer)
import safe MtgPure.Model.Variable (ForceVars (..))

main :: HasCallStack => IO ()
main = mainUnitPayMana

emptyPool :: HasCallStack => CompleteManaPool
emptyPool = mempty

toPlayer :: HasCallStack => ToCompleteManaPool pool => Life -> pool -> Player
toPlayer life pool =
  emptyPlayer
    { playerMana = toCompleteManaPool pool
    , playerLife = life
    }

data Solution
  = None
  | Unique
  | Ambiguous
  deriving (Eq, Show)

testPayment :: HasCallStack => (ToCompleteManaPool pool, ToManaCost cost) => Life -> pool -> cost -> Solution
testPayment life pool cost = go $ playerCanPayManaCost player cost'
 where
  player = toPlayer life pool
  cost' = forceVars $ toManaCost cost
  go = \case
    CantPayMana -> None
    CanPayMana mPayment -> case mPayment of
      Nothing -> Ambiguous
      Just _ -> Unique

testPaymentIO :: HasCallStack => (ToCompleteManaPool pool, ToManaCost cost) => Solution -> Life -> pool -> cost -> IO ()
testPaymentIO expected life pool cost = do
  let actual = testPayment life pool cost
  case actual == expected of
    True -> pure ()
    False -> error $ "Expected " <> show expected <> " but got " <> show actual

_debugCost :: (HasCallStack, ToManaCost cost) => cost -> IO ()
_debugCost cost = do
  let payments = possiblePayments $ costDynamic $ forceVars $ toManaCost cost
  mapM_ print payments
  print $ length payments

mainUnitPayMana :: HasCallStack => IO ()
mainUnitPayMana = do
  testEmptyPool
  testSingletonPool
  testSnowCosts
  testTwoBridCosts
  testMixedCosts
  testPhyrexianCosts
  putStrLn "Unit tests for PayMana passed"

testEmptyPool :: HasCallStack => IO ()
testEmptyPool = do
  testPaymentIO Unique 20 emptyPool 0
  testPaymentIO None 20 emptyPool 1
  testPaymentIO None 20 emptyPool 2
  testPaymentIO None 20 emptyPool W
  testPaymentIO None 20 emptyPool U
  testPaymentIO None 20 emptyPool B
  testPaymentIO None 20 emptyPool R
  testPaymentIO None 20 emptyPool G
  testPaymentIO None 20 emptyPool C
  testPaymentIO None 20 emptyPool S
  testPaymentIO None 20 emptyPool WU
  testPaymentIO None 20 emptyPool W2
  testPaymentIO None 20 emptyPool (W, W)
  testPaymentIO None 20 emptyPool (W, U)
  testPaymentIO None 20 emptyPool (1, U)

testSingletonPool :: HasCallStack => IO ()
testSingletonPool = do
  testPaymentIO Unique 20 W 0
  testPaymentIO Unique 20 W 1
  testPaymentIO Unique 20 W W
  testPaymentIO Unique 20 W WU
  testPaymentIO Unique 20 W W2
  testPaymentIO None 20 W U
  testPaymentIO None 20 W C
  testPaymentIO None 20 W S
  testPaymentIO None 20 W UB
  testPaymentIO None 20 W U2
  testPaymentIO Unique 20 SW S
  testPaymentIO Unique 20 SW WU
  testPaymentIO Unique 20 SW W2
  testPaymentIO None 20 SW UB
  testPaymentIO None 20 SW U2

testSnowCosts :: HasCallStack => IO ()
testSnowCosts = do
  testPaymentIO Unique 20 SW S
  testPaymentIO Unique 20 SW 0
  testPaymentIO Ambiguous 20 (SW, W) 1
  testPaymentIO Ambiguous 20 (SW, SG) 1
  testPaymentIO Unique 20 (SW, W) 2
  testPaymentIO Unique 20 (SW, W) (S, W)
  testPaymentIO Unique 20 (SW, W) (1, W)
  testPaymentIO Unique 20 (SW, W) (1, S)
  testPaymentIO None 20 (SW, SG) (W, W)
  testPaymentIO Ambiguous 20 (SW, SG) 1
  testPaymentIO Unique 20 (SW, SG) 2
  testPaymentIO Unique 20 (SW, SG) (W, G)
  testPaymentIO Unique 20 (SW, SG) (1, G)
  testPaymentIO Unique 20 (SW, SG) (1, W)
  testPaymentIO Unique 20 (SW, SG) (S, G)
  testPaymentIO Unique 20 (SW, SG) (S, W)
  testPaymentIO Unique 20 (SW, SG) (1, S)
  testPaymentIO Unique 20 (SW, SG) (1, S)
  testPaymentIO Unique 20 (SW, SG) (S, S)
  testPaymentIO None 20 (SW, SG) 3
  testPaymentIO Unique 20 (SW, W) S
  testPaymentIO Unique 20 (SW, (W, W)) S
  testPaymentIO Ambiguous 20 (SW, SU) S
  testPaymentIO Unique 20 (SW, SW) S
  testPaymentIO Unique 20 (SW, SW) (S, S)
  testPaymentIO Unique 20 (SW, SW, SW) (S, S)
  testPaymentIO Unique 20 (SW, SU) (S, S)
  testPaymentIO None 20 (SW, SW) (S, S, S)

testTwoBridCosts :: HasCallStack => IO ()
testTwoBridCosts = do
  testPaymentIO None 20 emptyPool W2
  testPaymentIO Unique 20 W W2
  testPaymentIO Unique 20 (U, U) W2
  testPaymentIO Unique 20 (W, W) (W2, W2)
  testPaymentIO None 20 (W, U) (W2, W2)
  testPaymentIO None 20 (U, U) (W2, W2)
  testPaymentIO None 20 (U, U, U) (W2, W2)
  testPaymentIO Unique 20 (W, U, U) (W2, W2)
  testPaymentIO Ambiguous 20 (W, W, U) (W2, W2)
  testPaymentIO Unique 20 (U, U, U, U) (W2, W2)
  testPaymentIO Ambiguous 20 (W, W) W2
  testPaymentIO Ambiguous 20 (W, B) W2
  testPaymentIO Unique 20 (W, W) (W2, W)
  testPaymentIO Ambiguous 20 (W, W, W) (W2, W)
  testPaymentIO Ambiguous 20 (W, W, U) (W2, W)
  testPaymentIO Unique 20 (W, U, U) (W2, W)
  testPaymentIO Ambiguous 20 (W, U, U) (W2, U)
  testPaymentIO Ambiguous 20 (W, W, U) (W2, U)
  testPaymentIO Unique 20 (W, U) (W2, U)
  testPaymentIO None 20 (U, U, U) (W2, W)

testMixedCosts :: HasCallStack => IO ()
testMixedCosts = do
  testPaymentIO Unique 20 (W, W, W) 0
  testPaymentIO Unique 20 (W, W, W) 1
  testPaymentIO Unique 20 (W, W, W) 2
  testPaymentIO Unique 20 (W, W, W) 3
  testPaymentIO None 20 (W, W, W) 4
  testPaymentIO Unique 20 (W, W, W, U, U) (W, U)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (U, U)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (W, W, W)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (W, W, W, U, U)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (W, W, U, U)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (W, W, W, U)
  testPaymentIO Unique 20 (W, W, W, U, U, C) 0
  testPaymentIO Unique 20 (W, W, W, U, U, C) (3, W, U, C)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (3, W, W, C)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (4, W, C)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (4, W, U)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (4, W, W)
  testPaymentIO Unique 20 (W, W, W, U, U, C) (5, W)
  testPaymentIO Unique 20 (W, W, W, U, U, C) 6
  testPaymentIO Ambiguous 20 (W, W, W, U, U, C) 1
  testPaymentIO Ambiguous 20 (W, W, W, U, U, C) 2
  testPaymentIO Ambiguous 20 (W, W, W, U, U, C) 3
  testPaymentIO Ambiguous 20 (W, W, W, U, U, C) 4
  testPaymentIO Ambiguous 20 (W, W, W, U, U, C) (4, W)
  testPaymentIO Ambiguous 20 (W, W, W, U, U, C) 5
  testPaymentIO None 20 (W, W, W, U, U, C) 7
  testPaymentIO Unique 20 (SW, SU) (W, U)

testPhyrexianCosts :: HasCallStack => IO ()
testPhyrexianCosts = do
  testPaymentIO Unique 20 emptyPool PW
  testPaymentIO Unique 3 emptyPool PW
  testPaymentIO None 2 emptyPool PW
  testPaymentIO None 1 emptyPool PW
  testPaymentIO None 0 emptyPool PW

  testPaymentIO Ambiguous 20 W PW
  testPaymentIO Ambiguous 3 W PW
  testPaymentIO Unique 2 W PW
  testPaymentIO Unique 1 W PW
  testPaymentIO Unique 0 W PW

  testPaymentIO Unique 5 emptyPool (PW, PW)
  testPaymentIO None 4 emptyPool (PW, PW)
  testPaymentIO None 3 emptyPool (PW, PW)
  testPaymentIO None 2 emptyPool (PW, PW)
  testPaymentIO None 1 emptyPool (PW, PW)
  testPaymentIO None 0 emptyPool (PW, PW)

  testPaymentIO Unique 5 emptyPool (PW, PU)
  testPaymentIO None 4 emptyPool (PW, PU)
  testPaymentIO None 3 emptyPool (PW, PU)
  testPaymentIO None 2 emptyPool (PW, PU)
  testPaymentIO None 1 emptyPool (PW, PU)
  testPaymentIO None 0 emptyPool (PW, PU)

  testPaymentIO Ambiguous 20 W (PW, PW)
  testPaymentIO Ambiguous 5 W (PW, PW)
  testPaymentIO Unique 4 W (PW, PW)
  testPaymentIO Unique 3 W (PW, PW)
  testPaymentIO None 2 W (PW, PW)
  testPaymentIO None 1 W (PW, PW)
  testPaymentIO None 0 W (PW, PW)
  testPaymentIO Unique 1 (W, W) (PW, PW)
  testPaymentIO Ambiguous 3 (W, W) (PW, PW)
  testPaymentIO Unique 2 (W, W) (PW, PW)
  testPaymentIO Unique 1 (W, W) (PW, PW)

  testPaymentIO Ambiguous 20 W (PW, PU)
  testPaymentIO Ambiguous 5 W (PW, PU)
  testPaymentIO Unique 4 W (PW, PU)
  testPaymentIO Unique 3 W (PW, PU)
  testPaymentIO None 2 W (PW, PU)
  testPaymentIO None 1 W (PW, PU)
  testPaymentIO None 0 W (PW, PU)
  testPaymentIO None 1 (W, W) (PW, PU)
  testPaymentIO None 1 (W, W) (PW, PU)
  testPaymentIO Unique 1 (W, U) (PW, PU)
  testPaymentIO Ambiguous 3 (W, U) (PW, PU)
  testPaymentIO Unique 1 (W, U, U) (PW, PU)
  testPaymentIO Unique 1 (W, U, U) (PW, PU)

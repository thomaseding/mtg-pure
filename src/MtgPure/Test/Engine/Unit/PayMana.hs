{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Test.Engine.Unit.PayMana (
  main,
  mainUnitPayMana,
) where

import safe GHC.Stack (HasCallStack)
import safe MtgPure.Engine.Pay (CanPayManaCost (..), playerCanPayManaCost)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.Mana (freezeMana)
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool)
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ToMana (ToMana (toMana))
import safe MtgPure.Model.Mana.ToManaCost (ToManaCost (..))
import safe MtgPure.Model.Mana.ToManaPool (ToCompleteManaPool (..))
import safe MtgPure.Model.Player (Player (..), emptyPlayer)
import safe MtgPure.Model.Variable (ForceVars (..))

main :: HasCallStack => IO ()
main = mainUnitPayMana

emptyPool :: HasCallStack => CompleteManaPool
emptyPool = mempty

mainUnitPayMana :: HasCallStack => IO ()
mainUnitPayMana = do
  testEmptyPool
  testSingletonPool
  testSnowCosts
  testMixedCosts
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
  testPaymentIO None 20 emptyPool BG
  testPaymentIO None 20 emptyPool (W, W)
  testPaymentIO None 20 emptyPool (W, U)
  testPaymentIO None 20 emptyPool (1, U)

testSingletonPool :: HasCallStack => IO ()
testSingletonPool = do
  testPaymentIO Unique 20 W 0
  testPaymentIO Unique 20 W 1
  testPaymentIO Unique 20 W W
  testPaymentIO None 20 W U
  testPaymentIO None 20 W C
  testPaymentIO None 20 W S
  testPaymentIO Unique 20 (freezeMana (toMana W)) S

testSnowCosts :: HasCallStack => IO ()
testSnowCosts = do
  testPaymentIO Unique 20 (freezeMana (toMana W)) S
  testPaymentIO Unique 20 (freezeMana (toMana W), W) 0
  testPaymentIO Ambiguous 20 (freezeMana (toMana W), W) 1
  testPaymentIO Ambiguous 20 (freezeMana (toMana W), freezeMana (toMana G)) 1
  testPaymentIO Unique 20 (freezeMana (toMana W), W) 2
  testPaymentIO Unique 20 (freezeMana (toMana W), W) (S, W)
  testPaymentIO Unique 20 (freezeMana (toMana W), W) (1, W)
  testPaymentIO Unique 20 (freezeMana (toMana W), W) (1, S)
  testPaymentIO None 20 (freezeMana (toMana W), freezeMana (toMana G)) (W, W)
  testPaymentIO Ambiguous 20 (freezeMana (toMana W), freezeMana (toMana G)) 1
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) 2
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) (W, G)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) (1, G)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) (1, W)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) (S, G)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) (S, W)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana W)) (1, S)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) (1, S)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana G)) (S, 2 :: Int)
  testPaymentIO None 20 (freezeMana (toMana W), freezeMana (toMana G)) 3
  testPaymentIO Unique 20 (freezeMana (toMana W), W) S
  testPaymentIO Unique 20 (freezeMana (toMana W), (W, 2)) S
  testPaymentIO Ambiguous 20 (freezeMana (toMana W), freezeMana (toMana U)) S
  testPaymentIO Unique 20 (freezeMana (toMana (W, 2 :: Int))) S
  testPaymentIO Unique 20 (freezeMana (toMana (W, 2 :: Int))) (S, 2 :: Int)
  testPaymentIO Unique 20 (freezeMana (toMana (W, 3 :: Int))) (S, 2 :: Int)
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana U)) (S, 2 :: Int)
  testPaymentIO None 20 (freezeMana (toMana (W, 2 :: Int))) (S, 3 :: Int)

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
  testPaymentIO Unique 20 (freezeMana (toMana W), freezeMana (toMana U)) (W, U)

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

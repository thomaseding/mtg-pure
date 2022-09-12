{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Recursive.Eq where

import safe qualified Control.Monad as M
import safe qualified Control.Monad.State.Strict as State
import safe Data.Inst (Inst1, Inst2)
import safe qualified Data.List as List
import safe Data.Typeable (Typeable, cast)
import safe MtgPure.Model.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.ManaPool (ManaPool)
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectId (ObjectId (..))
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectN.Type (OPlayer)
import safe MtgPure.Model.ObjectType (OT, ObjectType)
import safe MtgPure.Model.ObjectType.Permanent (WPermanent)
import safe MtgPure.Model.Recursive
  ( Effect (..),
    Elect (..),
    Requirement,
    Token (..),
    TypeableOT,
    WithObject (..),
  )
import safe MtgPure.Model.Selection (Selection)

instance Eq (Effect e) where
  (==) x y = runEnvM $ eqEffect x y

type EnvM = State.State Env

data Env = Env
  { nextRawId :: Int,
    envThing :: ()
  }

mkEnv :: Env
mkEnv =
  Env
    { nextRawId = 0,
      envThing = ()
    }

runEnvM :: EnvM a -> a
runEnvM m = State.evalState m mkEnv

andM :: Monad m => [m Bool] -> m Bool
andM = List.foldl' (M.liftM2 (&&)) $ pure True

eqEffect :: Effect e -> Effect e -> EnvM Bool
eqEffect = curry $ \case
  (AddMana mana1 player1, AddMana mana2 player2) ->
    andM [eqManaPool mana1 mana2, eqOPlayer player1 player2]
  (AddToBattlefield wPerm1 player1 token1, AddToBattlefield wPerm2 player2 token2) ->
    let go :: forall k ot. TypeableOT k ot => WPermanent ot -> Token ot -> EnvM Bool
        go wPerm1 token1 = case cast (wPerm2, token2) of
          Nothing -> pure False
          Just (wPerm2, token2) -> andM [eqWPermanent wPerm1 wPerm2, eqOPlayer player1 player2, eqToken token1 token2]
     in go wPerm1 token1
  (ChangeTo wPerm1 perm1 card1, ChangeTo wPerm2 perm2 card2) ->
    undefined (ChangeTo wPerm1 perm1 card1, ChangeTo wPerm2 perm2 card2)
  (CounterAbility ability1, CounterAbility ability2) ->
    undefined (CounterAbility ability1, CounterAbility ability2)
  (CounterSpell spell1, CounterSpell spell2) ->
    undefined (CounterSpell spell1, CounterSpell spell2)
  (DealDamage source1 victim1 damage1, DealDamage source2 victim2 damage2) ->
    undefined (DealDamage source1 victim1 damage1, DealDamage source2 victim2 damage2)
  (Destroy perm1, Destroy perm2) ->
    undefined (Destroy perm1, Destroy perm2)
  (DrawCards player1 amount1, DrawCards player2 amount2) ->
    undefined (DrawCards player1 amount1, DrawCards player2 amount2)
  (Sacrifice wPerm1 player1 reqs1, Sacrifice wPerm2 player2 reqs2) ->
    undefined (Sacrifice wPerm1 player1 reqs1, Sacrifice wPerm2 player2 reqs2)
  _ -> pure False

eqElect :: Elect e ot -> Elect e ot -> EnvM Bool
eqElect = curry $ \case
  (A sel1 with1, A sel2 with2) ->
    andM [eqSelection sel1 sel2, eqWithObjectElect with1 with2]
  _ -> pure False

eqManaPool :: ManaPool -> ManaPool -> EnvM Bool
eqManaPool = undefined

eqOPlayer :: OPlayer -> OPlayer -> EnvM Bool
eqOPlayer = undefined

eqRequirements :: [Requirement ot] -> [Requirement ot] -> EnvM Bool
eqRequirements = undefined

eqSelection :: Selection -> Selection -> EnvM Bool
eqSelection = undefined

eqToken :: Token ot -> Token ot -> EnvM Bool
eqToken = undefined

eqWithObject :: forall ot x. (x ot -> x ot -> EnvM Bool) -> WithObject x ot -> WithObject x ot -> EnvM Bool
eqWithObject eq = curry $ \case
  (O1 reqs1 cont1, O1 reqs2 cont2) -> case cast (reqs2, cont2) of
    Nothing -> pure False
    Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectCont eq cont1 cont2]
  (O2 reqs1 cont1, O2 reqs2 cont2) ->
    undefined (O2 reqs1 cont1, O2 reqs2 cont2)
  _ -> undefined

withObjectCont :: (x ot -> x ot -> EnvM Bool) -> (ObjectN a -> x ot) -> (ObjectN a -> x ot) -> EnvM Bool
withObjectCont = undefined

-- Can't figure out how to DRY this with `eqWithObject` 😢
eqWithObjectElect :: forall e ot. WithObject (Elect e) ot -> WithObject (Elect e) ot -> EnvM Bool
eqWithObjectElect = curry $ \case
  (O1 reqs1 cont1, O1 reqs2 cont2) ->
    let go :: forall a. Inst1 IsObjectType a => [Requirement '(OT, a)] -> (ObjectN '(OT, a) -> Elect e ot) -> EnvM Bool
        go reqs1 cont1 = case cast (reqs2, cont2) of
          Nothing -> pure False
          Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectElectCont @a O cont1 cont2]
     in go reqs1 cont1
  (O2 reqs1 cont1, O2 reqs2 cont2) ->
    let go :: forall a b. Inst2 IsObjectType a b => [Requirement '(OT, a, b)] -> (ObjectN '(OT, a, b) -> Elect e ot) -> EnvM Bool
        go reqs1 cont1 = case cast (reqs2, cont2) of
          Nothing -> pure False
          Just (reqs2, cont2) -> andM [eqRequirements reqs1 reqs2, withObjectElectCont @a O2a cont1 cont2]
     in go reqs1 cont1
  _ -> pure False

withObjectElectCont ::
  forall (a :: ObjectType) k ot' ot e.
  (IsObjectType a, TypeableOT k ot') =>
  (Object a -> ObjectN ot') ->
  (ObjectN ot' -> Elect e ot) ->
  (ObjectN ot' -> Elect e ot) ->
  EnvM Bool
withObjectElectCont cons cont1 cont2 = do
  objN <- newObjectN @a cons
  eqElect (cont1 objN) (cont2 objN)

eqWPermanent :: WPermanent ot -> WPermanent ot -> EnvM Bool
eqWPermanent = undefined

newObject :: forall a. IsObjectType a => EnvM (Object a)
newObject = do
  raw <- State.gets nextRawId
  let i = ObjectId raw
      obj = idToObject @a i
  State.modify' $ \st -> st {nextRawId = nextRawId st + 1}
  pure obj

newObjectN ::
  forall a ot.
  -- NOTE: The `Typeable (ObjectN ot)` not needed, but keep it for now to prove it's possible
  (Typeable (ObjectN ot), IsObjectType a) =>
  (Object a -> ObjectN ot) ->
  EnvM (ObjectN ot)
newObjectN make = do
  obj <- newObject @a
  let objN = make obj
  pure objN

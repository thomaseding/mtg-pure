{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use Lit" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

-- https://github.com/stoeffel/haskell-simple-dsl-examples/blob/master/ChatGADT.hs
module MtgPure.Test.Direct (
  module MtgPure.Test.Direct,
) where

import safe Data.IORef (modifyIORef', newIORef, readIORef)
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Test.Variabled (Var (Lit, Var), VarID)
import safe qualified MtgPure.Test.Variabled as X

data InterpM (dsl :: Type -> Type -> Type) (s :: Type) (a :: Type) :: Type where
  Interp :: dsl s a -> InterpM dsl s a
  Pure :: a -> InterpM dsl s a
  Then :: (Read a, Show a) => InterpM dsl s a -> InterpM dsl s b -> InterpM dsl s b
  Bind :: (Read a, Show a) => InterpM dsl s (Var s a) -> (Var s a -> InterpM dsl s b) -> InterpM dsl s b

instance X.Monad s (InterpM dsl) where
  pure = Pure
  (>>=) = Bind
  (>>) = Then

--https://www.reddit.com/r/haskell/comments/gxcxgl/comment/ftatasa/?utm_source=share&utm_medium=web2x&context=3
newtype I dsl s m = I {unI :: forall x. dsl s x -> m x}

--interpret :: Monad m => (forall s x. (dsl s x -> m x), InterpM dsl s a) -> m a
--interpret :: Monad m => (forall s x. (dsl s x -> m x, InterpM dsl s a)) -> m a
interpret :: Monad m => (forall s. (I dsl s m, InterpM dsl s a)) -> m a
interpret (go, m) = interpret' (unI go) m

interpret' :: Monad m => (forall x. dsl s x -> m x) -> InterpM dsl s a -> m a
interpret' go = \case
  Interp dsl -> go dsl
  Pure x -> pure x
  Bind x f -> interpret' go x >>= interpret' go . f
  Then x y -> interpret' go x >> interpret' go y

data Store (s :: Type) (a :: Type) :: Type where
  Get :: Var s String -> Store s (Var s String)
  Set :: Var s String -> Var s String -> Store s ()
  Print :: Var s String -> Store s ()

deriving instance (Show a) => Show (Store s a)

type StoreM s = InterpM Store s

instance Show a => Show (StoreM s a) where
  show = \case
    Interp dsl -> "Interp (" ++ show dsl ++ ")"
    Pure a -> "Pure (" ++ show a ++ ")"
    Then a b -> "Then (" ++ show a ++ ") (" ++ show b ++ ")"
    Bind a _f ->
      let sa = "(" ++ show a ++ ")"
          sf = "(show f)"
       in "Bind " ++ sa ++ " " ++ sf

indirection :: Var s String -> Var s String -> StoreM s ()
indirection key val =
  Interp (Set (Lit "x") (Lit "hello"))
    X.>> Interp (Set (Lit "y") (Lit "world"))
    X.>> Interp (Set (Lit "hello") (Lit "good"))
    X.>> Interp (Set (Lit "world") (Lit "riddance"))
    X.>> Interp (Get key) X.>>= \key' ->
      Interp (Set key' val)
        X.>> Interp (Get (Lit "x")) X.>>= \x ->
          Interp (Get (Lit "y")) X.>>= \y ->
            Interp (Get (Lit "hello")) X.>>= \hello ->
              Interp (Get (Lit "world")) X.>>= \world ->
                Interp (Print x)
                  X.>> Interp (Print y)
                  X.>> Interp (Print hello)
                  X.>> Interp (Print world)

dsl2 :: StoreM s (Var s String)
dsl2 = Pure (Lit "hello") X.>>= \x -> X.pure x

interpretStore :: (forall s. StoreM s a) -> IO a
interpretStore dsl = do
  varMapRef <- newIORef (mempty :: Map.Map VarID String)
  storeRef <- newIORef (mempty :: Map.Map String String)

  let readVar :: Var s String -> IO String
      readVar = \case
        Lit x -> pure x
        Var vid -> readVarID vid

      readVarID :: VarID -> IO String
      readVarID vid = do
        varMap <- readIORef varMapRef
        case Map.lookup vid varMap of
          Nothing -> error "internal logic error... forgot to hide Var constructor?"
          Just x -> pure x

      go :: Store s x -> IO x
      go = \case
        Get keyVar -> do
          varMap <- readIORef varMapRef
          store <- readIORef storeRef
          key <- readVar keyVar
          let val = Map.findWithDefault "EMPTY" key store
              vid = Map.size varMap
          modifyIORef' varMapRef $ Map.insert vid val
          pure $ Var vid
        Set keyVar valVar -> do
          key <- readVar keyVar
          val <- readVar valVar
          modifyIORef' storeRef $ Map.insert key val
        Print var -> case var of
          Lit x -> print ("Lit", show x)
          Var vid -> do
            x <- readVarID vid
            print ("Var", vid, show x)

  interpret (I go, dsl)

test :: IO ()
test = interpretStore $ indirection (Lit "x") (Lit "magic")

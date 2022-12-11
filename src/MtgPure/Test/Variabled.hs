{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Test.Variabled (
  VarID,
  Var (..),
  DString,
  Env (..),
  EnvM,
  EnvRead,
  EnvShow (..),
  RS,
  runEnvM,
) where

import safe qualified Control.Monad.State.Strict as State
import safe qualified Data.DList as DList
import safe Data.Kind (Constraint, Type)

type VarID = Int

data Var s (a :: Type) :: Type where
  Lit :: a -> Var s a
  Var :: VarID -> Var s a

type DString = DList.DList Char

data Env = Env
  { env_ :: ()
  , envVarID :: VarID
  }

type EnvM = State.State Env

runEnvM :: EnvM DString -> String
runEnvM = DList.toList . (`State.evalState` st)
 where
  st =
    Env
      { env_ = ()
      , envVarID = 0
      }

type RS a = (EnvRead a, EnvShow a)

type EnvRead (a :: Type) = () :: Constraint -- TODO

class EnvShow (a :: Type) where
  envShow :: a -> EnvM DString

instance EnvShow () where
  envShow _ = pure "()"

instance EnvShow Int where
  envShow = pure . DList.fromList . show

instance EnvShow String where
  envShow = pure . DList.fromList . show

instance EnvShow a => EnvShow (Var s a) where
  envShow = \case
    Lit a -> do
      sa <- envShow a
      pure $ "Lit (" <> sa <> ")"
    Var i -> do
      let sVar = DList.fromList $ "v" ++ show i
      pure sVar

instance EnvShow a => Show (Var s a) where
  show = runEnvM . envShow

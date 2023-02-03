{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Variable (
  Variable (..),
  VariableId' (..),
  VariableId,
  getVariableId,
  readVariable,
  Var (..),
  ForceVars (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

newtype VariableId' a = VariableId a
  deriving (Eq, Functor, Ord, Show)

type VariableId = VariableId' Int

data Variable (a :: Type) :: Type where
  ReifiedVariable :: VariableId -> a -> Variable a
  deriving (Functor, Typeable)

deriving instance Eq a => Eq (Variable a) -- TODO: Make this an orphan

deriving instance Ord a => Ord (Variable a) -- TODO: Make this an orphan

deriving instance Show a => Show (Variable a) -- TODO: Make this an orphan

getVariableId :: Variable a -> VariableId
getVariableId = \case
  ReifiedVariable vid _ -> vid

readVariable :: Variable a -> a
readVariable (ReifiedVariable _ a) = a

data Var
  = Var
  | NoVar
  deriving (Eq, Ord, Show)

class ForceVars vars noVars | vars -> noVars where
  forceVars :: vars -> noVars

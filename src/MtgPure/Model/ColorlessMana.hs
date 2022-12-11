{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ColorlessMana (
  ColorlessMana (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Variable (ForceVars (..), Var (..), Variable (..))

data ColorlessMana (v :: Var) :: Type where
  ColorlessMana' :: Int -> ColorlessMana v
  VariableColorlessMana :: Variable Int -> ColorlessMana 'Var
  SumColorlessMana :: ColorlessMana 'Var -> ColorlessMana 'Var -> ColorlessMana 'Var
  deriving (Typeable) --  TODO: Make some of these orphans

deriving instance Eq (ColorlessMana v)

deriving instance Ord (ColorlessMana v)

deriving instance Show (ColorlessMana v)

instance Semigroup (ColorlessMana v) where
  (<>) x y = case (x, y) of
    (ColorlessMana' a, ColorlessMana' b) -> ColorlessMana' (a + b)
    (ColorlessMana' 0, _) -> y
    (_, ColorlessMana' 0) -> x
    (VariableColorlessMana{}, _) -> SumColorlessMana x y
    (_, VariableColorlessMana{}) -> SumColorlessMana x y
    (SumColorlessMana{}, _) -> SumColorlessMana x y
    (_, SumColorlessMana{}) -> SumColorlessMana x y

instance Monoid (ColorlessMana v) where
  mempty = ColorlessMana' 0

instance Num (ColorlessMana 'NoVar) where
  (+) (ColorlessMana' x) (ColorlessMana' y) = ColorlessMana' $ x + y
  (-) (ColorlessMana' x) (ColorlessMana' y) = ColorlessMana' $ x - y
  (*) (ColorlessMana' x) (ColorlessMana' y) = ColorlessMana' $ x * y
  abs (ColorlessMana' x) = ColorlessMana' $ abs x
  signum (ColorlessMana' x) = ColorlessMana' $ signum x
  negate (ColorlessMana' x) = ColorlessMana' $ negate x
  fromInteger = ColorlessMana' . fromInteger

instance ForceVars (ColorlessMana v) (ColorlessMana 'NoVar) where
  forceVars = \case
    ColorlessMana' n -> ColorlessMana' n
    VariableColorlessMana (ReifiedVariable _ n) -> ColorlessMana' n
    SumColorlessMana x y -> forceVars x + forceVars y

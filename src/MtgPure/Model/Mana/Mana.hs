{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.Mana (
  Mana (..),
  IsManaNoVar,
  castManaType,
  litMana,
  thawMana,
  freezeMana,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Variable (
  ForceVars (..),
  Var (..),
  Variable (ReifiedVariable),
 )

type IsManaNoVar snow color =
  ( Typeable snow
  , Typeable color
  , Num (Mana 'NoVar snow color)
  )

data Mana (var :: Var) (snow :: Snow) (mt :: ManaType) :: Type where
  Mana :: Int -> Mana v snow mt
  VariableMana :: Variable Int -> Mana 'Var snow mt
  SumMana :: Mana 'Var snow mt -> Mana 'Var snow mt -> Mana 'Var snow mt
  deriving (Typeable)

-- TODO: Make this an orphan
deriving instance Eq (Mana var snow mt)

-- TODO: Make this an orphan
deriving instance Ord (Mana var snow mt)

-- TODO: Make this an orphan
deriving instance Show (Mana var snow mt)

-- TODO: Make this an orphan
instance Semigroup (Mana var snow mt) where
  (<>) :: Mana var snow mt -> Mana var snow mt -> Mana var snow mt
  (<>) x y = case (x, y) of
    (Mana a, Mana b) -> Mana (a + b)
    (Mana 0, _) -> y
    (_, Mana 0) -> x
    (VariableMana{}, _) -> SumMana x y
    (_, VariableMana{}) -> SumMana x y
    (SumMana{}, _) -> SumMana x y
    (_, SumMana{}) -> SumMana x y

-- TODO: Make this an orphan
instance Monoid (Mana var snow mt) where
  mempty :: Mana var snow mt
  mempty = Mana 0

-- TODO: Make this an orphan
instance (Num (Mana 'NoVar snow mt)) => ForceVars (Mana var snow mt) (Mana 'NoVar snow mt) where
  forceVars :: (Num (Mana 'NoVar snow mt)) => Mana var snow mt -> Mana 'NoVar snow mt
  forceVars = \case
    Mana n -> Mana n
    VariableMana (ReifiedVariable _ n) -> Mana n
    SumMana x y -> forceVars x + forceVars y

castManaType :: forall mt' mt snow var. Mana var snow mt -> Mana var snow mt'
castManaType = castManaImpl

castManaImpl :: Mana var snow mt -> Mana var snow' mt'
castManaImpl = \case
  Mana n -> Mana n
  VariableMana v -> VariableMana v
  SumMana x y -> SumMana (castManaImpl x) (castManaImpl y)

litMana :: Mana 'NoVar snow mt -> Mana 'Var snow mt
litMana (Mana x) = Mana x

thawMana :: Mana var 'Snow mt -> Mana var 'NonSnow mt
thawMana = castManaImpl

freezeMana :: Mana var 'NonSnow mt -> Mana var 'Snow mt
freezeMana = castManaImpl

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.Mana (
  Mana (..),
  IsManaNoVar,
  castManaSnow,
  castManaType,
  litMana,
  thawMana,
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

data Mana (v :: Var) (snow :: Snow) (mt :: ManaType) :: Type where
  Mana :: Int -> Mana v snow mt
  VariableMana :: Variable Int -> Mana 'Var snow mt
  SumMana :: Mana 'Var snow mt -> Mana 'Var snow mt -> Mana 'Var snow mt
  deriving (Typeable)

-- TODO: Make this an orphan
deriving instance Eq (Mana v snow mt)

-- TODO: Make this an orphan
deriving instance Ord (Mana v snow mt)

-- TODO: Make this an orphan
deriving instance Show (Mana v snow mt)

-- TODO: Make this an orphan
instance Semigroup (Mana v snow mt) where
  (<>) x y = case (x, y) of
    (Mana a, Mana b) -> Mana (a + b)
    (Mana 0, _) -> y
    (_, Mana 0) -> x
    (VariableMana{}, _) -> SumMana x y
    (_, VariableMana{}) -> SumMana x y
    (SumMana{}, _) -> SumMana x y
    (_, SumMana{}) -> SumMana x y

-- TODO: Make this an orphan
instance Monoid (Mana v snow mt) where
  mempty = Mana 0

-- TODO: Make this an orphan
instance Num (Mana 'NoVar snow mt) => ForceVars (Mana v snow mt) (Mana 'NoVar snow mt) where
  forceVars = \case
    Mana n -> Mana n
    VariableMana (ReifiedVariable _ n) -> Mana n
    SumMana x y -> forceVars x + forceVars y

castManaSnow :: forall snow' snow mt v. Mana v snow mt -> Mana v snow' mt
castManaSnow = castManaImpl

castManaType :: forall mt' mt snow v. Mana v snow mt -> Mana v snow mt'
castManaType = castManaImpl

castManaImpl :: Mana v snow mt -> Mana v snow' mt'
castManaImpl = \case
  Mana n -> Mana n
  VariableMana v -> VariableMana v
  SumMana x y -> SumMana (castManaImpl x) (castManaImpl y)

litMana :: Mana 'NoVar snow color -> Mana 'Var snow color
litMana (Mana x) = Mana x

thawMana :: Mana 'NoVar 'Snow color -> Mana 'NoVar 'NonSnow color
thawMana (Mana x) = Mana x

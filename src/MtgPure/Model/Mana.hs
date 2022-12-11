{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana (
  Mana (..),
  Snow (..),
  IsSnow (..),
  IsManaNoVar,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.ColoredMana (ColoredMana)
import safe MtgPure.Model.ColorlessMana (ColorlessMana)
import safe MtgPure.Model.GenericMana (GenericMana)
import safe MtgPure.Model.ManaType (ManaType (..))
import safe MtgPure.Model.Variable (ForceVars (..), Var (..))

type IsManaNoVar snow color =
  ( Typeable snow
  , Typeable color
  , Num (Mana 'NoVar snow color)
  )

data Snow = Snow | NonSnow
  deriving (Typeable)

class Typeable snow => IsSnow (snow :: Snow) where
  litSnow :: Snow

instance IsSnow 'Snow where
  litSnow = Snow

instance IsSnow 'NonSnow where
  litSnow = NonSnow

data Mana (var :: Var) (snow :: Snow) (mt :: ManaType) :: Type where
  WhiteMana :: ColoredMana v 'White -> Mana v snow 'MTWhite
  BlueMana :: ColoredMana v 'Blue -> Mana v snow 'MTBlue
  BlackMana :: ColoredMana v 'Black -> Mana v snow 'MTBlack
  RedMana :: ColoredMana v 'Red -> Mana v snow 'MTRed
  GreenMana :: ColoredMana v 'Green -> Mana v snow 'MTGreen
  ColorlessMana :: ColorlessMana v -> Mana v snow 'MTColorless
  GenericMana :: GenericMana v -> Mana v snow 'MTGeneric
  deriving (Typeable)

deriving instance Eq (Mana v snow mt) -- TODO: Make this an orphan

deriving instance Ord (Mana v snow mt) -- TODO: Make this an orphan

deriving instance Show (Mana v snow mt) -- TODO: Make this an orphan

instance Semigroup (Mana v snow 'MTWhite) where
  WhiteMana x <> WhiteMana y = WhiteMana (x <> y)

instance Semigroup (Mana v snow 'MTBlue) where
  BlueMana x <> BlueMana y = BlueMana (x <> y)

instance Semigroup (Mana v snow 'MTBlack) where
  BlackMana x <> BlackMana y = BlackMana (x <> y)

instance Semigroup (Mana v snow 'MTRed) where
  RedMana x <> RedMana y = RedMana (x <> y)

instance Semigroup (Mana v snow 'MTGreen) where
  GreenMana x <> GreenMana y = GreenMana (x <> y)

instance Semigroup (Mana v snow 'MTColorless) where
  ColorlessMana x <> ColorlessMana y = ColorlessMana (x <> y)

instance Semigroup (Mana v snow 'MTGeneric) where
  GenericMana x <> GenericMana y = GenericMana (x <> y)

instance Monoid (Mana v snow 'MTWhite) where
  mempty = WhiteMana mempty

instance Monoid (Mana v snow 'MTBlue) where
  mempty = BlueMana mempty

instance Monoid (Mana v snow 'MTBlack) where
  mempty = BlackMana mempty

instance Monoid (Mana v snow 'MTRed) where
  mempty = RedMana mempty

instance Monoid (Mana v snow 'MTGreen) where
  mempty = GreenMana mempty

instance Monoid (Mana v snow 'MTColorless) where
  mempty = ColorlessMana mempty

instance Monoid (Mana v snow 'MTGeneric) where
  mempty = GenericMana mempty

instance Num (Mana 'NoVar snow 'MTWhite) where
  (+) (WhiteMana x) (WhiteMana y) = WhiteMana $ x + y
  (-) (WhiteMana x) (WhiteMana y) = WhiteMana $ x - y
  (*) (WhiteMana x) (WhiteMana y) = WhiteMana $ x * y
  abs (WhiteMana x) = WhiteMana $ abs x
  signum (WhiteMana x) = WhiteMana $ signum x
  negate (WhiteMana x) = WhiteMana $ negate x
  fromInteger = WhiteMana . fromInteger

instance Num (Mana 'NoVar snow 'MTBlue) where
  (+) (BlueMana x) (BlueMana y) = BlueMana $ x + y
  (-) (BlueMana x) (BlueMana y) = BlueMana $ x - y
  (*) (BlueMana x) (BlueMana y) = BlueMana $ x * y
  abs (BlueMana x) = BlueMana $ abs x
  signum (BlueMana x) = BlueMana $ signum x
  negate (BlueMana x) = BlueMana $ negate x
  fromInteger = BlueMana . fromInteger

instance Num (Mana 'NoVar snow 'MTBlack) where
  (+) (BlackMana x) (BlackMana y) = BlackMana $ x + y
  (-) (BlackMana x) (BlackMana y) = BlackMana $ x - y
  (*) (BlackMana x) (BlackMana y) = BlackMana $ x * y
  abs (BlackMana x) = BlackMana $ abs x
  signum (BlackMana x) = BlackMana $ signum x
  negate (BlackMana x) = BlackMana $ negate x
  fromInteger = BlackMana . fromInteger

instance Num (Mana 'NoVar snow 'MTRed) where
  (+) (RedMana x) (RedMana y) = RedMana $ x + y
  (-) (RedMana x) (RedMana y) = RedMana $ x - y
  (*) (RedMana x) (RedMana y) = RedMana $ x * y
  abs (RedMana x) = RedMana $ abs x
  signum (RedMana x) = RedMana $ signum x
  negate (RedMana x) = RedMana $ negate x
  fromInteger = RedMana . fromInteger

instance Num (Mana 'NoVar snow 'MTGreen) where
  (+) (GreenMana x) (GreenMana y) = GreenMana $ x + y
  (-) (GreenMana x) (GreenMana y) = GreenMana $ x - y
  (*) (GreenMana x) (GreenMana y) = GreenMana $ x * y
  abs (GreenMana x) = GreenMana $ abs x
  signum (GreenMana x) = GreenMana $ signum x
  negate (GreenMana x) = GreenMana $ negate x
  fromInteger = GreenMana . fromInteger

instance Num (Mana 'NoVar snow 'MTColorless) where
  (+) (ColorlessMana x) (ColorlessMana y) = ColorlessMana $ x + y
  (-) (ColorlessMana x) (ColorlessMana y) = ColorlessMana $ x - y
  (*) (ColorlessMana x) (ColorlessMana y) = ColorlessMana $ x * y
  abs (ColorlessMana x) = ColorlessMana $ abs x
  signum (ColorlessMana x) = ColorlessMana $ signum x
  negate (ColorlessMana x) = ColorlessMana $ negate x
  fromInteger = ColorlessMana . fromInteger

instance Num (Mana 'NoVar snow 'MTGeneric) where
  (+) (GenericMana x) (GenericMana y) = GenericMana $ x + y
  (-) (GenericMana x) (GenericMana y) = GenericMana $ x - y
  (*) (GenericMana x) (GenericMana y) = GenericMana $ x * y
  abs (GenericMana x) = GenericMana $ abs x
  signum (GenericMana x) = GenericMana $ signum x
  negate (GenericMana x) = GenericMana $ negate x
  fromInteger = GenericMana . fromInteger

instance ForceVars (Mana v snow mt) (Mana 'NoVar snow mt) where
  forceVars = \case
    WhiteMana x -> WhiteMana $ forceVars x
    BlueMana x -> BlueMana $ forceVars x
    BlackMana x -> BlackMana $ forceVars x
    RedMana x -> RedMana $ forceVars x
    GreenMana x -> GreenMana $ forceVars x
    ColorlessMana x -> ColorlessMana $ forceVars x
    GenericMana x -> GenericMana $ forceVars x

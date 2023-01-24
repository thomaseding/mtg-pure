{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaSymbol (
  ManaSymbol (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))

data ManaSymbol :: ManaType -> Type where
  W :: ManaSymbol 'MTWhite
  U :: ManaSymbol 'MTBlue
  B :: ManaSymbol 'MTBlack
  R :: ManaSymbol 'MTRed
  G :: ManaSymbol 'MTGreen
  C :: ManaSymbol 'MTColorless
  S :: ManaSymbol 'MTSnow
  WU :: ManaSymbol 'MTHybridWU
  UB :: ManaSymbol 'MTHybridUB
  BR :: ManaSymbol 'MTHybridBR
  RG :: ManaSymbol 'MTHybridRG
  GW :: ManaSymbol 'MTHybridGW
  WB :: ManaSymbol 'MTHybridWB
  UR :: ManaSymbol 'MTHybridUR
  BG :: ManaSymbol 'MTHybridBG
  RW :: ManaSymbol 'MTHybridRW
  GU :: ManaSymbol 'MTHybridGU
  W2 :: ManaSymbol 'MTHybridW2
  U2 :: ManaSymbol 'MTHybridU2
  B2 :: ManaSymbol 'MTHybridB2
  R2 :: ManaSymbol 'MTHybridR2
  G2 :: ManaSymbol 'MTHybridG2
  PW :: ManaSymbol 'MTPhyrexianWhite
  PU :: ManaSymbol 'MTPhyrexianBlue
  PB :: ManaSymbol 'MTPhyrexianBlack
  PR :: ManaSymbol 'MTPhyrexianRed
  PG :: ManaSymbol 'MTPhyrexianGreen
  deriving (Typeable)

deriving instance Eq (ManaSymbol a)

deriving instance Ord (ManaSymbol a)

deriving instance Show (ManaSymbol a)

instance Semigroup (ManaSymbol a) where
  x <> _ = x

instance Monoid (ManaSymbol 'MTWhite) where
  mempty = W

instance Monoid (ManaSymbol 'MTBlue) where
  mempty = U

instance Monoid (ManaSymbol 'MTBlack) where
  mempty = B

instance Monoid (ManaSymbol 'MTRed) where
  mempty = R

instance Monoid (ManaSymbol 'MTGreen) where
  mempty = G

instance Monoid (ManaSymbol 'MTColorless) where
  mempty = C

instance Monoid (ManaSymbol 'MTSnow) where
  mempty = S

instance Monoid (ManaSymbol 'MTHybridWU) where
  mempty = WU

instance Monoid (ManaSymbol 'MTHybridUB) where
  mempty = UB

instance Monoid (ManaSymbol 'MTHybridBR) where
  mempty = BR

instance Monoid (ManaSymbol 'MTHybridRG) where
  mempty = RG

instance Monoid (ManaSymbol 'MTHybridGW) where
  mempty = GW

instance Monoid (ManaSymbol 'MTHybridWB) where
  mempty = WB

instance Monoid (ManaSymbol 'MTHybridUR) where
  mempty = UR

instance Monoid (ManaSymbol 'MTHybridBG) where
  mempty = BG

instance Monoid (ManaSymbol 'MTHybridRW) where
  mempty = RW

instance Monoid (ManaSymbol 'MTHybridGU) where
  mempty = GU

instance Monoid (ManaSymbol 'MTHybridW2) where
  mempty = W2

instance Monoid (ManaSymbol 'MTHybridU2) where
  mempty = U2

instance Monoid (ManaSymbol 'MTHybridB2) where
  mempty = B2

instance Monoid (ManaSymbol 'MTHybridR2) where
  mempty = R2

instance Monoid (ManaSymbol 'MTHybridG2) where
  mempty = G2

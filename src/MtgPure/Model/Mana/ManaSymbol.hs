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
  W :: ManaSymbol 'TyW
  U :: ManaSymbol 'TyU
  B :: ManaSymbol 'TyB
  R :: ManaSymbol 'TyR
  G :: ManaSymbol 'TyG
  C :: ManaSymbol 'TyC
  S :: ManaSymbol 'TyS
  SW :: ManaSymbol 'TySW
  SU :: ManaSymbol 'TySU
  SB :: ManaSymbol 'TySB
  SR :: ManaSymbol 'TySR
  SG :: ManaSymbol 'TySG
  SC :: ManaSymbol 'TySC
  WU :: ManaSymbol 'TyWU
  UB :: ManaSymbol 'TyUB
  BR :: ManaSymbol 'TyBR
  RG :: ManaSymbol 'TyRG
  GW :: ManaSymbol 'TyGW
  WB :: ManaSymbol 'TyWB
  UR :: ManaSymbol 'TyUR
  BG :: ManaSymbol 'TyBG
  RW :: ManaSymbol 'TyRW
  GU :: ManaSymbol 'TyGU
  W2 :: ManaSymbol 'TyW2
  U2 :: ManaSymbol 'TyU2
  B2 :: ManaSymbol 'TyB2
  R2 :: ManaSymbol 'TyR2
  G2 :: ManaSymbol 'TyG2
  C2 :: ManaSymbol 'TyC2
  PW :: ManaSymbol 'TyPW
  PU :: ManaSymbol 'TyPU
  PB :: ManaSymbol 'TyPB
  PR :: ManaSymbol 'TyPR
  PG :: ManaSymbol 'TyPG
  PC :: ManaSymbol 'TyPC
  deriving (Typeable)

deriving instance Eq (ManaSymbol a)

deriving instance Ord (ManaSymbol a)

deriving instance Show (ManaSymbol a)

instance Semigroup (ManaSymbol a) where
  x <> _ = x

instance Monoid (ManaSymbol 'TyW) where
  mempty = W

instance Monoid (ManaSymbol 'TyU) where
  mempty = U

instance Monoid (ManaSymbol 'TyB) where
  mempty = B

instance Monoid (ManaSymbol 'TyR) where
  mempty = R

instance Monoid (ManaSymbol 'TyG) where
  mempty = G

instance Monoid (ManaSymbol 'TyC) where
  mempty = C

instance Monoid (ManaSymbol 'TyS) where
  mempty = S

instance Monoid (ManaSymbol 'TyWU) where
  mempty = WU

instance Monoid (ManaSymbol 'TyUB) where
  mempty = UB

instance Monoid (ManaSymbol 'TyBR) where
  mempty = BR

instance Monoid (ManaSymbol 'TyRG) where
  mempty = RG

instance Monoid (ManaSymbol 'TyGW) where
  mempty = GW

instance Monoid (ManaSymbol 'TyWB) where
  mempty = WB

instance Monoid (ManaSymbol 'TyUR) where
  mempty = UR

instance Monoid (ManaSymbol 'TyBG) where
  mempty = BG

instance Monoid (ManaSymbol 'TyRW) where
  mempty = RW

instance Monoid (ManaSymbol 'TyGU) where
  mempty = GU

instance Monoid (ManaSymbol 'TyW2) where
  mempty = W2

instance Monoid (ManaSymbol 'TyU2) where
  mempty = U2

instance Monoid (ManaSymbol 'TyB2) where
  mempty = B2

instance Monoid (ManaSymbol 'TyR2) where
  mempty = R2

instance Monoid (ManaSymbol 'TyG2) where
  mempty = G2

instance Monoid (ManaSymbol 'TyC2) where
  mempty = C2

instance Monoid (ManaSymbol 'TyPW) where
  mempty = PW

instance Monoid (ManaSymbol 'TyPU) where
  mempty = PU

instance Monoid (ManaSymbol 'TyPB) where
  mempty = PB

instance Monoid (ManaSymbol 'TyPR) where
  mempty = PR

instance Monoid (ManaSymbol 'TyPG) where
  mempty = PG

instance Monoid (ManaSymbol 'TyPC) where
  mempty = PC

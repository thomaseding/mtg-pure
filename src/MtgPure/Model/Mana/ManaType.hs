{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaType (
  ManaType (..),
  SManaType (..),
  IsManaType (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.Snow (Snow (..))

data ManaType
  = MTWhite
  | MTBlue
  | MTBlack
  | MTRed
  | MTGreen
  | MTColorless
  | MTGeneric
  | MTSnow
  | MTHybridBG
  deriving (Eq, Ord, Typeable)

data SManaType (snow :: Snow) (mt :: ManaType) where
  SMTWhite :: SManaType 'NonSnow 'MTWhite
  SMTBlue :: SManaType 'NonSnow 'MTBlue
  SMTBlack :: SManaType 'NonSnow 'MTBlack
  SMTRed :: SManaType 'NonSnow 'MTRed
  SMTGreen :: SManaType 'NonSnow 'MTGreen
  SMTColorless :: SManaType 'NonSnow 'MTColorless
  SMTGeneric :: SManaType 'NonSnow 'MTGeneric
  SMTSnow :: SManaType 'Snow 'MTSnow
  SMTHybridBG :: SManaType 'NonSnow 'MTHybridBG

class IsManaType (snow :: Snow) (mt :: ManaType) where
  singManaType :: SManaType snow mt

instance IsManaType 'NonSnow 'MTWhite where
  singManaType = SMTWhite

instance IsManaType 'NonSnow 'MTBlue where
  singManaType = SMTBlue

instance IsManaType 'NonSnow 'MTBlack where
  singManaType = SMTBlack

instance IsManaType 'NonSnow 'MTRed where
  singManaType = SMTRed

instance IsManaType 'NonSnow 'MTGreen where
  singManaType = SMTGreen

instance IsManaType 'NonSnow 'MTColorless where
  singManaType = SMTColorless

instance IsManaType 'NonSnow 'MTGeneric where
  singManaType = SMTGeneric

instance IsManaType 'Snow 'MTSnow where
  singManaType = SMTSnow

instance IsManaType 'NonSnow 'MTHybridBG where
  singManaType = SMTHybridBG

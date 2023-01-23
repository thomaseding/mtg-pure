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
  | MTPhyrexianWhite
  | MTPhyrexianBlue
  | MTPhyrexianBlack
  | MTPhyrexianRed
  | MTPhyrexianGreen
  | MTPhyrexianColorless
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
  SMTPhyrexianWhite :: SManaType 'NonSnow 'MTPhyrexianWhite
  SMTPhyrexianBlue :: SManaType 'NonSnow 'MTPhyrexianBlue
  SMTPhyrexianBlack :: SManaType 'NonSnow 'MTPhyrexianBlack
  SMTPhyrexianRed :: SManaType 'NonSnow 'MTPhyrexianRed
  SMTPhyrexianGreen :: SManaType 'NonSnow 'MTPhyrexianGreen
  SMTPhyrexianColorless :: SManaType 'NonSnow 'MTPhyrexianColorless

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

instance IsManaType 'NonSnow 'MTPhyrexianWhite where
  singManaType = SMTPhyrexianWhite

instance IsManaType 'NonSnow 'MTPhyrexianBlue where
  singManaType = SMTPhyrexianBlue

instance IsManaType 'NonSnow 'MTPhyrexianBlack where
  singManaType = SMTPhyrexianBlack

instance IsManaType 'NonSnow 'MTPhyrexianRed where
  singManaType = SMTPhyrexianRed

instance IsManaType 'NonSnow 'MTPhyrexianGreen where
  singManaType = SMTPhyrexianGreen

instance IsManaType 'NonSnow 'MTPhyrexianColorless where
  singManaType = SMTPhyrexianColorless

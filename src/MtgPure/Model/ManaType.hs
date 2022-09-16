{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ManaType (
  ManaType (..),
  RealManaType (..),
  IsRealMana (..),
) where

import Data.Typeable (Typeable)

data ManaType
  = MTWhite
  | MTBlue
  | MTBlack
  | MTRed
  | MTGreen
  | MTColorless
  | MTGeneric
  | MTSnow
  deriving (Eq, Ord, Typeable)

data RealManaType (mt :: ManaType) where
  RealWhite :: RealManaType 'MTWhite
  RealBlue :: RealManaType 'MTBlue
  RealBlack :: RealManaType 'MTBlack
  RealRed :: RealManaType 'MTRed
  RealGreen :: RealManaType 'MTGreen
  RealColorless :: RealManaType 'MTColorless

class IsRealMana (mt :: ManaType) where
  realManaType :: RealManaType mt

instance IsRealMana 'MTWhite where
  realManaType = RealWhite

instance IsRealMana 'MTBlue where
  realManaType = RealBlue

instance IsRealMana 'MTBlack where
  realManaType = RealBlack

instance IsRealMana 'MTRed where
  realManaType = RealRed

instance IsRealMana 'MTGreen where
  realManaType = RealGreen

instance IsRealMana 'MTColorless where
  realManaType = RealColorless

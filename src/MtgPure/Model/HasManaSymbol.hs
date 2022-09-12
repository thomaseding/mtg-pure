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

module MtgPure.Model.HasManaSymbol
  ( HasManaSymbol (..),
  )
where

import MtgPure.Model.ManaSymbol (ManaSymbol)
import MtgPure.Model.ManaType (ManaType (..))

class HasManaSymbol a where
  manaSymbol :: ManaSymbol a

instance HasManaSymbol 'MTWhite where
  manaSymbol = mempty

instance HasManaSymbol 'MTBlue where
  manaSymbol = mempty

instance HasManaSymbol 'MTBlack where
  manaSymbol = mempty

instance HasManaSymbol 'MTRed where
  manaSymbol = mempty

instance HasManaSymbol 'MTGreen where
  manaSymbol = mempty

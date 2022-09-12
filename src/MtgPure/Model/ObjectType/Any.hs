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

module MtgPure.Model.ObjectType.Any
  ( WAny (..),
  )
where

import Data.Kind (Type)
import MtgPure.Model.ObjectType.Kind
  ( OTInstant,
    OTPlayer,
    OTSorcery,
  )
import MtgPure.Model.ObjectType.Permanent (WPermanent)

-- Witness type
data WAny :: forall a. a -> Type where
  WAnyInstant :: WAny OTInstant
  WAnySorcery :: WAny OTSorcery
  WAnyPlayer :: WAny OTPlayer
  WAnyPermanent :: WPermanent a -> WAny a

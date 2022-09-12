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

module MtgPure.Model.AnyObject
  ( AnyObject (..),
  )
where

import Data.Kind (Type)
import MtgPure.Model.ObjectType (OTInstant, OTPlayer, OTSorcery)
import MtgPure.Model.Permanent (Permanent)

-- Witness type
data AnyObject :: forall a. a -> Type where
  AnyInstant :: AnyObject OTInstant
  AnySorcery :: AnyObject OTSorcery
  AnyPlayer :: AnyObject OTPlayer
  AnyPermanent :: Permanent a -> AnyObject a

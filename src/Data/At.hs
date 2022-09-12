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

module Data.At
  ( At,
  )
where

import GHC.TypeLits (Nat)

type family At (i :: Nat) (a :: k)

type instance At 0 (a, b) = a

type instance At 1 (a, b) = b

type instance At 0 (a, b, c) = a

type instance At 1 (a, b, c) = b

type instance At 2 (a, b, c) = c

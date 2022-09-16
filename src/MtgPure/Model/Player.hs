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

module MtgPure.Model.Player (
  Player (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Graveyard (Graveyard)
import safe MtgPure.Model.Hand (Hand)
import safe MtgPure.Model.Life (Life)
import safe MtgPure.Model.ManaPool (CompleteManaPool)

data Player :: Type where
  Player ::
    { playerGraveyard :: Graveyard
    , playerHand :: Hand
    , playerLife :: Life
    , playerMana :: CompleteManaPool
    } ->
    Player
  deriving (Typeable)

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.PlayGame (
  playGame,
) where

import safe Data.Functor ((<&>))
import safe Data.Void (absurd)
import safe MtgPure.Engine.Fwd.Impl (fwdImpl)
import safe MtgPure.Engine.Fwd.Wrap (startGame)
import safe MtgPure.Engine.Monad (runMagicRW)
import safe MtgPure.Engine.Prompt (Prompt' (..))
import safe MtgPure.Engine.State (GameInput (..), GameResult, mkGameState)

playGame :: Monad m => GameInput m -> m (Maybe (GameResult m))
playGame input = case mkGameState fwdImpl input of
  Nothing -> do
    exceptionCantBeginGameWithoutPlayers $ gameInput_prompt input
    pure Nothing
  Just st ->
    runMagicRW st startGame <&> \case
      Left result -> Just result
      Right v -> absurd v

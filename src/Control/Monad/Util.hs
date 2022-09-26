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

module Control.Monad.Util (
  untilJust,
  AndLike (..),
) where

import safe Data.Void (Void)
import safe MtgPure.Engine.Legality (Legality (..), fromLegality, toLegality)

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m =
  m >>= \case
    Just x -> pure x
    Nothing -> untilJust m

class AndLike a where
  andM :: Monad m => [m a] -> m a

instance AndLike () where
  andM = sequence_

instance AndLike (Maybe Void) where
  andM ms = Nothing <$ sequence_ ms

instance AndLike Bool where
  andM = \case
    [] -> pure True
    m : ms ->
      m >>= \case
        True -> andM ms
        False -> pure False

instance AndLike Legality where
  andM m = do
    let bs = map (fmap fromLegality) m
    b <- andM bs
    pure $ toLegality b

instance AndLike (Maybe ()) where
  andM = \case
    [] -> pure $ Just ()
    m : ms ->
      m >>= \case
        Just () -> case ms of
          [] -> pure $ Just ()
          _ -> andM ms
        Nothing -> pure Nothing

instance AndLike (Maybe Legality) where
  andM = \case
    [] -> pure $ Just Legal
    m : ms ->
      m >>= \case
        Just Legal -> case ms of
          [] -> pure $ Just Legal
          _ -> andM ms
        Just Illegal -> pure Nothing
        Nothing -> pure Nothing

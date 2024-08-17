{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Control.Monad.Util (
  Attempt' (..),
  Attempt,
  untilJust,
  AndLike (..),
) where

import safe Data.Data (Typeable)
import safe Data.Void (Void)
import safe MtgPure.Engine.Legality (Legality (..), fromLegality, toLegality)

newtype Attempt' a = Attempt a
  deriving (Eq, Functor, Ord, Show, Typeable)

type Attempt = Attempt' Int

untilJust :: (Monad m) => (Attempt -> m (Maybe a)) -> m a
untilJust = untilJust' $ Attempt 0

untilJust' :: (Monad m) => Attempt -> (Attempt -> m (Maybe a)) -> m a
untilJust' attempt fm =
  fm attempt >>= \case
    Just x -> pure x
    Nothing -> untilJust' ((1 +) <$> attempt) fm

class AndLike a where
  andM :: (Monad m) => [m a] -> m a

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

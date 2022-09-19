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
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.IsCardList (
  IsCardList (..),
  pushCard,
  popCard,
  containsCard,
  removeCard,
  cardAtIndex,
) where

import safe qualified Data.List as List
import safe MtgPure.Model.Recursive (Card)
import safe MtgPure.Model.Recursive.Ord ()

class IsCardList cards where
  toCardList :: [Card ()] -> cards
  fromCardList :: cards -> [Card ()]

pushCard :: IsCardList cards => Card () -> cards -> cards
pushCard card cards = toCardList $ card : fromCardList cards

popCard :: IsCardList cards => cards -> Maybe (Card (), cards)
popCard cards = case fromCardList cards of
  card : cards' -> Just (card, toCardList cards')
  [] -> Nothing

containsCard :: IsCardList cards => Card () -> cards -> Bool
containsCard card cards = card `elem` fromCardList cards

removeCard :: IsCardList cards => Card () -> cards -> Maybe cards
removeCard card cards = case containsCard card cards of
  True -> Just $ toCardList $ List.delete card $ fromCardList cards
  False -> Nothing

cardAtIndex :: IsCardList cards => cards -> Int -> Maybe (Card ())
cardAtIndex cards i = case i < length cards' of
  True -> Just $ cards' !! i
  False -> Nothing
 where
  cards' = fromCardList cards

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
import safe MtgPure.Model.Recursive (AnyCard)
import safe MtgPure.Model.Recursive.Ord ()

class IsCardList cards where
  toCardList :: [AnyCard] -> cards
  fromCardList :: cards -> [AnyCard]

pushCard :: IsCardList cards => AnyCard -> cards -> cards
pushCard card cards = toCardList $ card : fromCardList cards

popCard :: IsCardList cards => cards -> Maybe (AnyCard, cards)
popCard cards = case fromCardList cards of
  card : cards' -> Just (card, toCardList cards')
  [] -> Nothing

containsCard :: IsCardList cards => AnyCard -> cards -> Bool
containsCard card cards = card `elem` fromCardList cards

removeCard :: IsCardList cards => AnyCard -> cards -> Maybe cards
removeCard card cards = case containsCard card cards of
  True -> Just $ toCardList $ List.delete card $ fromCardList cards
  False -> Nothing

cardAtIndex :: IsCardList cards => cards -> Int -> Maybe AnyCard
cardAtIndex cards i = case i < length cards' of
  True -> Just $ cards' !! i
  False -> Nothing
 where
  cards' = fromCardList cards

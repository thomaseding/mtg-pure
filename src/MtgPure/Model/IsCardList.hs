{-# LANGUAGE Safe #-}
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
import safe MtgPure.Model.Recursive.Ord ()

class (Eq card) => IsCardList cards card | cards -> card where
  toCardList :: [card] -> cards
  fromCardList :: cards -> [card]

pushCard :: (IsCardList cards card) => card -> cards -> cards
pushCard card cards = toCardList $ card : fromCardList cards

popCard :: (IsCardList cards card) => cards -> Maybe (card, cards)
popCard cards = case fromCardList cards of
  card : cards' -> Just (card, toCardList cards')
  [] -> Nothing

containsCard :: (IsCardList cards card) => card -> cards -> Bool
containsCard card cards = card `elem` fromCardList cards

removeCard :: (IsCardList cards card) => card -> cards -> Maybe cards
removeCard card cards = case containsCard card cards of
  True -> Just $ toCardList $ List.delete card $ fromCardList cards
  False -> Nothing

cardAtIndex :: (IsCardList cards card) => cards -> Int -> Maybe card
cardAtIndex cards i = case i < length cards' of
  True -> Just $ cards' !! i
  False -> Nothing
 where
  cards' = fromCardList cards

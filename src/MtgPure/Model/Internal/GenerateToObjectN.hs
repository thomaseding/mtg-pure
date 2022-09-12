{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use tuple-section" #-}

module MtgPure.Model.Internal.GenerateToObjectN
  ( internalGenerateToObjectN,
  )
where

import Control.Monad (forM_)
import Data.List (intercalate, subsequences, (\\))
import Data.Maybe (catMaybes)

header :: String
header =
  "\
  \{-# LANGUAGE ConstraintKinds #-}\n\
  \{-# LANGUAGE DataKinds #-}\n\
  \{-# LANGUAGE FlexibleContexts #-}\n\
  \{-# LANGUAGE FlexibleInstances #-}\n\
  \{-# LANGUAGE GADTs #-}\n\
  \{-# LANGUAGE MultiParamTypeClasses #-}\n\
  \{-# LANGUAGE PolyKinds #-}\n\
  \{-# LANGUAGE RankNTypes #-}\n\
  \{-# LANGUAGE Safe #-}\n\
  \{-# LANGUAGE ScopedTypeVariables #-}\n\
  \{-# LANGUAGE TypeFamilyDependencies #-}\n\
  \{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}\n\
  \\n\
  \{-# HLINT ignore \"Avoid lambda\" #-}\n\
  \{-# HLINT ignore \"Use const\" #-}\n\
  \\n\
  \-- This file is generated by MtgPure.Model.Internal.GenerateToObjectN\n\
  \module MtgPure.Model.ToObjectN\n\
  \  ( ToObject2 (..),\n\
  \    ToObject3 (..),\n\
  \  )\n\
  \where\n\
  \\n\
  \import Data.Inst\n\
  \  ( Inst2,\n\
  \    Inst3,\n\
  \  )\n\
  \import MtgPure.Model.IsObjectType (IsObjectType)\n\
  \import MtgPure.Model.Object (Object)\n\
  \import MtgPure.Model.ObjectN (ObjectN (..))\n\
  \"

internalGenerateToObjectN :: IO ()
internalGenerateToObjectN = do
  putStrLn header
  let gens = do
        let limit = 3
        m <- [1 .. limit]
        n <- [1 .. limit]
        map (\g -> (g, m, n)) $ generateObjectMsToObjectN m n
  forM_ gens $ \(gen, m, n) -> do
    putStrLn $ "-- " ++ show (m, n)
    putStrLn gen

--class Inst2 IsObjectType a b => ToObject2 o a b where
--  toObject2 :: o -> ObjectN '(a, b)
--
--class Inst3 IsObjectType a b c => ToObject3 o a b c where
--  toObject3 :: o -> ObjectN '(a, b, c)

generateObjectMsToObjectN :: Int -> Int -> [String]
generateObjectMsToObjectN m n =
  concat
    [ do
        letter <- lettersN
        catMaybes [generateObjectToObjectN letter lettersN],
      reverse $ do
        lettersM <- lettersMs
        catMaybes [generateObjectMToObjectN lettersM lettersN]
    ]
  where
    letters = map (: []) ['a' .. 'z']
    lettersN = take n letters
    subseqs = subsequences lettersN
    lettersMs = filter (\xs -> length xs == m) subseqs

generateObjectToObjectN :: String -> [String] -> Maybe String
generateObjectToObjectN letter lettersN =
  if
      | n > 1 -> Just $ instanceLine ++ "\n  " ++ funcLine ++ "\n"
      | otherwise -> Nothing
  where
    n = length lettersN

    instanceLine =
      unwords $
        [ "instance",
          "Inst" ++ show n,
          "IsObjectType"
        ]
          ++ lettersN
          ++ [ "=>",
               "TOObject" ++ show n,
               "(Object " ++ letter ++ ")"
             ]
          ++ lettersN
          ++ ["where"]

    funcLine =
      unwords
        [ "toObject" ++ show n,
          "=",
          "O" ++ show n ++ letter
        ]

generateObjectMToObjectN :: [String] -> [String] -> Maybe String
generateObjectMToObjectN lettersM lettersN =
  if
      | n <= 1 -> Nothing
      | m == n -> Just $ instanceLine ++ "\n  toObject" ++ show n ++ " = id\n"
      | m + 1 == n -> Just $ instanceLine ++ "\n  toObject" ++ show n ++ " = ON" ++ show n ++ letterMissing ++ "\n"
      | otherwise -> Nothing
  where
    m = length lettersM
    n = length lettersN

    letterMissing = case lettersN \\ lettersM of
      [x] -> x
      _ -> undefined

    seqLettersM = intercalate ", " lettersM

    instanceLine =
      unwords $
        [ "instance",
          "Inst" ++ show n,
          "IsObjectType"
        ]
          ++ lettersN
          ++ [ "=>",
               "TOObject" ++ show n,
               "(ObjectN '(" ++ seqLettersM ++ "))"
             ]
          ++ lettersN
          ++ ["where"]

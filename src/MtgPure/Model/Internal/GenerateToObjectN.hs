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
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Internal.GenerateToObjectN
  ( main,
    internalGenerateToObjectN,
  )
where

import Data.List (intercalate, sort, sortBy, subsequences, (\\))
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import MtgPure.Model.ObjectType (ObjectType)

-- runhaskell MtgPure/Model/Internal/GenerateToObjectN.hs > MtgPure/Model/ToObjectN.hs
main :: IO ()
main = internalGenerateToObjectN

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
  \{-# HLINT ignore \"Redundant bracket\" #-}\n\
  \\n\
  \-- This file is generated by MtgPure.Model.Internal.GenerateToObjectN\n\
  \module MtgPure.Model.ToObjectN\n\
  \  ( ToObject2 (..),\n\
  \    ToObject3 (..),\n\
  \    ToObject4 (..),\n\
  \    ToObject5 (..),\n\
  \    ToObject6 (..),\n\
  \    ToObject7 (..),\n\
  \    ToObject8 (..),\n\
  \  )\n\
  \where\n\
  \\n\
  \import Data.Inst\n\
  \  ( Inst2,\n\
  \    Inst3,\n\
  \    Inst4,\n\
  \    Inst5,\n\
  \    Inst6,\n\
  \    Inst7,\n\
  \    Inst8,\n\
  \  )\n\
  \import MtgPure.Model.IsObjectType (IsObjectType)\n\
  \import MtgPure.Model.Object (Object)\n\
  \import MtgPure.Model.ObjectN (ObjectN (..))\n\
  \import MtgPure.Model.ObjectType (ObjectType(..))\n\
  \\n\
  \type A = 'OTArtifact\n\
  \type B = 'OTCreature\n\
  \type C = 'OTEnchantment\n\
  \type D = 'OTInstant\n\
  \type E = 'OTLand\n\
  \type F = 'OTPlaneswalker\n\
  \type G = 'OTPlayer\n\
  \type H = 'OTSorcery\n\
  \"

limit :: Int
limit = 1 + fromEnum (maxBound :: ObjectType)

newtype Sym = Sym Int
  deriving (Eq, Ord, Show)

data SymDesc = SymObject [String] | SymLetter
  deriving (Eq, Ord, Show)

interpretSym :: SymDesc -> Sym -> String
interpretSym desc (Sym n) = case desc of
  SymObject os -> os !! n
  SymLetter -> [['a' ..] !! n]

allSyms :: [Sym]
allSyms = map Sym [0 .. fromEnum (maxBound :: ObjectType)]

allLetters :: [String]
allLetters = map (interpretSym SymLetter) allSyms

objectTypeDescs :: [SymDesc]
objectTypeDescs = map SymObject $ filter p $ subsequences objectTypes
  where
    p xs = not (null xs) && length xs <= limit

objectTypes :: [String]
objectTypes = map f [minBound :: ObjectType .. maxBound]
  where
    f o = [['A' ..] !! fromEnum o]

-- https://stackoverflow.com/a/3100764
nubOrd :: Ord a => [a] -> [a]
nubOrd = go Set.empty
  where
    go s (x : xs)
      | x `Set.member` s = go s xs
      | otherwise = x : go (Set.insert x s) xs
    go _ _ = []

internalGenerateToObjectN :: IO ()
internalGenerateToObjectN = do
  putStrLn header
  mapM_ putStrLn toObjectNs
  let results1 = nubOrd $ concatMap objectToObjectNs objectTypeDescs
  let results2 = nubOrd $ concatMap objectMsToObjectN objectTypeDescs
  mapM_ putStrLn results1
  mapM_ putStrLn results2

----------------------------------------

toObjectNs :: [String]
toObjectNs = mapMaybe generateToObjectN [1 .. limit]

generateToObjectN :: Int -> Maybe String
generateToObjectN n =
  if
      | n > 1 -> Just $ classLine ++ "\n  " ++ funcLine ++ "\n"
      | otherwise -> Nothing
  where
    lettersN = take n allLetters
    seqLettersN = commas lettersN

    classLine =
      unwords $
        [ "class",
          "Inst" ++ show n,
          "IsObjectType"
        ]
          ++ lettersN
          ++ [ "=>",
               "ToObject" ++ show n,
               "o"
             ]
          ++ lettersN
          ++ ["where"]

    funcLine =
      unwords
        [ "toObject" ++ show n,
          "::",
          "o",
          "->",
          "(ObjectN '(" ++ seqLettersN ++ "))"
        ]

----------------------------------------

objectToObjectNs :: SymDesc -> [String]
objectToObjectNs desc = do
  let lim = case desc of
        SymObject syms -> length syms
        SymLetter -> error "should be supplied SymObject instead"
  n <- [1 .. lim]
  s <- generateObjectsToObjectN desc n
  pure $ "-- (" ++ show n ++ ")\n" ++ s

generateObjectsToObjectN :: SymDesc -> Int -> [String]
generateObjectsToObjectN desc n = do
  sym <- symsN
  catMaybes [generateObjectToObjectN desc sym symsN]
  where
    symsN = take n allSyms

generateObjectToObjectN :: SymDesc -> Sym -> [Sym] -> Maybe String
generateObjectToObjectN desc sym symN =
  if
      | n <= 1 -> Nothing
      | n > 1 -> Just $ instanceLine ++ "\n  " ++ funcLine ++ "\n"
      | otherwise -> Nothing
  where
    n = length symN

    instanceLine =
      unwords $
        [ "instance",
          "ToObject" ++ show n,
          "(Object " ++ interpretSym desc sym ++ ")"
        ]
          ++ map (interpretSym desc) symN
          ++ ["where"]

    funcLine =
      unwords
        [ "toObject" ++ show n,
          "=",
          "O" ++ show n ++ interpretSym SymLetter sym
        ]

----------------------------------------

objectMsToObjectN :: SymDesc -> [String]
objectMsToObjectN desc = do
  (m, n) <- indexPairs
  s <- generateObjectMsToObjectN desc m n
  pure $ "-- " ++ show (m, n) ++ "\n" ++ s
  where
    lim = case desc of
      SymObject syms -> length syms
      SymLetter -> error "should be passed SymObject instead"
    indexPairs = sortBy cmp $ do
      m <- [2 .. lim]
      n <- [m .. lim]
      pure (m, n)
    cmp (a, b) (x, y) = compare (b, a) (y, x)

generateObjectMsToObjectN :: SymDesc -> Int -> Int -> [String]
generateObjectMsToObjectN desc m n = reverse $ do
  symsM <- symsMs
  catMaybes [generateObjectMToObjectN desc symsM symsN]
  where
    symsN = take n allSyms
    subseqs = subsequences symsN
    symsMs = filter (\xs -> length xs == m) subseqs

generateObjectMToObjectN :: SymDesc -> [Sym] -> [Sym] -> Maybe String
generateObjectMToObjectN desc symsM symsN =
  if
      | n <= 1 -> Nothing
      | m < 2 -> Nothing
      --  | m == 1 && symsM /= ["a"] -> Nothing
      | m == n -> Just $ instanceLine ++ "\n  toObject" ++ show n ++ " = id\n"
      | m + 1 == n -> Just $ instanceLine ++ "\n  toObject" ++ show n ++ " = ON" ++ show n ++ letterMissing ++ "\n"
      | otherwise -> Just $ instanceLine ++ "\n  toObject" ++ show n ++ " x = " ++ telescope ++ "\n"
  where
    m = length symsM
    n = length symsN
    letterMissing = case symsN \\ symsM of
      [x] -> interpretSym SymLetter x
      _ -> error "impossible"
    seqSymsM = commas $ map (interpretSym desc) symsM
    telescope = telescopeToObjectN desc "x" symsM symsN
    instanceLine =
      unwords $
        [ "instance",
          "ToObject" ++ show n,
          "(ObjectN '(" ++ seqSymsM ++ "))"
        ]
          ++ map (interpretSym desc) symsN
          ++ ["where"]

telescopeToObjectN :: SymDesc -> String -> [Sym] -> [Sym] -> String
telescopeToObjectN desc acc symsM symsN = case m < n of
  True -> telescopeToObjectN desc acc' symsSucc symsN
  False -> acc
  where
    m = length symsM
    n = length symsN
    toObjectSucc = "toObject" ++ show (m + 1)
    newSym = head $ symsN \\ symsM
    symsSucc = sort $ newSym : symsM
    typeSucc = "(ObjectN '(" ++ commas (map (interpretSym desc) symsSucc) ++ "))"
    acc' = "(" ++ toObjectSucc ++ " " ++ acc ++ " :: " ++ typeSucc ++ ")"

commas :: [String] -> String
commas = intercalate ", "

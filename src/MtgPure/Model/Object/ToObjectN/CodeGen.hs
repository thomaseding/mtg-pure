-- XXX: Keep these LANGUAGE pragmas in this file for code generation scripts
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use camelCase" #-}

module MtgPure.Model.Object.ToObjectN.CodeGen (
  main,
  mainCodeGenToObjectN,
) where

import safe Data.List (sort, sortBy, subsequences, (\\))
import safe Data.Maybe (catMaybes)
import safe qualified Data.Set as Set
import safe qualified Data.Traversable as T
import safe MtgPure.Model.Object.OT (OT)
import safe System.FilePath (dropExtension)
import safe System.IO (IOMode (..), hFlush, hPutStrLn, withFile)
import System.Process (system)

--------------------------------------------------------------------------------

srcDir :: FilePath
srcDir = "."

--------------------------------------------------------------------------------

-- runhaskell MtgPure/Model/Object/ToObjectN/CodeGen.hs
main :: IO ()
main = mainCodeGenToObjectN

runFourmolu :: FilePath -> IO ()
runFourmolu path = do
  let cmd = "fourmolu --mode inplace " ++ path
  _ <- system cmd
  pure ()

mkInstancesHeader :: [String] -> String
mkInstancesHeader importPaths =
  unlines
    [ "{-# LANGUAGE Safe #-}"
    , ""
    , "-- This file is generated by MtgPure.Model.Object.ToObjectN.CodeGen"
    , "module MtgPure.Model.Object.ToObjectN.Instances () where"
    , ""
    ]
    ++ unlines (map (\x -> "import safe " ++ x ++ " ()") importPaths)

mkInstancesHeaderN :: Int -> String
mkInstancesHeaderN n =
  unlines
    [ "{-# LANGUAGE ConstraintKinds #-}"
    , "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE FlexibleContexts #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE GADTs #-}"
    , "{-# LANGUAGE MultiParamTypeClasses #-}"
    , "{-# LANGUAGE NoMonomorphismRestriction #-}"
    , "{-# LANGUAGE PolyKinds #-}"
    , "{-# LANGUAGE RankNTypes #-}"
    , "{-# LANGUAGE Safe #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE TypeFamilyDependencies #-}"
    , "{-# OPTIONS_GHC -Wno-orphans #-}"
    , "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}"
    , ""
    , "{-# HLINT ignore \"Avoid lambda\" #-}"
    , "{-# HLINT ignore \"Use const\" #-}"
    , "{-# HLINT ignore \"Redundant bracket\" #-}"
    , ""
    , "-- This file is generated by MtgPure.Model.Object.ToObjectN.CodeGen"
    , "module MtgPure.Model.Object.ToObjectN.Instances.ToObject_" ++ padShow n ++ " () where"
    , ""
    , "import safe Data.Inst (Inst" ++ show n ++ ")"
    , "import safe MtgPure.Model.Object.IsObjectType (IsObjectType)"
    , "import safe MtgPure.Model.Object.OTN (OT" ++ show n ++ ")"
    , "import safe MtgPure.Model.Object.Object (Object)"
    , "import safe MtgPure.Model.Object.ObjectN (ObjectN (..))"
    , "import safe MtgPure.Model.Object.ToObjectN.Classes (ToObject" ++ show n ++ "'(..))"
    , ""
    ]

mkInstancesHeaderMN :: [Dependency] -> Int -> Int -> String
mkInstancesHeaderMN deps m n =
  unlines
    [ "{-# LANGUAGE ConstraintKinds #-}"
    , "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE FlexibleContexts #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE GADTs #-}"
    , "{-# LANGUAGE MultiParamTypeClasses #-}"
    , "{-# LANGUAGE NoMonomorphismRestriction #-}"
    , "{-# LANGUAGE PolyKinds #-}"
    , "{-# LANGUAGE RankNTypes #-}"
    , "{-# LANGUAGE Safe #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE TypeFamilyDependencies #-}"
    , "{-# OPTIONS_GHC -Wno-orphans #-}"
    , "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}"
    , ""
    , "{-# HLINT ignore \"Avoid lambda\" #-}"
    , "{-# HLINT ignore \"Use const\" #-}"
    , "{-# HLINT ignore \"Redundant bracket\" #-}"
    , ""
    , "-- This file is generated by MtgPure.Model.Object.ToObjectN.CodeGen"
    , "module MtgPure.Model.Object.ToObjectN.Instances.ToObject_" ++ padShow n ++ "_" ++ padShow m ++ " () where"
    , ""
    , "import safe Data.Inst (Inst" ++ show n ++ ")"
    , "import safe MtgPure.Model.Object.IsObjectType (IsObjectType)"
    , "import safe MtgPure.Model.Object.ObjectN (ObjectN (..))"
    , "import safe MtgPure.Model.Object.OTN ("
    , forDeps \case
        Dep_ToObject{} -> ""
        Dep_toObject{} -> ""
        Dep_OT x -> "  OT" ++ show x ++ ","
    , "  )"
    , "import safe MtgPure.Model.Object.ToObjectN.Classes ("
    , forDeps \case
        Dep_ToObject x -> case x == 1 && n > 1 of
          True -> ""
          False -> "  ToObject" ++ show x ++ "(..),"
        Dep_toObject{} -> ""
        Dep_OT{} -> ""
    , "  )"
    , m_to_n_pairs \x y -> do
        let s = "import safe MtgPure.Model.Object.ToObjectN.Instances.ToObject_" ++ padShow y ++ "_" ++ padShow x ++ " ()"
        if
          | x == y -> ""
          | x == m && y == n -> ""
          | x == m && y == m + 1 -> s
          | x == m + 1 && y == n -> s
          | otherwise -> ""
    , ""
    ]
 where
  forDeps f = unlines $ map f deps
  m_to_n_pairs f = unlines $ filter (not . null) $ map (uncurry f) $ nubOrd do
    x <- [m .. n]
    y <- [x .. n]
    pure (x, y)

padShow :: (Show a) => a -> String
padShow x = case show x of
  [y] -> ['0', y]
  ys -> ys

useDebugLimit :: Bool
useDebugLimit = False

limit :: Int
limit = case useDebugLimit of
  True -> 4
  False -> 1 + fromEnum (maxBound :: OT)

newtype Sym = Sym Int
  deriving (Eq, Ord, Show)

data SymDesc
  = SymObjectN [Char]
  | SymObject
  deriving (Eq, Ord, Show)

interpretSym :: SymDesc -> Sym -> String
interpretSym desc (Sym n) = case desc of
  SymObjectN os -> [os !! n]
  SymObject -> [['a' ..] !! n]

allSyms :: [Sym]
allSyms = case useDebugLimit of
  True -> map Sym [0 .. limit - 1]
  False -> map Sym [0 .. fromEnum (maxBound :: OT)]

objectTypeDescs :: [SymDesc]
objectTypeDescs =
  map SymObjectN $
    filter p $
      map
        (`take` objectTypes)
        [0 .. limit]
 where
  p xs = not (null xs) && length xs <= limit

objectTypes :: [Char]
objectTypes = map f [minBound :: OT .. maxBound]
 where
  f o = ['A' ..] !! fromEnum o

-- https://stackoverflow.com/a/3100764
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go Set.empty
 where
  go s (x : xs)
    | x `Set.member` s = go s xs
    | otherwise = x : go (Set.insert x s) xs
  go _ _ = []

mainCodeGenToObjectN :: IO ()
mainCodeGenToObjectN = do
  let commonDir = srcDir ++ "/MtgPure/Model/Object/ToObjectN"
  let instancesHs = commonDir ++ "/Instances.hs"
  let instancesDir = commonDir ++ "/Instances"
  hsPaths' <- T.for objectTypeDescs \desc -> do
    let letters = case desc of
          SymObjectN os -> os
          SymObject -> error "should be passed SymObjectN instead"
    hsPaths1 <- T.for (getNIndices desc) \n ->
      if
        | length letters > n -> pure ""
        | otherwise -> do
            let results = objectToObjectNs desc n
            let hsPath :: FilePath = instancesDir ++ "/ToObject_" ++ padShow n ++ ".hs"
            withFile hsPath WriteMode \h -> do
              hPutStrLn h $ mkInstancesHeaderN n
              hPutStrLn h $ unlines results
              hFlush h -- needed?
            runFourmolu hsPath
            pure hsPath
    hsPaths2 <- T.for (getMNIndexPairs desc) \(m, n) ->
      if
        | length letters > n -> pure ""
        | otherwise -> do
            let zippedResults = objectMsToObjectN desc m n
            let (results, deps') = unzip zippedResults
            let deps = nubOrd $ concat deps'
            let hsPath :: FilePath = instancesDir ++ "/ToObject_" ++ padShow n ++ "_" ++ padShow m ++ ".hs"
            withFile hsPath WriteMode \h -> do
              print (m, n)
              hPutStrLn h $ mkInstancesHeaderMN deps m n
              hPutStrLn h $ unlines results
              hFlush h -- needed?
            runFourmolu hsPath
            pure hsPath
    pure $ hsPaths1 ++ hsPaths2
  let hsPaths = concatMap (filter (not . null)) hsPaths'
  withFile instancesHs WriteMode \h -> do
    let importPaths = map filePathToImport hsPaths
    hPutStrLn h $ mkInstancesHeader importPaths
    hFlush h -- needed?
  runFourmolu instancesHs

-- example: Foo\Bar/ToObject.hs becomes Foo.Bar.ToObject
filePathToImport :: FilePath -> String
filePathToImport = dropExtension . drop (length srcDir + 1) . go
 where
  go = \case
    [] -> []
    '/' : xs -> '.' : go xs
    '\\' : xs -> '.' : go xs
    x : xs -> x : go xs

----------------------------------------

objectToObjectNs :: SymDesc -> Int -> [String]
objectToObjectNs desc n = nubOrd do
  generateObjectsToObjectN (if False then desc else SymObject) n

getNIndices :: SymDesc -> [Int]
getNIndices desc = case desc of
  SymObjectN syms -> [1 .. length syms]
  SymObject -> error "should be passed SymObjectN instead"

generateObjectsToObjectN :: SymDesc -> Int -> [String]
generateObjectsToObjectN desc n = do
  sym <- symsN
  catMaybes [generateObjectToObjectN desc sym symsN]
 where
  symsN = take n allSyms

generateObjectToObjectN :: SymDesc -> Sym -> [Sym] -> Maybe String
generateObjectToObjectN desc sym symN =
  if
    | n < 1 -> Nothing
    | otherwise -> Just $ instanceLine ++ "\n" ++ signatureLine ++ "\n" ++ implLine ++ "\n"
 where
  n = length symN

  letter = interpretSym desc sym
  spacesLetters = unwords $ map (interpretSym desc) symN
  s_Inst = "Inst" ++ show n ++ " IsObjectType " ++ spacesLetters
  s_ToObject' = "ToObject" ++ show n ++ "' " ++ letter ++ " " ++ spacesLetters
  s_toObject' = "toObject" ++ show n ++ "'"
  s_OT = "OT" ++ show n ++ " " ++ spacesLetters

  instanceLine = "instance " ++ s_Inst ++ " => " ++ s_ToObject' ++ " where"

  signatureLine =
    "  "
      ++ s_toObject'
      ++ " :: "
      ++ s_Inst
      ++ " => Object "
      ++ letter
      ++ " -> ObjectN "
      ++ parens s_OT

  implLine =
    "  toObject"
      ++ show n
      ++ "'= O"
      ++ if n == 1
        then "1"
        else show n ++ letter

----------------------------------------

data Dependency
  = Dep_ToObject Int
  | Dep_toObject Int
  | Dep_OT Int
  deriving (Eq, Ord, Show)

objectMsToObjectN :: SymDesc -> Int -> Int -> [(String, [Dependency])]
objectMsToObjectN desc m n = nubOrd do
  generateObjectMsToObjectN (if False then desc else SymObject) m n

getMNIndexPairs :: SymDesc -> [(Int, Int)]
getMNIndexPairs desc = sortBy cmp $ nubOrd do
  m <- [1 .. lim]
  n <- [m .. lim]
  pure (m, n)
 where
  lim = case desc of
    SymObjectN syms -> length syms
    SymObject -> error "should be passed SymObjectN instead"
  cmp (a, b) (x, y) = compare (b, a) (y, x)

generateObjectMsToObjectN :: SymDesc -> Int -> Int -> [(String, [Dependency])]
generateObjectMsToObjectN desc m n = reverse do
  symsM <- symsMs
  catMaybes [generateObjectMToObjectN desc symsM symsN]
 where
  symsN = take n allSyms
  subSeqs = subsequences symsN
  symsMs = filter (\xs -> length xs == m) subSeqs

generateObjectMToObjectN :: SymDesc -> [Sym] -> [Sym] -> Maybe (String, [Dependency])
generateObjectMToObjectN desc symsM symsN =
  if
    | m == n ->
        let
          implLine = "  toObject" ++ show n ++ " = id"
         in
          Just
            ( instanceLine ++ "\n" ++ signatureLine ++ "\n" ++ implLine ++ "\n"
            , [Dep_ToObject n, Dep_OT n, Dep_OT m]
            )
    | n <= 1 ->
        Nothing
    | m < 1 ->
        Nothing
    | m + 1 == n ->
        let
          implLine = "  toObject" ++ show n ++ " = ON" ++ show n ++ letterMissing
         in
          Just
            ( instanceLine ++ "\n" ++ signatureLine ++ "\n" ++ implLine ++ "\n"
            , [Dep_ToObject n, Dep_OT n, Dep_OT m]
            )
    | otherwise ->
        let
          implLine = "  " ++ telescope
         in
          Just
            ( instanceLine ++ "\n" ++ signatureLine ++ "\n" ++ implLine ++ "\n"
            , map Dep_ToObject [m + 1, n]
                ++ map Dep_toObject [m + 1, n]
                ++ map Dep_OT [m, m + 1, n]
            )
 where
  m = length symsM
  n = length symsN
  letterMissing = case symsN \\ symsM of
    [x] -> interpretSym SymObject x
    _ -> error "impossible"
  lettersM = map (interpretSym desc) symsM
  spacesLettersM = unwords lettersM
  telescope = telescopeToObjectN desc symsM symsN
  letters = map (interpretSym desc) symsN
  s_Inst = parens $ "Inst" ++ show n ++ " IsObjectType " ++ unwords letters
  s_OT_N = "OT" ++ show n ++ " " ++ unwords letters
  s_OT_M = "OT" ++ show m ++ " " ++ spacesLettersM
  s_ToObject = "ToObject" ++ show n
  s_toObject = "toObject" ++ show n
  instanceLine =
    "instance "
      ++ s_Inst
      ++ "=>"
      ++ s_ToObject
      ++ parens s_OT_M
      ++ unwords letters
      ++ " where"
  signatureLine =
    "  "
      ++ s_toObject
      ++ "::"
      ++ s_Inst
      ++ "=>"
      ++ "ObjectN"
      ++ parens s_OT_M
      ++ "->"
      ++ "ObjectN"
      ++ parens s_OT_N

telescopeToObjectN :: SymDesc -> [Sym] -> [Sym] -> String
telescopeToObjectN desc symsM symsN = "toObject" ++ show n ++ " x = " ++ toN
 where
  newSym = head $ symsN \\ symsM
  symsSucc = sort $ newSym : symsM
  m = length symsM + 1
  n = length symsN
  lettersM = map (interpretSym desc) symsSucc
  spacesLettersM = unwords lettersM
  otnM = "OT" ++ show m ++ " " ++ spacesLettersM
  objectM = "ObjectN " ++ parens otnM
  toM = parens $ "toObject" ++ show m ++ " x :: " ++ objectM
  toN = "toObject" ++ show n ++ " " ++ toM

parens :: String -> String
parens x = "(" ++ x ++ ")"

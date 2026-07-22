{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | A compile-check for the code rendered by "MtgPure.Model.Recursive.Show".
--
-- The @Test.Render.CardOracle*@ tests only pin the rendered text against golden
-- files; nothing verifies the rendered string is still valid, type-checking
-- Haskell. This test closes that gap: it renders every card and token from
-- "MtgPure.AllCards" under each 'ShowOptions' variant the oracle tests cover,
-- wraps all of them as top-level bindings in a single generated module
-- (@.output\/ShowCompileCheck.hs@), and shells out to GHC to type-check it. A
-- renamed constructor, a bad @\@Ty@ application, or a dropped combinator that
-- kept the text stable but broke compilation shows up here.
--
-- This module is deliberately not @Safe@: it shells out ('System.Process') and
-- touches the filesystem ('System.Directory').
module Test.Render.ShowCompiles (
  main,
  mainRenderShowCompiles,
) where

import Data.List (isInfixOf)
import GHC.Stack (HasCallStack)
import MtgPure.AllCards (allCards, allTokens)
import MtgPure.Model.CardName (CardName (unCardName), getCardName)
import MtgPure.Model.Recursive.Show (
  DataCombinators (..),
  ShowOptions (..),
  defaultShowOptions,
  showAnyCardWith,
  showAnyTokenWith,
 )
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Git-ignored scratch/build directory (same one the gallery scripts use).
outputDir :: FilePath
outputDir = ".output"

-- | The generated module GHC type-checks. Its base name must match its
-- @module@ header (@ShowCompileCheck@).
modulePath :: FilePath
modulePath = outputDir ++ "/ShowCompileCheck.hs"

-- | The 'ShowOptions' variants to compile, mirroring the @Test.Render.CardOracle*@
-- suite: the all-on default plus each single-knob configuration the oracle
-- tests isolate (plus the low\/no @dataCombinators@ renderings). The
-- slug is used verbatim in generated binding names, so it must be a valid
-- identifier fragment.
variants :: [(String, ShowOptions)]
variants =
  [ ("default", defaultShowOptions)
  , ("multiline", ShowOptions{showOptions_wildcardUnusedVars = False, showOptions_blockArguments = False, showOptions_multiline = True, showOptions_smartAliasing = False, showOptions_dataCombinators = HighDataCombinators})
  , ("wildcard", ShowOptions{showOptions_wildcardUnusedVars = True, showOptions_blockArguments = False, showOptions_multiline = False, showOptions_smartAliasing = False, showOptions_dataCombinators = HighDataCombinators})
  , ("block", ShowOptions{showOptions_wildcardUnusedVars = False, showOptions_blockArguments = True, showOptions_multiline = False, showOptions_smartAliasing = False, showOptions_dataCombinators = HighDataCombinators})
  , ("smart", ShowOptions{showOptions_wildcardUnusedVars = False, showOptions_blockArguments = False, showOptions_multiline = False, showOptions_smartAliasing = True, showOptions_dataCombinators = HighDataCombinators})
  , -- The high variant currently duplicates what "default" compiles (the
    -- default level is 'HighDataCombinators'), but pins the level explicitly
    -- so each 'DataCombinators' rendering stays compiled even if
    -- 'defaultShowOptions' ever changes.
    ("highdatacombinators", ShowOptions{showOptions_wildcardUnusedVars = False, showOptions_blockArguments = False, showOptions_multiline = False, showOptions_smartAliasing = False, showOptions_dataCombinators = HighDataCombinators})
  , ("lowdatacombinators", ShowOptions{showOptions_wildcardUnusedVars = False, showOptions_blockArguments = False, showOptions_multiline = False, showOptions_smartAliasing = False, showOptions_dataCombinators = LowDataCombinators})
  , ("nodatacombinators", ShowOptions{showOptions_wildcardUnusedVars = False, showOptions_blockArguments = False, showOptions_multiline = False, showOptions_smartAliasing = False, showOptions_dataCombinators = NoDataCombinators})
  ]

-- | One generated binding: (name, type, rendered body, human label). The label
-- is used only to point back at the offending card in a failure report.
type Gen = (String, String, String, String)

gens :: [Gen]
gens =
  [ ("c_" ++ slug ++ "_" ++ show i, "AnyCard", showAnyCardWith opts c, unCardName (getCardName c) ++ " [" ++ slug ++ "]")
  | (slug, opts) <- variants
  , (i, c) <- zip [0 :: Int ..] allCards
  ]
    ++ [ ("t_" ++ slug ++ "_" ++ show i, "AnyToken", showAnyTokenWith opts t, "token: " ++ unCardName (getCardName t) ++ " [" ++ slug ++ "]")
       | (slug, opts) <- variants
       , (i, t) <- zip [0 :: Int ..] allTokens
       ]

-- | Every language extension in the cabal @default-extensions@ list, plus
-- @ExtendedDefaultRules@ (rendered bodies contain bare numeric literals like
-- @Damage 5@ / @toManaCost 7@, exactly as "MtgPure.Cards" does per-file).
languageExtensions :: [String]
languageExtensions =
  [ "AllowAmbiguousTypes"
  , "ApplicativeDo"
  , "BangPatterns"
  , "BlockArguments"
  , "ConstraintKinds"
  , "DataKinds"
  , "DefaultSignatures"
  , "DeriveFunctor"
  , "EmptyDataDecls"
  , "ExtendedDefaultRules"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "FunctionalDependencies"
  , "GADTs"
  , "InstanceSigs"
  , "LambdaCase"
  , "MagicHash"
  , "MultiParamTypeClasses"
  , "MultiWayIf"
  , "OverloadedStrings"
  , "PatternSynonyms"
  , "PolyKinds"
  , "QuantifiedConstraints"
  , "RankNTypes"
  , "RecursiveDo"
  , "ScopedTypeVariables"
  , "StandaloneDeriving"
  , "StrictData"
  , "TemplateHaskellQuotes"
  , "TypeApplications"
  , "TypeFamilyDependencies"
  , "TypeOperators"
  , "ViewPatterns"
  ]

-- | The imports that put every name the renderer can emit in scope. Derived
-- from the "MtgPure.Cards" import block, but the two DSL-vocabulary modules
-- ('MtgPure.Model.Recursive' and 'MtgPure.Model.Combinators') are opened so a
-- combinator the renderer emits but a given card didn't happen to use is still
-- in scope. @Mana.Snow@ is intentionally omitted: its @Snow@ constructor clashes
-- with 'MtgPure.Model.Supertype.Snow', which is the only @Snow@ the renderer
-- emits.
importLines :: [String]
importLines =
  [ "import Data.Nat (NatList (..))"
  , "import MtgPure.Model.BasicLandType (BasicLandType (..))"
  , "import MtgPure.Model.CardName (CardName (CardName))"
  , "import MtgPure.Model.Color (Color (..))"
  , "import MtgPure.Model.Colors (Colors (..))"
  , "import MtgPure.Model.ColorsLike (ColorsLike (toColors))"
  , "import MtgPure.Model.Combinators"
  , "import MtgPure.Model.CreatureType (CreatureType (..))"
  , "import MtgPure.Model.Damage (Damage' (..))"
  , "import MtgPure.Model.LandType (LandType (..))"
  , "import MtgPure.Model.Mana.Mana (Mana (..))"
  , "import MtgPure.Model.Mana.ManaCost (DynamicManaCost (..), HybridManaCost (..), ManaCost (..), PhyrexianManaCost (..))"
  , "import MtgPure.Model.Mana.ManaPool (ManaPool (..))"
  , "import MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))"
  , "import MtgPure.Model.Mana.ManaType (ManaType (..))"
  , "import MtgPure.Model.Mana.Snow (Snow (NonSnow))"
  , "import MtgPure.Model.Mana.ToManaCost (ToManaCost (..))"
  , "import MtgPure.Model.Mana.ToManaPool (ToManaPool (..))"
  , "import MtgPure.Model.Object.OT (OT (..))"
  , "import MtgPure.Model.Object.ObjectN (ObjectN (..))"
  , "import MtgPure.Model.Object.OTN"
  , "import MtgPure.Model.Object.OTNAliases"
  , "import MtgPure.Model.Object.ToObjectN.Instances ()"
  , "import MtgPure.Model.Power (Power (..))"
  , "import MtgPure.Model.Recursive"
  , "import MtgPure.Model.Step (Step (..))"
  , "import MtgPure.Model.Supertype (Supertype (..))"
  , "import MtgPure.Model.TimePoint (TimePoint (..))"
  , "import MtgPure.Model.Toughness (Toughness (..))"
  , "import MtgPure.Model.Variable (Variable (..))"
  , "import MtgPure.Model.ZoneObject.Convert"
  , "import MtgPure.Model.Zone (Zone (..))"
  , "import MtgPure.Model.ZoneObject.ZoneObject (ZO, ZoneObject (..))"
  ]

-- | The full text of the module GHC type-checks.
generatedSource :: String
generatedSource =
  unlines $
    [ "{-# LANGUAGE " ++ ext ++ " #-}" | ext <- languageExtensions
    ]
      ++ [ "{-# OPTIONS_GHC -Wno-type-defaults #-}"
         , ""
         , "module ShowCompileCheck where"
         , ""
         ]
      ++ importLines
      ++ [""]
      ++ concatMap renderGen gens
 where
  renderGen (name, ty, body, label) =
    [ "-- " ++ label
    , name ++ " :: " ++ ty
    , name ++ " = " ++ body
    , ""
    ]

-- | GHC invocation. @cabal exec@ exposes the project's dependency package-db and
-- the already-built inplace @mtg-pure@ library; @-package mtg-pure@ then lets the
-- generated module import the precompiled model, so GHC only type-checks the one
-- small module (@-fno-code@, no source recompile). @-w@ silences the expected
-- unused-import\/binder warnings; we only care that it type-checks.
ghcArgs :: [String]
ghcArgs =
  [ "exec"
  , "--"
  , "ghc"
  , "-fno-code"
  , "-w"
  , "-package"
  , "mtg-pure"
  , "-i" ++ outputDir
  , modulePath
  ]

main :: (HasCallStack) => IO ()
main = mainRenderShowCompiles

mainRenderShowCompiles :: (HasCallStack) => IO ()
mainRenderShowCompiles = do
  createDirectoryIfMissing True outputDir
  writeFile modulePath generatedSource
  (code, out, err) <- readProcessWithExitCode "cabal" ghcArgs ""
  case code of
    ExitSuccess ->
      putStrLn $
        "[show-compiles] type-checked "
          ++ show (length gens)
          ++ " rendered bindings from "
          ++ modulePath
    ExitFailure n -> error $ failureReport n out err

-- | Assemble a diagnostic from a failed GHC run: the rendered bindings named in
-- the error map back to specific cards\/variants, so surface those first.
failureReport :: Int -> String -> String -> String
failureReport code out err =
  unlines $
    [ "Rendered Show output failed to type-check (ghc exit " ++ show code ++ ")."
    , "Generated module left on disk for inspection: " ++ modulePath
    , ""
    ]
      ++ culpritLines
      ++ ["----- ghc stderr -----", err]
      ++ (if null out then [] else ["----- ghc stdout -----", out])
 where
  culprits = [label | (name, _, _, label) <- gens, name `isInfixOf` err]
  culpritLines = case culprits of
    [] -> []
    _ -> "Likely offending card(s):" : map ("  - " ++) culprits ++ [""]

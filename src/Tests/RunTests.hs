{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Aggregate test runner for mtg-pure.
--
-- There is no cabal test-suite; every test is an @IO ()@ entry point that
-- computes/replays to completion and throws on failure. This runner invokes
-- them in sequence, names each case, isolates failures, and reports a summary.
--
-- Usage: @cabal run run-tests -- [OPTIONS] [PATTERN...]@
--
--   -f, --fail-fast   Stop at the first failing test.
--   -l, --list        List test names and exit (don't run).
--   -h, --help        Show this help.
--   PATTERN...        Run only tests whose name contains one of these
--                     substrings (case-insensitive). Default: run all.
module Main (
  main,
) where

import safe Control.Exception (SomeException, displayException, try)
import safe Control.Monad (unless, when)
import safe Data.Char (toLower)
import safe Data.List (isInfixOf)
import safe System.Environment (getArgs, getProgName)
import safe System.Exit (exitFailure, exitSuccess)
import safe System.IO (BufferMode (..), hFlush, hPutStrLn, hSetBuffering, stderr, stdout)
import safe qualified Test.Engine.Unit.MagicCont as MagicCont
import safe qualified Test.Engine.Unit.PayMana as PayMana
import safe qualified Test.Game.Headless.Hybrid as HeadlessHybrid
import safe qualified Test.Game.Headless.ManaAbility as HeadlessManaAbility
import safe qualified Test.Game.Headless.RagingGoblin as HeadlessRagingGoblin
import safe qualified Test.Game.Headless.Shock as HeadlessShock
import safe qualified Test.Game.Headless.StoneRain as HeadlessStoneRain
import safe qualified Test.Game.Resume.Replay as ResumeReplay
import safe qualified Test.Render.CardOracleAll as RenderCardOracleAll
import safe qualified Test.Render.CardOracleBlockArguments as RenderCardOracleBlockArguments
import safe qualified Test.Render.CardOracleDefault as RenderCardOracleDefault
import safe qualified Test.Render.CardOracleDefaultHighDataCombinators as RenderCardOracleDefaultHighDataCombinators
import safe qualified Test.Render.CardOracleDefaultLowDataCombinators as RenderCardOracleDefaultLowDataCombinators
import safe qualified Test.Render.CardOracleDefaultNoDataCombinators as RenderCardOracleDefaultNoDataCombinators
import safe qualified Test.Render.CardOracleMultiline as RenderCardOracleMultiline
import safe qualified Test.Render.CardOracleSmartAliasing as RenderCardOracleSmartAliasing
import safe qualified Test.Render.CardOracleWildcardUnusedVars as RenderCardOracleWildcardUnusedVars
import safe qualified Test.Render.ShowCompiles as RenderShowCompiles

data Test = Test
  { testName :: String
  , testRun :: IO ()
  }

-- | Only tests that run in a non-interactive harness. The interactive terminal
-- game replays (@Test.Game.Play.Shock@ et al.) need a real console and are covered
-- here by their @Test.Game.Headless.*@ equivalents; run the terminal versions
-- from a REPL if you want to watch them.
tests :: [Test]
tests =
  [ Test "Engine.Unit.PayMana" PayMana.mainUnitPayMana
  , Test "Engine.Unit.MagicCont" MagicCont.mainUnitMagicCont
  , Test "Render.CardOracleDefault" RenderCardOracleDefault.mainRenderCardOracleDefault
  , Test "Render.CardOracleWildcardUnusedVars" RenderCardOracleWildcardUnusedVars.mainRenderCardOracleWildcardUnusedVars
  , Test "Render.CardOracleBlockArguments" RenderCardOracleBlockArguments.mainRenderCardOracleBlockArguments
  , Test "Render.CardOracleMultiline" RenderCardOracleMultiline.mainRenderCardOracleMultiline
  , Test "Render.CardOracleSmartAliasing" RenderCardOracleSmartAliasing.mainRenderCardOracleSmartAliasing
  , Test "Render.CardOracleAll" RenderCardOracleAll.mainRenderCardOracleAll
  , Test "Render.CardOracleDefaultHighDataCombinators" RenderCardOracleDefaultHighDataCombinators.mainRenderCardOracleDefaultHighDataCombinators
  , Test "Render.CardOracleDefaultLowDataCombinators" RenderCardOracleDefaultLowDataCombinators.mainRenderCardOracleDefaultLowDataCombinators
  , Test "Render.CardOracleDefaultNoDataCombinators" RenderCardOracleDefaultNoDataCombinators.mainRenderCardOracleDefaultNoDataCombinators
  , Test "Render.ShowCompiles" RenderShowCompiles.mainRenderShowCompiles
  , Test "Game.Headless.Shock" HeadlessShock.mainHeadlessShock
  , Test "Game.Headless.StoneRain" HeadlessStoneRain.mainHeadlessStoneRain
  , Test "Game.Headless.RagingGoblin" HeadlessRagingGoblin.mainHeadlessRagingGoblin
  , Test "Game.Headless.ManaAbility" HeadlessManaAbility.mainHeadlessManaAbility
  , Test "Game.Headless.Hybrid" HeadlessHybrid.mainHeadlessHybrid
  , Test "Game.Resume.PriorityBoundary" ResumeReplay.mainResumePriorityBoundary
  , Test "Game.Resume.Combat" ResumeReplay.mainResumeCombat
  , Test "Game.Resume.MidOperation" ResumeReplay.mainResumeMidOperation
  ]

data Options = Options
  { optFailFast :: Bool
  , optList :: Bool
  , optHelp :: Bool
  , optPatterns :: [String]
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optFailFast = False
    , optList = False
    , optHelp = False
    , optPatterns = []
    }

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
 where
  go opts [] = Right opts
  go opts (a : as) = case a of
    "-f" -> go opts{optFailFast = True} as
    "--fail-fast" -> go opts{optFailFast = True} as
    "-l" -> go opts{optList = True} as
    "--list" -> go opts{optList = True} as
    "-h" -> go opts{optHelp = True} as
    "--help" -> go opts{optHelp = True} as
    _
      | take 1 a == "-" -> Left $ "unknown option: " ++ a
      | otherwise -> go opts{optPatterns = optPatterns opts ++ [a]} as

usage :: String -> String
usage prog =
  unlines
    [ "Usage: " ++ prog ++ " [OPTIONS] [PATTERN...]"
    , ""
    , "  -f, --fail-fast   Stop at the first failing test."
    , "  -l, --list        List test names and exit."
    , "  -h, --help        Show this help."
    , "  PATTERN...        Run only tests whose name contains one of these"
    , "                    substrings (case-insensitive). Default: run all."
    ]

-- | Case-insensitive substring match against a test's name.
matches :: [String] -> Test -> Bool
matches [] _ = True
matches pats t = any (`isInfixOf` map toLower (testName t)) (map (map toLower) pats)

runOne :: Test -> IO Bool
runOne t = do
  putStrLn $ "\n=== RUN  " ++ testName t ++ " ==="
  hFlush stdout
  result <- try (testRun t) :: IO (Either SomeException ())
  case result of
    Right () -> do
      putStrLn $ "=== PASS " ++ testName t ++ " ==="
      pure True
    Left err -> do
      hPutStrLn stderr $ "=== FAIL " ++ testName t ++ " ===\n" ++ displayException err
      pure False

-- | Run tests, honoring fail-fast. Returns (passed, failed, skipped) counts.
runTests :: Bool -> [Test] -> IO (Int, Int, Int)
runTests failFast = go 0 0
 where
  go passed failed [] = pure (passed, failed, 0)
  go passed failed (t : ts) = do
    ok <- runOne t
    if ok
      then go (passed + 1) failed ts
      else
        if failFast
          then pure (passed, failed + 1, length ts)
          else go passed (failed + 1) ts

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  prog <- getProgName
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ prog ++ ": " ++ err
      hPutStrLn stderr $ usage prog
      exitFailure
    Right opts
      | optHelp opts -> putStr (usage prog) >> exitSuccess
      | otherwise -> do
          let selected = filter (matches (optPatterns opts)) tests
          when (optList opts) $ do
            mapM_ (putStrLn . testName) selected
            exitSuccess
          when (null selected) $ do
            hPutStrLn stderr "no tests matched the given pattern(s)"
            exitFailure
          (passed, failed, skipped) <- runTests (optFailFast opts) selected
          putStrLn $
            "\n=== SUMMARY: "
              ++ show passed
              ++ " passed, "
              ++ show failed
              ++ " failed"
              ++ (if skipped > 0 then ", " ++ show skipped ++ " skipped" else "")
              ++ " ==="
          hFlush stdout
          unless (failed == 0) exitFailure

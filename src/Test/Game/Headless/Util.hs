{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Shared reporting for headless game replays: dump the serial decision log
-- and summarize the outcome. A replay that runs its whole script and then
-- halts on \"out of decisions\" is the normal, successful ending.
module Test.Game.Headless.Util (
  reportHeadless,
  checkHeadlessExpectations,
) where

import safe Control.Monad (unless)
import safe MtgPure.Client.Headless.Monad (Headless, HeadlessResult (..))
import safe MtgPure.Engine.State (GameResult (..))

reportHeadless :: HeadlessResult (Maybe (GameResult Headless)) -> IO ()
reportHeadless result = do
  putStrLn $ "captured " ++ show (length $ headlessStates result) ++ " game states"
  putStrLn $ "captured " ++ show (length $ headlessErrors result) ++ " gameplay errors"
  mapM_ print $ reverse $ headlessErrors result
  putStrLn "--- outcome ---"
  case headlessOutcome result of
    Left reason -> putStrLn $ "halted: " ++ reason
    Right Nothing -> putStrLn "no game result"
    Right (Just gr) ->
      putStrLn $ "winners=" ++ show (gameWinners gr) ++ " losers=" ++ show (gameLosers gr)

-- | Fail the test (via 'error') if the replay recorded any expectation
-- violation: an 'ExpectGameplayError' whose message didn't match, an error the
-- engine produced that no expectation covered, or (at end of script) an error
-- left unmatched / an expectation whose error never occurred.
checkHeadlessExpectations :: HeadlessResult a -> IO ()
checkHeadlessExpectations result =
  unless (null failures) $
    error $
      "headless expectation failures:\n" ++ unlines (map ("  - " ++) failures)
 where
  failures = headlessExpectationFailures result

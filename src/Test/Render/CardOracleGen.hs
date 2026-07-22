{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Shared machinery for the per-option card-render oracles.
--
-- Each @Test.Render.CardOracle\<Foo\>@ module renders every card and token from
-- "MtgPure.AllCards" under a single 'ShowOptions' knob (all others at their
-- default) and compares the concatenated result against its own checked-in
-- oracle file. That isolates the effect of each option: a diff in
-- @card-oracle-multiline.txt@ can only come from the @multiline@ knob.
--
-- Opt-in update: set env var @MTG_UPDATE_ORACLE@ to @1@/@true@/@yes@/@on@ to
-- (re)write the oracle instead of comparing. Paths are relative to the project
-- root, which is the working directory under @cabal run run-tests@.
module Test.Render.CardOracleGen (
  renderAllCardsWith,
  runCardOracle,
) where

import safe Control.Exception (SomeException, try)
import safe Control.Monad (unless)
import safe Data.Char (toLower)
import safe Data.List (find)
import safe GHC.Stack (HasCallStack)
import safe MtgPure.AllCards (allCards, allTokens)
import safe MtgPure.Model.CardName (CardName (unCardName), getCardName)
import safe MtgPure.Model.Recursive.Show (
  ShowOptions,
  showAnyCardWith,
  showAnyTokenWith,
 )
import safe System.Environment (lookupEnv)
import safe System.IO (readFile')

-- | The full rendered text of all cards and tokens under @opts@, in declaration
-- order. Each entry gets a @==== <name> ====@ header so a diff points at the
-- offending card.
renderAllCardsWith :: ShowOptions -> String
renderAllCardsWith opts =
  concatMap (renderEntry . labelCard) allCards
    ++ concatMap (renderEntry . labelToken) allTokens
 where
  labelCard c = (unCardName (getCardName c), showAnyCardWith opts c)
  labelToken t = ("token: " ++ unCardName (getCardName t), showAnyTokenWith opts t)
  renderEntry (name, body) = "==== " ++ name ++ " ====\n" ++ body ++ "\n\n"

-- | Compare (or, under @MTG_UPDATE_ORACLE@, rewrite) the oracle at @path@
-- against everything rendered under @opts@.
runCardOracle :: (HasCallStack) => FilePath -> ShowOptions -> IO ()
runCardOracle path opts = do
  update <- isTruthy <$> lookupEnv "MTG_UPDATE_ORACLE"
  let actual = renderAllCardsWith opts
  case update of
    True -> do
      writeFile path actual
      putStrLn $
        "[oracle updated] wrote " ++ show (length actual) ++ " chars to " ++ path
    False -> do
      eExpected <- try (readFile' path) :: IO (Either SomeException String)
      case eExpected of
        Left _ ->
          error $
            "Card-render oracle not found or unreadable: "
              ++ path
              ++ "\nCreate it by running with MTG_UPDATE_ORACLE=1."
        Right expected ->
          unless (actual == expected) $ error $ mismatchMessage path expected actual

isTruthy :: Maybe String -> Bool
isTruthy = \case
  Nothing -> False
  Just s -> map toLower s `elem` ["1", "true", "yes", "on"]

-- | A compact description of the first line that differs.
mismatchMessage :: FilePath -> String -> String -> String
mismatchMessage path expected actual =
  case find differs (zip3 [1 :: Int ..] eLines aLines) of
    Just (n, e, a) ->
      unlines
        [ "Card-render oracle mismatch (" ++ path ++ ") at line " ++ show n ++ ":"
        , "  expected: " ++ show e
        , "  actual:   " ++ show a
        , "Run with MTG_UPDATE_ORACLE=1 to accept the new rendering."
        ]
    Nothing ->
      "Card-render oracle mismatch ("
        ++ path
        ++ "): outputs share a common prefix but differ in length ("
        ++ show (length eLines)
        ++ " vs "
        ++ show (length aLines)
        ++ " lines). Run with MTG_UPDATE_ORACLE=1 to accept the new rendering."
 where
  eLines = lines expected
  aLines = lines actual
  differs (_, e, a) = e /= a

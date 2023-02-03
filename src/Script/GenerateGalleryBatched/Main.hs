{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module Script.GenerateGalleryBatched.Main (
  main,
) where

import safe Control.Concurrent (forkFinally)
import safe Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import safe Control.Exception (evaluate)
import safe qualified Control.Monad as M
import safe qualified Data.Foldable as F
import safe Data.IORef (IORef, atomicModifyIORef', newIORef)
import safe Data.List (isPrefixOf)
import safe Data.Time.Clock (diffUTCTime, getCurrentTime)
import safe Data.Time.Format (defaultTimeLocale, formatTime)
import safe qualified Data.Traversable as T
import safe MtgPure.AllCards (allCards)
import safe MtgPure.Model.CardName (getCardName, unCardName)
import Script.ScryfallDownloader (downloadSpecificCards)
import safe System.Process (callProcess)

type CardName = String

concurrencyLimit :: Int
concurrencyLimit = 12

main :: IO ()
main = mainGenerateGalleryBatched

data Env = Env
  { env_ :: ()
  , env_pendingCards :: IORef [CardName]
  , env_printMutex :: MVar ()
  , env_downloadImages :: Bool
  }

mainGenerateGalleryBatched :: IO ()
mainGenerateGalleryBatched = do
  start <- getCurrentTime
  callProcess "compile-generate-gallery-single.bat" [] -- TODO: abort if this has non-zero exit code
  cardNames <- getCardNames
  pendingCards <- newIORef cardNames
  printMutex <- newMVar ()
  let env =
        Env
          { env_ = ()
          , env_pendingCards = pendingCards
          , env_printMutex = printMutex
          , env_downloadImages = not False
          }
  M.when (env_downloadImages env) do
    downloadSpecificCards cardNames
  putStrLn $ "Concurrency limit: " <> show concurrencyLimit
  threads <- T.for [0 .. concurrencyLimit - 1] \_ -> do
    createThread $ threadLoop env
  F.traverse_ joinThread threads
  end <- getCurrentTime
  let elapsed = diffUTCTime end start
  let elapsedStr = formatTime defaultTimeLocale "%H:%M:%S" elapsed
  putStrLn $ "Total time: " <> elapsedStr

isCommentedLine :: String -> Bool
isCommentedLine = ("#" `isPrefixOf`)

getCardNames :: IO [CardName]
getCardNames = do
  if True
    then do
      let file = "Script/GenerateGalleryBatched/card-names.txt"
      contents <- readFile file
      let ls = filter (\s -> not (null s) && not (isCommentedLine s)) $ lines contents
      M.void $ evaluate $ length ls
      pure ls
    else do
      pure $ map (unCardName . getCardName) allCards

lockedPrint :: Env -> String -> IO ()
lockedPrint env str = do
  takeMVar $ env_printMutex env
  putStrLn str
  putMVar (env_printMutex env) ()

processCard :: Env -> CardName -> IO ()
processCard env cardName = do
  start <- getCurrentTime
  lockedPrint env $ "+Card : " <> cardName
  callProcess ".output/generate-gallery-single.exe" [cardName]
  end <- getCurrentTime
  let elapsed = diffUTCTime end start
  let elapsedStr = formatTime defaultTimeLocale "%H:%M:%S" elapsed
  lockedPrint env $ "-Card : " <> cardName <> " (" <> elapsedStr <> ")"

threadLoop :: Env -> IO ()
threadLoop env = do
  mCardName <- atomicModifyIORef' (env_pendingCards env) $ \case
    [] -> ([], Nothing)
    (x : xs) -> (xs, Just x)
  case mCardName of
    Nothing -> pure ()
    Just cardName -> do
      processCard env cardName
      threadLoop env

newtype Thread = Thread (MVar ())

createThread :: IO () -> IO Thread
createThread action = do
  var <- newEmptyMVar
  M.void $ forkFinally action $ const $ putMVar var ()
  pure $ Thread var

joinThread :: Thread -> IO ()
joinThread (Thread var) = takeMVar var

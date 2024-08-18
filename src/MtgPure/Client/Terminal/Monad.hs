{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Use fromRight" #-}

module MtgPure.Client.Terminal.Monad (
  Fwd,
  TerminalInput (..),
  TerminalState (..),
  Terminal,
  runTerminal,
  getTerminalState,
  getsTerminalState,
  prompt,
  terminalLogCallPush,
  terminalLogCallPop,
  quitTerminal,
  pause,
) where

import safe Ansi.Box (withAnsi)
import safe Ansi.Compile (RenderedCells)
import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe qualified Control.Monad.State.Strict as State
import safe Control.Monad.Trans (MonadIO (..))
import safe Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Set as Set
import safe MtgPure.Client.Terminal.Fwd.Type (Fwd' (..))
import safe MtgPure.Engine.Monad (CallFrameInfo (..))
import safe MtgPure.Engine.Prompt (InternalLogicError (CorruptCallStackLogging))
import safe MtgPure.Engine.State (OpaqueGameState, queryMagic)
import safe qualified System.IO as IO

type Fwd = Fwd' Terminal

data Quit = Quit

data TerminalInput = TerminalInput
  { terminalInput_ :: ()
  , terminalInput_fwd :: Fwd
  , terminalInput_replayInputs :: [String]
  , terminalInput_replayLog :: Maybe FilePath
  }

-- TODO: Don't expose this.
data TerminalState = TerminalState
  { terminal_ :: ()
  , terminal_fwd :: Fwd
  , terminal_logDisabled :: Int
  , terminal_replayInputs :: [String]
  , terminal_replayLog :: Maybe FilePath
  , terminal_prevGameRender :: RenderedCells
  }

newtype Terminal a = Terminal
  { unTerminal :: ExceptT Quit (State.StateT TerminalState IO) a
  }
  deriving (Functor)

instance Applicative Terminal where
  pure :: a -> Terminal a
  pure = Terminal . pure

  (<*>) :: Terminal (a -> b) -> Terminal a -> Terminal b
  Terminal f <*> Terminal a = Terminal $ f <*> a

instance Monad Terminal where
  (>>=) :: Terminal a -> (a -> Terminal b) -> Terminal b
  Terminal a >>= f = Terminal $ a >>= unTerminal . f

instance MonadIO Terminal where
  liftIO :: IO a -> Terminal a
  liftIO = Terminal . liftIO

instance State.MonadState TerminalState Terminal where
  get :: Terminal TerminalState
  get = Terminal State.get

  put :: TerminalState -> Terminal ()
  put = Terminal . State.put

runTerminal :: TerminalInput -> Terminal () -> IO ()
runTerminal input m = withAnsi $ either (const ()) id <$> runTerminal' input m

runTerminal' :: TerminalInput -> Terminal a -> IO (Either Quit a)
runTerminal' input action = do
  State.runStateT (runExceptT (unTerminal action)) st >>= \case
    (Left Quit, _) -> pure $ Left Quit
    (Right result, st') -> case terminal_logDisabled st' of
      0 -> pure $ Right result
      _ -> error $ show CorruptCallStackLogging
 where
  st =
    TerminalState
      { terminal_ = ()
      , terminal_fwd = terminalInput_fwd input
      , terminal_logDisabled = 0
      , terminal_replayInputs = terminalInput_replayInputs input
      , terminal_replayLog = terminalInput_replayLog input
      , terminal_prevGameRender = mempty
      }

quitTerminal :: Terminal a
quitTerminal = Terminal $ throwE Quit

getTerminalState :: Terminal TerminalState
getTerminalState = Terminal State.get

getsTerminalState :: (TerminalState -> a) -> Terminal a
getsTerminalState = Terminal . State.gets

pause :: (MonadIO m) => m ()
pause = M.void $ liftIO getLine

prompt :: String -> Terminal String
prompt msg = do
  liftIO do
    putStr msg
    IO.hFlush IO.stdout
  Terminal (State.gets terminal_replayInputs) >>= \case
    [] -> liftIO getLine
    s : ss -> do
      Terminal $ State.modify' \st' -> st'{terminal_replayInputs = ss}
      liftIO $ putStrLn s
      pure s

--------------------------------------------------------------------------------

logTabWidth :: Int
logTabWidth = 2

terminalLogCallPush :: OpaqueGameState Terminal -> CallFrameInfo -> Terminal ()
terminalLogCallPush opaque frame = case name == show 'queryMagic of
  True -> Terminal $ State.modify' \st -> st{terminal_logDisabled = terminal_logDisabled st + 1}
  False ->
    Terminal (State.gets terminal_logDisabled) >>= \case
      0 -> case logIgnore name of
        Just _ -> pure ()
        Nothing -> do
          let indent = replicate (logTabWidth * i) ' '
          liftIO $ putStrLn $ indent ++ "+" ++ name ++ ": " ++ show i
          case Set.member name logDetailed of
            False -> pure ()
            True -> do
              fwd <- getsTerminalState terminal_fwd
              fwd_printGameState fwd opaque Nothing
      _ -> pure ()
 where
  name = callFrameName frame
  i = callFrameId frame

terminalLogCallPop :: OpaqueGameState Terminal -> CallFrameInfo -> Terminal ()
terminalLogCallPop _opaque frame = case name == show 'queryMagic of
  True -> Terminal $ State.modify' \st ->
    assert (terminal_logDisabled st > 0) st{terminal_logDisabled = terminal_logDisabled st - 1}
  False ->
    Terminal (State.gets terminal_logDisabled) >>= \case
      0 -> case logIgnore name of
        Just _ -> pure ()
        _ -> do
          let indent = replicate (logTabWidth * i) ' '
          liftIO $ putStrLn $ indent ++ "-" ++ name ++ ": " ++ show i
      _ -> pure ()
 where
  name = callFrameName frame
  i = callFrameId frame

logDetailed :: Set.Set String
logDetailed =
  Set.fromList
    [ ""
    , "MtgPure.Engine.Turn.cleanupStep"
    , "MtgPure.Engine.Turn.precombatMainPhase"
    ]

-- TODO: Make these actually do something
data IgnoreBehavior = IgnoreAll -- IgnoreNested

logIgnore :: String -> Maybe IgnoreBehavior
-- logIgnore = flip Map.lookup _logIgnoreMap
logIgnore = const (Just IgnoreAll)

_logIgnoreMap :: Map.Map String IgnoreBehavior
_logIgnoreMap =
  Map.fromList
    [ ("", IgnoreAll)
    , ("MtgPure.Engine.Core.findHandCard", IgnoreAll)
    , ("MtgPure.Engine.Core.findLibraryCard", IgnoreAll)
    , ("MtgPure.Engine.Core.findPlayer", IgnoreAll)
    , ("MtgPure.Engine.Core.getActivePlayer", IgnoreAll)
    , ("MtgPure.Engine.Core.getAlivePlayerCount", IgnoreAll)
    , ("MtgPure.Engine.Core.getAPNAP", IgnoreAll)
    , ("MtgPure.Engine.Core.getPlayer", IgnoreAll)
    , ("MtgPure.Engine.Core.getPlayers", IgnoreAll)
    , ("MtgPure.Engine.Core.newObjectId", IgnoreAll)
    , ("MtgPure.Engine.Core.pushHandCard", IgnoreAll)
    , ("MtgPure.Engine.Core.pushLibraryCard", IgnoreAll)
    , ("MtgPure.Engine.Core.removeHandCard", IgnoreAll)
    , ("MtgPure.Engine.Core.removeLibraryCard", IgnoreAll)
    , ("MtgPure.Engine.Core.setPlayer", IgnoreAll)
    ]

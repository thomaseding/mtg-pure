{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Shared driver for the interactive @Test.Game.Play.*@ games.
--
-- Rather than scripting the terminal client with replay-input strings, each play
-- test fast-forwards the game to an interesting point with a typed 'Decision'
-- script run under the pure headless backend, then hands the suspended game off
-- to the interactive terminal so it can be played by hand from there.
--
-- The hand-off is a 'ResumePoint': the game state at the most recent priority
-- boundary plus the answers made since it. The common case (the script stops at a
-- priority prompt) has an empty replay and resumes directly; a script that stops
-- mid-operation (e.g. mid-cast, before a target is picked) is replayed the short
-- distance from the boundary back to the exact suspension point before the user
-- takes over. Both work because the game is deterministic.
module Test.Game.Play.Util (
  playFromDecisions,
) where

import safe Control.Monad.Trans (liftIO)
import safe Data.IORef (atomicModifyIORef', newIORef)
import safe MtgPure.Client.Headless.Monad (
  Decision,
  Headless,
  HeadlessResult (..),
  ResumePoint (..),
  runHeadless,
 )
import safe MtgPure.Client.Headless.Replay (playHeadlessGame, replayThenPrompt)
import safe MtgPure.Client.Terminal.Monad (
  Terminal,
  TerminalInput,
  runTerminal,
 )
import safe MtgPure.Client.Terminal.PriorityAction (terminalPrompt)
import safe MtgPure.Engine.Fwd.Impl (fwdImpl)
import safe MtgPure.Engine.PlayGame (resumeGame)
import safe MtgPure.Engine.State (
  GameCheats,
  GameInput (..),
  GameState (..),
  OpaqueGameState,
  Prompt,
  mkOpaqueGameState,
  unOpaqueGameState,
 )
import safe MtgPure.Model.Deck (Deck)
import safe MtgPure.Model.Sideboard (Sideboard)

-- | Fast-forward the game through the given 'Decision' script (headlessly), then
-- resume it under the interactive terminal so the rest is played by hand. The
-- 'TerminalInput' supplies the terminal's own wiring; its replay inputs should be
-- empty, since the point is to hand control to the user.
playFromDecisions :: GameCheats -> [(Deck, Sideboard)] -> [Decision] -> TerminalInput -> IO ()
playFromDecisions cheats decks decisions input =
  case headlessResumeState (runHeadless decisions (playHeadlessGame cheats decks)) of
    Nothing ->
      error "playFromDecisions: the headless fast-forward produced no resume point; there is nothing to hand off to the terminal"
    Just resumePoint -> do
      -- Replay the (usually empty) answer suffix, then let the terminal prompt
      -- take over for interactive play.
      queue <- newIORef $ resumePoint_replay resumePoint
      let popNext =
            liftIO $ atomicModifyIORef' queue \case
              [] -> ([], Nothing)
              d : ds -> (ds, Just d)
          prompt = replayThenPrompt popNext terminalPrompt
      runTerminal input do
        outcome <- resumeGame $ retargetToTerminal prompt (resumePoint_checkpoint resumePoint)
        liftIO $ print outcome

-- | Re-target a headless checkpoint for continued play under the terminal
-- backend, installing the given prompt. Only the monad-specific fields differ
-- between the two backends: the forwarding table, the game input, the prompt, and
-- the (currently always empty) event listeners. Everything else is monad-agnostic
-- and carries over unchanged.
retargetToTerminal :: Prompt Terminal -> OpaqueGameState Headless -> OpaqueGameState Terminal
retargetToTerminal prompt opaque =
  let st = unOpaqueGameState opaque
      input = terminalGameInput prompt (magicGameInput st)
   in mkOpaqueGameState
        st
          { magicFwd = fwdImpl
          , magicGameInput = input
          , magicListeners = mempty
          , magicPrompt = prompt
          }

terminalGameInput :: Prompt Terminal -> GameInput Headless -> GameInput Terminal
terminalGameInput prompt src =
  GameInput
    { gameInput_ = ()
    , gameInput_decks = gameInput_decks src
    , gameInput_gameCheats = gameInput_gameCheats src
    , gameInput_gameFormat = gameInput_gameFormat src
    , gameInput_mulligan = gameInput_mulligan src
    , gameInput_prompt = prompt
    }

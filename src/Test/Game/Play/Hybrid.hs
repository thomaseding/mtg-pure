{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Interactive terminal play of the hybrid-mana scenario. The shared
-- 'Test.Game.Headless.Hybrid.decisions' script is empty, so the fast-forward
-- stops at the very first priority prompt and the whole game is played by hand
-- from the terminal.
module Test.Game.Play.Hybrid (
  main,
  mainHybrid,
) where

import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (TerminalInput (..))
import safe Test.Game.Headless.Hybrid (cheats, decisions, decks)
import safe Test.Game.Play.Util (playFromDecisions)

main :: IO ()
main = mainHybrid

mainHybrid :: IO ()
mainHybrid = playFromDecisions cheats decks decisions terminalInput

terminalInput :: TerminalInput
terminalInput =
  TerminalInput
    { terminalInput_ = ()
    , terminalInput_fwd = fwdImpl
    , terminalInput_replayInputs = []
    , terminalInput_replayLog = Nothing
    }

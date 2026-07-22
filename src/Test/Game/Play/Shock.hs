{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Interactive terminal play of the Shock scenario: fast-forward through the
-- shared 'Test.Game.Headless.Shock.decisions' script under the headless backend,
-- then hand the game off to the terminal to be played by hand from there.
module Test.Game.Play.Shock (
  main,
  mainShock,
) where

import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (TerminalInput (..))
import safe Test.Game.Headless.Shock (cheats, decisions, decks)
import safe Test.Game.Play.Util (playFromDecisions)

main :: IO ()
main = mainShock

mainShock :: IO ()
mainShock = playFromDecisions cheats decks decisions terminalInput

terminalInput :: TerminalInput
terminalInput =
  TerminalInput
    { terminalInput_ = ()
    , terminalInput_fwd = fwdImpl
    , terminalInput_replayInputs = []
    , terminalInput_replayLog = Nothing
    }

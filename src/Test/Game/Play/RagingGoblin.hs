{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Interactive terminal play of the Raging Goblin scenario: fast-forward
-- through the shared 'Test.Game.Headless.RagingGoblin.decisions' script under the
-- headless backend, then hand the game off to the terminal to be played by hand
-- from there.
module Test.Game.Play.RagingGoblin (
  main,
  mainRagingGoblin,
) where

import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (TerminalInput (..))
import safe Test.Game.Headless.RagingGoblin (cheats, decisions, decks)
import safe Test.Game.Play.Util (playFromDecisions)

main :: IO ()
main = mainRagingGoblin

mainRagingGoblin :: IO ()
mainRagingGoblin = playFromDecisions cheats decks decisions terminalInput

terminalInput :: TerminalInput
terminalInput =
  TerminalInput
    { terminalInput_ = ()
    , terminalInput_fwd = fwdImpl
    , terminalInput_replayInputs = []
    , terminalInput_replayLog = Nothing
    }

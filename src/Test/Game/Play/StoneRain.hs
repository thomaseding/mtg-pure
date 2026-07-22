{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Interactive terminal play of the Stone Rain scenario: fast-forward through
-- the shared 'Test.Game.Headless.StoneRain.decisions' script under the headless
-- backend, then hand the game off to the terminal to be played by hand from
-- there.
module Test.Game.Play.StoneRain (
  main,
  mainStoneRain,
) where

import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (TerminalInput (..))
import safe Test.Game.Headless.StoneRain (cheats, decisions, decks)
import safe Test.Game.Play.Util (playFromDecisions)

main :: IO ()
main = mainStoneRain

mainStoneRain :: IO ()
mainStoneRain = playFromDecisions cheats decks decisions terminalInput

terminalInput :: TerminalInput
terminalInput =
  TerminalInput
    { terminalInput_ = ()
    , terminalInput_fwd = fwdImpl
    , terminalInput_replayInputs = []
    , terminalInput_replayLog = Nothing
    }

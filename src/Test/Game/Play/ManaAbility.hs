{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Interactive terminal play of the mana-ability scenario: fast-forward through
-- the shared 'Test.Game.Headless.ManaAbility.decisions' script under the headless
-- backend, then hand the game off to the terminal to be played by hand from
-- there.
module Test.Game.Play.ManaAbility (
  main,
  mainManaAbility,
) where

import safe MtgPure.Client.Terminal.Fwd.Impl (fwdImpl)
import safe MtgPure.Client.Terminal.Monad (TerminalInput (..))
import safe Test.Game.Headless.ManaAbility (cheats, decisions, decks)
import safe Test.Game.Play.Util (playFromDecisions)

main :: IO ()
main = mainManaAbility

mainManaAbility :: IO ()
mainManaAbility = playFromDecisions cheats decks decisions terminalInput

terminalInput :: TerminalInput
terminalInput =
  TerminalInput
    { terminalInput_ = ()
    , terminalInput_fwd = fwdImpl
    , terminalInput_replayInputs = []
    , terminalInput_replayLog = Nothing
    }

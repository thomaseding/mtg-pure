# `Test.Game.Play.*` — interactive terminal play

Each module here (`StoneRain`, `Shock`, `RagingGoblin`, `ManaAbility`,
`Hybrid`) is an interactive terminal game. It fast-forwards to an interesting
board state and then hands control to **you** at the console:

1. A typed `Decision` script is run under the pure **headless** backend
   ([`Util.playFromDecisions`](Util.hs)), fast-forwarding to a `ResumePoint`.
2. That suspended game is re-targeted to the **terminal** backend and resumed,
   so the rest is played by hand.

The headless equivalents under `Test.Game.Headless.*` replay the *same*
`decisions` scripts to completion non-interactively; those are the ones wired
into the `run-tests` executable. The `Play` modules are **not** in `run-tests`
by design — they need a real interactive console.

## How to run

There is a dedicated `play` executable that launches these games as their own
process. Do **not** run them from `cabal repl` — see the pitfall below.

1. Edit [`Main.hs`](Main.hs) to select the game: uncomment the one
   `Test.Game.Play.*` import you want (it is aliased as `Main`, so `main` picks
   it up automatically). Leave exactly one uncommented — `-Wunused-imports`
   with `-Werror` fails the build otherwise.
2. Build and run it:

   ```powershell
   cabal run play
   ```

## Pitfalls

### Don't run under `cabal repl` — stdin theft crashes the game

The whole reason the `play` executable exists: interactive play breaks under
`cabal repl`.

**Symptom.** You reach the replayed state, issue a command that redraws the
screen (e.g. `?` for help), and the game dies with (possibly after pressing
ENTER):

```
*** Exception: Assertion failed
CallStack (from HasCallStack):
  assert, called at src\Ansi\Box.hs:120 in ...:Ansi.Box
ghci> [71;158R
```

**Cause.** Clearing the screen goes through
[`Ansi.Box.clearScreenWithoutPaging`](../../../Ansi/Box.hs), which calls
`getTerminalSize`. On terminals without a native console query,
`ansi-terminal` measures the terminal by writing an ANSI cursor-position
request (`ESC[6n`) and reading the reply back off **stdin**. Under `cabal
repl` the REPL owns stdin, so that reply never reaches `getTerminalSize` (it
returns `Nothing`) — and the trailing `[71;158R` you see at the prompt
afterward *is* that stolen reply (here: 71 rows × 158 cols). The `Nothing`
branch hits `assert False`, which throws because assertions are on.

This is a general REPL-vs-interactive-terminal conflict, not specific to Stone
Rain — it can bite any of these games as soon as a size-dependent redraw runs.
The echo hazard noted in `Ansi.Box` (`_withEcho`, "issues in ghci when calling
`main` from the repl multiple times") is the same family of problem.

**Fix.** Run via `cabal run play` (a standalone process). Outside the REPL
nothing intercepts the cursor-position reply, so `getTerminalSize` succeeds.

### Nothing to hand off

If a `Decision` script never reaches a priority boundary,
`playFromDecisions` errors with *"the headless fast-forward produced no resume
point"*. That means the script drove the game to a point with no resumable
checkpoint — shorten/adjust the script so it stops at (or before) a priority
prompt.

---
name: run-tests
description: Run mtg-pure's tests and demo. Use when the user wants to run/verify a test, exercise a card or rules interaction, reproduce a game scenario, or check that a change still passes. Prefer the `run-tests` executable (unit + headless game replays) — it runs non-interactively. mtg-pure has no cabal test-suite; the terminal game replays and demo are `main<Name>` entry points run from cabal repl.
---

# Run mtg-pure tests

**There is no `cabal test` suite.** Prefer the **`run-tests` executable**, which
links the library and runs every harness-runnable test (2 unit + 5 headless game
replays) in sequence. Fall back to the REPL only for the terminal game replays
and the demo, which need a real console. Build first if you haven't — see the
**`build`** skill.

## Preferred: the `run-tests` executable (headless)

`run-tests` (source [`src/Tests/RunTests.hs`](../../../src/Tests/RunTests.hs))
runs the unit tests plus the **headless** game replays
(`Test.Game.Headless.*`) — pure, non-interactive ports of the terminal game
scripts. **This is the way to verify a change from an automated shell**: it needs
no console and exits non-zero if any test fails.

Use PowerShell and prepend the toolchain to PATH (see the `build` skill for why):

```powershell
$env:Path = "C:\ghcup\bin;C:\Program Files\Git\usr\bin;" + $env:Path
cabal run run-tests
```

CLI: `-f/--fail-fast`, `-l/--list`, `-h/--help`, and positional case-insensitive
name-substring PATTERNs. Examples:

```powershell
cabal run run-tests -- --list        # list test names, don't run
cabal run run-tests -- unit          # run only Engine.Unit.*
cabal run run-tests -- headless      # run only the headless game replays
cabal run run-tests -- shock         # run only Game.Headless.Shock
```

Tests covered (all pass, exit 0):

| Kind | Name |
| --- | --- |
| Unit | `Engine.Unit.PayMana` |
| Unit | `Engine.Unit.MagicCont` |
| Game (headless replay) | `Game.Headless.Shock` |
| Game (headless replay) | `Game.Headless.StoneRain` |
| Game (headless replay) | `Game.Headless.RagingGoblin` |
| Game (headless replay) | `Game.Headless.ManaAbility` |
| Game (headless replay) | `Game.Headless.Hybrid` |

The headless replays are 1:1 ports of the terminal string scripts (same engine +
identity shuffle ⇒ identical object IDs). A headless replay's *expected*
successful halt is a `Left "... out of decisions"` after the script is exhausted
— the runner treats a completed replay as a pass. To *write or modify* a game
replay, use the **`add-game-test`** skill.

## Fallback: the REPL (terminal game replays + demo) — AI-unfriendly

> **AI agents: do not run the REPL launchers or terminal game replays unless the
> user explicitly asks for them.** They require a **real interactive Windows
> console** that this automated shell doesn't provide, so they'll fail (or hang)
> under any piped/redirected/`-NonInteractive` run. For verifying a change,
> always use `cabal run run-tests` (headless) above — it covers the same
> scenarios. This whole section is for a human at a terminal, not for
> unattended runs.

The original **terminal** game replays (`Test.Game.Shock` et al.) drive the ANSI
terminal client and only run in a **real interactive Windows console** — under a
piped/redirected or `-NonInteractive` shell they throw
`GetNumberOfConsoleInputEvents: invalid argument (The handle is invalid.)`.
Prefer their `Test.Game.Headless.*` equivalents above for verification; reach for
the REPL only to *watch* a terminal replay or to run the demo.

| Kind | Module | Run |
| --- | --- | --- |
| Terminal game (replay) | `Test.Game.Shock` | `mainShock` |
| Terminal game (replay) | `Test.Game.StoneRain` | `mainStoneRain` |
| Terminal game (replay) | `Test.Game.RagingGoblin` | `mainRagingGoblin` |
| Terminal game (replay) | `Test.Game.ManaAbility` | `mainManaAbility` |
| Terminal game (replay) | `Test.Game.Hybrid` | `mainHybrid` |
| Demo game | `Demo.MtgPure.Gameplay` | `mainDemoGameplay` |

Each module's `main = main<Name>`, so `main` also works once that module is in
scope.

```powershell
$env:Path = "C:\ghcup\bin;C:\Program Files\Git\usr\bin;" + $env:Path
cabal repl
```
```
ghci> :m + Test.Game.Shock
ghci> mainShock
```

### The `-fobject-code` caveat

`ToObjectN.Instances` pulls in the large generated tree and is painfully slow to
load as interpreted bytecode, so the project caches it as object code
(`-fobject-code`, baked into `src/ghci-compiled.bat`). Two consequences, both
from the README:

- Prefer `-fobject-code` (via the `src/*.bat` launchers below) to avoid a
  multi-minute interpreted load.
- With `-fobject-code` you must **quit and re-enter** the REPL to pick up code
  changes — a hot `:reload` won't reflect edits reliably.

### The project's REPL launchers (human-only)

`src/` ships batch launchers that call `ghci` directly with the full flag set:
`run-ghci.bat` (interpreted), `ghci-compiled.bat` (`-O2 -fobject-code`), and
`mtg-ghci.bat` (`ghci-compiled MtgPure`). **These are AI-unfriendly** — they open
an interactive `ghci` session that never returns in an automated shell, so an AI
agent should ignore them unless the user explicitly asks to launch one.

## What "passing" looks like

- **`run-tests`**: named `=== PASS ... ===` lines and a `=== SUMMARY: N passed,
  0 failed ===`, exit 0.
- **Unit tests** compute and check expected values; a clean finish with no
  error/exception is a pass. `PayMana`'s `main` carries `HasCallStack`, so a
  failure surfaces with a call stack.
- **Game replays** play the scripted inputs to completion. Object ids in the
  scripts are engine-assigned per shuffle; if a script is stale or a run prompts
  for input it didn't expect, the ids likely need refreshing (see
  `add-game-test`).

## Verifying a change

For a code change, the first bar is that it **builds** (`build` skill). Then run
`cabal run run-tests` — or narrow to the relevant area with a PATTERN, e.g. a
mana/cost change → `cabal run run-tests -- unit manaability`; a damage/targeting
change → `cabal run run-tests -- shock`.

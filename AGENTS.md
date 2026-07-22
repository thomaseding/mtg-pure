# AGENTS.md

Orientation for AI agents working in `mtg-pure` — a purely functional Haskell
"Magic: The Gathering" card model and rules engine. This file is loaded every
session; keep it short. Detail lives in the linked docs.

## Read first

- [CODING_STYLE.md](CODING_STYLE.md) — the authoritative style guide (module
  headers, `import safe`, GADT decls, `InstanceSigs`, DSL conventions,
  `undefined` vs `InternalLogicError`). When in doubt, mimic a neighboring file.
- [README.md](README.md) — project goals and the quickstart REPL session.

## Shell

On **Windows**, prefer **PowerShell** for building and running the project
(cabal, ghc, tests); Git Bash is fine for auxiliary tasks (grep, git, file
wrangling). On Linux/macOS this doesn't apply — use the normal shell.

**Do NOT use `cabal repl`.** It is off-limits for agents. To exercise code or
render output, add or extend a `run-tests` entry (see the `run-tests` skill /
`src/Tests/RunTests.hs`) and run the compiled executable instead.

## Skills

Project skills live in `.claude/skills/`. Consult them when a task matches one.

## Layout

- `src/MtgPure/Cards.hs` + `src/MtgPure/AllCards.hs` — the card definitions
  (the showcase). `src/MtgPure/Model/Recursive.hs` — the core DSL.
- `src/MtgPure/Engine/` — the rules engine; cross-module calls go through the
  `Fwd/{Api,Impl,Type}` trio to break import cycles.
- `src/MtgPure/Client/Terminal/` — the solitaire CLI.
- `src/Test/` — game/unit tests, each exposing a `main`/`mainX` run from the
  REPL (not a cabal test-suite).

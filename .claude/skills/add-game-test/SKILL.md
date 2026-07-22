---
name: add-game-test
description: Add a scripted gameplay test under src/Test/Game/ in mtg-pure (a replayed terminal game that exercises specific cards/interactions). Use when the user wants a test for a card or rules interaction, a regression test for an engine change, or a reproducible game scenario. Covers the test skeleton, the replay-input mini-language, and registering the module.
---

# Add a gameplay test

Tests in [src/Test/Game/](../../../src/Test/Game/) (`Shock`, `StoneRain`,
`RagingGoblin`, `ManaAbility`, `Hybrid`) are **not** a cabal test-suite. Each is
a module exposing `main`/`main<Name>` that replays a fixed list of input strings
through the terminal client. Copy the closest existing one and edit it.

Do not run it here (see memory `mtg-pure-no-builds`); the user runs
`main<Name>` from the REPL.

## Skeleton (copy from `Test.Game.StoneRain`)

- Header: `{-# LANGUAGE Safe #-}` + `-Wno-unrecognised-pragmas` + the standard
  test HLINT block (copy verbatim from a neighbor — it's larger than the model
  one: adds `"Use ++"`, `"Redundant fmap"`, `"Evaluate"`).
- Exports both `main` and `main<Name>`; `main = main<Name>`.
- `main<Name> = runTerminal input do playTerminalGame cheats players` where
  `players` is `replicate 2 (deck, side)` (mirror match) or an explicit
  `[(deck1, side), (deck2, side)]` (asymmetric, as in `ManaAbility`).
- `input = TerminalInput { terminalInput_ = (), terminalInput_fwd = fwdImpl,
  terminalInput_replayInputs = replayInputs, terminalInput_replayLog = replayLog }`.
- `deck = Deck $ ... [AnyCard1 card1, AnyCard1 card2, ...]`;
  `side = Sideboard $ ...`. Use `replicate` to pad to a legal deck size.
- `cheats`: `noGameCheats`, or `noGameCheats{gameCheats_disableLosing = True}`
  (then also import `GameCheats (..)` from `MtgPure.Engine.State`).
- `replayLog :: Maybe FilePath` — `Nothing`, or `Just "replay-<Name>.log"` to
  dump the game log while iterating.

## The replay-input mini-language (`replayInputs :: [String]`)

One action per list element. Everything after `#` on a line is a **comment** —
by convention the expected `O=<player-with-priority> <Step> Turn<N>` — kept for
readability and eyeballing; it does not affect execution.

| Input | Meaning |
| --- | --- |
| `"Pass # ..."` | pass priority |
| `"PlayLand 124 # ..."` | play the land whose object id is 124 |
| `"CastSpell 123 # ..."` | cast the spell object 123 |
| `"ActivateAbility 137 R # ..."` | activate ability of object 137, feeding `R` (a mana/choice token) |
| `"ActivateAbility 89 0 # ..."` | activate; `0` selects the 0th option |
| `"2 # PickZO"` | answer a "pick zone-object" prompt with object id 2 |
| `"# Attack ; ..."` | comment-only line (a readable placeholder, e.g. declaring no attackers) |

Trailing comments like `-- fail: not enough mana` (Haskell comments, outside the
string) document expected failures inline; see `StoneRain`.

## The object ids are not guessable

Ids like `124`, `137`, `89` are assigned by the engine as the shuffled game
runs — you can't compute them by hand. The real workflow: set
`replayLog = Just "..."`, run `main<Name>` in the REPL, read the prompts/log to
learn the actual ids and required prompt answers, then paste them into
`replayInputs`. Since builds aren't runnable in this environment, scaffold the
deck + skeleton and leave a note that the user must fill in / verify the ids by
running it.

## Register the module

Add `Test.Game.<Name>` to `exposed-modules` in
[mtg-pure.cabal](../../../mtg-pure.cabal), alphabetized within the `Test.Game.*`
group. It won't be loadable otherwise.

## Checklist

- [ ] Module copied from a neighbor; `Safe` + full test HLINT block
- [ ] Exports `main` and `main<Name>`; `main = main<Name>`
- [ ] `deck`/`side` built from `AnyCard1 <card>`; cards imported from `MtgPure.Cards`
- [ ] `cheats` chosen (import `GameCheats (..)` if using a field)
- [ ] `replayInputs` scripted (ids verified against a real run, or flagged as TODO)
- [ ] `Test.Game.<Name>` added to `exposed-modules` (alphabetized)

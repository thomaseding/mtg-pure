# Orientation — agent quick-start

A one-page map so you can act quickly. Read [architecture.md](architecture.md)
for the "why".

## The 30-second model

`mtg-pure` has two showcases and a client:

1. **The model** — cards are *pure data*, a deeply-embedded, type-safe DSL. A
   card type-checks *iff* it is a legal card. The DSL is expressive enough that a
   card can reproduce its own source through `Show`. Core type: `Recursive.hs`.
2. **The engine** — a pure, exhaustively type-checked rules engine that runs a
   game over those cards. It runs in the `Magic` monad with visibility /
   read-write capabilities encoded in phantom types.
3. **The client** — a solitaire terminal UI (you drive every player) plus an
   ANSI card renderer.

## Layout at a glance

```
src/
  Data/, Control/, System/     Generic utilities (Nat, Carousel, Access monad, …)
  Ansi/                        True-color terminal rendering primitives
  MtgPure/
    Cards.hs                   *** the card definitions (the showcase) ***
    AllCards.hs                registry: allCards / allTokens
    Model/
      Recursive.hs             *** the core card DSL (GADTs) ***
      Recursive/Show.hs        hand-written Show (source round-trip)
      Recursive/Ord.hs         hand-written Ord
      Combinators.hs           card-authoring vocabulary (thin wrappers)
      Object/                  the OT / OTN / ZO type-level object system
        ToObjectN/             GENERATED code — never hand-edit (see regen skill)
      Mana/                    mana, costs, pools, symbols
      ... many small one-type modules (CardName, Color, Power, Zone, …)
    Engine/
      PlayGame.hs              top-level game driver
      State.hs                 GameState, the Magic monad type aliases
      Prompt.hs                player-decision interface + InternalLogicError
      Priority.hs, Turn.hs, Resolve.hs, Enact.hs, ...  rules subsystems
      Fwd/{Type,Api,Impl}.hs   indirection trio that breaks import cycles
    Client/Terminal/           the solitaire CLI
  Demo/MtgPure/Gameplay.hs     runnable demo (mainDemoGameplay)
  Test/
    Game/                      scripted replay games (Shock, RagingGoblin, …)
    Engine/Unit/               unit tests (PayMana, MagicCont)
  Script/                      one-off scripts (gallery gen, scryfall, codegen)
```

## Task → where to go

| Task | Skill | Primary files | Watch out for |
| --- | --- | --- | --- |
| Add/model a card or token | `add-card` | `Cards.hs`, `AllCards.hs`, `.cspell/mtg.dict` | 3-place wiring: export + `AllCards` + spelling. A card not in `allCards` silently vanishes. |
| Need DSL vocabulary a card can't express | `add-combinator` | `Model/Combinators.hs`, `Recursive.hs` | Prefer a thin combinator over reaching for a raw constructor in card code. |
| Add/rename/remove a `Recursive.*` constructor | `model-round-trip` | `Recursive.hs`, `Recursive/Show.hs`, `Recursive/Ord.hs`, `Data/ConsIndex.hs` | These three instances are hand-written and must move in lockstep, or the source round-trip breaks. |
| Engine fn in one module must call another | `add-engine-fwd` | `Engine/Fwd/{Type,Api,Impl}.hs` | Direct cross-module engine calls cause import cycles; route through `Fwd`. |
| New rules behavior / bug in resolution, priority, costs | — | `Engine/{Resolve,Enact,Priority,Turn,Pay,PayMana,ActivateCast}.hs` | Respect the `Magic v rw m` capability level; cite rule numbers `-- (509.1a)`. |
| Add a gameplay/regression test | `add-game-test` | `Test/Game/*.hs` | Tests are replay scripts, not a cabal suite; each exposes `main`/`mainX`. |
| New module of any kind | `new-module` | (new file) + `mtg-pure.cabal` | House header + explicit export list + `import safe`; register in the cabal `exposed-modules`. |
| Change `ObjectN`/`OT` machinery | `regen-object-code` | `Object/ToObjectN/CodeGen.hs` | The `ToObjectN/Instances*` output is generated & git-ignored — edit the generator, not the output. |

## The rules you cannot skip

From [CODING_STYLE.md](../CODING_STYLE.md) — internalize these before writing
Haskell here:

- **`{-# LANGUAGE Safe #-}` first**, then `-Wno-unrecognised-pragmas`, then the
  alphabetized `HLINT ignore` block. Explicit export list is **mandatory**.
- **`import safe [qualified]`** for every import. Use the conventional aliases
  (`M`, `List`, `Map`, `Set`, …).
- **GADT syntax** for all `data` decls; **derive `Typeable`**; record fields are
  **underscore-prefixed** by a per-type tag (`creature_power`).
- **`InstanceSigs`**: restate the method signature in every instance.
- Prefer **`\case`**, explicit lambdas, and spelled-out `case`/`if` — the HLINT
  pragmas exist *because* the project rejects point-free "simplifications".
- **Warnings are errors** (`-Wall -Werror`, incomplete patterns fatal). Code must
  be exhaustive.
- Unimplemented path → **`undefined`** (+ `-- TODO: [Card]`). Impossible path →
  a named **`InternalLogicError`** constructor, not bare `undefined`.
- Language extensions live in the **cabal `default-extensions`**, not per-file.
- **`TemplateHaskell` is banned** (`TemplateHaskellQuotes` only, for `'name`).

## Claude AI notes

- **Don't hand-edit** `Object/ToObjectN/Instances*` — it's generated.
- **Alphabetize** export lists, import lists, and the `.cspell` additions.
- **Leading commas**, 2-space indent, no space inside record braces
  (`Foo{field = x}`), exactly one blank line between top-level decls.
- New card-name / keyword words → add to the right `.cspell/*.dict`.

# Architecture

How the parts of `mtg-pure` fit together, and the invariants that shape the
whole design. If you only read one non-trivial doc, read this one.

## Design north star

Two goals govern every decision (see [README.md](../README.md)):

> **Type-safety, purity, and elegance.** Prefer making illegal states
> unrepresentable at the type level over runtime checks.

Concretely, the model aims for: *cards type-check if and only if they are valid
cards*. Creatures in graveyards can't be dealt damage; non-players can't draw
cards; a `Target` can only be something targetable — these are compile errors,
not runtime guards. When you extend the model, your first question is always
"can the type system forbid the illegal case?" before "what do I check at
runtime?".

## The three layers

```
        ┌────────────────────────────────────────────────────┐
        │  Client  (Client/Terminal, Ansi, App, Demo)         │  impure edges
        │  - solitaire CLI, ANSI rendering, replay scripts    │  (may drop Safe)
        └───────────────────────┬────────────────────────────┘
                                │ drives via Prompt
        ┌───────────────────────▼────────────────────────────┐
        │  Engine  (MtgPure/Engine/*)                         │  pure, Safe
        │  - the Magic monad; turn/priority/stack/resolution  │
        │  - reads & mutates GameState under capability types │
        └───────────────────────┬────────────────────────────┘
                                │ interprets
        ┌───────────────────────▼────────────────────────────┐
        │  Model  (MtgPure/Model/*, Cards.hs)                 │  pure, Safe
        │  - cards as data: the Recursive DSL                 │
        │  - the OT/OTN/ZO type-level object system           │
        └────────────────────────────────────────────────────┘
```

- **Model** knows nothing about games. It is a vocabulary for *describing*
  cards and their abilities as algebraic data.
- **Engine** knows nothing about terminals. It *interprets* the model against a
  `GameState`, asking a `Prompt` whenever a player decision is needed.
- **Client** supplies a `Prompt` implementation (terminal input, or a scripted
  replay) and renders state. This is where `Safe` may be dropped to talk to
  unsafe libraries.

The dependency arrows point **downward only**. Model never imports Engine;
Engine never imports Client.

## `Safe` Haskell as an architectural boundary

The model and engine are compiled with `{-# LANGUAGE Safe #-}` and every import
is `import safe`. This is a *deliberate guarantee*, not a formality: the pure
core provably cannot perform IO, use `unsafePerformIO`, or violate module
encapsulation. Only the outer UI/interop layer omits `Safe`. Preserve this: if
your change to the model or engine wants an unsafe dependency, that is a signal
the logic belongs in the client layer instead.

## Cards are data, twice over

A card is a value of `Card ot` (`Recursive.hs`). Because it is *just data*:

- The same card works in any format without modification (formats are engine
  configuration, not card properties).
- A rules change doesn't require re-modelling cards.
- **Introspection round-trip:** a card can reproduce its own source code. The
  hand-written `Show` in [`Recursive/Show.hs`](../src/MtgPure/Model/Recursive/Show.hs)
  prints a card as the Haskell expression that constructs it, and it compares
  equal to that expression. This invariant is why adding a constructor is a
  *lockstep* edit across `Recursive.hs` + `Show` + `Ord` + `ConsIndex` — see the
  `model-round-trip` skill and [model-dsl.md](model-dsl.md).

The model explicitly refuses brute-force special-casing: there is no
`ScrambleverseEffect`. Outlier cards must be expressible by *composing* the
existing algebra. If you cannot express a card, that is usually a missing
*general* combinator or constructor, not a missing special case.

## The `Fwd` indirection (engine)

The engine is split into many modules (`Priority`, `Resolve`, `Enact`, `Pay`,
…) that need to call each other, which naively creates import cycles. The
project breaks these with a **forward-reference trio**:

- [`Fwd/Type.hs`](../src/MtgPure/Engine/Fwd/Type.hs) — a record `Fwd'` of
  function fields, one per cross-module operation. Depends on nobody.
- [`Fwd/Api.hs`](../src/MtgPure/Engine/Fwd/Api.hs) — thin wrappers that pull a
  field out of the `Fwd` in `GameState` and call it. This is what engine modules
  import to invoke "someone else's" function (~100 operations).
- [`Fwd/Impl.hs`](../src/MtgPure/Engine/Fwd/Impl.hs) — builds the concrete `Fwd`
  record by wiring each field to its real implementation. Depends on everything.

So a call graph edge `A → B` becomes `A → Fwd/Api → (Fwd field) → B`, and only
`Fwd/Impl` sits at the bottom of the dependency order. Adding a cross-module
operation means extending all three — that is exactly what the `add-engine-fwd`
skill automates. The client layer has its own parallel trio under
`Client/Terminal/Fwd/`.

## The `Magic` monad and capabilities (engine)

Engine actions run in `Magic v rw m a` (aliases in
[`State.hs`](../src/MtgPure/Engine/State.hs), built on `Engine/Monad.hs`), where:

- `v :: Visibility` is `'Public` or `'Private` — what game information this
  action is allowed to see.
- `rw :: ReadWrite` is `'RO` or `'RW` — whether it may mutate `GameState`.

You move between capability levels **only** through the provided coercions
(`fromRO`, `fromPublic`, `internalFromPrivate`, …), never by hand. This encodes,
in types, facts like "computing whether a player *may* play a land is read-only
and needs private info" (`Magic 'Private 'RO m PlayLandReqs`). See
[engine.md](engine.md) for the full treatment.

## Player decisions: the `Prompt`

The engine never reads a keyboard. Whenever it needs a choice — which target,
which ability, pay or not — it calls a function on the `Prompt'` record
([`Prompt.hs`](../src/MtgPure/Engine/Prompt.hs)). The client supplies it. This
is what makes the engine pure and testable: a test supplies a `Prompt` that
replays a fixed list of input strings (see [engine.md](engine.md) and the
`add-game-test` skill). `headlessPrompt` in `State.hs` is a no-op prompt for
contexts that must not ask anything.

## Data flow of a single game

1. `PlayGame.playGame` takes a `GameInput` (players, decks, format, cheats,
   `Fwd`, `Prompt`) and builds the initial `GameState`.
2. The turn structure (`Turn.hs`) advances phases/steps; players receive
   **priority** (`Priority.hs`).
3. On priority a player may cast a spell / activate an ability / play a land
   (`ActivateCast.hs`, `PlayLand.hs`), paying costs (`Pay.hs`, `PayMana.hs`).
   Spells/abilities go on the **stack** (`Model/Stack.hs`).
4. When all players pass, the top of stack **resolves** (`Resolve.hs`), whose
   effects are **enacted** (`Enact.hs`) against `GameState`.
5. **State-based actions** (`StateBasedActions.hs`) are checked continually.
6. Every player decision along the way is a `Prompt` call answered by the
   client.

## Where to make a change

- New card / token → **Model layer**, `Cards.hs`. Never the engine.
- New *kind* of card behavior the DSL can't say → new `Recursive` constructor
  (model-round-trip) **and** engine interpretation of it.
- New rules mechanic / bug fix in how the game plays → **Engine layer**.
- New way to display or drive the game → **Client / Demo layer**.
- New reusable card-authoring shorthand → **`Combinators.hs`** (add-combinator).

# The rules engine (`MtgPure.Engine`)

How a game actually runs. Read [architecture.md](architecture.md) first for the
layering and the `Fwd` / capability ideas; this doc goes module-by-module.
Relevant skills: `add-engine-fwd` (cross-module ops) and `add-game-test`.

## The `Magic` monad

Engine code runs in `Magic v rw m a` — aliased in
[`State.hs`](../src/MtgPure/Engine/State.hs) over the primitives in
[`Monad.hs`](../src/MtgPure/Engine/Monad.hs):

```haskell
type Magic v rw m       = Magic' (GameResult m) (GameState m) v rw m
type MagicCont v rw bail m a = MagicCont' … v rw bail m a
```

- `m` — the underlying monad (usually the client's, e.g. terminal `IO`), kept
  polymorphic so the engine stays testable and pure-at-the-edges.
- `v :: Visibility` ∈ `{ 'Public, 'Private }` — how much hidden information the
  action may read (hands, libraries).
- `rw :: ReadWrite` ∈ `{ 'RO, 'RW }` — may it mutate `GameState`?
- `MagicCont` is the continuation-passing variant used where control flow bails
  out early (priority loops, payment that can be cancelled). See
  [`Test/Engine/Unit/MagicCont.hs`](../src/Test/Engine/Unit/MagicCont.hs).

### Capabilities are the point

The `v`/`rw` phantoms make illegal engine access a *type error*. A function that
merely computes whether a land can be played is `Magic 'Private 'RO m
PlayLandReqs`: it needs private info but must not mutate. You **cross levels only
through the sanctioned coercions** — `fromRO`, `fromPublic`,
`internalFromPrivate`, and friends defined in
[`Monad.hs`](../src/MtgPure/Engine/Monad.hs) — never by constructing a
differently-typed action by hand. The raw `Visibility`/`ReadWrite` kinds and the
`unsafe*` coercions live in
[`Control/Monad/Access.hs`](../src/Control/Monad/Access.hs); the `unsafe*` ones
are plumbing and should not appear in ordinary engine logic.

## `GameState`

[`State.hs`](../src/MtgPure/Engine/State.hs) defines `GameState m` — the entire
mutable world: players, zones, the stack, the current turn/phase/step, priority,
pending elections, event listeners, the `Prompt`, the `Fwd` record, and cheats.
Companion types:

- `OpaqueGameState m` — a read-only wrapper handed to the `Prompt` so client
  decision code can *observe* state without mutating it (`queryMagic`,
  `getOpaqueGameState`).
- `GameInput m` — everything needed to start a game (players/decks, `GameFormat`,
  `GameCheats`, `Fwd`, `Prompt`).
- `GameResult m` — the outcome; `concatGameResults` folds sub-results.
- `GameCheats` (e.g. `gameCheats_disableLosing`) — test/among affordances;
  `noGameCheats` is the default.

## `logCall` — every engine function announces itself

Engine functions begin with `logCall` using a `TemplateHaskellQuotes` name
quote, so the call log carries the real function name:

```haskell
playLand oPlayer (PlayLand oLand) = logCall 'playLand do
  …
```

`logCall` / `logCallRec` and the call-frame machinery live in `State.hs`
(`envLogCall`, `logCallPush/Pop/Unwind`, `Named`). This is house style, not
optional — mirror it in new engine functions. (Recall `TemplateHaskell` proper is
banned; only the `'name` quote form is used.)

## The `Fwd` trio (breaking import cycles)

The engine's modules mutually recurse, so cross-module calls are routed through a
forward-reference record instead of direct imports:

| File | Role |
| --- | --- |
| [`Fwd/Type.hs`](../src/MtgPure/Engine/Fwd/Type.hs) | `data Fwd'` — one field per cross-module operation. Imports nothing cyclic. |
| [`Fwd/Api.hs`](../src/MtgPure/Engine/Fwd/Api.hs) | Wrappers each engine module imports to *call* another module's op (~100). |
| [`Fwd/Impl.hs`](../src/MtgPure/Engine/Fwd/Impl.hs) | Wires every field to its real implementation; sits at the bottom of the dep order. |

To add an operation that one engine module must expose to another, extend **all
three** (add the field, the `Api` wrapper, the `Impl` wiring) — the
`add-engine-fwd` skill does exactly this and also explains which capability type
to give it. The terminal client has its own parallel trio under
[`Client/Terminal/Fwd/`](../src/MtgPure/Client/Terminal/).

## The subsystem modules

| Module | Responsibility |
| --- | --- |
| [`PlayGame.hs`](../src/MtgPure/Engine/PlayGame.hs) | `playGame :: GameInput m -> m (Maybe (GameResult m))` — top driver; builds state, seats players, runs the game. |
| [`Turn.hs`](../src/MtgPure/Engine/Turn.hs) | Turn structure: phases and steps (`Model/Phase.hs`, `PhaseStep.hs`, `Step.hs`). |
| [`Priority.hs`](../src/MtgPure/Engine/Priority.hs) | Passing priority; the "everyone passed → resolve top of stack" loop. |
| [`ActivateCast.hs`](../src/MtgPure/Engine/ActivateCast.hs) | Casting spells / activating abilities; putting them on the stack. |
| [`PlayLand.hs`](../src/MtgPure/Engine/PlayLand.hs) | Playing lands (special action, not the stack). `PlayLandReqs` legality. |
| [`Pay.hs`](../src/MtgPure/Engine/Pay.hs) / [`PayMana.hs`](../src/MtgPure/Engine/PayMana.hs) | Paying costs; mana payment (see unit test `Test/Engine/Unit/PayMana.hs`). |
| [`PerformElections.hs`](../src/MtgPure/Engine/PerformElections.hs) | Driving an `Elect` computation to a resolved value, asking the `Prompt` for choices. |
| [`Resolve.hs`](../src/MtgPure/Engine/Resolve.hs) | Resolving the top stack object into effects. |
| [`Enact.hs`](../src/MtgPure/Engine/Enact.hs) | Applying `Effect`s to `GameState`. |
| [`PutOntoBattlefield.hs`](../src/MtgPure/Engine/PutOntoBattlefield.hs) | Moving permanents onto the battlefield (ETB, tapped-ness, …). |
| [`StateBasedActions.hs`](../src/MtgPure/Engine/StateBasedActions.hs) | Continuous SBA checks (lethal damage, 0-loyalty, …). |
| [`Satisfies.hs`](../src/MtgPure/Engine/Satisfies.hs) | Evaluating `Requirement`/`Condition` against state. |
| [`Legality.hs`](../src/MtgPure/Engine/Legality.hs) | `Legality` (legal/illegal) results for attempted actions. |
| [`CaseOf.hs`](../src/MtgPure/Engine/CaseOf.hs) | Evaluating the DSL's `Case`/`CaseFin` branches. |
| [`Core.hs`](../src/MtgPure/Engine/Core.hs) | Low-level state accessors/mutators the others build on. |
| [`Orphans.hs`](../src/MtgPure/Engine/Orphans.hs), [`Orphans/ZO.hs`](../src/MtgPure/Engine/Orphans/ZO.hs) | Orphan instances; import for-instances-only with `()`. |

## `Prompt` — the player decision interface

[`Prompt.hs`](../src/MtgPure/Engine/Prompt.hs) defines `Prompt' opaque m` — a
record of callbacks the engine invokes for every decision (choose target, pick
ability, order triggers, pay-or-decline, mulligan, …). The client supplies the
implementation; the engine never does IO itself. Two important residents of this
module:

- **`InternalLogicError`** — the GADT for *"can't happen"* sites (states the
  types allow but the rules forbid). Each constructor names *why*
  (`InvalidPlayer`, `ExpectedStackObjectToExist`, `ManaAbilitiesDontHaveTargets…`).
  When you hit such a site, **add a descriptive constructor** and
  `error $ show YourCtor` rather than a bare `undefined`. Replace the generic
  `CantHappenByConstruction` with a specific one when you touch it.
- Result/index newtypes (`PlayerCount`, `CardIndex`, `RelativeAbilityIndex`,
  `ActivateResult`, `QueryObjectResult`, …) and the `Pending`/`Ready`/`Elected`
  election-progress types.

### `undefined` vs `InternalLogicError` (recap of the style rule)

- Path **not yet implemented** → `undefined` (GHC prints file+line when forced),
  ideally with `-- TODO: [Card]` or `-- TODO: Something descriptive`.
- Path **truly unreachable** by game logic → a named `InternalLogicError`
  constructor. Prefer exhaustive `case`; incomplete patterns are fatal warnings.

## Running the engine (for humans with a toolchain)

The engine is exercised through the terminal client / demo, not a `cabal test`
suite:

- Demo: `mainDemoGameplay` in
  [`Demo/MtgPure/Gameplay.hs`](../src/Demo/MtgPure/Gameplay.hs).
- Tests: each module in [`Test/Game/`](../src/Test/Game/) and
  [`Test/Engine/Unit/`](../src/Test/Engine/Unit/) exposes `main` / `mainX`, run
  from the REPL. Game tests are **replay scripts** — a `Prompt` that feeds a
  fixed `[String]` of commands like `"CastSpell 7 # …"`. See
  [working-without-builds.md](working-without-builds.md) and the `add-game-test`
  skill for the replay mini-language.

Agents cannot run these (no toolchain); verify engine changes by reading and by
type reasoning.

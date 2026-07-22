---
name: add-engine-fwd
description: Add or wire a cross-module engine operation through the Fwd indirection in mtg-pure (the Fwd/Type, Fwd/Impl, Fwd/Api trio). Use when an engine function in one MtgPure.Engine.* module needs to call a function defined in another engine module, when you hit an import cycle between engine modules, or when adding a new primitive the engine exposes. Also covers the Magic visibility/read-write capability types those signatures use.
---

# Add a cross-module engine operation (the `Fwd` idiom)

The engine modules under `src/MtgPure/Engine/` would form an import cycle if they
called each other directly. Instead, every cross-module call goes through a
record of function pointers, `Fwd'`, threaded through the `Magic` monad's state.
Adding or exposing an operation is a **fixed edit across three files**; miss one
and it won't compile.

Do not build here (see memory `mtg-pure-no-builds`). Match the pattern by hand.

## The three files

1. **[Fwd/Type.hs](../../../src/MtgPure/Engine/Fwd/Type.hs)** — declares the
   `Fwd'` GADT record. Add a field:

   ```haskell
   , fwd_myOp :: forall zone ot. (IsZO zone ot) => ZO zone ot -> Magic' ex st 'Private 'RO m Foo
   ```

   Keep fields **alphabetized by `fwd_` name** (leading commas). Note the type
   here is `Magic' ex st v rw m a` (the primed, fully-parameterized form) — copy
   the shape of a neighbor with the same arity/capability.

2. **[Fwd/Impl.hs](../../../src/MtgPure/Engine/Fwd/Impl.hs)** — builds the
   concrete `fwdImpl` record. Two edits:
   - `import safe` the real implementation from its home module
     (`MtgPure.Engine.Core`, `.Pay`, `.Resolve`, `.Satisfies`, …), added
     alphabetically to that module's import list.
   - Add `, fwd_myOp = myOp` to the `Fwd { ... }` record, alphabetized.

3. **[Fwd/Api.hs](../../../src/MtgPure/Engine/Fwd/Api.hs)** — the thin callable
   wrappers other modules import. Two edits:
   - Add the wrapper name to the module export list (alphabetized).
   - Write the wrapper using the arity helper. Pick `fwd0`/`fwd1`/`fwd2`/`fwd3`
     by argument count:

     ```haskell
     myOp :: (IsZO zone ot, Monad m) => ZO zone ot -> Magic 'Private 'RO m Foo
     myOp = fwd1 fwd_myOp
     ```

     The wrapper's type uses the unprimed `Magic v rw m a`. `fwd0` for no args,
     up to `fwd3` for three (`fwd4` is commented out — add it if you truly need
     four, following the pattern).

## Continuation-returning ops (`MagicCont`)

If the operation returns `MagicCont`/`MagicCont'` (priority, resolution — e.g.
`askPriorityAction`, `gainPriority`, `endTheTurn`, `resolveTopOfStackCont`), the
`fwdN` helpers don't apply (they're `Magic`, not `MagicCont`). Write the wrapper
explicitly instead, matching the existing cont wrappers:

```haskell
myContOp :: (Monad m) => Object 'OTPlayer -> MagicCont 'Private 'RW PriorityEnd m ()
myContOp a = do
  fwd <- liftCont getFwd
  fwd_myContOp fwd a
```

## When the wrapper is more than a pass-through

Some `Api.hs` wrappers do real work rather than forward verbatim — e.g.
`modifyPlayer` composes `getPlayer` + `setPlayer`, `endTheGame` adapts
`GameResult` to the `ex` type. That's fine; a pure forward is the default, but
put small composition logic here rather than adding a redundant `fwd_` field.

## Capability types in the signature (get these right)

Engine actions run in `Magic v rw m a` where the phantoms encode access control:

- **Visibility** `v`: `'Private` (full state) vs `'Public` (opponent-visible
  only). Most engine internals are `'Private`.
- **ReadWrite** `rw`: `'RO` (read-only) vs `'RW` (mutates state).

Pick the *least* capability the operation needs (an `'RO` op should not be typed
`'RW`). Never cross capability levels by hand — only through the provided
wrappers `fromRO`, `fromPublic`, `internalFromPrivate`. Object/zone args are
typically constrained `(IsZO zone ot)` or take a concrete `Object 'OTPlayer` /
`ZO 'ZBattlefield OTNPermanent`.

## The separate `Api`/`ApiCont` reification (only sometimes)

`Api.hs` also defines `Api`/`ApiCont` GADTs plus `run`/`runCont`, a *reified*
form of a subset of these calls (used by the serializable-prompt layer). This is
**not** required for every fwd op — only add an `Api` constructor + a `run` arm
if the operation must be reifiable as data. Most new ops just need the three-file
wiring above.

## Checklist

- [ ] `fwd_myOp` field added to `Fwd'` in `Fwd/Type.hs` (alphabetized, `Magic'` type)
- [ ] Implementation imported into `Fwd/Impl.hs` (alphabetized)
- [ ] `fwd_myOp = myOp` wired in the `fwdImpl` record (alphabetized)
- [ ] Wrapper exported from `Fwd/Api.hs` (alphabetized)
- [ ] Wrapper defined with the right `fwdN` (or explicit `liftCont getFwd` for cont)
- [ ] Capability phantoms (`'Private/'Public`, `'RO/'RW`) are the minimum needed
- [ ] Begins engine impl with `logCall 'myOp do ...` where applicable

---
name: model-round-trip
description: Add or change a constructor in the recursive card DSL (MtgPure.Model.Recursive types like Effect, Elect, Cost, Requirement, Condition, abilities) in mtg-pure, keeping the hand-written ConsIndex / Show / Ord instances in lockstep. Use whenever you add, remove, or rename a constructor of a Recursive.* GADT — the project's "a card reproduces its own source" invariant depends on these instances being updated together.
---

# Change a recursive DSL constructor (keep the round-trip intact)

A headline goal of the model (README) is that a card can **reproduce its own
source** through its `Show` instance, and that `Eq`/`Ord` are structural. Those
instances are **hand-written, not derived** (the GADTs can't derive them). So a
constructor added to a type in [Recursive.hs](../../../src/MtgPure/Model/Recursive.hs)
must be mirrored in several places or the model silently breaks.

Applies to the recursive GADTs: `Effect`, `Elect`, `Cost`, `Requirement`,
`Condition`, `Ability`/`ActivatedAbility`/`StaticAbility`/`TriggeredAbility`,
`CardCharacteristic`, `EnchantmentType`, etc.

Do not build here (see memory `mtg-pure-no-builds`) — reason it through by hand.

## The lockstep, per constructor

Take `Effect` as the worked example. Adding a constructor `Foo :: ... -> Effect 'OneShot`:

1. **[Recursive.hs](../../../src/MtgPure/Model/Recursive.hs)** — the constructor
   in the `data Effect` GADT, **and** an arm in that type's `ConsIndex`
   instance:

   ```haskell
   instance ConsIndex (Effect ef) where
     consIndex = \case
       AddMana{} -> 1
       ...
       Foo{} -> 29   -- unique index
   ```

   Existing instances list constructors alphabetically with sequential ints;
   follow that (the note in [Data.ConsIndex](../../../src/Data/ConsIndex.hs)
   mentions the "NumberMonger" VSCode extension / Copilot for repopulating these).
   The integers only need to be **unique and total** — they define the `Ord`
   ordering *between different constructors*.

2. **[Recursive/Show.hs](../../../src/MtgPure/Model/Recursive/Show.hs)** — a
   `showEffect` arm that emits **valid, re-parseable source** matching how the
   value is written in card code (constructor or combinator spelling). Use the
   local helpers: `yesParens`/`noParens`, `dollar`, `showListM`, and the
   per-type `showX` recursors. Copy a structurally similar arm.

3. **[Recursive/Ord.hs](../../../src/MtgPure/Model/Recursive/Ord.hs)** — a
   pairwise arm in `ordEffect` (the nested `\case`), with the standard
   `y -> compareIndexM x y` fallback for the mismatched-constructor case. `Eq`
   is derived from this (`runEnvM (ordEffect x y) == EQ`), so you get it for
   free once `ordEffect` is updated. For constructors carrying an existential
   `ot`, mirror the `cast`/`compareOT`/`compareZoneOT` pattern the neighbors use.

4. **[Recursive/Tree.hs](../../../src/MtgPure/Model/Recursive/Tree.hs)** — marked
   `-- WIP` and currently has no `BuildTree` instances for these types, so
   usually nothing to do. Check anyway; if you're extending it, update it too.

## What GHC catches vs what it doesn't

- **GHC catches** missing arms in `consIndex`, `showEffect`, `ordEffect`: they're
  exhaustive `\case`s and `-Wincomplete-patterns -Werror` is on. So a fresh
  constructor won't compile until you add all three arms.
- **GHC does NOT catch** a *wrong* `Show` arm. Producing a string that doesn't
  round-trip (wrong constructor name, missing `$`, bad parens) still compiles.
  That correctness is on you — mirror how the card is actually authored, and if
  practical eyeball `print`/`==` against a hand-written literal (as the README
  quickstart does with `island`).

## Removing / renaming a constructor

Same files, in reverse: drop the constructor and its `consIndex`/`showX`/`ordX`
arms. If you remove one from the middle, you don't have to renumber `consIndex`
(uniqueness is all that matters), but renumber if you want to preserve the
alphabetical-sequential convention.

## Checklist (for each changed type)

- [ ] Constructor added/removed in the `Recursive.hs` GADT
- [ ] `ConsIndex` arm updated (unique index)
- [ ] `show<Type>` arm in `Recursive/Show.hs` emits re-parseable source
- [ ] `ord<Type>` arm in `Recursive/Ord.hs` (Eq follows automatically)
- [ ] `Recursive/Tree.hs` checked (usually WIP no-op)
- [ ] Sanity: Show output matches the combinator/constructor spelling a card would use

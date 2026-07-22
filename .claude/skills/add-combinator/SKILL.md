---
name: add-combinator
description: Add a card-authoring combinator to MtgPure.Model.Combinators in mtg-pure. Use when a card definition needs DSL vocabulary that doesn't exist yet (a new effect/cost/requirement/ability helper), or when card code is reaching for a raw Recursive constructor because no combinator wraps it. Explains the thin-wrapper + As* coercion pattern and how combinators relate to the Recursive DSL.
---

# Add a card-authoring combinator

[MtgPure.Model.Combinators](../../../src/MtgPure/Model/Combinators.hs) is the
surface vocabulary card definitions are written in (see the big import block at
the top of [Cards.hs](../../../src/MtgPure/Cards.hs)). Combinators are **thin
wrappers over the raw `MtgPure.Model.Recursive` constructors**, existing so cards
read like Magic and so multiple concrete object types can be accepted uniformly.

House rule (from [CODING_STYLE.md](../../../CODING_STYLE.md)): card definitions
use a combinator wherever one exists rather than a raw constructor. So when a
card needs a constructor that isn't yet wrapped, add the wrapper here.

Do not build here (see memory `mtg-pure-no-builds`).

## Before adding: check what exists

- Is there already a combinator that does this? (Grep the export list.)
- Does the underlying `Recursive` constructor exist? If **the constructor itself**
  is missing, that's a DSL change — see the `model-round-trip` skill first
  (adding a constructor touches Show/Ord/ConsIndex), then come back and wrap it.

## The pattern

Most combinators are a one-liner: the constructor, possibly post-composed with an
`As*` coercion. Examples from the file:

```haskell
manaCost :: (ToManaCost a) => a -> Cost
manaCost = ManaCost . toManaCost

destroy :: (AsPermanent ot) => ZO 'ZBattlefield ot -> Effect 'OneShot
destroy = Destroy . asPermanent

dealDamage ::
  (AsDamageSource source, AsCreaturePlayerPlaneswalker target, AsDamage damage, IsZO zone OTNDamageSource) =>
  ZO zone source -> ZO 'ZBattlefield target -> damage -> Effect 'OneShot
dealDamage source target = DealDamage (asDamageSource source) (asCreaturePlayerPlaneswalker target) . asDamage

is :: (IsZone zone, CoAny ot) => ZO zone ot -> Requirement zone ot
is = Is
```

Conventions:

- **Pick the right result layer** the card author needs to produce:
  `Effect 'OneShot` / `Effect 'Continuous`, `Cost`, `Requirement zone ot`,
  `Condition`, `Elect s el ot`, `StaticAbility`/`ActivatedAbility`/... Match the
  constructor's own result type.
- **Prefer an `As*` coercion class** (`AsDamage`, `AsPermanent`, `AsAny`,
  `AsSpell`, `AsCreaturePlayerPlaneswalker`, …) on arguments so the combinator
  accepts any convertible object type, exactly like the neighbors. If you need a
  coercion that doesn't exist, add a small `As*` class here following the
  existing ones rather than forcing a concrete type.
- Keep the same constraint style (`IsZO zone ot`, `CoPermanent ot`, `CoAny ot`,
  `IsOTN ot`) the surrounding combinators use.
- `InstanceSigs`, `\case`, explicit lambdas — same house style as everywhere.

## Wire it up

1. Add the name to the `MtgPure.Model.Combinators` export list. The list mixes
   plain values and `As*(..)`/class exports; keep it roughly alphabetized like
   the existing entries (leading commas).
2. Import whatever `Recursive` constructor / type you now reference into this
   module's `import safe MtgPure.Model.Recursive ( ... )` block (alphabetized).
3. In [Cards.hs](../../../src/MtgPure/Cards.hs), add the new name to the big
   `import safe MtgPure.Model.Combinators ( ... )` block (alphabetized) so cards
   can use it.

## Checklist

- [ ] Confirmed no existing combinator covers it, and the `Recursive` constructor exists
- [ ] Wrapper defined with explicit type, right result layer, `As*` coercions on args
- [ ] Exported from `Combinators` (alphabetized)
- [ ] Needed `Recursive` names imported into `Combinators` (alphabetized)
- [ ] Added to the `Combinators` import block in `Cards.hs` (alphabetized)

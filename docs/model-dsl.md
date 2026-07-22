# The card DSL (`MtgPure.Model.Recursive`)

How cards are represented as data. Pair this with [type-system.md](type-system.md)
(the `OT`/`OTN`/`ZO` machinery the signatures use) and the `add-card` /
`add-combinator` / `model-round-trip` skills.

Everything here lives in [`Recursive.hs`](../src/MtgPure/Model/Recursive.hs)
unless noted. It is a large GADT module — the type index on each constructor is
what enforces "type-checks iff legal".

## The shape of a card

A card definition is one top-level binding: an explicit type, a string name, and
a nested record body built from combinators. The two burn instants are the
canonical example — nearly identical, differing only in damage:

```haskell
lightningBolt :: Card OTNInstant
lightningBolt = Card "Lightning Bolt" $
  Your \you ->
    ElectCardFacet
      InstantCharacteristic
        { instant_colors = toColors R
        , instant_supertypes = []
        , instant_spec =
            Target you $ masked @OTNCreaturePlayerPlaneswalker [] \target ->
              ElectCardSpec
                InstantSpec
                  { instant_cost = manaCost R
                  , instant_abilities = []
                  , instant_effect = thisObject \this ->
                      effect $ dealDamage this target 3
                  }
        }
```

Read it top-down: *"For its controller `you`, this is an instant with red color;
choose a target creature/player/planeswalker; it costs {R}; on resolution this
object deals 3 damage to that target."*

## The core type stack

Roughly, a `Card ot` wraps an `Elect` computation that yields a
`CardCharacteristic ot`, which contains a `CardSpec ot`, which contains the
abilities and effects:

```
Card ot
  └─ Elect 'IntrinsicStage (CardCharacteristic ot) ot     -- Card constructor
        └─ CardCharacteristic ot   (InstantCharacteristic, CreatureCharacteristic, …)
              ├─ printed characteristics (colors, supertypes, power/toughness…)
              └─ CardSpec ot        (InstantSpec, CreatureSpec, …)
                    ├─ cost
                    ├─ abilities :: [Ability zone ot]
                    └─ effect / (creature body, etc.)
```

- **`Card ot`** — `Card name electedCharacteristic`, plus `DoubleSidedCard` and
  `SplitCard` for multi-faced cards. `ot` is the object-type index
  (`OTNInstant`, `OTNCreature`, …).
- **`CardCharacteristic ot`** — one constructor per card type
  (`InstantCharacteristic`, `CreatureCharacteristic`, `LandCharacteristic`, …).
  Holds the *printed* characteristics. Fields are underscore-prefixed by type
  (`creature_colors`, `instant_spec`).
- **`CardSpec ot`** — the "spec" half (`CreatureSpec`, `InstantSpec`, …): cost,
  abilities, and the type-specific payload.
- **`SpecificCard ot`** — a singleton/witness GADT enumerating the card types
  (`InstantCard`, `CreatureCard`, …), used to recover the concrete type from a
  generic `ot`.
- **`AnyCard`** — existential wrapper (`AnyCard1` for single-faced,
  `AnyCard2` for double-faced) so heterogeneous card lists (decks, `allCards`)
  can be held. Cards enter a deck as `AnyCard1 someCard`.

## `Elect` — the election monad-ish structure

`Elect (s :: ElectStage) (el :: Type) (ot :: Type)` is the heart of the DSL. It
represents *"a value of type `el`, possibly after making choices"* — choosing
targets, players, options, paying costs, branching on conditions. It threads
continuations, so you see the `\you -> …`, `\target -> …`, `\this -> …` lambdas.

### Election stages (`ElectStage`)

The `s` index is a small state machine that orders *when* each kind of choice is
legal, mirroring the rules' sequence for putting something on the stack:

| Stage | Meaning | Example constructors valid here |
| --- | --- | --- |
| `'IntrinsicStage` | intrinsic/printed facts, before anything is cast | `ElectCardFacet`, `Cost`, `Your`, `Listen` |
| `'TargetStage` | choosing targets & modes | `Target`, `EndTargets`, `ElectActivated`, `VariableInt` |
| `'ResolveStage` | on-resolution, effects happen | `Effect`, `Event`, `PlayerPays`, `VariableFromPower` |

A constructor's type pins the stage(s) it may appear in, so an illegal ordering
(e.g. producing an `Effect` at intrinsic stage) is a type error. Notable
constructors:

- `Your \you -> …` — bind the controller/owner-to-be.
- `ElectCardFacet char` — commit the printed characteristics (intrinsic stage).
- `Target you $ masked @OT [reqs] \t -> …` — declare a target constrained by
  `Requirement`s; `EndTargets` closes the targeting phase.
- `ElectCardSpec spec` — commit the spec (target stage).
- `Effect [ … ]` — the resolution effects (resolve stage).
- `If cond a (Else…)`, `ElectCase (CaseFin …)`, `ChooseOption`, `Random` —
  branching / choice.
- `ActivePlayer`, `ControllerOf`, `OwnerOf` — bind a player from context.

## Effects, costs, requirements, conditions

These are the leaf vocabularies effects compose from:

- **`Effect (ef :: EffectType)`** — `'OneShot` vs `'Continuous`. E.g.
  `DealDamage src target amount`, `AddMana`, `AddToBattlefield`,
  `CounterSpell`, `ChangeTo`, `CantBeRegenerated`. The `ef` index keeps one-shot
  and continuous effects from being confused.
- **`Cost`** — `ManaCost`, `TapCost [reqs]`, `SacrificeCost`, `ExileCost`,
  `PayLife`, `LoyaltyCost`, `DiscardRandomCost`, and combinators `AndCosts` /
  `OrCosts` / `CostCase`.
- **`Requirement zone ot`** — a predicate on an object in a zone: `ControlledBy`,
  `OwnedBy`, `IsTapped`, `HasLandType`, `OfColors`, `HasAbility`, `Is`,
  `IsOpponentOf`, plus `Not` / `RAnd` / `ROr` / `Req2..Req5`. These constrain
  targets and searches.
- **`Condition`** — boolean over game state: `Satisfies zo [reqs]`, `CAnd`,
  `COr`, `CNot`. Feeds `If` and `Elect Condition`.
- **`Ability zone ot`** — `Activated`, `Static`, `Triggered`. Abilities are
  themselves `Elect` computations (activated abilities elect their cost/effect).

## Abilities

- **Activated** (`Activated …`): `Ability` with a cost and an effect; e.g. a
  mana ability `{T}: Add {G}`. Also `Cycling`.
- **Static** (`Static …`): continuous, e.g. `Flying`, cost reductions.
- **Triggered** (`Triggered …`): fired by an `EventListener` — `EntersBattlefield`,
  `BecomesTapped`, `SpellIsCast`, `TimePoint`, etc. (`EventListener'` GADT).

## Combinators vs raw constructors

Card code should read like MTG, so it is written with the **combinator DSL** in
[`Combinators.hs`](../src/MtgPure/Model/Combinators.hs) — `toColors`, `manaCost`,
`tapCost`, `effect`, `dealDamage`, `controllerOf`, `masked`, `thisObject`,
`static`, `activated`, `Target`, … These are thin, well-typed wrappers (often
inserting `As*` coercions) over `Recursive` constructors.

Rule of thumb: **in card definitions, reach for a combinator; only touch a raw
`Recursive` constructor when no combinator exists** — and when that happens,
consider adding the combinator (`add-combinator` skill) so the next card is
clean.

## The round-trip invariant (why edits are lockstep)

`Show (Recursive.*)` is **hand-written** in
[`Recursive/Show.hs`](../src/MtgPure/Model/Recursive/Show.hs) to print a card as
the Haskell that builds it; `Ord` is hand-written in
[`Recursive/Ord.hs`](../src/MtgPure/Model/Recursive/Ord.hs); both rely on
`ConsIndex` ([`Data/ConsIndex.hs`](../src/Data/ConsIndex.hs)) assigning each
constructor a stable index. Therefore:

> **Adding, removing, or renaming any `Recursive.*` constructor requires updating
> `ConsIndex`, `Show`, and `Ord` in the same change**, or the round-trip / total
> ordering breaks. This is exactly what the `model-round-trip` skill walks you
> through. Do not add a constructor and stop.

## Authoring checklist (see `add-card` for the full version)

1. Copy the nearest existing card of the same *shape*; edit it.
2. Explicit `foo :: Card OTN<Type>`; `camelCase` name; string literal = real name.
3. Build with combinators; underscore-prefixed fields.
4. Export it (alphabetized) in `Cards.hs`; register `toCard foo` in
   [`AllCards.hs`](../src/MtgPure/AllCards.hs) (a card missing here silently
   disappears from `allCards`).
5. Add new name-words to [`.cspell/mtg.dict`](../.cspell/mtg.dict).
6. Hand-format to fourmolu output.

# The object type system (`OT` / `OTN` / `ZO` / `Zone`)

The type-level machinery that lets "type-checks iff legal" work. This is the
densest part of the codebase; you rarely *modify* it (that's the
`regen-object-code` skill's territory), but you constantly *read* its types in
model and engine signatures. This doc is a reading aid.

Lives under [`src/MtgPure/Model/Object/`](../src/MtgPure/Model/Object/) and
[`ZoneObject/`](../src/MtgPure/Model/ZoneObject/).

## The layers, bottom to top

```
OT      a single object-type tag        'OTCreature, 'OTLand, 'OTPlayer, …   (kind OT)
OTN     a *set* of allowed tags         OTN '[ 'OTCreature ]                 (a Type)
ZO      an OTN placed in a Zone         ZO 'ZBattlefield OTNCreature         (a Type)
Object  a runtime-identified object     Object 'OTPlayer                     (an id + tag)
```

### `OT` — object type tags

`OT` ([`Object/OT.hs`](../src/MtgPure/Model/Object/OT.hs)) is the kind of
promoted tags: `'OTArtifact`, `'OTCreature`, `'OTEnchantment`, `'OTInstant`,
`'OTLand`, `'OTPlaneswalker`, `'OTPlayer`, `'OTSorcery`, `'OTBattle`,
`'OTActivatedAbility`, … Promoted, so they appear with the leading tick.

### `OTN` — "object type, N of them"

An `OTN` is a *type* built from a type-level **list** of `OT` tags — it says
"one of these object types". Aliases live in
[`OTNAliases.hs`](../src/MtgPure/Model/Object/OTNAliases.hs):

```haskell
type OTNCreature = OTN '[ 'OTCreature ]
type OTNInstant  = OTN '[ 'OTInstant ]
type OTNLand     = OTN '[ 'OTLand ]
-- unions:
type OTNCreaturePlayerPlaneswalker = OTN '[ 'OTCreature, 'OTPlayer, 'OTPlaneswalker ]
type OTNPermanent = OTN '[ 'OTArtifact, 'OTCreature, 'OTEnchantment, 'OTLand, 'OTPlaneswalker, 'OTBattle ]
```

So a burn spell targets `OTNCreaturePlayerPlaneswalker` — the union that MTG
calls "any target". A card's own type is a singleton `OTN`, e.g.
`Card OTNInstant`. The `N` naming (`OT1`, `OT2`, `OT5`, `OTN`) refers to how many
tags are in the set; the generated `ToObjectN` code provides conversions among
these arities (this is the big generated tree — see below).

### `Zone` and `ZO` — an object located somewhere

[`Zone.hs`](../src/MtgPure/Model/Zone.hs) enumerates the game zones:
`'ZBattlefield`, `'ZExile`, `'ZGraveyard`, `'ZHand`, `'ZLibrary`, `'ZStack`.

`ZO` (= `ZoneObject`,
[`ZoneObject/ZoneObject.hs`](../src/MtgPure/Model/ZoneObject/ZoneObject.hs)) pairs
a zone with an `OTN`: `ZO 'ZBattlefield OTNCreature` is "a creature on the
battlefield". This is what makes *"you can't deal damage to a creature in a
graveyard"* a type error — the damage effect wants a battlefield `ZO`, and a
graveyard `ZO` won't unify. Convenience aliases: `ZOPlayer`, `ZOCreature`,
`ZOAny`, `ZOPermanent`, `ZOCreaturePlayerPlaneswalker`, …

Zone conversions (e.g. a card moving hand→stack→battlefield) go through
[`ZoneObject/Convert.hs`](../src/MtgPure/Model/ZoneObject/Convert.hs).

### `Object` and `ObjectId`

At runtime an object is an `Object 'OTPlayer` etc. — an identity
([`ObjectId.hs`](../src/MtgPure/Model/Object/ObjectId.hs)) tagged with its type.
The engine's `GameState` maps ids to their data. `PromoteIdToObjectN` lifts a
plain id into the `OTN` world.

## The constraint vocabulary you'll see in signatures

- **`IsOTN ot`**, **`IsZO zone ot`** — "`ot` is a well-formed object-type set" /
  "`zo` is a well-formed zoned object". Almost every model/engine function that
  is generic over object type carries one of these.
- **`IsObjectType a`** — `a` is a single `OT` tag.
- **`CoPermanent ot`, `CoCard ot`, `CoSpell ot`, `CoAny ot`, `CoNonBattlefield
  zone`** — "coercion/membership" witnesses: e.g. `CoPermanent ot` proves the
  object set is a subset of the permanent types, so an effect that only makes
  sense for permanents can demand it. These come from the `Singleton/*` modules.
- **`Inst2 / Inst5 IsObjectType …`** ([`Data/Inst.hs`](../src/Data/Inst.hs)) —
  apply a constraint to several type args at once (used by the `OT2`/`OT5`
  arities and `AnyCard2`).
- **`SpecificCard ot` / `IsSpecificCard ot`** — recover the concrete card type
  from a generic index (used by `Card`, `AnyCard`).

Reading tip: when a signature is a wall of constraints, they are almost always
just *"this object is of a legal kind for this operation"*. Skim to the value
types; the constraints are the safety net, not the logic.

## Singletons (term ⇄ type bridge)

Because the tags are type-level, the engine needs runtime witnesses to branch on
them. That's the `Sing*` family — `SingOT`
([`SingOT.hs`](../src/MtgPure/Model/Object/SingOT.hs)),
[`Singleton/*`](../src/MtgPure/Model/Object/Singleton/) (`Any`, `Card`,
`Permanent`, `Spell`, `NonCreatureCard`). You'll see engine `\case
SingZBattlefield -> … ; SingZExile -> …` matches; each arm recovers type info.
Some arms are `undefined -- TODO: [SomeCard]` for zones/types not yet handled —
that's the sanctioned stub (see [CODING_STYLE.md](../CODING_STYLE.md)).

## The generated `ToObjectN` tree — do not hand-edit

[`Object/ToObjectN/`](../src/MtgPure/Model/Object/ToObjectN/) contains a large
tree of `Instances*` modules (`ToObject_01`, `ToObject_13_13`, …). These provide
the boilerplate conversions between `OTN` arities and are **produced by a code
generator and git-ignored**:

- Generator: [`ToObjectN/CodeGen.hs`](../src/MtgPure/Model/Object/ToObjectN/CodeGen.hs).
- To change the output: **edit `CodeGen.hs` and regenerate** — never edit an
  `Instances*` file. This is the whole point of the `regen-object-code` skill.
- Regeneration (for the human running it, not in the agent env):
  `cd src && runhaskell MtgPure/Model/Object/ToObjectN/CodeGen.hs`.

The project deliberately forbids `OverlappingInstances`/`UndecidableInstances`;
the generator exists to emit the non-overlapping instance set by hand-rolled
enumeration instead.

## Why all this instead of a runtime enum?

A single `data CardType = Creature | Instant | …` with runtime checks would be a
tenth of the code — and would allow `dealDamage` to a land, targeting an object
in the wrong zone, drawing cards as a non-player, etc., only catching them at
runtime (if at all). The `OT`/`OTN`/`ZO` system pushes all of that into the type
checker. That trade — more type machinery, zero illegal-state runtime bugs — is
the project's entire thesis.

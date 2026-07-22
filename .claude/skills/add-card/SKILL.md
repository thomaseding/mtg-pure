---
name: add-card
description: Add a new Magic card (or token) definition to mtg-pure. Use whenever the user asks to add, implement, model, or encode an MTG card/token — anything that ends up as a new binding in src/MtgPure/Cards.hs. Handles the DSL body plus the three-place wiring (export, AllCards, cSpell) that is easy to miss by hand.
---

# Add a card to mtg-pure

Adding one card touches **three files in a fixed way**. Miss any and the build
breaks (`-Wall -Werror`, `-Wmissing-export-lists`) or the card silently never
appears in `allCards`. Do all of them.

Read [CODING_STYLE.md](../../../CODING_STYLE.md) §"The card DSL" first if you
haven't this session. Do **not** run the build — see memory `mtg-pure-no-builds`.

## Step 1 — Find a template card

Avoid writing a card from a blank page if a card exists that is suitable as a starting point. Open [src/MtgPure/Cards.hs](../../../src/MtgPure/Cards.hs)
and copy the closest existing card of the **same shape**, then edit it:

| Card shape | Copy from | Type |
| --- | --- | --- |
| Vanilla creature | `grizzlyBears` | `Card OTNCreature` |
| Creature w/ activated ability | `llanowarElves` | `Card OTNCreature` |
| Creature w/ static keyword | look for `static \_this -> Flying` | |
| Burn instant (targets creature/player/pw) | `lightningBolt` | `Card OTNInstant` |
| Burn sorcery | `lavaAxe` | `Card OTNSorcery` |
| Counterspell | `counterspell` | `Card OTNInstant` |
| Aura enchantment | `holyStrength` | `Card OTNEnchantment` |
| Mana artifact | `blackLotus` / a `mox*` | `Card OTNArtifact` |
| Artifact creature | `icehideGolem` | `Card OTNArtifactCreature` |
| Basic land | `island` (via `mkBasicLand`) | `Card OTNLand` |
| Tap/dual land | the `mk*Land` helpers near the top | `Card OTNLand` |
| Token | `birdToken` / `soldierToken` | `Token OTN...` |

## Step 2 — Write the binding in `src/MtgPure/Cards.hs`

- One top-level binding, `camelCase` name matching the card
  (`lightningBolt`, `wear_tear` for split cards).
- Explicit type signature: `foo :: Card OTN<Type>`.
- The literal string name (`Card "Lightning Bolt" $ ...`) is the real card name.
- Build the body with the **combinator DSL** already imported at the top of the
  file (`toColors`, `manaCost`, `tapCost`, `effect`, `controllerOf`, `masked`,
  `static`, `activated`, `dealDamage`, …) — not raw constructors where a
  combinator exists.
- Underscore-prefixed record fields per characteristic: `creature_*`,
  `instant_*`, `sorcery_*`, `enchantment_*`, `artifact_*`, `land_*`.
- Colorless is `toColors ()`; mana costs use tuples like `manaCost (1, G)` or a
  single symbol `manaCost R`; free is `noCost` (tokens) / `manaCost 0`.
- Rely on `-Wno-type-defaults`: do **not** annotate `Integer`/`Int` on
  `Power 2`, `manaCost 3`, damage amounts, etc.
- If a constructor/combinator you need isn't imported yet, add it to the
  relevant `import safe ... (...)` block near the top **alphabetically** (leading
  commas). Prefer an existing combinator in
  [MtgPure.Model.Combinators](../../../src/MtgPure/Model/Combinators.hs); only
  reach into `Recursive` constructors when no combinator fits.

## Step 3 — Export it (same file)

Add the binding name to the module export list at the top of `Cards.hs`,
**keeping it alphabetized** with leading commas. Tokens go in the small
`--` separated group at the bottom of the list (with `birdToken`, `soldierToken`).

## Step 4 — Register in `src/MtgPure/AllCards.hs`

Two edits, both alphabetized:

1. Add the name to the `import safe MtgPure.Cards ( ... )` list.
2. Add `, toCard <name>` to the `allCards` list (or `, toToken <name>` to
   `allTokens` for a token).

A card that compiles but isn't in `allCards` won't show up in the gallery,
`allCards`, or round-trip tests — this step is the easy one to forget.

## Step 5 — Spelling

Any **new** word in the card name (or a new keyword/type you introduced) that
isn't already a real English/software word must be added to the right
[.cspell](../../../.cspell/) dictionary — card-name and Magic words go in
[.cspell/mtg.dict](../../../.cspell/mtg.dict), kept in the file's existing
(roughly alphabetical, lowercase-friendly) order. Multi-word names: only the
unusual tokens need entries (e.g. "Llanowar" but not "Elves").

## Step 6 — Hand-format

The user's fourmolu isn't runnable here, so match its output by hand: 2-space
indent, leading commas, no space inside record braces
(`Foo{field = x}`), one blank line between top-level bindings. Mirror the
template card's layout exactly.

## Final check

- [ ] Binding + explicit type in `Cards.hs`
- [ ] Name in `Cards.hs` export list (alphabetized)
- [ ] Name in `AllCards.hs` import list (alphabetized)
- [ ] `toCard`/`toToken` entry in `allCards`/`allTokens`
- [ ] New name-words added to `.cspell/mtg.dict`
- [ ] Any newly-used combinators/constructors imported
- [ ] Unimplemented corners use `undefined` with a `-- TODO: [Card Name]` note,
      never `error "..."` (see CODING_STYLE.md)

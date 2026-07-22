# Glossary & naming cheat-sheet

Fast lookup for the abbreviations, prefixes, and terms that saturate this
codebase. For the *why*, follow the links.

## Type-level object system

| Term | Meaning |
| --- | --- |
| `OT` | Object-type tag kind. Promoted tags: `'OTCreature`, `'OTLand`, `'OTPlayer`, … See [type-system.md](type-system.md). |
| `OTN` | A *set* of `OT` tags as a `Type`: `OTN '[ 'OTCreature ]`. "N of them." |
| `OTN*` alias | Named `OTN`s: `OTNCreature`, `OTNInstant`, `OTNPermanent`, `OTNCreaturePlayerPlaneswalker` ("any target"). |
| `OT1`/`OT2`/`OT5` | Arity-specific `OTN`s (1, 2, 5 tags). Conversions among them are the generated `ToObjectN` tree. |
| `Zone` | A game zone: `'ZBattlefield`, `'ZExile`, `'ZGraveyard`, `'ZHand`, `'ZLibrary`, `'ZStack`. |
| `ZO` / `ZoneObject` | An `OTN` located in a `Zone`: `ZO 'ZBattlefield OTNCreature`. Alias `ZO = ZoneObject`. |
| `ZO*` alias | `ZOPlayer`, `ZOCreature`, `ZOAny`, `ZOPermanent`, … |
| `Object` | A runtime object: identity + tag, e.g. `Object 'OTPlayer`. |
| `ObjectId` | The bare identity of an object. |
| `Sing*` | Runtime singleton witnesses bridging a type-level tag to a value to `case` on (`SingOT`, `SingZBattlefield`). |
| `Co*` (`CoPermanent`, `CoCard`, `CoSpell`, `CoAny`) | Membership/coercion witnesses: "this object set is a legal permanent/card/spell/…". |
| `Is*` (`IsOTN`, `IsZO`, `IsObjectType`, `IsSpecificCard`) | Well-formedness constraints on object/zoned-object types. |
| `Inst2` / `Inst5` | Apply a constraint to 2 / 5 type parameters at once. |

## Card DSL

| Term | Meaning |
| --- | --- |
| `Card ot` | A single card, `Card name electedCharacteristic`. Also `DoubleSidedCard`, `SplitCard`. |
| `AnyCard` | Existential card wrapper (`AnyCard1` single-faced, `AnyCard2` double). Decks hold these. |
| `Token` / `AnyToken` | Token analogue of `Card` / `AnyCard`. |
| `CardCharacteristic ot` | Printed characteristics; one ctor per card type (`InstantCharacteristic`, `CreatureCharacteristic`, …). |
| `CardSpec ot` | The spec half: cost + abilities + type payload (`InstantSpec`, `CreatureSpec`, …). |
| `SpecificCard ot` | Witness enumerating card types (`InstantCard`, `LandCard`, …). |
| `Elect s el ot` | The election structure: a value `el` reached after choices, at stage `s`. The DSL's spine. |
| `ElectStage` | `'IntrinsicStage` → `'TargetStage` → `'ResolveStage`; orders when each choice is legal. |
| `Effect ef` | An effect; `ef ∈ { 'OneShot, 'Continuous }`. `DealDamage`, `AddMana`, `CounterSpell`, … |
| `Cost` | `ManaCost`, `TapCost`, `SacrificeCost`, `PayLife`, `AndCosts`/`OrCosts`, … |
| `Requirement zone ot` | Predicate on a zoned object (`ControlledBy`, `IsTapped`, `HasLandType`, `Not`, `RAnd`, …). |
| `Condition` | Boolean over game state (`Satisfies`, `CAnd`, `COr`, `CNot`). |
| `Ability zone ot` | `Activated` / `Static` / `Triggered`. |
| `EventListener'` | Trigger sources: `EntersBattlefield`, `BecomesTapped`, `SpellIsCast`, `TimePoint`, … |
| combinator | A card-authoring helper in [`Combinators.hs`](../src/MtgPure/Model/Combinators.hs) (`toColors`, `manaCost`, `effect`, `masked`, `dealDamage`). Prefer over raw ctors in cards. |
| round-trip | The invariant that a card `Show`s as the source that builds it; upheld by hand-written `Show`/`Ord`/`ConsIndex`. |

## Engine

| Term | Meaning |
| --- | --- |
| `Magic v rw m a` | The engine monad. `v` = visibility, `rw` = read/write, `m` = base monad. |
| `MagicCont` | Continuation variant for early-bailing control flow. |
| `Visibility` | `'Public` / `'Private` — hidden-info access level. |
| `ReadWrite` | `'RO` / `'RW` — may mutate `GameState`. |
| capability coercions | `fromRO`, `fromPublic`, `internalFromPrivate` — the *only* sanctioned way to change `v`/`rw`. |
| `GameState m` | The whole mutable world. `OpaqueGameState` = read-only view for prompts. |
| `GameInput` / `GameResult` | Start config / outcome of a game. |
| `Prompt'` | Record of player-decision callbacks the engine calls; supplied by the client. |
| `logCall 'fn` | Mandatory first thing in an engine fn; logs its real name (via `TemplateHaskellQuotes`). |
| `Fwd` trio | `Fwd/{Type,Api,Impl}` — indirection that breaks engine import cycles. ~100 ops. |
| `InternalLogicError` | GADT for provably-unreachable sites; add a *descriptive* ctor instead of bare `undefined`. |
| `Legality` | Legal/illegal result of an attempted action. |
| SBA | State-based actions (`StateBasedActions.hs`). |

## Value-naming prefixes (from CODING_STYLE.md)

| Prefix / form | Meaning | Example |
| --- | --- | --- |
| `camelCase` | values & functions | `playLand`, `natToInt` |
| `PascalCase` | types & constructors | `Battle`, `PlayLandReqs` |
| `type_field` | record field, tagged by its type | `creature_power`, `instant_cost` |
| `un<Name>` | newtype accessor | `unCardName`, `unPlayerCount` |
| `o…` | an `Object`-typed value | `oPlayer`, `oLand`, `oActive` |
| `zo…` | a `ZO` zone-object value | `zoLand` |
| `m…` | a `Maybe` value | `mCard` |
| `'Foo` | promoted (type-level) constructor | `'Private`, `'RO`, `'ZBattlefield` |
| `Type_Case` | pattern synonym | `PlayLandReqs_Satisfied` |
| conventional qualified aliases | `M`, `List`, `NonEmpty`, `Map`, `Set`, `Stream`, `Char`, `GHC` | — |

## House-style shorthands

| Thing | Rule |
| --- | --- |
| `undefined` | Unimplemented-yet path; GHC prints file+line. Pair with `-- TODO: [Card]`. |
| `InternalLogicError` | Impossible-by-logic path. Prefer over `undefined`; name a specific ctor. |
| `-- (509.1a)` | Comment citing the MTG comprehensive rule a line implements. Multiple rules are space-separated, each parenthesized: `-- (117.1b) (117.1d)`. Rule citation precedes a `TODO` on the same line: `-- (103.5) TODO: leylines and such`. See CODING_STYLE.md §engine. |
| `-- TODO: [Card Name]` | Future work named by the card/feature that motivates it. |
| `import safe` | Every import; preserves the `Safe` guarantee. |
| `InstanceSigs` | Restate each method's type signature inside the instance. |
| `\case` | Preferred for match-on-last-arg functions. |

# Coding Style

This document describes the conventions and idiosyncrasies used throughout
`mtg-pure`. It is descriptive: it captures what the existing code already does,
so that new code blends in. When in doubt, open a neighboring file and mimic it.

The overriding goals are the same as the model's: **type-safety, purity, and
elegance**. Prefer making illegal states unrepresentable at the type level over
runtime checks.

---

## Formatting

Formatting is mechanical. Run [`fourmolu`](https://github.com/fourmolu/fourmolu)
and let it settle every question of whitespace. The config lives at
[`src/fourmolu.yaml`](src/fourmolu.yaml):

```yaml
indentation: 2
comma-style: leading
haddock-style: single-line
import-export-style: diff-friendly
indent-wheres: false
record-brace-space: false
newlines-between-decls: 1
respectful: false
```

Consequences you will see everywhere, and must reproduce by hand when writing
before running the formatter:

- **2-space indentation.** No tabs.
- **Leading commas** in lists, records, export lists, and multi-line import
  lists.
- **One blank line between top-level declarations** (never zero, never two).
- **No space inside record braces at the type level:** `PlayLandReqs{playLandReqs_hasPriority = False}`, `Record{field}`.
- **`where` is not extra-indented** relative to its bindings (`indent-wheres: false`).
- Single-line Haddock (`-- |`) style, not block Haddock.

Because the formatter is authoritative, do not fight it with manual alignment;
if something looks awkward, that is fourmolu's opinion, and it is the one we
keep.

---

## Module header

Every module opens with the same fixed block, in this order:

```haskell
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.CardName (
  CardName (..),
  HasCardName (..),
) where
```

Rules:

1. **`{-# LANGUAGE Safe #-}` first** for model and engine modules. `Safe` is a
   deliberate design goal (see the README). Only UI / interop modules that must
   touch unsafe libraries omit it.
2. **`{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}`** always follows, so the
   HLINT pragmas below don't trip `-Werror`.
3. A **block of `{-# HLINT ignore ... #-}` pragmas**, one per line, alphabetized.
   These are a house style, not clutter: this codebase *prefers* explicit
   lambdas and `case`/`if` spelled out, and *rejects* HLINT's suggestions to
   point-free-ify or collapse them. The recurring set includes:
   - `"Avoid lambda"`, `"Avoid lambda using \`infix\`"`
   - `"Use const"`, `"Use if"`, `"Use camelCase"`
   - `"Replace case with fromMaybe"`, `"Replace case with maybe"`
   - `"Redundant pure"`
   Copy the subset that applies to the file. When unsure, copy the full set from
   a similar module.
4. An **explicit export list is mandatory** — `-Wmissing-export-lists` is on and
   warnings are errors. Use `(..)` to export a type with all its constructors /
   fields.

Additional per-file language pragmas (e.g. `{-# LANGUAGE ExtendedDefaultRules #-}`
in [`src/MtgPure/Cards.hs`](src/MtgPure/Cards.hs)) go alongside `Safe` at the top.

---

## Imports

- Use **`import safe`** for every import (and `import safe qualified`). This
  preserves the `Safe` guarantee.
- Imports are **alphabetized** and fourmolu-formatted with leading commas on
  multi-line explicit lists:

  ```haskell
  import safe MtgPure.Engine.Fwd.Api (
    getActivePlayer,
    getHasPriority,
    getPlayer,
    putOntoBattlefield,
   )
  ```

- **Qualified-import aliases are conventional.** Reuse the established short
  names rather than inventing new ones:

  | Module                    | Alias      |
  | ------------------------- | ---------- |
  | `Control.Monad`           | `M`        |
  | `Control.Monad.Trans`     | `M`        |
  | `Data.List`               | `List`     |
  | `Data.List.NonEmpty`      | `NonEmpty` |
  | `Data.Map.Strict`         | `Map`      |
  | `Data.Set`                | `Set`      |
  | `Data.Stream`             | `Stream`   |
  | `Data.Char`               | `Char`     |
  | `GHC.TypeLits`            | `GHC`      |

- Prefer explicit import lists over open imports. Import orphan-instance modules
  for their instances with an empty list: `import safe MtgPure.Engine.Orphans ()`.

---

## Naming

- **Values and functions:** `camelCase` (`playLand`, `getPlayLandReqs`, `natToInt`).
- **Types and constructors:** `PascalCase` (`Battle`, `CardName`, `PlayLandReqs`).
- **Record fields are prefixed** with a per-type lowercase tag and an underscore.
  This is a hard convention, chosen so field names never collide across the many
  record types:

  ```haskell
  data PlayLandReqs = PlayLandReqs
    { playLandReqs_hasPriority :: Bool
    , playLandReqs_isActive :: Bool
    , ...
    }
  ```

  Card characteristics follow the same rule: `creature_colors`, `creature_power`,
  `instant_cost`, `sorcery_effect`.
- **Newtype accessors** use the `un` prefix: `unCardName`.
- **Object-typed value names** carry a terse type hint prefix: `oPlayer`,
  `oLand`, `oActive` for `Object`s; `zoLand` for `ZO` zone-objects. `m`-prefixed
  names are `Maybe` (`mCard`).
- **Promoted (type-level) constructors take the leading tick:** `'Private`,
  `'RO`, `'Z`, `'OTPlayer`, `'ZBattlefield`. Kind-level `Object` type synonyms
  read `OT...` / `OTN...` (e.g. `OTNLand`, `OTNCreature`).
- **Pattern synonyms** are named with a `Type_Case` shape, e.g.
  `PlayLandReqs_Satisfied`. (Note: pattern synonyms don't contribute to
  exhaustiveness checking — this is called out in a comment where it matters.)

---

## Data declarations

- **Use GADT syntax for `data` declarations**, even for simple records. This is
  the house style throughout the model:

  ```haskell
  data Battle :: Type where
    Battle ::
      { battleTypes :: [BattleType]
      } ->
      Battle
    deriving (Typeable)
  ```

- **Always derive `Typeable`.** It is relied upon pervasively for the DSL's
  introspection.
- Use **standalone `deriving instance`** for instances that need context or that
  the inline `deriving` clause can't express:

  ```haskell
  deriving instance (Inst2 Eq user elem) => Eq (NatList user n elem)
  ```

- Fields are strict by default via `StrictData` (enabled globally) — don't add
  `!` unless you have a specific reason.

---

## Instances

`InstanceSigs` is on and used consistently: **repeat the method's type signature
inside every instance.** This is non-optional house style:

```haskell
instance Show CardName where
  show :: CardName -> String
  show = show . unCardName
```

For class methods with class constraints, restate them too (see `IsNat` in
[`src/Data/Nat.hs`](src/Data/Nat.hs)).

---

## Expression style

- **Prefer `\case`** (LambdaCase) for functions that immediately pattern-match
  their last argument:

  ```haskell
  natToInt :: Nat -> Int
  natToInt = \case
    Z -> 0
    S n -> 1 + natToInt n
  ```

- **Explicit lambdas and `case`/`if` are preferred over point-free cleverness.**
  This is *why* the HLINT pragmas above are disabled — do not "simplify" a lambda
  into `const`, or a `case` into `fromMaybe`, just because HLINT suggests it.
- **`BlockArguments`** is on: write `logCall 'foo do ...` and `pure Foo{...}`
  without a `$` before `do`/records where the formatter allows it.
- Use `assert` from `Control.Exception` to document and check invariants that the
  types can't capture:

  ```haskell
  FZ -> assert (i == 0) case input == i of ...
  ```

---

## The card DSL (`MtgPure.Cards`)

Card definitions are the showcase. Each is a top-level binding with an explicit
type and a nested record-construction body:

```haskell
lightningBolt :: Card OTNInstant
lightningBolt = Card "Lightning Bolt" $
  ...

birdsOfParadise :: Card OTNCreature
birdsOfParadise =
  Card "Birds of Paradise" $
    ElectCardFacet
      CreatureCharacteristic
        { creature_colors = toColors G
        , creature_creatureTypes = [Bird]
        , creature_power = Power 0
        , creature_toughness = Toughness 1
        , creature_spec =
            CreatureSpec
              { creature_cost = manaCost G
              , creature_abilities = [ ... ]
              }
        }
```

Conventions:

- One binding per card, `camelCase` name matching the card, exported from the
  module's export list (kept alphabetized).
- Build with the **combinator DSL** (`toColors`, `manaCost`, `tapCost [is this]`,
  `effect`, `controllerOf`, `activated`, `static`, `Target`, `masked`) rather
  than raw constructors where a combinator exists.
- Prefixed record fields as above (`creature_*`, `instant_*`, `sorcery_*`).
- `-Wno-type-defaults` is deliberately set so numeric literals like `Power 0` and
  `manaCost (2, B, B)` don't require type annotations. Rely on it; don't annotate
  `Integer`/`Int` noise into card definitions.

---

## The engine

- **Access-control is encoded in phantom types.** Engine actions run in
  `Magic 'Visibility 'ReadWrite m a`, e.g. `Magic 'Private 'RO m PlayLandReqs`.
  Cross between capability levels only through the provided wrappers — `fromRO`,
  `fromPublic`, `fromPublic`, `internalFromPrivate` — never by hand.
- **Begin engine functions with `logCall`**, using `TemplateHaskellQuotes` name
  quoting so the log carries the function's real name:

  ```haskell
  playLand oPlayer (PlayLand oLand) = logCall 'playLand do
    ...
  ```

- **The `Fwd` indirection.** Cross-module engine calls go through the
  `Fwd/Api`, `Fwd/Impl`, `Fwd/Type` trio to break import cycles. Add a new
  cross-cutting engine operation by extending all three, following the existing
  entries.
- **Cite the comprehensive rules** in comments where behavior implements a
  specific rule, using the rule number in **parentheses**, one pair of
  parentheses per rule:

  ```haskell
  , playLandReqs_isActive = oPlayer == oActive -- (116.2a)
  , playLandReqs_atMaxLands = landsPlayed >= maxLands -- (305.2)
  ```

  Conventions for these citations:

  - **The rule number is bare inside the parens** — no `CR`/`rule` prefix, no
    trailing period: `-- (103.5)`, `-- (116.2a)`. Subrule letters are lowercase
    (`117.1b`).
  - **List multiple rules space-separated, each in its own parens**, when a line
    implements or is justified by several rules. Order them by relevance to the
    line (most-directly-implemented first), not necessarily numerically:

    ```haskell
    ActivateAbility{} -> tryAgainOnFail $ activateAbilityCont oPlayer x -- (117.1b) (117.1d)
    -- (104.1) (727.1)
    -- (117.4) (405.5)
    ```

  - **Combine with a `TODO`** by putting the rule citation(s) first, then the
    `TODO`, on the same comment. This flags a rule that is only partially
    implemented (or stubbed) and names what is missing:

    ```haskell
    pure () -- (103.5) TODO: leylines and such
    pure () -- (103.6) TODO: planechase stuff
    ```

  - A citation may stand **on its own comment line** above the code it annotates,
    or **trail the line** — both are used; trail short expressions, hoist to the
    line above when the expression is already long.

- **`TODO` markers name the card or feature** that motivates the future work:
  `-- TODO: [Crucible of Worlds]`.

### Unimplemented code: prefer `undefined`

For code paths that are not yet implemented, **use `undefined`** — not a custom
placeholder, not `error "..."` with a hand-written message. This is the
preferred stub throughout the engine:

```haskell
SingZBattlefield -> invalid undefined
SingZExile -> invalid undefined -- TODO: [Misthollow Griffin]
SingZLibrary -> invalid undefined -- TODO: [Panglacial Wurm]
...
ArtifactLandSpec{} -> undefined -- TODO: Not a spell
```

Why `undefined` specifically: when it is evaluated, GHC automatically prints
file and line information associated with it.

Conventions around it:

- Pair it with a **`-- TODO`** comment naming the card or reason where one is
  useful: `-- TODO: [Gravecrawler]`, `-- TODO: singZone`,
  `-- TODO: prompt user for reified value`.
- A bare `undefined` (no comment) is acceptable for exhaustiveness stubs — a
  `\case` arm that isn't reachable yet or isn't implemented — but a short note
  (`-- should not be possible`, `-- cant happen?`) is preferred when the reason
  isn't obvious.
- Prefer exhaustive `case` over partial matches; `-Wincomplete-uni-patterns` and
  `-Wincomplete-record-updates` are on and fatal.

### Impossible code: prefer `InternalLogicError` over `undefined`

`undefined` is for code that *isn't written yet*. For code that *can never be
reached* — states the types permit but the game logic forbids — use the
`InternalLogicError` type in
[`src/MtgPure/Engine/Prompt.hs`](src/MtgPure/Engine/Prompt.hs) instead of a bare
`undefined`. Each constructor names *why* the site is unreachable, and its
`Show` instance yields an informative message if the "impossible" ever happens.

**Add a new, descriptively-named constructor** for your site rather than reusing
a generic one, then raise it (`error $ show YourNewConstructor`). The existing
`CantHappenByConstruction` is a generic placeholder — acceptable as a stopgap,
but prefer an informative constructor, and replace `CantHappenByConstruction`
with one when you touch a site that uses it.

---

## Warnings, extensions, and the build

- **Warnings are errors.** `ghc-options` includes
  `-Wall -Werror -Wincomplete-uni-patterns -Wincomplete-record-updates
  -Wcpp-undef -Wmissing-export-lists`. Code must compile clean.
- **Language extensions are declared once, project-wide**, in the `cabal`
  file's `default-extensions`. Enable a new extension by adding it there
  (alphabetized), not with a per-file `{-# LANGUAGE #-}`, unless the extension is
  genuinely local to one module (e.g. `ExtendedDefaultRules`, which lives in
  `other-extensions` and is turned on per-file).
- The project targets `Haskell2010` with the extension set above. Notable
  always-on extensions include `GADTs`, `DataKinds`, `PolyKinds`,
  `TypeFamilyDependencies`, `PatternSynonyms`, `StrictData`, `InstanceSigs`,
  `LambdaCase`, `BlockArguments`, `TypeApplications`, `ScopedTypeVariables`.
- **`TemplateHaskell` is banned; `TemplateHaskellQuotes` is allowed** (used only
  for `'name` quoting in `logCall`). The following are forbidden `IncoherentInstances`,
  `OverlappingInstances`, and `UndecidableInstances` — the model is designed to
  not need them.

---

## Generated code

Some files are produced by a generator and are **git-ignored — never hand-edit**
them:

- `src/MtgPure/Model/Object/ToObjectN/Instances.hs` and the
  `.../Instances/` directory.

Regenerate with:

```
$ cabal install fourmolu
$ cd src
$ runhaskell MtgPure/Model/Object/ToObjectN/CodeGen.hs
```

If you need to change the generated output, change the generator
(`CodeGen.hs`), keep its output fourmolu-clean, and re-run it.

---

## Spelling

The repo uses cSpell with per-domain custom dictionaries under
[`.cspell/`](.cspell/) (`haskell`, `math`, `mtg`, `software`, `vernacular`,
etc.). When you introduce a legitimate new term (a card name, a Magic keyword, a
domain word), add it to the appropriate dictionary rather than leaving a spelling
warning.

---

## Quick checklist for a new module

1. `Safe` + `-Wno-unrecognised-pragmas` + the HLINT-ignore block, in that order.
2. Explicit, alphabetized export list.
3. `import safe [qualified] ...` with conventional aliases.
4. GADT-syntax data decls; derive `Typeable`; underscore-prefixed record fields.
5. `InstanceSigs` on every instance method.
6. `\case`, explicit lambdas, no HLINT-driven point-free rewrites.
7. Run `fourmolu`; build with `-Wall -Werror` clean.

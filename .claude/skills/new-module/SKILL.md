---
name: new-module
description: Scaffold a new Haskell module in mtg-pure that conforms to the strict house style (Safe header, HLINT-ignore block, mandatory export list, `import safe`, GADT-syntax data decls, InstanceSigs). Use when adding any new .hs module under src/, or when an existing module's header/imports need to be brought in line with the project conventions.
---

# New module in mtg-pure

The project builds with `-Wall -Werror -Wmissing-export-lists` and treats the
header shape as house style. A new module that doesn't follow this won't compile
or won't blend in. Full rationale is in
[CODING_STYLE.md](../../../CODING_STYLE.md); this is the actionable recipe.

Do **not** run the build to check — see memory `mtg-pure-no-builds`. Match the
conventions by hand and let the user compile.

## Header template

```haskell
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Foo (
  Foo (..),
  HasFoo (..),
) where
```

Order is fixed:

1. `{-# LANGUAGE Safe #-}` first — mandatory for model/engine modules. Only
   UI/interop modules that must touch unsafe libraries use `{-# LANGUAGE Unsafe #-}`
   or omit it.
2. `{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}` so the HLINT pragmas below
   don't trip `-Werror`.
3. Alphabetized block of `{-# HLINT ignore ... #-}` — copy the subset that
   applies from a neighboring module. This codebase *keeps* explicit lambdas and
   `case`/`if`; the pragmas suppress HLINT's point-free suggestions. Common set:
   `"Avoid lambda"`, `"Avoid lambda using \`infix\`"`, `"Use const"`, `"Use if"`,
   `"Use camelCase"`, `"Replace case with fromMaybe"`,
   `"Replace case with maybe"`, `"Redundant pure"`.
4. Any module-local `{-# LANGUAGE ... #-}` (e.g. `ExtendedDefaultRules`) sits
   next to `Safe`. Project-wide extensions are already on via the cabal
   `default-extensions` — don't re-declare those per file.
5. **Explicit export list is mandatory.** Use `(..)` to export a type with all
   its constructors/fields.

## Imports

- Use `import safe` (and `import safe qualified`) for every import to preserve
  the `Safe` guarantee.
- Alphabetize; leading commas on multi-line explicit lists.
- Reuse the conventional qualified aliases: `M` (Control.Monad / .Trans),
  `List`, `NonEmpty`, `Map` (Data.Map.Strict), `Set`, `Stream`, `Char`, `GHC`
  (GHC.TypeLits).
- Import orphan-instance modules for effect with an empty list:
  `import safe MtgPure.Engine.Orphans ()`.

## Body conventions

- **GADT syntax for `data` declarations**, even simple records; `deriving (Typeable)`.
- Underscore-prefixed, per-type record fields (`foo_bar`), so names never
  collide across record types.
- **`InstanceSigs`: repeat the method signature inside every instance**, e.g.

  ```haskell
  instance Show Foo where
    show :: Foo -> String
    show = show . unFoo
  ```

- Prefer `\case`, explicit lambdas, and spelled-out `case`/`if` over point-free.
- `newtype` accessors use the `un` prefix (`unFoo`). Fields are strict by
  default (`StrictData`) — no manual `!`.
- Unimplemented paths: `undefined` (not `error "..."`), ideally with a
  `-- TODO: [reason]`. Truly-impossible states: an `InternalLogicError`
  constructor (see [Prompt.hs](../../../src/MtgPure/Engine/Prompt.hs)), not a
  bare `undefined`.
- Engine functions start with `logCall 'funcName do ...` (TemplateHaskellQuotes
  name quoting).

## Register the module

Add the module name to `exposed-modules` in
[mtg-pure.cabal](../../../mtg-pure.cabal), keeping that list alphabetized. GHC
won't see the file otherwise. `LoadAllModules.hs` is generated
(via `Script.GenerateLoadAllModules`) — don't hand-edit it.

## Checklist

- [ ] `Safe` + `-Wno-unrecognised-pragmas` + alphabetized HLINT block
- [ ] Explicit, alphabetized export list (`(..)` for full types)
- [ ] `import safe [qualified]` with conventional aliases
- [ ] GADT-syntax data decls; `deriving (Typeable)`; underscore fields
- [ ] `InstanceSigs` on every instance method
- [ ] Module added to `exposed-modules` in the cabal file (alphabetized)
- [ ] Hand-matched fourmolu layout (2-space, leading commas, `Foo{field}`)

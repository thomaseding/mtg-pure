---
name: update-cabal-toolchain
description: Update mtg-pure's cabal dependency bounds and source to build on a newer GHC/toolchain than it was pinned to. Use when cabal build fails with dependency version-bound conflicts, "unknown package", Cabal-8011 configure errors, or newly-fatal GHC warnings after a compiler upgrade. Covers bumping build-depends to the boot-library versions and the recurring GHC-9.10 / base-4.20 source fixes.
---

# Update mtg-pure for a newer toolchain

The cabal file was originally pinned to a GHC 9.4-era world (`base ^>=4.17`).
When the installed GHC is newer (currently **GHC 9.10.3 / base 4.20**), the
solver and `-Wall -Werror` reject the old pins and stricter warnings. This skill
is the migration playbook. Build via the **`build`** skill; those environment
presteps (`cabal update`, `sh` on PATH for `configure`, regenerating `ToObjectN`)
are assumed here.

## 1. Bump `build-depends` bounds to the active GHC's boot libraries

The tight `^>=` upper caps block the versions that ship with the new GHC. Query
the actual versions and set the caret lower bound to match, **keeping the caret
style** (do not convert to bare `>=` — the author prefers bounded deps):

```powershell
$env:Path = "C:\ghcup\bin;" + $env:Path
foreach ($p in 'base','array','binary','bytestring','containers','filepath',
                'mtl','template-haskell','time','transformers') { ghc-pkg latest $p }
```

Then edit `ghc-options`'s neighbor block in
[mtg-pure.cabal](../../../mtg-pure.cabal). For GHC 9.10 the known-good set is:

| Package | Bound |
| --- | --- |
| `base` | `^>=4.20` |
| `array` | `^>=0.5.8.0` |
| `binary` | `^>=0.8.9.0` |
| `bytestring` | `^>=0.12.2.0` |
| `containers` | `^>=0.7` |
| `filepath` | `^>=1.5.4.0` |
| `mtl` | `^>=2.3.1` |
| `template-haskell` | `^>=2.22.0.0` |
| `time` | `^>=1.12.2` |
| `transformers` | `^>=0.6.1.1` |

Non-boot Hackage deps whose old caps also blocked 9.10: `aeson ^>=2.2`,
`ansi-terminal ^>=1.1`. Everything else (`async`, `colour`, `directory`,
`dlist`, `http-conduit`, `JuicyPixels`, `parsec`, `process`, `random`, `split`,
`Stream`, `vector`, …) already allowed 9.10-era versions — leave those alone.
Also drop any accidental duplicate lines (there was a repeated `parsec`).

If `cabal build` says **`unknown package: <x>`**, the Hackage index is empty →
`cabal update` (this is an environment issue, not a bounds issue).

## 2. Fix new build errors (if applicable). Do not try to hide the errors.

Fix, rebuild, read the next error, repeat — GHC surfaces a handful at a time
because the build aborts modules in parallel.

## 3. Regenerate generated code if the object machinery changed

If you touched the `OT`/`ObjectN` types (or the generated tree is missing/stale),
re-run the generator — see the **`regen-object-code`** skill. A boot-only
`runghc CodeGen.hs` from `src/` is enough; fourmolu is optional.

## Gotchas

- **`cabal repl --repl-options=-fobject-code` recompiles everything** with
  `[Flags changed]` if those flags differ from the build. For a plain REPL use
  `cabal repl` with no extra flags to reuse the build artifacts.
- Log with `Out-File -Encoding utf8`; the PowerShell default UTF-16 reads back as
  spaced-out garbage.
- After a successful migration, update the stale "no toolchain" claims in
  `docs/` (working-without-builds.md, README, orientation) and the
  `mtg-pure-no-builds` memory if they haven't been already.

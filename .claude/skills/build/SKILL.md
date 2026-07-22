---
name: build
description: Build the mtg-pure Haskell project with cabal/GHC. Use whenever you need to compile the project, verify a change type-checks, do a clean build, or diagnose a build failure. Covers the PowerShell/Windows setup and the ToObjectN code-generation prestep (which you should NOT run proactively — it forces a long recompile).
---

# Build mtg-pure

Toolchain is installed: GHC 9.10.3 + cabal 3.16 in `C:\ghcup\bin`.

## Use PowerShell (not Git Bash)

Run build commands with the **PowerShell** tool. Not for caching reasons — both
shells reuse the compiled `.hi`/`.o` cache equally (see below). PowerShell wins
on everything *around* the build: Git Bash mangles Windows paths (`/c/...`), sets
`HOME`, and reorders PATH, which bites the `sh.exe`/`configure` steps, `runghc`
codegen, and log encoding.

## Build

```powershell
cabal build
```

- **First build** compiles the whole dep tree + generated `ToObjectN` modules —
  many minutes. Run it in the background and check the log.
- **Incremental rebuilds are very fast.** cabal reuses cached `.hi`/`.o` object
  code. A message like `mtg-pure ... (configuration changed)` is **not** a
  recompile — it's just a seconds-long *reconfigure* (rewrites `setup-config` /
  `plan.json`); your compiled objects are reused untouched. Editing a file
  ("touch") only recompiles the modules that actually changed.
- The `configuration changed` fingerprint is per-environment, so it ping-pongs
  between a tool shell and an interactive cmd.exe. Harmless — each side just pays
  one reconfigure, never a recompile.

## Presteps (only if they fail)

1. **`ToObjectN` code — assume it is already up to date; do NOT regenerate
   proactively.** Files under `src/MtgPure/Model/Object/ToObjectN/Instances*` are
   git-ignored + generated. Their *content* changes only when the `ObjectN`/`OT`
   machinery changes (rare), but regenerating **rewrites ~130 files' timestamps**,
   which forces cabal to recompile every one of them — many minutes — even though
   nothing actually changed. So leave them alone by default.

   Only regenerate when an **independent build error clearly requires it**: the
   generated files are missing entirely (`Instances.hs` absent → build fails), or
   a build error points at a stale/missing `ToObject` instance after you changed
   the `ObjectN`/`OT` types (see **`regen-object-code`**). Then:

   ```powershell
   New-Item -ItemType Directory -Force src\MtgPure\Model\Object\ToObjectN\Instances | Out-Null
   Set-Location src; runghc MtgPure/Model/Object/ToObjectN/CodeGen.hs; Set-Location ..
   ```

2. **`cabal update`** once if the solver says `unknown package: ...` (empty
   Hackage index).

3. If `./configure` fails with `[Cabal-8011]` (needs Unix toolchain), put
   `sh.exe` on PATH: prepend `C:\Program Files\Git\usr\bin` to `$env:Path`.

## Diagnose failures

- `cabal build 2>&1 | Out-File -Encoding utf8 "$env:TEMP\mtg-build.log"` (utf8, or
  the log reads as garbage), then grep `error:|Failed to build|Cabal-[0-9]`.
- Dependency-version / new-warning failures → see **`update-cabal-toolchain`**.

## Run after building

REPL: `cabal repl`. Tests/demo are `main<Name>` entry points — see
**`run-tests`**.

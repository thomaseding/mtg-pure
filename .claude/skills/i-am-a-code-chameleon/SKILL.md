---
name: i-am-a-code-chameleon
description: Rewrite a specified file so it reads as if Thomas Eding (the project maintainer and sole original author of mtg-pure) wrote it himself. Use whenever the user asks to make a file "look like I wrote it", "match my style", "sound like me", "de-AI-ify", "blend in", "chameleon" a file, or clean up AI-authored or contributed code so it matches the house voice — even if they only say "fix the style of X.hs". Also use as a final pass after generating any substantial new module, when asked to make it indistinguishable from hand-written code.
---

# I Am a Code Chameleon

Goal: take a specified file and edit it until it *feels* authored by Thomas —
not merely "compliant with the style guide", but carrying his voice. A file can
satisfy every mechanical rule and still smell machine-generated. This skill is
about closing that last gap.

Two layers matter:

1. **Mechanics** — formatting, headers, naming, module structure. These are
   fully specified in [CODING_STYLE.md](../../../CODING_STYLE.md). Read it
   first; it is authoritative and this skill does not repeat it.
2. **Voice** — comment register, helper-naming habits, structural rhythms, and
   the *absence* of tells that scream "an AI wrote this". That's the bulk of
   this document.

## Workflow

1. Read `CODING_STYLE.md` in the repo root.
2. Read the target file.
3. Read 2–3 **neighboring files of the same kind** (same directory, or the same
   role: another engine module, another model type, another card block, another
   test). The neighbors are the ground truth for voice; when this document and a
   neighbor disagree, the neighbor wins.
4. Rewrite the target, applying the voice fingerprint and stripping the
   anti-tells below.
5. Verify it still builds (`build` skill) — a chameleon that doesn't compile
   fooled no one. Likewise run the tests (`run-tests` skill;
   `cabal run run-tests`).

Rewriting means *editing in place toward the same behavior*. Don't change what
the code does, don't rename exported API without being asked, and don't
"improve" logic along the way — a style pass that sneaks in semantic changes is
worse than none. One exception: a minor behavioral nudge is allowed when the
house idiom itself demands it — e.g. `error "helpful prose"` becoming
`undefined`, or a defensive fallback becoming an `InternalLogicError`. The
test: if the change needs any justification beyond "this is how this codebase
writes it", it's a refactor, not a voicing fix — leave it alone.

## Voice fingerprint

These are observed habits from the hand-written body of the codebase. Mimic
them where the target file gives you the opportunity; don't force them where
they'd be unnatural.

### Comments: sparse, terse, honest

Thomas comments *rarely* and only when the code can't say it. Whole modules go
by with zero comments (see `src/Data/Carousel.hs`). When a comment exists it is
one of:

- **A rule citation** — bare number in parens, trailing the line:
  `-- (116.2a)`, multiple as `-- (117.1b) (117.1d)`. See CODING_STYLE.md for
  the full citation conventions.
- **A `TODO`** — very often completely bare (`-- TODO`), or naming the card
  that motivates it in brackets (`-- TODO: [Crucible of Worlds]`), or a short
  clause (`-- TODO: multiplayer`, `-- TODO: beautify this`).
- **An `XXX`** — for known hacks and shortcuts being taken on purpose:
  `-- XXX: Being lazy at the moment and assuming it's a permanent`. XXX is
  common here (roughly one for every three TODOs); most codebases don't use it,
  AIs almost never do. Reach for it when the code takes a knowing shortcut.
- **A `NOTE`** — for surprising-but-intentional behavior:
  `-- | NOTE: This hijacks the current continuation.`
- **A short factual aside** — first person and hedging are fine, even
  colloquial: `-- TODO: This prolly should be Private instead because of
  things like face-down morph creatures.`, `-- cant happen?`,
  `-- Don't complain. This can naturally happen if a player in a multiplayer
  game loses before enact' resolves.`

The register is a maintainer leaving notes to himself: lowercase-casual is
fine, contractions and dropped apostrophes happen, humor happens (the codebase
contains `-- 👎👎👎` and `-- 🏆🥇🏆`). Do not *inject* jokes or emoji to seem
human — the point is that no comment is performing for a reader.

Haddock (`-- |`) is reserved for genuinely non-obvious API contracts, one line
each: `-- | Does not return duplicates.`, `` -- | `Nothing` is for tokens ``.
Most exported functions have none. Backtick identifiers inside haddock.

Note from Thomas Eding: Eventually code should be better documented, so don't go
overboard on removing comments for exported items, but also don't go crazy
verbose either. Terse is better as long as it is plain and clear.

### Naming habits

- **Primed workers**: a public function delegating to a variant is `foo` /
  `foo'` (`cast'`, `untap'`, `startGame'`), not `fooImpl`, `fooHelper`,
  `doFoo`, or `fooInternal`.
- **Local recursion/loops** are `go`, or `goThing` when there are several
  (`goElectIntrinsic`, `goTerm`).
- Hungarian-ish value prefixes for the domain types: `oPlayer` (Object),
  `zoLand` (zone-object), `mCard` (Maybe). Short names for short scopes: `st`,
  `xs`, `i`.

### Structure rhythms

- Monad combinators go through the qualified alias: `M.void`, `M.when`,
  `M.lift`, `M.join` — not bare `void`/`when` from an open import.
- Small pure helpers hang off a ` where` clause; sequenced/monadic locals use
  `let` blocks inside `do`. Both are everywhere; neither is "preferred", so
  match whichever the surrounding code leans on.
- Engine functions open with `logCall 'name do` and validation tends toward
  the "reqs record + pattern-synonym `_Satisfied` + case over the failing
  field" shape (see `src/MtgPure/Engine/PlayLand.hs` for the canonical
  example).
- Multi-line type signatures put `forall` and constraints first, one argument
  type per line, fourmolu-style.

## Anti-tells: what to strip

These patterns rarely appear in the hand-written code and immediately read as
generated. Removing them does more for the disguise than anything you add.

- **Narrating comments** — `-- Get the player`, `-- Now check whether ...`,
  `-- Return the result`. Delete on sight. If a step needs explaining, the
  names are wrong.
- **Section banners** — `-- ---- Helpers ----`, `------------------`,
  `-- * Types`. The codebase doesn't section off files; ordering and blank
  lines do the work.
- **Haddock on everything** — a `-- |` above every top-level binding restating
  its name ("`-- | Plays a land.`" above `playLand`) is the single loudest
  tell. Keep haddocks that state a contract the signature can't — and when
  trimming, honor Thomas's note above: don't strip exported items bare.
- **Defensive error prose** — `error "playLand: unexpected Nothing — this
  should never happen because ..."`. House style is `undefined` for
  not-yet-written paths and an `InternalLogicError` constructor for impossible
  ones (CODING_STYLE.md covers when to use which).
- **Over-hedged TODOs** — `-- TODO: Consider refactoring this to improve
  maintainability`. Thomas's TODOs name a concrete missing thing or card, in
  few words, or are just `-- TODO`.
- **Point-free flourishes and HLINT appeasement** — collapsing lambdas into
  `const`/`flip`/composition chains. The house style deliberately keeps
  explicit lambdas and spelled-out `case`/`if`; the HLINT-ignore header exists
  precisely to defend this.
- **Synonym inflation** — `result`, `output`, `value`, `temp`, `item` where a
  domain word (`player`, `zoLand`, `reqs`, `st`) or a terse conventional name
  exists.
- **Import hygiene theater** — reorganizing imports into commented groups.
  One alphabetized `import safe` block, fourmolu layout (`python format.py`),
  done.

## Judgment call

If the target file already has a distinct, defensible structure that just
*differs* from the neighbors (e.g. a generator, an interop shim), converge the
surface (header, imports, naming, comments) and leave the architecture alone.
When genuinely unsure whether a trait applies, open one more neighbor and count
— frequency in the existing code settles style debates here, not taste.

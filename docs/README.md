# mtg-pure docs

Reference material for working in `mtg-pure` — a purely functional Haskell
"Magic: The Gathering" card model and rules engine. These docs are written
**primarily for AI agents**, but are equally readable by humans.

The canonical, always-loaded instructions live in
[AGENTS.md](../AGENTS.md) (aliased as `CLAUDE.md`) and
[CODING_STYLE.md](../CODING_STYLE.md). **Those two win any conflict with this
folder.** This folder exists to give you the *mental model* those files assume
you already have, plus task-oriented navigation.

## Start here

| If you want to… | Read |
| --- | --- |
| Get oriented fast / route a task to the right place | [orientation.md](orientation.md) |
| Understand how the pieces fit together | [architecture.md](architecture.md) |
| Understand the card DSL (`Elect`, `Card`, characteristics) | [model-dsl.md](model-dsl.md) |
| Understand the type-level object system (`OT`/`OTN`/`ZO`/`Zone`) | [type-system.md](type-system.md) |
| Understand the rules engine (`Magic` monad, capabilities, `Fwd`) | [engine.md](engine.md) |
| Look up a naming convention or abbreviation | [glossary.md](glossary.md) |
| Run / verify things without a Haskell toolchain | [working-without-builds.md](working-without-builds.md) |

## Skills come first

Many common tasks already have a **project skill** under
[`.claude/skills/`](../.claude/skills/). When a task matches one, invoke the
skill instead of improvising — the skills encode the multi-file wiring that is
easy to miss. Map:

| Task | Skill |
| --- | --- |
| Add a card or token | `add-card` |
| Add a card-authoring combinator | `add-combinator` |
| Add/route a cross-module engine op through `Fwd` | `add-engine-fwd` |
| Add a scripted gameplay test | `add-game-test` |
| Add/change a `Recursive.*` GADT constructor | `model-round-trip` |
| Scaffold a new module in house style | `new-module` |
| Regenerate `ToObjectN` instance code | `regen-object-code` |

See [orientation.md](orientation.md) for the full task → skill/files routing
table.

## Keeping these docs honest

These docs describe the code as of the time of writing. They deliberately avoid
copying large code blocks that will drift. When you find a discrepancy between a
doc and the source, **the source is authoritative** — fix the doc in the same
change if it is cheap to do so.

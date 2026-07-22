---
name: regen-object-code
description: Regenerate the git-ignored generated ToObjectN instance code in mtg-pure. Use when the user changes the ObjectN / OT type machinery and needs the ToObjectN.Instances files rebuilt, when a build fails on missing ToObject instances, or when someone has hand-edited a generated file (which must never be done). Explains what regenerates, how, and what to change instead of the output.
---

# Regenerate ToObjectN code

The `ToObject_NN[_MM]` instance files are **generated and git-ignored — never
hand-edit them**. They are produced from
[src/MtgPure/Model/Object/ToObjectN/CodeGen.hs](../../../src/MtgPure/Model/Object/ToObjectN/CodeGen.hs).

## What is generated

- [src/MtgPure/Model/Object/ToObjectN/Instances.hs](../../../src/MtgPure/Model/Object/ToObjectN/Instances.hs)
  (the aggregating module) and everything under
  `src/MtgPure/Model/Object/ToObjectN/Instances/` (`ToObject_01`, `ToObject_01_01`,
  …). These are the many modules already listed in `exposed-modules` in
  [mtg-pure.cabal](../../../mtg-pure.cabal).

## How to regenerate

```
$ cd src
$ runhaskell MtgPure/Model/Object/ToObjectN/CodeGen.hs
$ cd ..
$ python format.py --everything
```

`CodeGen.hs` treats `srcDir = "."`, so it must be run from inside `src/`.

## Symptoms that you need to regenerate

- Build error about a missing `ToObject`/`ToObjectN` instance after adding a new
  object type or arity.
- A generated file shows up as modified in `git status` — it should be
  git-ignored; a diff there usually means someone hand-edited it. Revert and
  regenerate instead.

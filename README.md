# mtg-pure
Purely functional Haskell "Magic: The Gathering" card model and game engine.

---

### Model goals:
- The world's best MTG encoding.
- Pure data modelling of cards as a type-safe DSL.
- Card model is a deep embedding.
- Card model is algebraic and recursive.
- Cards type-check if and only if they are valid cards. (Barring a few exceptions such as bottom(⊥) appearing in card definitions.)
- Card model is expressive enough to replicate its souce code through introspection.
- Cards can be used in arbitrary game formats without change.
- Formal changes to game rules should not require re-modelling cards. (Cards may be rewritten if they get errata.)

---

### Engine goals:
- Exhaustive and type-checked engine.
- Event-based programmatic control.
- Solitaire CLI. You control all player actions.
- Good for exploring card and game-state interactions.

---

### Quickstart:
```
$ cd src
$ ghci -hidir .output -odir .output -fobject-code -Wall -Werror -Wno-type-defaults -XDataKinds -XOverloadedStrings -XTypeApplications MtgPure
MtgPure> :i island
island :: Card OTLand   -- Defined in `MtgPure.Cards'
MtgPure> compare island island
EQ
MtgPure> print island
Card "Island" WCardLand $ thisObject @OTLand $ \_this1 -> LandDef [BasicLand Island] []
MtgPure> island == (Card "Island" WCardLand $ thisObject @OTLand $ \_this1 -> LandDef [BasicLand Island] [])
True
MtgPure> island == (Card "Island" WCardLand $ thisObject @OTLand $ \_this1 -> LandDef [BasicLand Mountain] [])
False
MtgPure> Card "WontCompile1" WCardLand $ thisObject @OTCreature $ \_this1 -> LandDef [BasicLand Island] []
<interactive>: error: * Couldn't match type 'OTCreature with 'OTLand
MtgPure> Card "WontCompile2" WCardLand $ thisObject @OTLand $ \this -> LandDef [] [effect $ DrawCards this 1]
<interactive>: error: * Couldn't match type 'OTLand with 'OTPlayer
```
Notes:
 - Setting `-Wno-type-defaults` so the `Show` instances for cards don't need to constantly specify `Num` types when `Integer` is good enough for authoring. (Too much noise adding annotations for `Integer` or even `Int` or an alias `I`.)
 - `src/MtgPure/Model/ToObjectN/Instances.hs` is a multi-megabyte file and will take a while to compile, hence the `-fobject-code` flag to cache the result. It is recommended you don't open this file with the Haskell Language Server active unless you want to max out and throttle your computer's RAM. I tried opening the file on github once too, and crashed my browser. I just crack open Vim if I want to look at it.

---

### Will not do:
- GUI
- Multiplayer
- AI

---

### Will not fix issues related to:
- Handling of integer overflow
- Memory performance
- Speed performance

---

### Haskell language options:
- Safe
- NoIncoherentInstances
- NoOverlappingInstances (todo: write a script that generates non-overlapping ToManaCost/ToManaPool instances needed to achieve this)
- NoTemplateHaskell
- NoUndecidableInstances 

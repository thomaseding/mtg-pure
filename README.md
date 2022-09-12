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
- Cards can be used in arbitrary formats without change.
- Formal changes to game rules should never require re-modelling a card. (Cards may be rewritten if they get erratta.)

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
$ ghci -hidir .output -odir .output -fobject-code -Wall -Werror -XDataKinds MtgPure
> length allCards
> :t swanSong -- Card OTInstant
> show island -- Card "Island" WCardLand $ T1 $ \(_this1 :: OLand) -> LandDef [BasicLand Island] []
> shock == shock -- True
> compare allIsDust vindicate -- something other than EQ
```
(Note that `MtgPure/Model/ToObjectN/Instances.hs` is a multi-megabyte file and will take a while to compile, hence the `-fobject-code` flag to cache the result. It is recommended you don't open this file with the Haskell Language Server active unless you want to max out and throttle your computer's RAM. Opening this file in a browser will also probably slow your computer to a halt.)

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

# mtg-pure
Purely functional Haskell MTG engine and encoding library

Haskell language options:
- Safe
- NoIncoherentInstances
- NoOverlappingInstances (todo)
- NoTemplateHaskell
- NoUndecidableInstances 

Model goals:
- Pure data modelling of cards as a type-safe DSL.
- Card model is a deep embedding.
- Card model is algebraic and recursive.
- Cards type-check if and only if they are valid cards. (Barring a few exceptions such as bottom(⊥) appearing in card definitions.)
- Card model is expressive enough to replicate its souce code through introspection.

Engine goals:
- Exhaustive and type-checked engine.
- Event-based programmatic control.
- Solitaire CLI. You control all player actions.
- Good for exploring card and game-state interactions.

Will not do:
- GUI
- Multiplayer
- AI

Will not fix issues related to:
- Handling of integer overflow
- Memory performance
- Speed performance

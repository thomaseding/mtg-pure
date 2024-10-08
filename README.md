# mtg-pure
Purely functional Haskell "Magic: The Gathering" card model and game engine.

---

### About:

 [MtgPure](https://github.com/thomaseding/mtg-pure) is a showcase of how to make an elegant purely functional "Magic: The Gathering" DSL model and rules engine.

---

### Model Goals:
- The world's best MTG encoding.
- Pure data modelling of cards as a type-safe DSL.
- Card model is a deep embedding.
- Card model is algebraic and recursive.
- Cards type-check if and only if they are valid cards. (Barring a few exceptions such as bottom(⊥) appearing in card definitions.) Examples: creatures in graveyards can't be dealt damage; non-player objects can't draw cards, etc.
- Card model is expressive enough to replicate its source code through introspection.
- Cards can be used in arbitrary game formats without change.
- Formal changes to game rules should not require re-modelling cards. (Cards may be rewritten if they get errata.)
- Model can encode complex and outlier interactions without brute-force special casing. For example, the model will not contain stuff such as `ScrambleverseEffect`.

---

### Engine Goals:
- Exhaustive and type-checked engine.
- Event-based programmatic control.
- Solitaire CLI. You control all player actions.
- Good for exploring card and game-state interactions.

---

### Quickstart Interesting Files For Model

- [`src/MtgPure/Cards.hs`](src/MtgPure/Cards.hs)
- [`src/MtgPure/Model/Recursive.hs`](src/MtgPure/Model/Recursive.hs)

---

### Quickstart Interesting Files For Engine

- [`src/MtgPure/Client/Terminal.hs`](src/MtgPure/Client/Terminal.hs)
- [`src/MtgPure/Engine/Fwd/Api.hs`](src/MtgPure/Engine/Fwd/Api.hs)
- [`src/MtgPure/Engine/Fwd/Impl.hs`](src/MtgPure/Engine/Fwd/Impl.hs)
- [`src/MtgPure/Engine/State.hs`](src/MtgPure/Engine/State.hs)
- [`src/Demo/MtgPure/Gameplay.hs`](src/Demo/MtgPure/Gameplay.hs)

---

### Quickstart:
```
$ cabal install fourmolu  # for code generation ; add to PATH if needed
$ cd src
$ runhaskell MtgPure/Model/Object/ToObjectN/CodeGen.hs
$ cd ..
$ cabal build
$ cabal repl
ghci> :m +MtgPure MtgPure.AllCards MtgPure.Cards MtgPure.Model.BasicLandType MtgPure.Model.LandType MtgPure.Model.Recursive
ghci> :i allCards
allCards :: [AnyCard]   -- Defined in `MtgPure.AllCards'
ghci> :i island
island :: Card OTLand   -- Defined in `MtgPure.Cards'
ghci> compare island island
EQ
ghci> print island
Card "Island" $ ElectCardFacet $ LandCharacteristic [] [BasicLand Island] $ LandSpec []
ghci> island == (Card "Island" $ ElectCardFacet $ LandCharacteristic [] [BasicLand Island] $ LandSpec [])
True
ghci> island == (Card "Island" $ ElectCardFacet $ LandCharacteristic [] [BasicLand Mountain] $ LandSpec [])
False
ghci> mainDemoGameplay -- runs Demo.MtgPure.Gameplay
```
Notes:
 - Setting `-Wno-type-defaults` so the `Show` instances for cards don't need to constantly specify `Num` types when `Integer` is good enough for authoring. (Too much noise adding annotations for `Integer` or even `Int` or an alias `I`.)
 - `src/MtgPure/Model/Object/ToObjectN/Instances.hs` imports large generated files and will take a while to compile, hence the `-fobject-code` flag to cache the result inside the `ghci-compiled.bat` script.
- Using `-fobject-code` with `ghci` seems to require quitting and reentering `ghci` in order to get it to pick up the right runtime behavior after making code changes.
---

### Screenshots:

`Demo.MtgPure.Gameplay`

https://user-images.githubusercontent.com/6971794/220497420-c35cd250-41be-44b8-bd27-16a470732c1c.mp4

`App.AnsiInspector`
![App.AnsiInspector](https://user-images.githubusercontent.com/6971794/216997518-b8ee0a7d-cc24-4c4b-8e6c-1a70494f24a4.png)

### Contributors:
Contributions may or may not be accepted. If you are concerned about your time being wasted, please open an issue first to discuss. Entry-level contributions would probably be for card definitions or tests. More advanced contributions would be for the engine or model design.

---

### Low priority:
- AI
- GUI

---

### Will not fix issues related to:
- Handling of integer overflow. I don't want to use unbounded integer types, and I'm okay with deviating from official MTG rules here.

---

### Haskell language options:
- Safe (For the model and rules engine. UI and other stuff needs to interface with Unsafe libraries.)
- NoIncoherentInstances
- NoOverlappingInstances (todo: write a script that generates non-overlapping ToManaCost/ToManaPool instances needed to achieve this)
- NoTemplateHaskell
- NoUndecidableInstances 

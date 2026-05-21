# Petri Net Library

A typed, extensible Petri net library for Scala 3, used internally to model the Hydrozoa protocol.
The library is designed around four separable concerns (topology, syntax, semantics, presentation)
and exposes a functional builder monad for constructing nets.

---

## Architecture Overview

```mermaid
graph TD
    CL["Component Layer\nPlace · Arc · Transition"]
    NL["Net Layer\nNet traits · MapNet"]
    SIM["SequentialSimulator\n(firing engine)"]
    BLD["MapNet.BuilderM\n(builder monad)"]

    CL --> NL
    NL --> SIM
    NL --> BLD
```

- **Component layer** — trait hierarchies for individual places, arcs, and transitions
- **Net layer** — `Net` trait that composes the component traits; `MapNet` is the concrete map-backed implementation
- **SequentialSimulator** — firing algorithm mixed into `MapNet`
- **BuilderM** — `IndexedStateT`-based builder monad for constructing nets in for-comprehensions

---

## Ontological Ordering

Every component (and the net as a whole) is defined in four layers, each building on the previous:

```mermaid
flowchart LR
    T["Topology\nWhat connects to what"]
    S["Syntax\nWhat data is carried"]
    SE["Semantics\nHow to interpret / validate / fire"]
    P["Presentation\nHow to render visually"]

    T --> S --> SE --> P
```

| Layer        | Place concern                          | Arc concern                              | Transition concern          |
|--------------|----------------------------------------|------------------------------------------|-----------------------------|
| Topology     | Arc-connection restrictions (stub)     | `arcPlaceId`, `arcTransitionId`          | Arc-connection restrictions |
| Syntax       | `marking`, `mark(newMarking)`          | Weight, inscription                      | `silent`, `priority`        |
| Semantics    | `markingErrors`, `validMarking`        | `enablingError`, `fire`                  | Pre/postcondition (planned) |
| Presentation | `label`, `position`, `radius`          | `label`, `points`                        | `label`, `width`, `height`  |

---

## Component Layer

### Places

```mermaid
classDiagram
    class PlaceTopology["Place.Topology"] {
        <<trait>>
    }
    class PlaceSyntax["Place.Syntax[Self]"] {
        <<trait>>
        +PlaceMarking : type
        +marking : PlaceMarking
        +mark(PlaceMarking) Self
    }
    class HasFinalMarking["Place.Syntax.HasFinalMarking[Self]"] {
        <<mixin trait>>
        +finalMarking : Option[PlaceMarking]
        +withFinalMarking(Option) Self
    }
    class PlaceSemantics["Place.Semantics[Self]"] {
        <<trait>>
        +markingErrors() List[MarkingError]
        +validMarking() Boolean
        +markValid(PlaceMarking) Either
    }
    class Bounded["Place.Semantics.Bounded[Self]"] {
        <<mixin trait>>
        +bound : PositiveInt
    }
    class PlacePresentation["Place.Presentation"] {
        <<trait>>
        +label : String
        +position : (Int, Int)
        +radius : PositiveInt
    }
    class UnboundedPlace {
        <<case class>>
        +label : String
        +marking : NonNegativeInt
    }
    class BoundedPlace {
        <<case class>>
        +label : String
        +marking : NonNegativeInt
        +bound : PositiveInt
    }

    PlaceSyntax <|-- HasFinalMarking
    PlaceSemantics <|-- Bounded

    UnboundedPlace ..|> PlaceTopology
    UnboundedPlace ..|> PlaceSyntax
    UnboundedPlace ..|> PlaceSemantics
    UnboundedPlace ..|> PlacePresentation

    BoundedPlace ..|> PlaceTopology
    BoundedPlace ..|> PlaceSyntax
    BoundedPlace ..|> Bounded
    BoundedPlace ..|> PlacePresentation
```

**`UnboundedPlace`** — no capacity limit; `markingErrors` always returns `Nil`.

**`BoundedPlace`** — has a `bound: PositiveInt`; constructed via a smart constructor that returns
`Either[TooManyTokens, BoundedPlace]`. `markingErrors` yields `TooManyTokens` if `marking > bound`.

> `Place.Syntax` uses F-bounded polymorphism (`Self <: Syntax[Self]`) so that `mark` returns
> the concrete type, not the trait. Every mixin trait that adds update methods follows the same
> pattern.

---

### Arcs

```mermaid
classDiagram
    class ArcTopology["Arc.Topology[PlaceId, TransitionId]"] {
        <<trait>>
        +arcPlaceId : PlaceId
        +arcTransitionId : TransitionId
    }
    class ArcSyntax["Arc.Syntax"] {
        <<trait>>
    }
    class ArcSemantics["Arc.Semantics[P]"] {
        <<trait>>
        +enablingError(P) Option[EnablingError]
        +fire(P) Either[FiringError, P]
        +enabled(P) Boolean
        +fireUnsafe(P) P
    }
    class PT["Arc.Semantics.PT[P]"] {
        <<mixin trait>>
        +weight : PositiveInt
    }
    class TP["Arc.Semantics.TP[P]"] {
        <<mixin trait>>
        +weight : PositiveInt
    }
    class Inhibitor["Arc.Semantics.Inhibitor[P]"] {
        <<mixin trait>>
    }
    class Reset["Arc.Semantics.Reset[P]"] {
        <<mixin trait>>
    }
    class Read["Arc.Semantics.Read[P]"] {
        <<mixin trait>>
        +weight : PositiveInt
    }

    ArcSemantics <|-- PT
    ArcSemantics <|-- TP
    ArcSemantics <|-- Inhibitor
    ArcSemantics <|-- Reset
    ArcSemantics <|-- Read
```

#### Arc type reference

| Arc type    | Direction   | Enabled when           | Effect on place              |
|-------------|-------------|------------------------|------------------------------|
| `PT`        | Place → Transition | `tokens >= weight`  | removes `weight` tokens      |
| `TP`        | Transition → Place | always              | adds `weight` tokens         |
| `Inhibitor` | Place → Transition | `tokens == 0`       | none (enabling only)         |
| `Reset`     | Place → Transition | always              | drains all tokens            |
| `Read`      | Place → Transition | `tokens >= weight`  | none (enabling only)         |

> `Inhibitor`, `Reset`, and `Read` do not consume or produce tokens; they only gate enabledness
> (Inhibitor, Read) or drain (Reset).

#### Petri net notation

```mermaid
graph LR
    P1(("P1"))
    P2(("P2"))
    T1["T1"]

    P1 -->|"PT  w=2"| T1
    T1 -->|"TP  w=1"| P2
```

Circles represent **places**; rectangles represent **transitions**. Arc labels show type and weight.
Inhibitor arcs are conventionally drawn with a circle arrowhead; Read arcs with a half-arrow.

---

### Transitions

`Transition` is currently a lightweight stub — semantics (pre/postconditions for data-aware nets)
are not yet implemented.

| Trait                        | Purpose                                              |
|------------------------------|------------------------------------------------------|
| `Transition.Topology`        | Stub; component-side arc restrictions deferred       |
| `Transition.Syntax.HasSilent`| Whether this transition appears in event logs        |
| `Transition.Syntax.HasPriority` | Relative priority for auto-firing selection       |
| `Transition.Semantics`       | Stub; guard expressions planned                      |
| `Transition.Presentation`    | `label`, `width`, `height`, `position`, `delay`      |

---

## Net Layer

### Trait hierarchy

```mermaid
classDiagram
    class NetIds["Net.Ids"] {
        <<trait>>
        +arcIds : Set[ArcId]
        +placeIds : Set[PlaceId]
        +transitionIds : Set[TransitionId]
    }
    class NetTopology["Net.Topology"] {
        <<trait>>
        +getArcTopology(ArcId)
        +getPlaceTopology(PlaceId)
        +getTransitionTopology(TransitionId)
        +topologyErrors() List
    }
    class NoDanglingArcs["Net.Topology.NoDanglingArcs"] {
        <<mixin trait>>
    }
    class SingleArc["Net.Topology.SingleArc"] {
        <<mixin trait>>
    }
    class NetSyntax["Net.Syntax"] {
        <<trait>>
        +getArcSyntax(ArcId)
        +getPlaceSyntax(PlaceId)
        +getTransitionSyntax(TransitionId)
    }
    class NetSemantics["Net.Semantics"] {
        <<trait>>
        +getArcSemantics(ArcId)
        +getPlaceSemantics(PlaceId)
        +getTransitionSemantics(TransitionId)
        +netEnablingPredicates(TransitionId)
    }
    class Net {
        <<trait>>
    }
    class MapNet {
        <<case class>>
        +placesMap : TreeMap
        +transitionsMap : TreeMap
        +arcsMap : TreeMap
    }

    NetIds <|-- NetTopology
    NetTopology <|-- NoDanglingArcs
    NetTopology <|-- SingleArc
    NetIds <|-- NetSyntax
    NetIds <|-- NetSemantics
    Net --|> NetTopology
    Net --|> NetSyntax
    Net --|> NetSemantics
    MapNet ..|> Net
```

**`MapNet`** stores places, transitions, and arcs in `TreeMap`s for deterministic iteration order.
All three type parameters (`ArcId`, `PlaceId`, `TransitionId`) require an `Ordering`.

### Topology validation mixins

| Mixin                  | What it checks                                          |
|------------------------|---------------------------------------------------------|
| `NoDanglingArcs`       | Every arc references a place and transition that exist  |
| `SingleArc`            | At most one arc per `(place, transition)` direction pair|

Both use the abstract-override list-accumulation pattern: `topologyErrors` chains via
`super.topologyErrors`, so multiple mixins compose cleanly.

> **Why `SingleArc`?** Firing endomorphisms do not commute in general (e.g. `PT(1)` then `Reset` ≠
> `Reset` then `PT(1)`). Restricting to one arc per direction makes firing trivially order-independent.

---

## Building a Net

`MapNet.BuilderMOps` provides a typed builder monad. Operations fail with a `BuilderError` on ID
conflicts or missing IDs; "force" variants (trailing `_`) silently overwrite instead.

```scala
// Fix the six net type parameters once
val ops = MapNet.BuilderMOps[String, String, String, MyArc, MyPlace, MyTransition]()
import ops.*

val program = for
    _ <- addPlace("p1", UnboundedPlace("input", marking = 2))
    _ <- addPlace("p2", UnboundedPlace("output"))
    _ <- addTransition("t1", myTransition)
    _ <- addArc("a1", PTArc("p1", "t1", weight = 1, label = "consume"))
    _ <- addArc("a2", TPArc("p2", "t1", weight = 1, label = "produce"))
yield ()

val result: Either[BuilderError, (MapNet[...], Unit)] = program.runEmpty
```

The `Monad` instance for `BuilderM` is provided as a `given`, so `traverse`, `sequence`, and other
Cats combinators are available when `import cats.implicits.*` is in scope.

---

## Enabling and Firing

### Enabling conditions compose under AND

Enabling is checked at four independent levels, all composed by conjunction:

```mermaid
flowchart LR
    PE["Place-side\nPlace.Semantics.validMarking"]
    AE["Arc-side\nArc.Semantics.enablingError"]
    TE["Transition-side\n(stub — always true)"]
    NE["Net-side\nNet.Semantics.netEnablingPredicate"]

    PE --> AND{"AND"}
    AE --> AND
    TE --> AND
    NE --> AND
    AND --> EN["Transition enabled?"]
```

Because conjunction is commutative and associative, the order of predicate evaluation is a
performance choice only — it does not affect correctness.

### SequentialSimulator firing algorithm

`SequentialSimulator.fire(t)` is purely functional: it returns `Either[FiringError, Self]` where
`Self` is the new net state with updated place markings.

```mermaid
flowchart TD
    A["fire(t)"] --> B{"transition\nexists?"}
    B -- No --> E1["FiringError.TransitionNotFound"]
    B -- Yes --> C{"netEnablingPredicate\nholds?"}
    C -- No --> E2["FiringError.NetEnablingFailed"]
    C -- Yes --> D["snapshot all (arc, place) pairs\nfor transition t"]
    D --> F{"for each arc:\narc-side enabled?\nenablingError(place) == None"}
    F -- No --> E3["FiringError.ArcNotEnabled"]
    F -- Yes --> G{"apply arc.fire(place)\nagainst pre-fire snapshot"}
    G -- Left --> E4["FiringError.ArcFiringFailed"]
    G -- Right --> H{"firedPlace.validMarking?\nmarkingErrors.isEmpty"}
    H -- No --> E5["FiringError.PlaceValidityViolated"]
    H -- Yes --> I["withUpdatedPlaces(updates)\n→ new Self"]
```

Key points:
- All arc/place pairs are snapshotted **before** any endo is applied (pre-fire state).
- `Arc.Semantics.fire` does **not** check enabledness or place validity — those are the simulator's responsibility.
- `Place.Semantics.validMarking` is the place-side enabling condition (E1): it checks that the fired result satisfies the place's own invariants (e.g. `BoundedPlace` capacity).

---

## Validation Summary

| Check                     | Method                         | When to call                            |
|---------------------------|--------------------------------|-----------------------------------------|
| No dangling arcs          | `net.isValidTopology`          | After building, before simulating       |
| At most one arc per pair  | `net.isValidTopology`          | After building, before simulating       |
| All IDs have syntax       | `net.isValidSyntax`            | Guaranteed by construction in `MapNet`  |
| All IDs have semantics    | `net.isValidSemantics`         | Guaranteed by construction in `MapNet`  |
| Final marking reached     | `net.isValidTerminal`          | After simulation terminates             |

---

## Net Composition (Planned)

Two composition operators are planned but not yet implemented:

```mermaid
graph LR
    subgraph "Monoidal combination (parallel)"
        N1["Net N1"] --- OP1["⊕"] --- N2["Net N2"]
        OP1 --> N12["N1 ⊕ N2\n(disjoint union of IDs)"]
    end

    subgraph "Gluing (shared interface places)"
        N3["Net N3"] --- OP2["glue(P3, P4)"] --- N4["Net N4"]
        OP2 --> N34["N3 ∪ N4\n(P3 and P4 merged)"]
    end
```

- **Monoidal combination** — disjoint union of two nets; each retains ownership of its own
  transitions and arcs. Requires disjoint ID sets.
- **Gluing** — identifies a subset of places from one net with places from another, creating shared
  interface places. After gluing, enabling and firing delegate to whichever sub-net owns the
  transition. Autofiring policy of the composed net is a separate semantic choice.

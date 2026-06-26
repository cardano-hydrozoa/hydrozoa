# `timing-viz` — Hydrozoa Timing-Rules Visualizer

Live + replay visualizer for the Hydrozoa timing rules
(`design/.../timing-rules.mdx`). Built as a Hydrozoa subproject that depends on
`core` for `TxTiming` (the spec's authoritative arithmetic) and projects state into a JSON
`Frame` consumed by a React frontend.

## Architecture

```
                ┌─────────────────────────┐
   Commands ──► │  TimingVisualizerState  │ ──► Presentation.render ──► Frame ──► WS / JSON
                └─────────────────────────┘
                  pure State[S, _] fold

  Sources of Commands:
    - WS clients (the React frontend, or anything that speaks the wire format)
    - POST /replay (batch JSON list, useful for log replay)
    - VizTracer (translates a running Hydrozoa node's MRMEvent stream into Commands)
    - Replay CLI (sbt "timingViz/runMain hydrozoa.timingviz.Replay path.json")
```

The state is a deterministic fold over an ordered `Command` log; the same `Command`s drive live
observation, hypothetical drafting, and batch replay.

## File map

| File | Purpose |
|------|---------|
| `Ids.scala` | Opaque IDs (`BlockId`, `RequestId`, `DepositId`, `EffectId`) + `ObjectId` enum |
| `TimedObject.scala` | The thing-on-the-timeline ADT + per-object `track` extension |
| `Source.scala` | `Observed` vs `Hypothetical` tag on each object |
| `Provenance.scala` | `FieldKey` + `DerivationMap` for cross-object dependencies |
| `Feedback.scala` | `Rejection` + `UIHint` (transition outputs) |
| `Command.scala` | The visualizer's input alphabet |
| `TimingVisualizerState.scala` | The pure state |
| `TimingVisualizer.scala` | The `Command => State[S, TransitionOutput[Unit]]` fold |
| `Frame.scala` | The renderer's view-model (wire-format target) |
| `Presentation.scala` | `TimingVisualizerState => Frame` projection |
| `Codecs.scala` | Circe `Encoder`/`Decoder` for the wire format |
| `Server.scala` | http4s WS server (`/ws`, `/frame`, `/replay`) |
| `Replay.scala` | CLI: fold a JSON command file, print the resulting `Frame` |
| `VizTracer.scala` | Bridge from Hydrozoa `MRMEvent` to visualizer `Command`s |
| `VizClient.scala` | WS client used by `VizTracer` to publish to a running `Server` |
| `frontend/` | Vite + React + d3 frontend |
| `examples/commands.json` | Sample command sequence (used by tests + Replay) |

## Demo

One command starts everything (backend + frontend) and pre-loads the sample data:

```bash
just timing-viz-demo
# then open http://localhost:5173
```

Ctrl-C tears it all down.

## Running

### Backend only

```bash
sbt "timingViz/runMain hydrozoa.timingviz.Server"
# listens on http://localhost:8765
curl http://localhost:8765/frame
curl -X POST http://localhost:8765/replay -H 'Content-Type: application/json' \
    -d @timing-viz/examples/commands.json
```

### Frontend (dev)

```bash
cd timing-viz/frontend
npm install
npm run dev      # http://localhost:5173 (proxies /ws, /frame, /replay to backend)
```

### Replay only (no server)

```bash
sbt "timingViz/runMain hydrozoa.timingviz.Replay timing-viz/examples/commands.json"
```

### Wiring a live Hydrozoa node

Add to `app/Main.scala` next to the existing root MRM tracer:

```scala
import cats.kernel.Monoid
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.timingviz.{VizClient, VizTracer}
import org.http4s.implicits.*

VizClient.open(uri"ws://localhost:8765/ws").use { send =>
    for
        vizTr   <- VizTracer.make(send)
        combined = Monoid[ContraTracer[IO, HeadMultisigRegimeManagerEvent]].combine(slf4jTr, vizTr)
        _       <- runRegimeManagerWith(combined)
    yield ()
}
```

The visualizer server can be running anywhere reachable; one node-side tracer feeds the
visualizer with `ObserveBlock` / `ObserveDeposit*` / `ObserveSpineEffect` / `ObserveFallback`
commands. Extend `VizTracer.commandFor` / `VizTracer.handle` to surface more events.

## Wire format

All times are **epoch milliseconds**. Sum types use a `"type"` discriminator.

**Frame (server → client):**

```json
{
  "nowMs": 1700000000000,
  "config": { "minSettlementMs": 43200000, "silenceMs": 300000, ... },
  "tracks": {
    "requests": [ ... ],
    "blocks":   [ { "type": "Bar", "id": "block-b1", "objectId": {"type":"OfBlock","id":"b1"},
                    "label": "block b1 (Major)", "kind": "BlockCreation",
                    "source": "Observed", "startMs": ..., "endMs": ... } ],
    "spineEffects": [...], "fallbacks": [...], "deposits": [...], "refunds": [...]
  },
  "derivations": [
    { "targetObject": {...}, "targetField": "ValidityEnd",
      "sourceObject": {...}, "sourceField": "FallbackStart" }
  ]
}
```

**Command (client → server):** see `Command.scala` + `examples/commands.json` for the canonical
shape per case.

## Phase 2 (not built yet)

- **Cross-compile `TxTiming` to JS** so the frontend can re-derive on parameter drag without a
  WS round-trip. The arithmetic in `TxTiming.scala` (~20 lines) is portable; the blocker is
  `scalus.cardano.ledger.SlotConfig`, which would need to be abstracted behind a polymorphic
  `TimingEquations[Instant, Dur]` trait.
- **State-level derivation provenance** (currently only Fallback↔SpineEffect and Refund↔Deposit
  edges are emitted, computed at presentation time).
- **Richer `VizTracer` mappings** — full BlockBrief lifecycle, deposit-state-machine events
  from `DepositsMapEvent`, fallback/settlement dispatches from `CardanoLiaisonEvent`.

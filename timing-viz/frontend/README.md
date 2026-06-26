# Hydrozoa Timing Visualizer — frontend

React + Vite + d3 frontend for the timing-rules visualizer. Connects via WebSocket to the Scala
backend (`hydrozoa.timingviz.Server`) and renders the live `Frame` projection of the visualizer
state.

## Quick start

```bash
# 1. Start the backend (separate shell, from repo root)
sbt "timingViz/runMain hydrozoa.timingviz.Server"
# listens on http://localhost:8765

# 2. Install + start the frontend
cd timing-viz/frontend
npm install
npm run dev
# open http://localhost:5173
```

Vite proxies `/ws`, `/frame`, `/replay` to `localhost:8765`, so the browser sees them as
same-origin requests.

## Driving the visualizer

- **Live**: any process that opens a WebSocket to `/ws` and sends `Command` JSON messages will
  update everyone's view. See `timing-viz/examples/commands.json` for the wire shape.
- **Replay**: `sbt "timingViz/runMain hydrozoa.timingviz.Replay timing-viz/examples/commands.json"`
  folds a JSON command list and prints the resulting `Frame`. The same JSON can be `POST`ed to
  `/replay` while the server is running.
- **Draft**: the side panel has inputs for `HypothesizeDeposit` / `HypothesizeBlock`. These render
  dashed/faded so you can see "what if X happened at T" without polluting the observed history.

## File map

- `src/types.ts` — mirror of the Scala wire types (`Frame`, `Command`).
- `src/useVisualizer.ts` — WS connection hook with reconnect.
- `src/Timeline.tsx` — d3-scaled SVG with the six tracks.
- `src/SidePanel.tsx` — per-object details on click.
- `src/Controls.tsx` — parameter sliders, clock control, hypothesize forms.

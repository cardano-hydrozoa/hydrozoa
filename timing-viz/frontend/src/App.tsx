import { useState } from "react";
import { Timeline } from "./Timeline";
import { SidePanel } from "./SidePanel";
import { Controls } from "./Controls";
import { useVisualizer } from "./useVisualizer";
import type { TrackItem } from "./types";

export function App() {
    const { frame, connState, send } = useVisualizer();
    const [selected, setSelected] = useState<string | null>(null);

    if (!frame) {
        return (
            <div className="loading">
                <h1>Hydrozoa Timing Visualizer</h1>
                <p>Connecting… (status: {connState})</p>
            </div>
        );
    }

    const onRetract = (item: TrackItem) => {
        send({ type: "Retract", id: item.objectId });
        setSelected(null);
    };

    return (
        <div className="app">
            <header>
                <h1>Hydrozoa Timing Visualizer</h1>
                <div className="meta">
                    WS: <span className={`pill pill-${connState}`}>{connState}</span>
                    {" · "}
                    now: {new Date(frame.nowMs).toISOString()}
                </div>
            </header>
            <main>
                <div className="timeline-pane">
                    <Timeline frame={frame} selectedKey={selected} onSelect={setSelected} />
                </div>
                <aside>
                    <SidePanel frame={frame} selectedKey={selected} onRetract={onRetract} />
                    <Controls frame={frame} send={send} />
                </aside>
            </main>
        </div>
    );
}

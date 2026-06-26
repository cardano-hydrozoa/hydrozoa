import type { Frame, TrackItem } from "./types";
import { TRACKS, objectIdKey } from "./types";

interface Props {
    frame: Frame;
    selectedKey: string | null;
    onRetract: (item: TrackItem) => void;
}

export function SidePanel({ frame, selectedKey, onRetract }: Props) {
    if (!selectedKey) {
        return (
            <div className="side-panel">
                <h3>Selection</h3>
                <p className="muted">Click an item on the timeline to inspect it.</p>
            </div>
        );
    }
    const matching = TRACKS.flatMap((t) =>
        frame.tracks[t.key].filter((it) => objectIdKey(it.objectId) === selectedKey),
    );
    if (matching.length === 0) {
        return (
            <div className="side-panel">
                <h3>Selection</h3>
                <p className="muted">No items match the current selection.</p>
            </div>
        );
    }
    const head = matching[0];
    return (
        <div className="side-panel">
            <h3>Selection</h3>
            <div className="row">
                <span className="label">Object</span>
                <span className="value">
                    {head.objectId.type} <code>{head.objectId.id}</code>
                </span>
            </div>
            <div className="row">
                <span className="label">Source</span>
                <span className="value">{head.source}</span>
            </div>
            <h4>Items ({matching.length})</h4>
            <ul className="item-list">
                {matching.map((it) => (
                    <li key={it.id}>
                        <span className="item-kind">{it.kind}</span>
                        <span className="item-range">
                            {it.type === "Bar"
                                ? `${ms(it.startMs)} → ${ms(it.endMs)} (${humanDuration(it.endMs - it.startMs)})`
                                : `at ${ms(it.atMs)}`}
                        </span>
                        <span className="item-label">{it.label}</span>
                    </li>
                ))}
            </ul>
            <button onClick={() => onRetract(head)}>Retract</button>
        </div>
    );
}

function ms(t: number): string {
    return new Date(t).toISOString().replace("T", " ").replace("Z", "");
}

function humanDuration(ms: number): string {
    if (ms < 1000) return `${ms} ms`;
    const s = ms / 1000;
    if (s < 60) return `${s.toFixed(1)} s`;
    const m = s / 60;
    if (m < 60) return `${m.toFixed(1)} min`;
    const h = m / 60;
    if (h < 24) return `${h.toFixed(1)} h`;
    return `${(h / 24).toFixed(1)} d`;
}

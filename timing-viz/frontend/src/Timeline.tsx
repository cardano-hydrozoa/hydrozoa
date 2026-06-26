import { useMemo } from "react";
import { scaleLinear, scaleUtc } from "d3-scale";
import type { Frame, TrackItem, ItemKind } from "./types";
import { TRACKS, itemAt, itemEnd, objectIdKey } from "./types";

const ITEM_KIND_COLOR: Record<ItemKind, string> = {
    Request: "#7aa2f7",
    BlockCreation: "#bb9af7",
    SpineInit: "#9ece6a",
    SpineSettlement: "#73daca",
    SpineFinalization: "#2ac3de",
    Fallback: "#f7768e",
    Refund: "#e0af68",
    DepositSubmissionWindow: "#a0a0d0",
    DepositMaturityWait: "#666690",
    DepositAbsorptionWindow: "#7aa2f7",
    DepositSilenceWindow: "#3b3b58",
    DepositRefundStart: "#e0af68",
};

const ROW_HEIGHT = 36;
const ROW_GAP = 6;
const LEFT_PAD = 130;
const RIGHT_PAD = 30;

interface Props {
    frame: Frame;
    selectedKey: string | null;
    onSelect: (key: string | null) => void;
}

export function Timeline({ frame, selectedKey, onSelect }: Props) {
    const allItems = useMemo<TrackItem[]>(() => {
        const t = frame.tracks;
        return [
            ...t.requests,
            ...t.blocks,
            ...t.spineEffects,
            ...t.fallbacks,
            ...t.deposits,
            ...t.refunds,
        ];
    }, [frame]);

    const { minMs, maxMs } = useMemo(() => {
        if (allItems.length === 0) {
            const now = frame.nowMs;
            return { minMs: now - 60_000, maxMs: now + 60_000 };
        }
        const starts = allItems.map(itemAt);
        const ends = allItems.map(itemEnd);
        const lo = Math.min(...starts, frame.nowMs);
        const hi = Math.max(...ends, frame.nowMs);
        const pad = (hi - lo) * 0.05 || 60_000;
        return { minMs: lo - pad, maxMs: hi + pad };
    }, [allItems, frame.nowMs]);

    const width = 1000;
    const x = scaleUtc().domain([minMs, maxMs]).range([LEFT_PAD, width - RIGHT_PAD]);
    const pxScale = scaleLinear().domain([minMs, maxMs]).range([LEFT_PAD, width - RIGHT_PAD]);
    const totalHeight = TRACKS.length * (ROW_HEIGHT + ROW_GAP) + 40;

    const nowX = pxScale(frame.nowMs);

    const rowY = (trackIdx: number) => 30 + trackIdx * (ROW_HEIGHT + ROW_GAP);

    return (
        <svg width={width} height={totalHeight} className="timeline">
            {/* track labels & row dividers */}
            {TRACKS.map((t, i) => (
                <g key={t.key}>
                    <rect
                        x={0}
                        y={rowY(i)}
                        width={LEFT_PAD - 10}
                        height={ROW_HEIGHT}
                        fill="#1f2335"
                    />
                    <text x={10} y={rowY(i) + ROW_HEIGHT / 2 + 4} fill="#c0caf5" fontSize={12}>
                        {t.label}
                    </text>
                    <line
                        x1={LEFT_PAD}
                        y1={rowY(i) + ROW_HEIGHT}
                        x2={width - RIGHT_PAD}
                        y2={rowY(i) + ROW_HEIGHT}
                        stroke="#2a2e42"
                    />
                </g>
            ))}
            {/* x-axis ticks */}
            {x.ticks(8).map((tick, i) => (
                <g key={i}>
                    <line
                        x1={pxScale(tick.valueOf())}
                        y1={20}
                        x2={pxScale(tick.valueOf())}
                        y2={totalHeight - 10}
                        stroke="#2a2e42"
                    />
                    <text
                        x={pxScale(tick.valueOf())}
                        y={16}
                        fontSize={10}
                        fill="#7681a0"
                        textAnchor="middle"
                    >
                        {tickLabel(tick)}
                    </text>
                </g>
            ))}
            {/* now line */}
            <line x1={nowX} y1={20} x2={nowX} y2={totalHeight - 10} stroke="#ff9e64" strokeDasharray="4 2" />
            <text x={nowX + 4} y={28} fontSize={11} fill="#ff9e64">
                now
            </text>
            {/* items */}
            {TRACKS.map((t, i) =>
                frame.tracks[t.key].map((item) => (
                    <ItemShape
                        key={item.id}
                        item={item}
                        x={pxScale}
                        rowY={rowY(i)}
                        selected={selectedKey === objectIdKey(item.objectId)}
                        onClick={() => onSelect(objectIdKey(item.objectId))}
                    />
                )),
            )}
        </svg>
    );
}

function ItemShape({
    item,
    x,
    rowY,
    selected,
    onClick,
}: {
    item: TrackItem;
    x: (v: number) => number;
    rowY: number;
    selected: boolean;
    onClick: () => void;
}) {
    const color = ITEM_KIND_COLOR[item.kind];
    const opacity = item.source === "Hypothetical" ? 0.55 : 1.0;
    const stroke = selected ? "#ff9e64" : "transparent";
    if (item.type === "Bar") {
        const startX = x(item.startMs);
        const endX = x(item.endMs);
        return (
            <g style={{ cursor: "pointer" }} onClick={onClick}>
                <rect
                    x={startX}
                    y={rowY + 4}
                    width={Math.max(2, endX - startX)}
                    height={ROW_HEIGHT - 8}
                    fill={color}
                    opacity={opacity}
                    stroke={stroke}
                    strokeWidth={selected ? 2 : 0}
                    rx={3}
                    strokeDasharray={item.source === "Hypothetical" ? "4 2" : undefined}
                >
                    <title>{item.label}</title>
                </rect>
            </g>
        );
    }
    const atX = x(item.atMs);
    return (
        <g style={{ cursor: "pointer" }} onClick={onClick}>
            <circle
                cx={atX}
                cy={rowY + ROW_HEIGHT / 2}
                r={6}
                fill={color}
                opacity={opacity}
                stroke={stroke}
                strokeWidth={selected ? 2 : 0}
            >
                <title>{item.label}</title>
            </circle>
        </g>
    );
}

function tickLabel(d: Date): string {
    return d.toISOString().slice(11, 19); // HH:MM:SS
}

import { useState } from "react";
import type { Command, Frame, ParameterKey } from "./types";

interface Props {
    frame: Frame;
    send: (cmd: Command) => void;
}

const PARAMETERS: { key: ParameterKey; label: string; field: keyof Frame["config"] }[] = [
    { key: "MinSettlementDuration", label: "min settlement", field: "minSettlementMs" },
    { key: "InactivityMarginDuration", label: "inactivity margin", field: "inactivityMarginMs" },
    { key: "SilenceDuration", label: "silence", field: "silenceMs" },
    { key: "DepositSubmissionDuration", label: "deposit submission", field: "depositSubmissionMs" },
    { key: "DepositMaturityDuration", label: "deposit maturity", field: "depositMaturityMs" },
    { key: "DepositAbsorptionDuration", label: "deposit absorption", field: "depositAbsorptionMs" },
];

export function Controls({ frame, send }: Props) {
    return (
        <div className="controls">
            <ParameterSliders frame={frame} send={send} />
            <ClockControl frame={frame} send={send} />
            <HypothesizeForms send={send} />
        </div>
    );
}

function ParameterSliders({ frame, send }: Props) {
    return (
        <div className="panel">
            <h4>Parameters</h4>
            {PARAMETERS.map((p) => {
                const value = frame.config[p.field];
                return (
                    <div className="param-row" key={p.key}>
                        <label>{p.label}</label>
                        <input
                            type="number"
                            min={0}
                            value={value}
                            onChange={(e) => {
                                const v = parseInt(e.target.value, 10);
                                if (!Number.isNaN(v)) {
                                    send({ type: "SetParameter", key: p.key, valueMs: v });
                                }
                            }}
                        />
                        <span className="muted">ms ({humanDuration(value)})</span>
                    </div>
                );
            })}
        </div>
    );
}

function ClockControl({ frame, send }: Props) {
    const [target, setTarget] = useState<string>(new Date(frame.nowMs).toISOString().slice(0, 19));
    return (
        <div className="panel">
            <h4>Clock</h4>
            <div className="param-row">
                <label>now</label>
                <span className="value">{new Date(frame.nowMs).toISOString()}</span>
            </div>
            <div className="param-row">
                <label>advance to</label>
                <input
                    type="datetime-local"
                    step="1"
                    value={target}
                    onChange={(e) => setTarget(e.target.value)}
                />
                <button
                    onClick={() => {
                        const t = new Date(target + "Z").getTime();
                        if (!Number.isNaN(t)) send({ type: "AdvanceClock", toMs: t });
                    }}
                >
                    advance
                </button>
            </div>
        </div>
    );
}

function HypothesizeForms({ send }: { send: (cmd: Command) => void }) {
    const [depositId, setDepositId] = useState("d-hypo-1");
    const [depositEnd, setDepositEnd] = useState(
        new Date(Date.now() + 60_000).toISOString().slice(0, 19),
    );
    const [blockId, setBlockId] = useState("b-hypo-1");
    const [blockStart, setBlockStart] = useState(
        new Date(Date.now() + 120_000).toISOString().slice(0, 19),
    );
    const [blockEnd, setBlockEnd] = useState(
        new Date(Date.now() + 121_000).toISOString().slice(0, 19),
    );
    return (
        <div className="panel">
            <h4>Hypothesize</h4>
            <div className="hypo-form">
                <div className="hypo-row">
                    <strong>Deposit</strong>
                    <input value={depositId} onChange={(e) => setDepositId(e.target.value)} />
                    <input
                        type="datetime-local"
                        step="1"
                        value={depositEnd}
                        onChange={(e) => setDepositEnd(e.target.value)}
                    />
                    <button
                        onClick={() => {
                            const t = new Date(depositEnd + "Z").getTime();
                            send({ type: "HypothesizeDeposit", id: depositId, endMs: t });
                        }}
                    >
                        add
                    </button>
                </div>
                <div className="hypo-row">
                    <strong>Block</strong>
                    <input value={blockId} onChange={(e) => setBlockId(e.target.value)} />
                    <input
                        type="datetime-local"
                        step="1"
                        value={blockStart}
                        onChange={(e) => setBlockStart(e.target.value)}
                    />
                    <input
                        type="datetime-local"
                        step="1"
                        value={blockEnd}
                        onChange={(e) => setBlockEnd(e.target.value)}
                    />
                    <button
                        onClick={() => {
                            const s = new Date(blockStart + "Z").getTime();
                            const e2 = new Date(blockEnd + "Z").getTime();
                            send({
                                type: "HypothesizeBlock",
                                id: blockId,
                                creation: { startMs: s, endMs: e2 },
                                kind: "Minor",
                            });
                        }}
                    >
                        add
                    </button>
                </div>
            </div>
        </div>
    );
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

import { useEffect, useRef, useState, useCallback } from "react";
import type { Command, Frame } from "./types";

export type ConnState = "connecting" | "open" | "closed";

/** Holds the live `Frame` from the WS, plus a `send` for dispatching `Command`s back.
  * Reconnects with a 1s backoff if the socket drops. */
export function useVisualizer(): {
    frame: Frame | null;
    connState: ConnState;
    send: (cmd: Command) => void;
} {
    const [frame, setFrame] = useState<Frame | null>(null);
    const [connState, setConnState] = useState<ConnState>("connecting");
    const wsRef = useRef<WebSocket | null>(null);
    const pendingRef = useRef<Command[]>([]);

    useEffect(() => {
        let cancelled = false;
        const connect = () => {
            const proto = location.protocol === "https:" ? "wss" : "ws";
            const ws = new WebSocket(`${proto}://${location.host}/ws`);
            wsRef.current = ws;
            setConnState("connecting");
            ws.onopen = () => {
                setConnState("open");
                pendingRef.current.forEach((c) => ws.send(JSON.stringify(c)));
                pendingRef.current = [];
            };
            ws.onmessage = (evt) => {
                try {
                    const f = JSON.parse(evt.data) as Frame;
                    setFrame(f);
                } catch {
                    /* ignore non-JSON / partial frames */
                }
            };
            ws.onclose = () => {
                setConnState("closed");
                if (!cancelled) setTimeout(connect, 1000);
            };
            ws.onerror = () => ws.close();
        };
        connect();
        return () => {
            cancelled = true;
            wsRef.current?.close();
        };
    }, []);

    const send = useCallback((cmd: Command) => {
        const ws = wsRef.current;
        if (ws && ws.readyState === WebSocket.OPEN) {
            ws.send(JSON.stringify(cmd));
        } else {
            pendingRef.current.push(cmd);
        }
    }, []);

    return { frame, connState, send };
}

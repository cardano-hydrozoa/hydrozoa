# Observability endpoints: /health and /ready

The user-facing HTTP server (`multisig/server/HydrozoaServer.scala`, default port 8080) exposes
two unauthenticated `GET` endpoints for process supervisors and load balancers. They answer
different questions for different consumers and must not be conflated:

| | `GET /health` | `GET /ready` |
|---|---|---|
| Question | Is this process alive? | Should user traffic be routed here right now? |
| Consumer | Process supervisor (systemd, container runtime, liveness probe) | Load balancer / proxy / frontend (readiness probe) |
| On failure | Restart the process | Take the node out of rotation — do **not** restart |
| Checks | Nothing beyond HTTP serving | The node lifecycle status (see below) |
| Flaps | Never while the process lives | By design, at lifecycle transitions |

Both endpoints exist only on head peers — coil peers run no user-facing HTTP server
(`Main.runCoilNode`).

## /health — liveness

Always `200 {"status": "ok"}`.

The response is deliberately static: a liveness probe must only check things a restart would
fix, so it must never depend on anything external — the L1 backend, peer connections, or head
state. If Ember answered the request, the process is alive; that is the entire contract.

## /ready — readiness

Reports the node lifecycle status (`multisig/NodeStatus.scala`). The verdict is the HTTP status
code — `200` = route traffic here, `503` = don't; the JSON body is diagnostic:

| `NodeStatus` | Code | Body | Meaning |
|---|---|---|---|
| `Initializing` | 503 | `{"status": "initializing"}` | Stack 0 not hard-confirmed; the head is not on L1 yet. Submits would be accepted but only sit in the mempool. |
| `Active` | 200 | `{"status": "active"}` | The head is open on L1; the node serves user traffic. |
| `Finalized` | 503 | `{"status": "finalized"}` | The head has been finalized. Terminal. |
| `HandedOffToRuleBased` | 503 | `{"status": "handed-off-to-rule-based"}` | The multisig regime handed off to the rule-based regime; the request-processing actors are stopped. Terminal. |

A permanently-503 `/ready` on a node whose `/health` is 200 is a valid, expected end state:
after finalization or the rule-based handoff the node is alive but must not receive user
traffic again. That "alive but not serving" pair is exactly what the two endpoints exist to
distinguish.

## How the status is maintained

The status lives in a plain `Ref[IO, NodeStatus]` owned by `MultisigRegimeManagerBase`
(next to `connectionsDeferred`); the `/ready` route is a pure `Ref` read — no actor messaging
and no L1 calls, so it is safe at any probe frequency. Two writers advance it:

- **`CardanoLiaison`** reports the status implied by its L1 `TargetState` at every state
  write: the live stack-0 hard-confirm path (`Uninitialized → Active`), regular-stack effects
  carrying a finalization (`→ Finalized`), and the boot recovery fold — so a restarted node
  whose head is already open reports `active` again as soon as recovery completes.
- **The regime manager** writes `HandedOffToRuleBased` when it processes the
  multisig→rule-based handoff.

Writes go through `NodeStatus.advanceTo`, a monotone advance: earlier statuses never overwrite
later ones and terminal statuses stick, so the two independent writers cannot race the status
backwards.

Timing notes:

- The server binds only after the regime manager completes `connectionsDeferred`, so during
  boot probes see connection-refused — which both probe kinds already treat as failure. No
  special boot handling is needed.
- On a restart of an already-open head there is a brief `initializing` window between the
  server binding and `CardanoLiaison` finishing its recovery fold.

## Usage

```bash
curl -i http://localhost:8080/health
# HTTP/1.1 200 OK
# {"status":"ok"}

curl -i http://localhost:8080/ready
# before stack 0 hard-confirms:  HTTP/1.1 503 Service Unavailable  {"status":"initializing"}
# while the head is open:        HTTP/1.1 200 OK                   {"status":"active"}
```

Kubernetes-style probe configuration:

```yaml
livenessProbe:
  httpGet: { path: /health, port: 8080 }
  periodSeconds: 10
readinessProbe:
  httpGet: { path: /ready, port: 8080 }
  periodSeconds: 5
```

For a plain reverse proxy / load balancer, use `/ready` as the backend health check so the
node only enters rotation while the head is open.

# L2 query endpoints: /api/l2/utxos and /api/l2/transactions

The user-facing HTTP server (`multisig/server/HydrozoaServer.scala`) exposes two read-only `GET`
endpoints for inspecting the L2 ledger's current state and recent activity. They are the
read counterpart to the existing write path (`POST /api/l2/submit`, `POST /api/deposit/register`).

| | `GET /api/l2/utxos/{address}` | `GET /api/l2/transactions` |
|---|---|---|
| Answers | what does this address control on L2 now? | what happened on L2 recently? |
| Shape | array of CIP-0116 utxos | array of transaction summaries, newest first |
| Source | live L2 utxo set | the L2 ledger's own command log |

## EUTXO-only

These queries are served by the **EUTXO reference ledger** (`EutxoL2Ledger`), which holds its
state locally. A node wired to the **remote** L2 ledger (`RemoteL2Ledger`, the SugarRush
WebSocket) owns its state behind the black box and exposes no query channel, so on such a node
both endpoints return an **empty array** (a valid, non-error response). This mirrors how the
ledger already stubs its other read/recovery methods on the remote path. The queries live on
the shared `L2LedgerReader` trait (a narrow, read-only view of `L2Ledger`), so the HTTP layer
can read L2 state without being able to issue ledger commands.

## GET /api/l2/utxos/{address}

The current L2 utxos controlled by a bech32 `address`, serialized with the repo's CIP-0116
codecs. A malformed address is a `400`; an unknown-but-well-formed address is `200 []`.

```bash
curl http://localhost:8080/api/l2/utxos/addr_test1vryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana6q4a064h
```
```json
[
  {
    "input": { "transaction_id": "808aff…4aff", "index": 1 },
    "output": {
      "address": "addr_test1vryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana6q4a064h",
      "value": {
        "coin": "1782598159115561",
        "assets": { "8001ac…1c": { "fdff00": "2005921964837086207" } }
      },
      "datum": null
    }
  }
]
```

`datum` is `null` when the output has none; otherwise it is an object with `inline` (the datum's
CBOR hex) or `hash` (a datum-hash reference), whichever applies — the two kinds are kept distinct.

## GET /api/l2/transactions

The most recent applied L2 transactions, newest first, as lightweight summaries. `?count=N`
bounds the number of **summaries** returned (default 50); a non-integer `count` is a `400`.

```bash
curl "http://localhost:8080/api/l2/transactions?count=10"
```
```json
[
  { "requestId": { "headPeerNumber": 0, "requestNumber": 3 },
    "blockNumber": 3, "kind": "depositRefunded" },
  { "requestId": { "headPeerNumber": 0, "requestNumber": 2 },
    "blockNumber": 2, "kind": "transaction" }
]
```

`kind` is one of `transaction`, `depositRegistered`, `depositAbsorbed`, `depositRefunded` — the
log records applied transactions and the steps of a deposit's lifecycle. Each entry is a command
that committed; `count` measures returned summaries (one logged command can expand to several, a
no-op deposit decision to none), so the ledger widens its log window until it has `count`
summaries or the log is exhausted.

## OpenAPI schema & Swagger UI

The whole HTTP API is described as [tapir](https://tapir.softwaremill.com) endpoint values in
`multisig/server/HydrozoaRoutes.scala`, so the OpenAPI schema is **generated from the same
definitions that serve traffic** — it cannot drift from the routes. The running node serves:

- **Swagger UI** at `GET /docs` — interactive docs for every endpoint;
- the **raw spec** at `GET /docs/docs.yaml`.

A committed snapshot lives at [`openapi.yaml`](openapi.yaml); `OpenApiSchemaTest` regenerates it
from the endpoints and fails if the checked-in copy is stale, so the snapshot stays in sync.

## Demo

`multisig/server/L2QueryEndpointsTest` boots an in-memory `EutxoL2Ledger`, seeds it, builds the
real routes, and drives both endpoints over HTTP — asserting the responses and printing the JSON,
so the flow can be shown in a screen recording.

# L2 query endpoints: /l2/cardano-eutxo/utxos and /l2/cardano-eutxo/transactions

The user-facing HTTP server (`multisig/server/HydrozoaServer.scala`) exposes two read-only `GET`
endpoints for inspecting the L2 ledger's current state and recent activity. They are the
read counterpart to the existing write path (`POST /head/tx`, `POST /head/deposit`).

| | `GET /l2/cardano-eutxo/utxos/{address}` | `GET /l2/cardano-eutxo/transactions` |
|---|---|---|
| Answers | what does this address control on L2 now? | what happened on L2 recently? |
| Shape | array of CIP-0116 utxos | array of transaction summaries, newest first |
| Source | live L2 utxo set | the L2 ledger's own command log |

## EUTXO-only

These queries exist only for the **EUTXO reference ledger** (`EutxoL2Ledger`), which holds its
state locally and implements the read-only `EutxoL2LedgerReader` trait. A node wired to the
**remote** L2 ledger (`RemoteL2Ledger`, the SugarRush WebSocket) owns its state behind the black
box and exposes no query channel, so it has no such reader: the server is handed `None` and
**these two endpoints are not mounted at all** (a request to them is a `404`). Handing the server
only this narrow reader lets the HTTP layer read L2 state without being able to issue ledger
commands.

## GET /l2/cardano-eutxo/utxos/{address}

The current L2 utxos controlled by a bech32 `address`, serialized with the repo's CIP-0116
codecs. A malformed address is a `400`; an unknown-but-well-formed address is `200 []`.

```bash
curl http://localhost:8080/l2/cardano-eutxo/utxos/addr_test1vryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana6q4a064h
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

## GET /l2/cardano-eutxo/transactions

The most recent applied L2 transactions, newest first, as lightweight summaries. `?count=N`
bounds the number of **summaries** returned (default 50); a non-integer `count` is a `400`.

```bash
curl "http://localhost:8080/l2/cardano-eutxo/transactions?count=10"
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

## OpenAPI schema

The HTTP API is described as [tapir](https://tapir.softwaremill.com) endpoint values in
`multisig/server/HydrozoaRoutes.scala`, so the OpenAPI schema is **generated from the same
definitions that serve traffic** — it cannot drift from the routes. Because the L2 query endpoints
are EUTXO-only, they get their **own** document, separate from the core API:

- [`openapi.yaml`](openapi.yaml) — the core node API (always served); Swagger UI at `GET /docs`.
- [`openapi-eutxo-l2.yaml`](openapi-eutxo-l2.yaml) — these two EUTXO L2-query endpoints.

Both committed snapshots are pinned by `OpenApiSchemaTest`, which regenerates each from the
endpoint definitions and fails if the checked-in copy is stale, so they stay in sync.

## Demo

`multisig/server/L2QueryEndpointsTest` boots an in-memory `EutxoL2Ledger`, seeds it, builds the
real routes, and drives both endpoints over HTTP — asserting the responses and printing the JSON,
so the flow can be shown in a screen recording.

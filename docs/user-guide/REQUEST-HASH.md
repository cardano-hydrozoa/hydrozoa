# Computing a request hash

Every request you submit to a head carries `requestHash`, a digest **you** compute over the payloads
you are sending. The head re-derives it from the bytes it received and refuses the request if the two
differ. This guide gives you the construction, so your client can produce a value the head accepts.

You need this for both request kinds — [DEPOSIT.md](DEPOSIT.md) and [L2TXS.md](L2TXS.md) — because
`POST {headUri}/head/requests` rejects a body without it.

## Why the client computes it

A digest the head computes and hands back tells you what the head *thinks* it received, and you have
nothing to check it against except the same head's word. A digest you compute and the head confirms
is an end-to-end check on the bytes: it fails exactly when the request the head holds is not the
request you built — a truncated payload, a client-side encoding change, a mangled `l1Payload`.

The same value identifies your request afterwards. It appears beside every request in a block
(`requestHash` in the block view, `GET {headUri}/head/blocks/{n}`), so you can recognize your own
request in a block by matching the digest you already hold, without trusting anyone's arithmetic.

## The construction

`blake2b-256` over an ASCII domain tag, a one-byte variant tag, and the payloads:

```
requestHash = blake2b_256(
     "gummiworm-request-v1"
  || u8(variant)                                    -- 0x00 deposit, 0x01 transaction
  || deposit:     blake2b_256(l1Payload) || blake2b_256(l2Payload)
  || transaction: l2Payload
)
```

| Piece | Bytes |
| --- | --- |
| domain tag | `67756d6d69776f726d2d726571756573742d7631` (20 bytes, ASCII, no terminator) |
| variant tag | one byte: `00` for a deposit, `01` for a transaction |
| deposit payload | two 32-byte digests, `l1Payload`'s then `l2Payload`'s |
| transaction payload | `l2Payload` verbatim, to the end |

Three rules the layout depends on:

- **The variant tag leads the payload.** Without it, a transaction whose `l2Payload` happens to be a
  deposit's 64-byte pair of digests would hash identically to that deposit.
- **A deposit's payloads are hashed before being concatenated.** Hashing the raw bytes instead would
  collapse `hash(abc + def)` and `hash(ab + cdef)`, so two different deposits could share a digest.
- **The `requestId` is not in the preimage.** You have no request id when you compute this, and the
  same bytes hash the same however the head sequences them.

`l1Payload` is the **unsigned** deposit-tx CBOR — the same bytes you put in the request body, not the
signed transaction you later submit to L1.

## Worked examples

These are the vectors pinned in `src/test/scala/hydrozoa/multisig/consensus/UserRequestTest.scala`.
Reproduce them before trusting your implementation.

**A transaction.** With `l2Payload` =
`7b226163636f756e744964223a226363613363636361373232636630666134353961316433343831376235353563616538333735383539383033383030613362363166316361222c2264656c6567617465644b6579223a223537633861663235366135643632616238643933396466613232613264373933623037623034306538613831656638633963333861333932227d`:

```
requestHash = 58828159aaac6c4575395db0ea87f5e2a378c2e3f4c6e78b27d61eddbd2b1e85
```

**A deposit.** With that same `l2Payload` and the `l1Payload` from the same test, the two
intermediate digests are:

```
blake2b_256(l1Payload) = 75223e9148063e52d6ee97845ef1272936bb5abb9d3d3f33d39d5e50a32de00f
blake2b_256(l2Payload) = 0c0d70d38b29875722199523c082858278c6a2790faee29b6db6b16b06855790
requestHash            = ac596c7fb689a6e4757fb6c782580d7cc2ca0ff812ae872753be1292063a28bb
```

If your transaction digest matches but your deposit digest does not, check the variant tag and the
order of the two intermediate digests.

## Submitting it

`requestHash` is a required field of the submit body, lowercase hex, 32 bytes:

```json
{
  "type": "transaction",
  "l2Payload": "84a400d9010281825820…",
  "requestHash": "58828159aaac6c4575395db0ea87f5e2a378c2e3f4c6e78b27d61eddbd2b1e85"
}
```

```json
{
  "type": "deposit",
  "l1Payload": "84a600d9010281825820…",
  "l2Payload": "a1024568656164…",
  "requestHash": "ac596c7fb689a6e4757fb6c782580d7cc2ca0ff812ae872753be1292063a28bb"
}
```

## When it does not match

The head refuses the request and names both digests:

```
requestHash does not match the submitted body: submitted=<yours>, derived=<the head's>
```

Nothing is stored and no request id is consumed, so fix the request and submit again. The head never
corrects the digest for you: a mismatch means the two of you disagree about what was submitted, and
there is no version of the request it would be safe to accept.

## Worked implementation

`hydrozoa submit-deposit` and `hydrozoa submit-l2-tx` compute it through
`UserRequestBody.mkHash` (`src/main/scala/hydrozoa/multisig/consensus/UserRequest.scala`), which is the
same function the head runs to verify. Read it alongside this guide.

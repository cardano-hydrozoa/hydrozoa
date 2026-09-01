package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import java.nio.ByteBuffer

/** The five recovery markers (§5.2), derived from a [[BackendStore]] at boot time.
  *
  * No marker is stored explicitly — each falls out of a single-CF scan:
  *
  *   - `softConfirmed     = max(SoftConfirmation.key)`
  *   - `fastBlockMark     = max(BlockResult.key)`
  *   - `hardConfirmed     = max(HardConfirmation.key)`
  *   - `hardAcked         = max(HardAck.hardAckNum where peer == own)`
  *   - `nextRequestNumber = max(own Request.key) + 1` (`RequestNumber(0)` cold, or on a coil peer —
  *     it assigns no user requests)
  *
  * `Markers.derive(backend, own)` runs the five reads (in parallel where possible) and returns a
  * fresh [[Markers]] value. Lives in a separate module — not on [[Persistence]] — because marker
  * derivation is intrinsically byte-level (uses `lastKey`) and is a recovery concern, not a
  * per-operation concern.
  */
final case class Markers(
    softConfirmed: Option[BlockNumber],
    fastBlockMark: Option[BlockNumber],
    hardConfirmed: Option[StackNumber],
    hardAcked: Option[HardAckNumber],
    nextRequestNumber: RequestNumber,
    evacuationMapMark: Option[BlockNumber],
    /** The stack this peer's last own hard-ack covers — `hardAcked` dereferenced into the journal
      * and unpacked. Unlike the six marks around it this is an *interpretation*, not a `lastKey`,
      * which is exactly why it belongs here: derived twice it can be adjusted once and disagree
      * everywhere. `None` on an empty own-ack journal.
      */
    hardAckedStack: Option[StackNumber]
)

object Markers:
    /** The marker set of an empty store: every anchor absent, the request counter at zero. What
      * [[derive]] returns for a cold store, spelled out so a caller that has no store to derive
      * from (a test wiring an actor against an empty backend) need not fabricate one.
      */
    val cold: Markers = Markers(None, None, None, None, RequestNumber(0), None, None)

    /** Read all five markers from `backend`, scoping the `hardAcked` and `nextRequestNumber`
      * derivations to `own`. With the per-author CF split each satellite CF holds exactly one
      * author's journal, so the own `hardAcked` mark is just `lastKey` of the own-author `HardAck`
      * CF — no prefix scan (§7.1). `own` is a [[PeerId]] (head or coil): the one `HardAck` journal
      * covers both peer types, and `nextRequestNumber` is `RequestNumber(0)` on a coil peer (the
      * user-request surface is head-only).
      */
    def derive(persistence: Persistence[IO], own: PeerId)(using
        CardanoNetwork.Section
    ): IO[Markers] =
        val backend = persistence.backend
        val nextRequest = own match
            case PeerId.Head(n) => recoverNextRequestNumber(backend, n)
            case PeerId.Coil(_) => IO.pure(RequestNumber(0))
        for {
            base <- (
              backend.lastKey(Cf.SoftConfirmation).map(_.map(decodeBlockNum)),
              recoverFastBlockMark(backend),
              backend.lastKey(Cf.HardConfirmation).map(_.map(decodeStackNum)),
              backend.lastKey(Cf.HardAck(own)).map(_.map(decodeSatelliteNumHard)),
              nextRequest,
              recoverEvacuationMapMark(backend)
            ).parTupled
            (soft, fast, hardConf, hardAck, nextReq, evacMark) = base
            // Sequenced after the parallel block: it is keyed BY `hardAcked`, so it cannot be read
            // alongside the mark it depends on.
            ackedStack <- hardAck.traverse(n =>
                persistence.getOrFail(JournalKey.HardAck(own, n)).map(_.payload.stackNum)
            )
        } yield Markers(soft, fast, hardConf, hardAck, nextReq, evacMark, ackedStack)

    /** The next request number this peer will assign after recovery: `max(own Request) + 1`, or
      * `RequestNumber(0)` for an empty store — the last key of the own-author `Request` CF (an
      * 8-byte index). RequestSequencer seeds its counter with this on boot (R3).
      */
    def recoverNextRequestNumber(
        backend: BackendStore[IO],
        own: HeadPeerNumber
    ): IO[RequestNumber] =
        backend
            .lastKey(Cf.Request(own))
            .map(_.fold(RequestNumber(0))(decodeSatelliteNumRequest(_).increment))

    /** The fast-side recovery anchor: `fastBlockMark = max(BlockResult.key)`, the highest block
      * this peer durably finalized, or `None` for an empty store — the same on head and coil peers
      * (the `BlockResult` CF is written every block by every peer, §6; on a head peer it equals
      * `max(own SoftAck)`, both written in the same atomic per-block batch, and a coil peer authors
      * no soft-ack). `JointLedger` and `ReplayActor` read it on boot.
      */
    def recoverFastBlockMark(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.BlockResult).map(_.map(decodeBlockNum))

    /** The highest block at which a cumulative evacuation map is durably stored, or `None` when the
      * slow side has closed no stack yet (the map is then the head config's initial one).
      *
      * `Cf.EvacuationMap` is written by `StackComposer` at stack close, only at blocks whose map
      * backs an on-chain KZG commitment ([[hydrozoa.multisig.persistence.StoreKey.EvacuationMap]]),
      * so it is sparse and lags [[recoverFastBlockMark]] — the slow side never runs ahead of the
      * fast one. `JointLedger` reads it on boot as the base to fold the remaining blocks'
      * `evacuationMapDiff`s onto, to reach the map at the fast anchor.
      */
    def recoverEvacuationMapMark(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.EvacuationMap).map(_.map(decodeBlockNum))

    /** The slow-side anchor: `hardAcked = max(own HardAck.key)` — this peer's last own hard-ack
      * number, or `None` for an empty store. Works for both peer types: `own` is a [[PeerId]], so a
      * head peer reads its head `HardAck` CF and a coil peer reads its coil `HardAck` CF (the stack
      * number is unpacked from the hard-ack value, §6 `StackComposer`). Exposed standalone so the
      * unified boot replay reads just this mark without the broader [[derive]] (which also re-scans
      * the soft/hard-confirmation spines the caller already holds).
      */
    def recoverHardAcked(
        backend: BackendStore[IO],
        own: PeerId
    ): IO[Option[HardAckNumber]] =
        backend.lastKey(Cf.HardAck(own)).map(_.map(decodeSatelliteNumHard))

    /** `softConfirmed = max(SoftConfirmation.key)` — the fast-side confirmation mark, written by
      * `FastConsensusActor` when a consensus cell saturates. Standalone like the other anchors, so
      * a consumer needing only this mark skips the full [[Markers]] bundle — and its parallel
      * reads, which a caller comparing two marks against each other must avoid.
      */
    def recoverSoftConfirmed(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.SoftConfirmation).map(_.map(decodeBlockNum))

    /** `hardConfirmed = max(HardConfirmation.key)` — the `HardConfirmation` CF is keyed by
      * `StackNumber`, written at confirmation by every peer (§6 `SlowConsensusActor`). Exposed
      * standalone so a consumer that needs only this mark (e.g. `StackComposer`'s recover) reads it
      * without deriving the full [[Markers]] bundle.
      */
    def recoverHardConfirmed(backend: BackendStore[IO]): IO[Option[StackNumber]] =
        backend.lastKey(Cf.HardConfirmation).map(_.map(decodeStackNum))

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `BlockNumber`. */
    private def decodeBlockNum(bytes: Array[Byte]): BlockNumber =
        requireWidth(bytes, 4, "BlockNumber")
        BlockNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `StackNumber`. */
    private def decodeStackNum(bytes: Array[Byte]): StackNumber =
        requireWidth(bytes, 4, "StackNumber")
        StackNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `HardAckNumber` from a per-author satellite key `[num:4]`. */
    private def decodeSatelliteNumHard(bytes: Array[Byte]): HardAckNumber =
        requireWidth(bytes, 4, "HardAck key")
        HardAckNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `RequestNumber` from a per-author Request key `[requestNum:8]` — the Request journal
      * uses an 8-byte index, unlike the 4-byte soft/hard-ack indices.
      */
    private def decodeSatelliteNumRequest(bytes: Array[Byte]): RequestNumber =
        requireWidth(bytes, 8, "Request key")
        RequestNumber(ByteBuffer.wrap(bytes).getLong)

    private def requireWidth(bytes: Array[Byte], expected: Int, label: String): Unit =
        if bytes.length != expected then
            throw new IllegalArgumentException(
              s"$label expected $expected bytes, got ${bytes.length}"
            )

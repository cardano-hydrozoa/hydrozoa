package hydrozoa.multisig.persistence

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.persistence.PersistenceEvent.*
import java.nio.ByteBuffer
import java.time.Instant

/** The **typed, actor-facing** persistence API.
  *
  * Every operation is expressed in terms of [[StoreKey]] and the path-dependent `key.Value` type —
  * no raw `Array[Byte]`s and no `Cf` references appear at the call site. A `Persistence` instance
  * sits on top of a [[BackendStore]] and runs each key's `encodeValue` / `decodeValue` to bridge
  * the two layers. Specialised modules (`Markers`, [[StoreDump]]) reach for the underlying
  * `BackendStore` directly when their work is intrinsically byte-level.
  *
  * Atomic multi-key writes go through [[WriteBatch]] — the same typed key + typed value pattern,
  * lowered to the backend's `RawWriteBatch` at `write` time.
  *
  * The `CardanoNetwork.Section` used by `Persistence.fromBackend` is **captured once** at
  * construction time and threaded into every [[StoreKey]] codec call — config-dependent codecs
  * (e.g. those involving `QuantizedInstant` or `Payout.Obligation`) pick it up implicitly so the
  * `Persistence` API itself stays config-free at the call site.
  *
  * **Tracing.** Every op (get / put / delete / write) is emitted as a [[PersistenceEvent]] through
  * the supplied [[ContraTracer]]. The default human format routes under `"Persistence"`.
  *
  * See `design/persistence-and-crash-recovery.md` §7.
  */
trait Persistence[F[_]]:
    /** Read the typed value at `key`. `None` if absent. */
    def get(key: StoreKey): F[Option[key.Value]]

    /** Write `value` at `key`. Overwrites any existing value. */
    def put(key: StoreKey)(value: key.Value): F[Unit]

    /** Delete the entry at `key`. No-op if absent. */
    def delete(key: StoreKey): F[Unit]

    /** Commit a typed [[WriteBatch]] atomically — all operations land or none, even across CFs
      * (CR4/CR6/CR8).
      */
    def write(batch: WriteBatch): F[Unit]

    /** A fresh [[ArrivalStamp]] for an entry admitted *now*: `(this process's generation, current
      * monotonic)`. Used to stamp journal values (§5.4) — own entries at creation, inbound at
      * receipt.
      */
    def arrivalStamp: F[ArrivalStamp]

    /** Like [[get]] but **fails** (`raiseError`) when the key is absent — for reads that must be
      * present. Recovery uses it for entries that, in a non-empty store, are store corruption when
      * missing (fail-safe, §5 / §7).
      */
    def getOrFail(key: StoreKey): F[key.Value]

    /** The underlying byte-level [[BackendStore]] this instance wraps — an escape hatch for
      * **byte-level / boot-time** work the typed API can't express (recovery range-scans via
      * `cursor`, marker derivation; §5). Steady-state actor code never needs it.
      */
    def backend: BackendStore[F]

    /** The wall-clock anchor for each arrival-stamp generation: `generation → epoch-nanos at that
      * generation's monotonic zero`. Lets any [[ArrivalStamp]] be converted to wall-clock time via
      * [[wallClockOf]] without ever persisting a wall clock — only monotonic stamps and this anchor
      * are stored. Captured once per generation at [[Persistence.fromBackend]].
      */
    def zeroTimes: F[Map[Int, Long]]

    /** Convert an arrival stamp to the wall-clock instant it was recorded at, using this
      * generation's [[zeroTimes]] anchor (`wall = zeroTime[generation] + monotonicNanos`). `None`
      * if the generation has no anchor (an entry written before anchors existed).
      */
    def wallClockOf(stamp: ArrivalStamp): F[Option[Instant]]

object Persistence:
    /** The arrival-stamp generation counter's key in `Cf.Meta` (a 4-byte big-endian `Int`). */
    private val generationKey: Array[Byte] = "arrival-generation".getBytes("UTF-8")

    /** The per-generation zero-time anchor map's key in `Cf.Meta`. */
    private val zeroTimesKey: Array[Byte] = "arrival-zero-times".getBytes("UTF-8")

    /** The per-process arrival-stamp generation: read the persisted counter, increment it, write it
      * back, and return the new value. Bumped **once per store-open per process** so stamps from a
      * later process always sort after earlier ones across restarts (§5.4, [[ArrivalStamp]]).
      */
    private def bumpGeneration(backend: BackendStore[IO]): IO[Int] =
        for
            prev <- backend.get(Cf.Meta, generationKey)
            next = prev.map(b => ByteBuffer.wrap(b).getInt).getOrElse(0) + 1
            _ <- backend.put(Cf.Meta, generationKey, ByteBuffer.allocate(4).putInt(next).array())
        yield next

    /** Nanoseconds in a second — the scale bridging `Instant` (seconds + nanos) and the epoch-nanos
      * zero-time anchor.
      */
    private val nanosPerSecond: Long = 1_000_000_000L

    private def instantToEpochNanos(t: Instant): Long =
        t.getEpochSecond * nanosPerSecond + t.getNano

    private def epochNanosToInstant(nanos: Long): Instant =
        Instant.ofEpochSecond(
          Math.floorDiv(nanos, nanosPerSecond),
          Math.floorMod(nanos, nanosPerSecond)
        )

    /** Read the persisted `generation → epoch-nanos` anchor map (JSON in `Cf.Meta`). Empty when the
      * store has never recorded one.
      */
    private def readZeroTimes(backend: BackendStore[IO]): IO[Map[Int, Long]] =
        backend.get(Cf.Meta, zeroTimesKey).map {
            case None => Map.empty
            case Some(bytes) =>
                io.circe.parser
                    .decode[Map[String, Long]](new String(bytes, "UTF-8"))
                    .getOrElse(Map.empty)
                    .map((g, n) => g.toInt -> n)
        }

    /** Anchor `generation` to now: record `zeroTime = epochNanos(realTime) − monotonicNanos`, the
      * wall-clock instant this generation's monotonic clock reads zero at, so any later stamp
      * `(generation, m)` converts to `zeroTime + m`. Sampled once at open, merged into the map.
      */
    private def recordZeroTime(backend: BackendStore[IO], generation: Int): IO[Map[Int, Long]] =
        for
            realTime <- IO.realTimeInstant
            monotonic <- IO.monotonic
            zeroTime = instantToEpochNanos(realTime) - monotonic.toNanos
            existing <- readZeroTimes(backend)
            merged = existing.updated(generation, zeroTime)
            json = io.circe.Json
                .obj(merged.map((g, n) => g.toString -> io.circe.Json.fromLong(n)).toSeq*)
                .noSpaces
            _ <- backend.put(Cf.Meta, zeroTimesKey, json.getBytes("UTF-8"))
        yield merged

    /** The standard `Persistence` implementation — wraps a [[BackendStore]] and threads keys /
      * values through each [[StoreKey]]'s codec on the way through. Bumps the arrival-stamp
      * generation once at open ([[bumpGeneration]]) so the returned instance's
      * [[Persistence.arrivalStamp]]s are durably ordered across restarts.
      *
      * Takes the [[CardanoNetwork.Section]] once and makes it implicitly available to every codec
      * invocation (`encodeValue` / `decodeValue` / `WriteBatch.toRaw`).
      */
    def fromBackend(
        store: BackendStore[IO],
        tracer: ContraTracer[IO, PersistenceEvent]
    )(using CardanoNetwork.Section): IO[Persistence[IO]] =
        for
            generation <- bumpGeneration(store)
            anchors <- recordZeroTime(store, generation)
        yield new Persistence[IO]:
            val backend: BackendStore[IO] = store

            def arrivalStamp: IO[ArrivalStamp] =
                IO.monotonic.map(m => ArrivalStamp(generation, m.toNanos))

            def zeroTimes: IO[Map[Int, Long]] = IO.pure(anchors)

            def wallClockOf(stamp: ArrivalStamp): IO[Option[Instant]] =
                IO.pure(
                  anchors
                      .get(stamp.generation)
                      .map(zero => epochNanosToInstant(zero + stamp.monotonicNanos))
                )

            def get(key: StoreKey): IO[Option[key.Value]] =
                backend
                    .get(key.cf, key.encode)
                    .flatTap(bytesOpt => tracer.traceWith(Get(key, bytesOpt.isDefined)))
                    .map(_.map(key.decodeValue))

            def getOrFail(key: StoreKey): IO[key.Value] =
                get(key).flatMap {
                    case Some(value) => IO.pure(value)
                    case None =>
                        IO.raiseError(
                          new IllegalStateException(s"Persistence.getOrFail: no value at $key")
                        )
                }

            def put(key: StoreKey)(value: key.Value): IO[Unit] =
                backend.put(key.cf, key.encode, key.encodeValue(value)) *>
                    tracer.traceWith(Put(key))

            def delete(key: StoreKey): IO[Unit] =
                backend.delete(key.cf, key.encode) *> tracer.traceWith(Delete(key))

            def write(batch: WriteBatch): IO[Unit] =
                backend.write(batch.toRaw) *> tracer.traceWith(Write(batch.size))

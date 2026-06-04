package hydrozoa.multisig.persistence

import cats.effect.{IO, IOLocal}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Tracer
import java.nio.ByteBuffer

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
  * **Logging.** Every op (get / put / delete / write) is traced at INFO with explicit
  * `routingKey = "Persistence"`, so log lines route to the `"Persistence"` logger in `logback.xml`
  * regardless of the ambient actor's [[Tracer.routeLocal]].
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
      * monotonic)`. Used to stamp lane values (§5.4) — own entries at creation, inbound at receipt.
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

object Persistence:
    /** Force the routing key to `"Persistence"` for the duration of `fa`, so every `Tracer.info` /
      * `Tracer.debug` inside lands on the `"Persistence"` Logback logger regardless of the calling
      * actor's `Tracer.routeLocal`. Restores the prior tracer on exit — does not corrupt the
      * caller's per-fiber routing.
      */
    private def withRoute[A](fa: IO[A])(using IOLocal[Tracer]): IO[A] =
        Tracer.scoped(_.copy(routingKey = Some("Persistence")))(fa)

    /** The arrival-stamp generation counter's key in `Cf.Meta` (a 4-byte big-endian `Int`). */
    private val generationKey: Array[Byte] = "arrival-generation".getBytes("UTF-8")

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

    /** The standard `Persistence` implementation — wraps a [[BackendStore]] and threads keys /
      * values through each [[StoreKey]]'s codec on the way through. Bumps the arrival-stamp
      * generation once at open ([[bumpGeneration]]) so the returned instance's
      * [[Persistence.arrivalStamp]]s are durably ordered across restarts.
      *
      * Takes the [[CardanoNetwork.Section]] once and makes it implicitly available to every codec
      * invocation (`encodeValue` / `decodeValue` / `WriteBatch.toRaw`). Also captures the ambient
      * [[IOLocal]][[Tracer]] for per-op INFO trace lines (see the trait docstring); per-op trace
      * lines are scoped through [[withRoute]] so they always route to the `"Persistence"` logger.
      */
    def fromBackend(
        store: BackendStore[IO]
    )(using CardanoNetwork.Section, IOLocal[Tracer]): IO[Persistence[IO]] =
        for generation <- bumpGeneration(store)
        yield new Persistence[IO]:
            val backend: BackendStore[IO] = store

            def arrivalStamp: IO[ArrivalStamp] =
                IO.monotonic.map(m => ArrivalStamp(generation, m.toNanos))

            def get(key: StoreKey): IO[Option[key.Value]] =
                withRoute(
                  backend
                      .get(key.cf, key.encode)
                      .flatTap(bytesOpt =>
                          Tracer.info(
                            s"get $key -> ${if bytesOpt.isDefined then "hit" else "miss"}"
                          )
                      )
                      .map(_.map(key.decodeValue))
                )

            def getOrFail(key: StoreKey): IO[key.Value] =
                get(key).flatMap {
                    case Some(value) => IO.pure(value)
                    case None =>
                        IO.raiseError(
                          new IllegalStateException(s"Persistence.getOrFail: no value at $key")
                        )
                }

            def put(key: StoreKey)(value: key.Value): IO[Unit] =
                withRoute(
                  backend.put(key.cf, key.encode, key.encodeValue(value)) *>
                      Tracer.info(s"put $key")
                )

            def delete(key: StoreKey): IO[Unit] =
                withRoute(
                  backend.delete(key.cf, key.encode) *> Tracer.info(s"delete $key")
                )

            def write(batch: WriteBatch): IO[Unit] =
                withRoute(
                  backend.write(batch.toRaw) *> Tracer.info(s"write batch (${batch.size} ops)")
                )

package hydrozoa.multisig.persistence

import cats.effect.{IO, IOLocal}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Tracer

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

object Persistence:
    /** Force the routing key to `"Persistence"` for the duration of `fa`, so every
      * `Tracer.info` / `Tracer.debug` inside lands on the `"Persistence"` Logback logger
      * regardless of the calling actor's `Tracer.routeLocal`. Restores the prior tracer on exit
      * — does not corrupt the caller's per-fiber routing.
      */
    private def withRoute[A](fa: IO[A])(using IOLocal[Tracer]): IO[A] =
        Tracer.scoped(_.copy(routingKey = Some("Persistence")))(fa)

    /** The standard `Persistence` implementation — wraps a [[BackendStore]] and threads keys /
      * values through each [[StoreKey]]'s codec on the way through.
      *
      * Takes the [[CardanoNetwork.Section]] once and makes it implicitly available to every codec
      * invocation (`encodeValue` / `decodeValue` / `WriteBatch.toRaw`). Also captures the ambient
      * [[IOLocal]][[Tracer]] for per-op INFO trace lines (see the trait docstring); per-op trace
      * lines are scoped through [[withRoute]] so they always route to the `"Persistence"` logger.
      */
    def fromBackend(
        backend: BackendStore[IO]
    )(using CardanoNetwork.Section, IOLocal[Tracer]): Persistence[IO] =
        new Persistence[IO]:
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

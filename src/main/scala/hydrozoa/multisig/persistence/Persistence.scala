package hydrozoa.multisig.persistence

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
    /** The standard `Persistence` implementation — wraps a [[BackendStore]] and threads keys /
      * values through each [[StoreKey]]'s codec on the way through.
      */
    def fromBackend[F[_]: cats.Functor](backend: BackendStore[F]): Persistence[F] =
        new Persistence[F]:
            def get(key: StoreKey): F[Option[key.Value]] =
                cats.Functor[F].map(backend.get(key.cf, key.encode))(_.map(key.decodeValue))

            def put(key: StoreKey)(value: key.Value): F[Unit] =
                backend.put(key.cf, key.encode, key.encodeValue(value))

            def delete(key: StoreKey): F[Unit] =
                backend.delete(key.cf, key.encode)

            def write(batch: WriteBatch): F[Unit] =
                backend.write(batch.toRaw)

package hydrozoa.multisig.persistence

import java.nio.file.Path

/** Typed events emitted by [[Persistence]] and its [[BackendStore]] implementations
  * ([[InMemoryBackendStore]], [[hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore]]). Pure
  * data; formatters in [[PersistenceEventFormat]] decide how each variant is rendered to a
  * particular sink.
  */
sealed trait PersistenceEvent

object PersistenceEvent:

    // ---- BackendStore open lifecycle ----

    /** An [[InMemoryBackendStore]] is being opened. */
    case object OpenInMemoryStart extends PersistenceEvent

    /** The in-memory backend finished opening with [[cfCount]] column families. */
    final case class OpenInMemoryReady(cfCount: Int) extends PersistenceEvent

    /** A RocksDB backend is being opened at [[path]]. */
    final case class OpenRocksDbStart(path: Path) extends PersistenceEvent

    /** The RocksDB backend at [[path]] finished opening with [[cfCount]] column families. */
    final case class OpenRocksDbReady(path: Path, cfCount: Int) extends PersistenceEvent

    // ---- Persistence operations ----

    /** A typed read of [[key]]; [[hit]] is true when the backend returned a value. */
    final case class Get(key: StoreKey, hit: Boolean) extends PersistenceEvent

    /** A typed write at [[key]]. */
    final case class Put(key: StoreKey) extends PersistenceEvent

    /** A typed delete at [[key]]. */
    final case class Delete(key: StoreKey) extends PersistenceEvent

    /** An atomic typed write batch committed [[ops]] operations. */
    final case class Write(ops: Int) extends PersistenceEvent

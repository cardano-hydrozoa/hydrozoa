package hydrozoa.multisig.persistence

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.persistence.PersistenceEvent.*

/** Renderers from [[PersistenceEvent]] to [[LogEvent]]. */
object PersistenceEventFormat:

    /** Routes under `Persistence` — matches the existing Logback config for this subsystem. */
    def humanFormat(e: PersistenceEvent): LogEvent = {
        val ev = LogEvent.From(Map.empty, "Persistence")
        import ev.*
        e match {
            case OpenInMemoryStart =>
                info("opening in-memory backend")
            case OpenInMemoryReady(cfCount) =>
                info(s"in-memory backend ready (CFs=$cfCount)")
            case OpenRocksDbStart(path) =>
                info(s"opening RocksDB backend at $path")
            case OpenRocksDbReady(path, cfCount) =>
                info(s"RocksDB backend at $path ready (CFs=$cfCount)")
            case Get(key, hit) =>
                info(s"get $key -> ${if hit then "hit" else "miss"}")
            case Put(key) =>
                info(s"put $key")
            case Delete(key) =>
                info(s"delete $key")
            case Write(ops) =>
                info(s"write batch ($ops ops)")
        }
    }

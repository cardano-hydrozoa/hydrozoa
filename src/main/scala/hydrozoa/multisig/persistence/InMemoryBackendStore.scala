package hydrozoa.multisig.persistence

import cats.effect.{IO, Ref, Resource}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.persistence.PersistenceEvent.{OpenInMemoryReady, OpenInMemoryStart}
import scala.collection.immutable.TreeMap

/** An in-memory [[BackendStore]] for tests. Mirrors the RocksDB contract (atomic batches across
  * CFs; range-scan / `lastKey` semantics; byte-lexicographic key order) using `Ref[IO, …]` and a
  * sorted map keyed by `Vector[Byte]` (so comparisons match RocksDB's unsigned-byte lex order).
  *
  * **Not durable.** Disposing the instance loses everything. Use [[rocksdb.RocksDbBackendStore]]
  * for production / on-disk persistence.
  */
object InMemoryBackendStore:

    private given byteVectorOrdering: Ordering[Vector[Byte]] =
        Ordering.Implicits.seqOrdering(
          Ordering.fromLessThan[Byte]((a, b) => (a & 0xff) < (b & 0xff))
        )

    /** Open a fresh in-memory store. Resource-managed for parity with the RocksDB factory; the
      * release is a no-op (no native handles to free).
      *
      * Unlike RocksDB, this backend is **CF-agnostic**: it does not pre-declare a CF set, treating
      * any [[Cf]] as an empty keyspace until first written. So it needs no config-derived CF list
      * (§7.1, the per-author split) — a CF the code touches simply springs into being.
      */
    def open(tracer: ContraTracer[IO, PersistenceEvent]): Resource[IO, BackendStore[IO]] =
        Resource.eval(
          for
              _ <- tracer.traceWith(OpenInMemoryStart)
              state <- Ref.of[IO, Map[Cf, TreeMap[Vector[Byte], Array[Byte]]]](Map.empty)
              _ <- tracer.traceWith(OpenInMemoryReady(Cf.fixed.size))
          yield new Impl(state)
        )

    private val emptyTree: TreeMap[Vector[Byte], Array[Byte]] =
        TreeMap.empty[Vector[Byte], Array[Byte]]

    private final class Impl(state: Ref[IO, Map[Cf, TreeMap[Vector[Byte], Array[Byte]]]])
        extends BackendStore[IO]:

        private def tree(
            s: Map[Cf, TreeMap[Vector[Byte], Array[Byte]]],
            cf: Cf
        ): TreeMap[Vector[Byte], Array[Byte]] = s.getOrElse(cf, emptyTree)

        def get(cf: Cf, key: Array[Byte]): IO[Option[Array[Byte]]] =
            state.get.map(s => tree(s, cf).get(key.toVector))

        def put(cf: Cf, key: Array[Byte], value: Array[Byte]): IO[Unit] =
            state.update(s => s.updated(cf, tree(s, cf).updated(key.toVector, value)))

        def delete(cf: Cf, key: Array[Byte]): IO[Unit] =
            state.update(s => s.updated(cf, tree(s, cf).removed(key.toVector)))

        def write(batch: RawWriteBatch): IO[Unit] =
            if batch.isEmpty then IO.unit
            else
                state.update(s =>
                    batch.ops.foldLeft(s) { (acc, op) =>
                        op match
                            case RawWriteBatch.Op.Put(cf, k, v) =>
                                acc.updated(cf, tree(acc, cf).updated(k.toVector, v))
                            case RawWriteBatch.Op.Delete(cf, k) =>
                                acc.updated(cf, tree(acc, cf).removed(k.toVector))
                            case RawWriteBatch.Op.DeleteRange(cf, from, to) =>
                                val fromV = from.toVector
                                val toV = to.toVector
                                val t = tree(acc, cf)
                                val pruned = t.rangeUntil(toV).rangeFrom(fromV)
                                acc.updated(cf, t -- pruned.keys)
                    }
                )

        def cursor(
            cf: Cf,
            fromInclusive: Array[Byte]
        ): Resource[IO, BackendStore.Cursor[IO]] =
            Resource.eval(
              for
                  s <- state.get
                  remaining <- Ref.of[IO, List[(Array[Byte], Array[Byte])]](
                    tree(s, cf)
                        .rangeFrom(fromInclusive.toVector)
                        .iterator
                        .map { case (k, v) =>
                            (k.toArray, v)
                        }
                        .toList
                  )
              yield new BackendStore.Cursor[IO]:
                  def next: IO[Option[(Array[Byte], Array[Byte])]] =
                      remaining.modify {
                          case Nil       => (Nil, None)
                          case h :: tail => (tail, Some(h))
                      }
            )

        def lastKey(cf: Cf): IO[Option[Array[Byte]]] =
            state.get.map(s => tree(s, cf).lastOption.map(_._1.toArray))

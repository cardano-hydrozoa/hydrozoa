package hydrozoa.multisig.persistence

import cats.effect.{IO, Ref, Resource}
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
        Ordering.Implicits.seqOrdering(Ordering.fromLessThan[Byte]((a, b) =>
            (a & 0xff) < (b & 0xff)
        ))

    /** Open a fresh in-memory store. Resource-managed for parity with the RocksDB factory; the
      * release is a no-op (no native handles to free).
      */
    def open: Resource[IO, BackendStore[IO]] =
        Resource.eval(
          Ref
              .of[IO, Map[Cf, TreeMap[Vector[Byte], Array[Byte]]]](
                Cf.all.iterator.map(cf => cf -> TreeMap.empty[Vector[Byte], Array[Byte]]).toMap
              )
              .map(new Impl(_))
        )

    private final class Impl(state: Ref[IO, Map[Cf, TreeMap[Vector[Byte], Array[Byte]]]])
        extends BackendStore[IO]:

        def get(cf: Cf, key: Array[Byte]): IO[Option[Array[Byte]]] =
            state.get.map(_(cf).get(key.toVector))

        def put(cf: Cf, key: Array[Byte], value: Array[Byte]): IO[Unit] =
            state.update(s => s.updated(cf, s(cf).updated(key.toVector, value)))

        def delete(cf: Cf, key: Array[Byte]): IO[Unit] =
            state.update(s => s.updated(cf, s(cf).removed(key.toVector)))

        def write(batch: RawWriteBatch): IO[Unit] =
            if batch.isEmpty then IO.unit
            else
                state.update(s =>
                    batch.ops.foldLeft(s) { (acc, op) =>
                        op match
                            case RawWriteBatch.Op.Put(cf, k, v) =>
                                acc.updated(cf, acc(cf).updated(k.toVector, v))
                            case RawWriteBatch.Op.Delete(cf, k) =>
                                acc.updated(cf, acc(cf).removed(k.toVector))
                            case RawWriteBatch.Op.DeleteRange(cf, from, to) =>
                                val fromV = from.toVector
                                val toV = to.toVector
                                val pruned = acc(cf).rangeUntil(toV).rangeFrom(fromV)
                                acc.updated(cf, acc(cf) -- pruned.keys)
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
                    s(cf).rangeFrom(fromInclusive.toVector).iterator.map { case (k, v) =>
                        (k.toArray, v)
                    }.toList
                  )
              yield new BackendStore.Cursor[IO]:
                  def next: IO[Option[(Array[Byte], Array[Byte])]] =
                      remaining.modify {
                          case Nil       => (Nil, None)
                          case h :: tail => (tail, Some(h))
                      }
            )

        def lastKey(cf: Cf): IO[Option[Array[Byte]]] =
            state.get.map(_(cf).lastOption.map(_._1.toArray))

        def lastKeyWithPrefix(cf: Cf, prefix: Array[Byte]): IO[Option[Array[Byte]]] =
            state.get.map { s =>
                val prefixV = prefix.toVector
                s(cf).iterator
                    .filter { case (k, _) =>
                        k.length >= prefixV.length && k.take(prefixV.length) == prefixV
                    }
                    .map(_._1)
                    .reduceOption((a, b) => if byteVectorOrdering.gt(a, b) then a else b)
                    .map(_.toArray)
            }

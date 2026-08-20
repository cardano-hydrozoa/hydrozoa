package hydrozoa.integration.harness

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.multisig.persistence.{ArrivalStamp, BackendStore, Persistence, StoreKey, WriteBatch}
import java.time.Instant

/** A [[Persistence]] decorator that yields the fiber a fixed number of times before each durable
  * write, making one peer's store slow relative to the rest of the node.
  *
  * It exists so a durability-ordering race can be tested deterministically. A race between two
  * fibers of the same peer — one writing a confirmation, the other writing the block result it must
  * not outrun — does not reproduce under [[cats.effect.testkit.TestControl]]: the scheduler is
  * single-threaded and the writing fiber runs straight through to its own write, so the losing
  * interleaving is never sampled. On a real multi-core runtime it is sampled all the time, which is
  * why the bug reached a production fleet and not the test suite.
  *
  * Yielding before the write closes that gap without pretending to control the scheduler: it hands
  * every other eligible fiber a turn at exactly the point where a slow disk would. A node whose
  * write ordering is correct is unaffected, because correct ordering means the write that must come
  * first has already happened — no amount of yielding can let the other one overtake it. A node
  * whose ordering depends on winning a race fails, deterministically and on every run.
  *
  * `yields` is a count of `IO.cede`s rather than a sleep so it costs no virtual time, which keeps
  * the surrounding scenario's timing intact.
  */
object SlowPersistence:

    def wrap(inner: Persistence[IO], yields: Int): Persistence[IO] =
        new Persistence[IO]:
            private val slow: IO[Unit] = List.fill(yields)(IO.cede).sequence_

            def put(key: StoreKey)(value: key.Value): IO[Unit] = slow >> inner.put(key)(value)

            def delete(key: StoreKey): IO[Unit] = slow >> inner.delete(key)

            def write(batch: WriteBatch): IO[Unit] = slow >> inner.write(batch)

            def get(key: StoreKey): IO[Option[key.Value]] = inner.get(key)

            def getOrFail(key: StoreKey): IO[key.Value] = inner.getOrFail(key)

            def arrivalStamp: IO[ArrivalStamp] = inner.arrivalStamp

            def zeroTimes: IO[Map[Int, Long]] = inner.zeroTimes

            def wallClockOf(stamp: ArrivalStamp): IO[Instant] = inner.wallClockOf(stamp)

            def backend: BackendStore[IO] = inner.backend

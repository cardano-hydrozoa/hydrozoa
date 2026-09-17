package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.contravariant.*
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalatest.funsuite.AnyFunSuite

/** [[BackendStore.wipeData]] — what a coil peer being seeded does to its own store.
  *
  * ⚠️ **`Cf.Meta` must survive.** It holds the schema version and the [[StoreIdentity]] stamp
  * checked at open, which is what binds a store to one peer in one head. Wiping those would turn a
  * reseed into a store any peer could adopt, and the check that exists to catch a peer running on
  * another peer's journals would have nothing to compare against.
  */
class BackendWipeTest extends AnyFunSuite {

    private val tracer: ContraTracer[IO, PersistenceEvent] =
        Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)

    private def bytes(b: Byte): Array[Byte] = Array.fill[Byte](4)(b)

    private def wiped(): (Option[Array[Byte]], Option[Array[Byte]], Option[Array[Byte]]) =
        InMemoryBackendStore
            .open(tracer)
            .use { backend =>
                for {
                    _ <- backend.put(Cf.Meta, bytes(1), bytes(0x11))
                    _ <- backend.put(Cf.Treasury, bytes(2), bytes(0x22))
                    _ <- backend.put(
                      Cf.HardAck(hydrozoa.multisig.consensus.peer.PeerId.Head(HeadPeerNumber(0))),
                      bytes(3),
                      bytes(0x33)
                    )
                    _ <- backend.wipeData
                    meta <- backend.get(Cf.Meta, bytes(1))
                    treasury <- backend.get(Cf.Treasury, bytes(2))
                    ack <- backend.get(
                      Cf.HardAck(hydrozoa.multisig.consensus.peer.PeerId.Head(HeadPeerNumber(0))),
                      bytes(3)
                    )
                } yield (meta, treasury, ack)
            }
            .unsafeRunSync()

    test("a wipe clears the data column families") {
        val (_, treasury, ack) = wiped()
        assert(treasury.isEmpty, "a singleton snapshot CF survived the wipe")
        assert(ack.isEmpty, "a per-author satellite CF survived the wipe")
    }

    test("a wipe keeps Cf.Meta — the store stays bound to its peer and head") {
        val (meta, _, _) = wiped()
        assert(
          meta.map(_.toSeq).contains(bytes(0x11).toSeq),
          "the identity stamp was wiped; any peer could then adopt this store"
        )
    }
}

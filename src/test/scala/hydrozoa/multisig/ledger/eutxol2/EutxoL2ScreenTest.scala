package hydrozoa.multisig.ledger.eutxol2

import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** EUTXO stateless screening (§5.4 Phase 3): a transaction payload must parse (and carry the headId
  * pin, tested separately in [[HeadIdPinTest]]); a deposit is deferred to registration for now. The
  * accept path for well-formed transactions is covered by the stage integration suites.
  */
class EutxoL2ScreenTest extends AnyFunSuite:

    private val nodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, Seed(0L))

    private val ledger = EutxoL2Ledger(nodeConfig).unsafeRunSync()

    private val garbage = ByteString.fromArray(Array[Byte](1, 2, 3))

    test("screen rejects a malformed transaction payload (No)") {
        assert(ledger.screen(garbage, l1Payload = None).value.unsafeRunSync().isLeft)
    }

    test("screen defers a deposit request (Yes)") {
        assert(ledger.screen(garbage, l1Payload = Some(garbage)).value.unsafeRunSync().isRight)
    }

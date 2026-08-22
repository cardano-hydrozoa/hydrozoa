package hydrozoa.config.head.network

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo

/** Validation coverage for [[CardanoNetwork.rejectStandardMagic]] — the guard that keeps a
  * hand-written `custom` chain from impersonating one of the standard ones.
  */
class CardanoNetworkTest extends AnyFunSuite:

    private val standardNetworks =
        List(CardanoNetwork.Mainnet, CardanoNetwork.Preprod, CardanoNetwork.Preview)

    test("the standard networks are accepted") {
        standardNetworks.foreach { n =>
            assert(CardanoNetwork.rejectStandardMagic(n).isRight, s"$n must be accepted")
        }
    }

    test("a custom network carrying a standard chain's magic is rejected by name") {
        standardNetworks.foreach { standard =>
            // Preview's CardanoInfo stands in for the chain description; only the magic decides.
            val impostor = CardanoNetwork.Custom(CardanoInfo.preview, standard.protocolMagic)
            val name = standard.toString.toLowerCase
            CardanoNetwork.rejectStandardMagic(impostor) match {
                case Left(message) =>
                    assert(
                      message.contains(name),
                      s"the error must name the chain to switch to; got: $message"
                    )
                case Right(_) =>
                    fail(s"a custom network with ${standard.protocolMagic} must be rejected")
            }
        }
    }

    test("a devnet magic is accepted") {
        // 42 is Yaci DevKit's; any magic that is not one of the three standard ones is fine.
        val devnet = CardanoNetwork.Custom(CardanoInfo.preview, protocolMagic = 42L)
        assert(CardanoNetwork.rejectStandardMagic(devnet).isRight)
    }

package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l2.Destination
import hydrozoa.multisig.persistence.codec.RefundTxCodec.given
import io.circe.parser.decode
import io.circe.syntax.*
import java.time.Instant
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{AddrKeyHash, Transaction}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** Round-trip test for [[RefundTx.PostDated]]'s persistence codec. */
class RefundTxCodecTest extends AnyFunSuite:

    given CardanoNetwork.Section = CardanoNetwork.Preview

    test("RefundTx.PostDated round-trips through the persistence Circe codec") {
        val tx: Transaction = Arbitrary.arbitrary[Transaction].sample.get
        val addrKeyHash: AddrKeyHash = Arbitrary.arbitrary[AddrKeyHash].sample.get
        val refundStart = QuantizedInstant(
          summon[CardanoNetwork.Section].slotConfig,
          Instant.ofEpochMilli(1_700_000_000_000L)
        )
        val refundDestination = Destination(
          address = ShelleyAddress(
            network = Network.Testnet,
            payment = ShelleyPaymentPart.Key(addrKeyHash),
            delegation = ShelleyDelegationPart.Null
          ),
          datum = None
        )
        val requestId = RequestId(HeadPeerNumber(2), RequestNumber(42L))
        val refund = RefundTx.PostDated(
          tx = tx,
          refundStart = refundStart,
          refundDestination = refundDestination,
          requestId = requestId,
          resolvedUtxos = ResolvedUtxos.empty
        )

        val json = refund.asJson.noSpaces
        val back = decode[RefundTx.PostDated](json)
        val _ = assert(back.isRight, s"decode failed: $back")
        // Equality on the EnrichedTx supertype only compares `tx`, but the case class has its own equals
        // derived from the case-class structure since RefundTx.PostDated extends EnrichedTx[PostDated].
        // Verify field-by-field round-trip.
        val got = back.toOption.get
        val _ = assert(got.tx == refund.tx)
        val _ = assert(got.refundStart == refund.refundStart)
        val _ = assert(got.refundDestination == refund.refundDestination)
        assert(got.requestId == refund.requestId)
    }

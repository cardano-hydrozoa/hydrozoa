package hydrozoa.multisig.consensus.transport

import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, Population}
import hydrozoa.multisig.consensus.liaison.BatchNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.{SettlementTx, genSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2StateExport, L2StateHash}
import hydrozoa.multisig.ledger.stack.{StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.codec.SecCodec.given
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import test.MinorBlocks

/** The wire codec for [[Join.Offer]] — the one message that seats a joining coil peer.
  *
  * **Why this is not folded into [[CoilCodecsTest]].** Every other hub↔coil frame carries numbers
  * and short signatures. An offer carries a multisigned settlement transaction, an optional SEC,
  * and an opaque ledger export — three nested structures whose codecs come from the persistence
  * layer, not from `Codecs`. A break in any of them is not a decode error at the coil: it is a
  * correct-looking offer that fails [[hydrozoa.multisig.consensus.JoinOfferVerifier]], which reads
  * as the hub being dishonest.
  *
  * ⚠️ Checked field-wise, never with `==` on the decoded offer. `SettlementTx` signatures, an SEC's
  * header, and an export's bytes are all `IArray`-backed, so case-class equality on any of them is
  * array *reference* equality — two values that print identically compare unequal after a
  * round-trip.
  */
class JoinOfferCodecTest extends AnyFunSuite {

    // With coil peers, so the SEC fixture carries a SPARSE signature list. A head-peers-only
    // config signs every slot, and a full list would pass a codec that compacted the holes away.
    private val env: MultiNodeConfig =
        MultiNodeConfig
            .generateWithCoil(nCoil = 5, quorum = 3)
            .pureApply(Gen.Parameters.default, Seed(0L))

    // The settlement carries network-tagged addresses, so the codec must run on the same network
    // the fixture was built for — not a hard-coded one.
    private given CardanoNetwork.Section = env.nodeConfigs(HeadPeerNumber.zero)

    private val signedSettlement: SettlementTx =
        val seq = genSettlementTxSeqBuilder(env.headConfig)()
            .pureApply(Gen.Parameters.default, Seed(1L))
            .result match {
            case Left(e)  => throw RuntimeException(s"settlement build failed: $e")
            case Right(s) => s
        }
        val unsigned = seq.settlementTx
        unsigned.txLens.replace(env.multisignTx(unsigned.tx))(unsigned)

    private val l2StateHash = ByteString.fromArray(Array.fill[Byte](32)(0x5c.toByte))

    private val sec: StandaloneEvacuationCommitment.MultiSigned =
        StandaloneEvacuationCommitment.MultiSigned(
          commitment = StandaloneEvacuationCommitment(
            blockNum = BlockNumber(4),
            blockVersion = BlockVersion.Full(1, 1),
            kzgCommitment = EvacuationMap.empty.kzgCommitment,
            l2StateHash = L2StateHash(l2StateHash),
            header = StandaloneEvacuationCommitmentOnchain(
              StandaloneEvacuationCommitmentOnchain(
                headId = env.headConfig.headTokenNames.treasuryTokenName.bytes,
                versionMajor = 1,
                versionMinor = 1,
                commitment = EvacuationMap.empty.kzgCommitment,
                l2StateHash = l2StateHash
              )
            )
          ),
          signatures = env.multisignHeaderSparse(
            StandaloneEvacuationCommitmentOnchain(
              headId = env.headConfig.headTokenNames.treasuryTokenName.bytes,
              versionMajor = 1,
              versionMinor = 1,
              commitment = EvacuationMap.empty.kzgCommitment,
              l2StateHash = l2StateHash
            )
          )
        )

    private val h0 = HeadPeerNumber(0)

    /** Deliberately not all-zero and not empty: a byte-order or truncation bug in the hex codec
      * survives a palindrome or a single byte.
      */
    private val exportBytes: Array[Byte] = Array[Byte](0x00, 0x01, 0x7f, -0x80, -0x01, 0x10)

    private val cursors = Population.Get(
      batchNum = BatchNumber(3),
      block = BlockNumber(5),
      stack = StackNumber(2),
      requests = Map(h0 -> RequestNumber(11)),
      softAcks = Map(h0 -> hydrozoa.multisig.consensus.ack.SoftAckNumber(6)),
      headHardAcks = Map(h0 -> HardAckNumber(7)),
      coilHardAcks = Map(h0 -> hydrozoa.multisig.consensus.ack.HubHardAckNumber(8))
    )

    private def offer(sec: Option[StandaloneEvacuationCommitment.MultiSigned]): Join.Offer =
        Join.Offer(
          startStack = StackNumber(2),
          cursors = cursors,
          ownHardAck = HardAckNumber(9),
          settlement = signedSettlement,
          sec = sec,
          state = L2StateExport(L2CommandNumber(42L), IArray.from(exportBytes)),
          block = lastBlock,
          deposits = DepositsMap.empty
        )

    private val lastBlock = MinorBlocks.brief(env.headConfig, 4).unsafeRunSync()

    private def roundTrip(o: Join.Offer): Join.Offer =
        CoilFrame.parse(CoilFrame.encode(CoilFrame.Msg(o))) match {
            case Right(CoilFrame.Msg(decoded: Join.Offer)) => decoded
            case Right(other) => fail(s"expected a Join.Offer frame, got: $other")
            case Left(err)    => fail(s"CoilFrame.parse failed: $err")
        }

    test("an offer at a minor start point survives the wire") {
        val decoded = roundTrip(offer(Some(sec)))

        val _ = assert(decoded.startStack == StackNumber(2))
        val _ = assert(decoded.cursors == cursors, "every population cursor must arrive as sent")
        val _ = assert(decoded.ownHardAck == HardAckNumber(9))
        val _ = assert(decoded.settlement.tx.id == signedSettlement.tx.id)
        val _ = assert(
          decoded.state.commandNumber == L2CommandNumber(42L) &&
              IArray.genericWrapArray(decoded.state.bytes).toArray.sameElements(exportBytes),
          "the export's bytes are opaque to the transport and must arrive byte-identical"
        )
        val _ = assert(decoded.sec.map(_.commitment.blockNum).contains(BlockNumber(4)))
        assert(
          decoded.sec.map(_.commitment.kzgCommitment).contains(EvacuationMap.empty.kzgCommitment)
        )
    }

    test("the fast-side anchor survives the wire") {
        val decoded = roundTrip(offer(Some(sec)))
        val _ = assert(decoded.block.blockNum == lastBlock.blockNum)
        val _ = assert(
          decoded.block.header == lastBlock.header,
          "the header is what the next block is built on; it must arrive intact"
        )
        assert(decoded.deposits == DepositsMap.empty)
    }

    test("an offer at a major start point carries no SEC") {
        assert(roundTrip(offer(None)).sec.isEmpty)
    }

    test("the settlement's witnesses survive — an offer that lost them would be refused") {
        // The verifier runs the settlement through scalus, so a codec that dropped or reordered
        // witnesses would turn every honest offer into a `SettlementInvalid` refusal.
        val decoded = roundTrip(offer(Some(sec)))
        val before = signedSettlement.tx.witnessSet.vkeyWitnesses.toSet
        val after = decoded.settlement.tx.witnessSet.vkeyWitnesses.toSet
        val _ = assert(before.nonEmpty, "fixture must actually be signed")
        val _ = assert(
          after.size == before.size,
          s"witness count changed: ${before.size} -> ${after.size}"
        )
        assert(after.map(_.vkey) == before.map(_.vkey), "the signing keys must arrive unchanged")
    }

    test("the SEC fixture is sparse — otherwise the slot test below proves nothing") {
        val _ = assert(
          sec.signatures.exists(_.isEmpty),
          "no empty slot: the fixture cannot catch packing"
        )
        assert(sec.signatures.exists(_.isDefined))
    }

    test("the SEC's signature slots keep their positions") {
        // Coil signatures sit at roster offsets, so a codec that compacted the list would move
        // every one of them and the quorum check would read zero valid coil signatures.
        val decoded = roundTrip(offer(Some(sec))).sec.get
        val _ = assert(decoded.signatures.length == sec.signatures.length)
        assert(
          decoded.signatures.map(_.isDefined) == sec.signatures.map(_.isDefined),
          "the empty slots are load-bearing — they are what keeps each signature at its own index"
        )
    }

    test("a SEC re-encodes stably") {
        // The equality trap in the class doc, stated as a check: the values compare unequal, so
        // stability of the JSON is what stands in for it.
        val once = offer(Some(sec)).sec.asJson.noSpaces
        val twice = roundTrip(offer(Some(sec))).sec.asJson.noSpaces
        assert(once == twice)
    }
}

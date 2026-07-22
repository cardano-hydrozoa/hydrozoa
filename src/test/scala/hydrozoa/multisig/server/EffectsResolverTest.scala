package hydrozoa.multisig.server

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.UserRequest.{DepositRequest, TransactionRequest}
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l2.Destination
import hydrozoa.multisig.ledger.stack.{EffectIds, PartitionEffects, StackBrief, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{ArrivalStamp, ConsensusStoreReader, DepositDecision, RequestBlockEntry, Timestamped}
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import java.time.Instant
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{AddrKeyHash, Transaction, TransactionHash}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.uplc.builtin.ByteString

/** Unit tests for [[EffectsResolver.relatedEffects]] — the request → effects resolution: a
  * transaction resolves to its inclusion-block partition's carriers (here, a minor block's SEC), a
  * deposit to its post-dated refund (matched by `requestId`), and an unknown / unprocessed request
  * to nothing.
  */
class EffectsResolverTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig = multiNodeConfig.headConfig
    private val peer0 = HeadPeerNumber(0)

    private val txRequestId = RequestId(peer0, RequestNumber(0))
    private val depositRequestId = RequestId(peer0, RequestNumber(1))

    private val txRequest: UserRequestWithId =
        UserRequestWithId(TransactionRequest(TransactionRequestBody(ByteString.empty)), txRequestId)
    private val depositRequest: UserRequestWithId =
        UserRequestWithId(
          DepositRequest(DepositRequestBody(ByteString.empty, ByteString.empty)),
          depositRequestId
        )

    /** A minor brief for block 1 (its stack's single partition). */
    private val minorBrief: BlockBrief.Minor =
        val startTime = BlockCreationStartTime(
          QuantizedInstant.ofEpochSeconds(headConfig.slotConfig, 0L)
        )
        val endTime = BlockCreationEndTime(
          QuantizedInstant.ofEpochSeconds(headConfig.slotConfig, 1L)
        )
        val fallbackTxStartTime = headConfig.txTiming.newFallbackStartTime(endTime)
        BlockBrief.Minor(
          BlockHeader.Minor(
            blockNum = BlockNumber(1),
            blockVersion = BlockVersion.Full(0, 0),
            startTime = startTime,
            endTime = endTime,
            fallbackTxStartTime = fallbackTxStartTime,
            forcedMajorBlockWakeupTime =
                headConfig.txTiming.forcedMajorBlockWakeupTime(fallbackTxStartTime),
            mDepositDecisionWakeupTime = None
          ),
          BlockBody.Minor(requests = List.empty, depositsRejected = List.empty)
        )

    private val sec: StandaloneEvacuationCommitment.MultiSigned =
        StandaloneEvacuationCommitment.MultiSigned(
          commitment = StandaloneEvacuationCommitment(
            blockNum = BlockNumber(1),
            blockVersion = BlockVersion.Full(1, 0),
            kzgCommitment = EvacuationMap.empty.kzgCommitment,
            header = StandaloneEvacuationCommitmentOnchain(
              StandaloneEvacuationCommitmentOnchain(
                headId = headConfig.headTokenNames.treasuryTokenName.bytes,
                versionMajor = BigInt(1),
                versionMinor = BigInt(0),
                commitment = EvacuationMap.empty.kzgCommitment
              )
            )
          ),
          headerMultiSigned = List(BlockHeader.Minor.HeaderSignature(IArray(0.toByte)))
        )
    private val secId: TransactionHash = EffectIds.secL1TxId(sec.commitment)

    /** A post-dated refund carrying the deposit request's id; its `l1TxId` is the empty tx's id. */
    private val refund: RefundTx.PostDated =
        RefundTx.PostDated(
          tx = Transaction.empty,
          refundStart = QuantizedInstant(headConfig.slotConfig, Instant.ofEpochMilli(0L)),
          refundDestination = Destination(
            ShelleyAddress(
              network = Network.Testnet,
              payment = ShelleyPaymentPart.Key(AddrKeyHash(ByteString.fromHex("00" * 28))),
              delegation = ShelleyDelegationPart.Null
            ),
            datum = None
          ),
          requestId = depositRequestId,
          resolvedUtxos = ResolvedUtxos.empty
        )
    private val refundId: TransactionHash = Transaction.empty.id

    private val stackEffects: StackEffects.HardConfirmed =
        StackEffects.HardConfirmed.Regular(
          NonEmptyList.of(PartitionEffects.Minor(sec = sec, refunds = List(refund)))
        )

    /** A reader over a single-minor stack (stack 1 = block 1) carrying the SEC and the refund, with
      * both the transaction and the deposit registered/processed in block 1.
      */
    private val reader: ConsensusStoreReader[IO] =
        new ConsensusStoreReader[IO]:
            def blockBriefs: IO[List[BlockBrief.Next]] = IO.pure(List(minorBrief))
            def blockBrief(num: BlockNumber): IO[Option[BlockBrief.Next]] =
                IO.pure(Option.when(num == BlockNumber(1))(minorBrief))
            def softConfirmation(
                num: BlockNumber
            ): IO[Option[Timestamped[Block.SoftConfirmed.Next]]] = IO.pure(None)
            def stackOf(num: BlockNumber): IO[Option[StackNumber]] =
                IO.pure(Option.when(num == BlockNumber(1))(StackNumber(1)))
            def hardConfirmation(
                num: StackNumber
            ): IO[Option[Timestamped[StackEffects.HardConfirmed]]] =
                IO.pure(
                  Option.when(num == StackNumber(1))(Timestamped(ArrivalStamp(0, 0L), stackEffects))
                )
            def stackBrief(num: StackNumber): IO[Option[StackBrief]] =
                IO.pure(
                  Option.when(num == StackNumber(1))(
                    StackBrief(
                      StackNumber(1),
                      BlockNumber(1),
                      BlockNumber(1),
                      StackCreationEndTime(
                        QuantizedInstant.ofEpochSeconds(headConfig.slotConfig, 0L)
                      )
                    )
                  )
                )
            def effectStack(l1TxId: TransactionHash): IO[Option[StackNumber]] =
                IO.pure(Option.when(l1TxId == secId)(StackNumber(1)))
            // The transaction's withdrawal is paid by an effect in stack 1 — here, reusing the SEC
            // id so it resolves via the same fixture; it coincides with the tx's carrier, exercising
            // the union + dedup. (Positive settlement/rollout payers are stage-suite-covered.)
            def withdrawalEffects(id: RequestId): IO[List[TransactionHash]] =
                IO.pure(if id == txRequestId then List(secId) else Nil)
            def requestsOf(peer: HeadPeerNumber): IO[List[Timestamped[UserRequestWithId]]] =
                IO.pure(Nil)
            def request(id: RequestId): IO[Option[Timestamped[UserRequestWithId]]] =
                IO.pure {
                    if id == txRequestId then Some(Timestamped(ArrivalStamp(0, 0L), txRequest))
                    else if id == depositRequestId then
                        Some(Timestamped(ArrivalStamp(0, 0L), depositRequest))
                    else None
                }
            def requestBlock(id: RequestId): IO[Option[RequestBlockEntry]] =
                IO.pure(
                  Option.when(id == txRequestId || id == depositRequestId)(
                    RequestBlockEntry(BlockNumber(1), ValidityFlag.Valid)
                  )
                )
            // The deposit is absorbed at the minor block 1 (its partition has no settlement), so
            // the absorption resolves to no effect — the positive major-settlement path is exercised
            // by the stage suites, which build real settlement transactions.
            def decision(id: RequestId): IO[Option[DepositDecision]] =
                IO.pure(
                  Option.when(id == depositRequestId)(DepositDecision.Absorbed(BlockNumber(1)))
                )
            def wallClockOf(stamp: ArrivalStamp): IO[Option[Instant]] = IO.pure(None)

    private val resolver = EffectsResolver(reader)

    test("a transaction resolves to its inclusion-block partition's carrier (the minor SEC)") {
        val effects = resolver.relatedEffects(txRequestId).unsafeRunSync()
        assert(effects.map(e => (e.l1TxId, e.kind)) == List((secId, EffectKind.Sec)))
    }

    test("a deposit resolves to its post-dated refund, matched by requestId") {
        val effects = resolver.relatedEffects(depositRequestId).unsafeRunSync()
        assert(effects.map(e => (e.l1TxId, e.kind)) == List((refundId, EffectKind.Refund)))
    }

    test("an unknown request resolves to no effects") {
        val effects = resolver.relatedEffects(RequestId(peer0, RequestNumber(99))).unsafeRunSync()
        assert(effects.isEmpty)
    }

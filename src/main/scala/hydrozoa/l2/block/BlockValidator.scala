package hydrozoa.l2.block

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l1.multisig.state.{DepositTag, DepositUtxos}
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.block.ValidationFailure.*
import hydrozoa.l2.block.ValidationResolution.*
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel.WithdrawalL2EventLabel
import hydrozoa.l2.ledger.state.UtxosSetOpaque

import scala.collection.mutable
import scala.language.strictEquality
import scala.util.boundary
import scala.util.boundary.break

// TODO: unify in terms of abstract ledger and types

enum ValidationFailure(msg: String):
    case MinorBlockContainsWithdrawals
        extends ValidationFailure("A minor block can't contain withdrawals.")

    case OnlyMajorBlockCanContainGenesis
        extends ValidationFailure("Only major blocks are allowed to contain a genesis event.")

    case MinorBlockWasExpected
        extends ValidationFailure(
          "Minor block was expected (no deposits, no withdrawals, no keep-alive)."
        )

    case L2EventNotValid(txId: TxId, err: String)
        extends ValidationFailure(s"L2 transaction $txId is not valid: $err")

    case ValidEventMarkedAsInvalid(txId: TxId)
        extends ValidationFailure(
          s"A valid transaction/withdrawal $txId is wrongly marked as invalid."
        )

    case NonEligibleDepositsInBlock(depositIds: Set[UtxoIdL1])
        extends ValidationFailure(s"Block contains deposits which are not eligible: $depositIds")

    case FinalBlockContainsDeposits(depositIds: Seq[UtxoIdL1])
        extends ValidationFailure(s"Final block contains deposits: $depositIds")

    case NotFinalBlockDuringFinalization(blockHeader: BlockHeader)
        extends ValidationFailure(s"Non final block during finalization: $blockHeader")

    case UnexpectedBlockNumber(expectedBlockNumber: Int, blockNumber: Int)
        extends ValidationFailure(
          s"Expected block number: $expectedBlockNumber, but got: $blockNumber"
        )

    case UnexpectedBlockVersion(expectedBlockVersion: (Int, Int), blockVersion: (Int, Int))
        extends ValidationFailure(
          s"Expected block version $expectedBlockVersion, but got: $blockVersion"
        )

enum ValidationResolution:
    case Valid(
        utxosActive: UtxosSetOpaque,
        mbGenesis: Option[(TxId, SimpleGenesis)],
        utxosWithdrawn: UtxosSet
    )
    case NotYetKnownL2Events(unknownEventIds: Set[TxId])
    case NotYetKnownDeposits(depositIds: Set[UtxoIdL1])
    case Invalid(reason: ValidationFailure)

object BlockValidator:

    private val log = Logger(getClass)

    def validateBlock(
        block: Block,
        prevHeader: BlockHeader,
        stateL2: AdaSimpleLedger[TBlockProduction],
        // FIXME: missing in the spec, empty for final block
        poolEventsL2: Seq[NonGenesisL2],
        // FIXME: missing in the spec, is not needed for minor and final blocks
        depositUtxos: DepositUtxos,
        // FIXME: missing in the spec, can be removed I guess
        finalizing: Boolean
    ): ValidationResolution =

        // 1. Initialize the variables and arguments.
        var mbGenesis: Option[(TxId, SimpleGenesis)] = None
        type UtxosDiffMutable = mutable.Set[(UtxoIdL2, Output[L2])]
        val utxosWithdrawn: UtxosDiffMutable = mutable.Set()

        // 2. Return Invalid if this fails to hold:
        //     block.timeCreation ∈ [timeCurrent ± blockLatencyTolerance)
        // TODO: TBD

        // 3. Check valid events from block
        val eventsValid = block.blockBody.eventsValid

        // 3.a Check for unknown valid events
        val unknownValidEventsL2 =
            eventsValid.map(_._1).toSet &~ poolEventsL2.map(_.getEventId).toSet
        if unknownValidEventsL2.nonEmpty then return NotYetKnownL2Events(unknownValidEventsL2)

        // 3.b Check for withdrawals in a minor block
        // FIXME: seems to be a bit redundant in presence of 7.b - only to fail-fast (or vice-versa)
        val blockHeader = block.blockHeader
        val blockType = blockHeader.blockType
        if blockType == Minor &&
            eventsValid.indexWhere(_._2 == WithdrawalL2EventLabel) >= 0
        then return Invalid(MinorBlockContainsWithdrawals)

        // 3.(c,d) For each non-genesis L2 event...
        val poolEventsL2Map = poolEventsL2.map(nge => (nge.getEventId, nge)).toMap
        val eventsValidWithEvents = eventsValid.map((txId, _) => poolEventsL2Map(txId))

        val validEventsResolution = boundary:
            eventsValidWithEvents.foreach {
                case tx: TransactionL2 =>
                    stateL2.submit(tx) match
                        case Right(txId, _)  => ()
                        case Left(txId, err) => break(Some(Invalid(L2EventNotValid(txId, err))))
                case wd: WithdrawalL2 =>
                    stateL2.submit(wd) match
                        case Right(txId, utxosDiff) =>
                            utxosWithdrawn.addAll(utxosDiff)
                        case Left(txId, err) =>
                            break(Some(Invalid(L2EventNotValid(txId, err))))
            }
            None

        validEventsResolution match
            case Some(resolution) => return resolution
            case None             => ()

        // 4. Check invalid events from block
        val eventsInvalid = block.blockBody.eventsInvalid

        // 4.a Check for unknown invalid events
        val unknownInvalidEventsL2 =
            eventsInvalid.map(_._1).toSet &~ poolEventsL2.map(_.getEventId).toSet
        if unknownInvalidEventsL2.nonEmpty then return NotYetKnownL2Events(unknownInvalidEventsL2)

        // 4.(b,c) Check all invalid events from the block are indeed invalid
        val eventsInvalidWithEvents = eventsInvalid.map((txId, _) => poolEventsL2Map(txId))
        val invalidEventsResolution = boundary:
            eventsInvalidWithEvents.foreach(invalidEvent =>
                val txOrWd = invalidEvent match
                    case tx: TransactionL2 => tx
                    case wd: WithdrawalL2  => wd
                stateL2.submit(txOrWd) match
                    case Right(txId, _)  => break(Option(Invalid(ValidEventMarkedAsInvalid(txId))))
                    case Left(txId, err) => ()
            )
            None

        invalidEventsResolution match
            case Some(resolution) => return resolution
            case None             => ()

        // 5. If not finalizing, the deposits are correct
        // 5.a all absorbed deposits are known
        val depositsAbsorbed = block.blockBody.depositsAbsorbed
        val knownDepositIds = depositUtxos.map.keySet
        val unknownDepositIds = depositsAbsorbed.toSet &~ knownDepositIds
        if unknownDepositIds.nonEmpty then return NotYetKnownDeposits(unknownDepositIds)

        // 5.(b,c)

        // TODO: check deposits timing - some check will be here
        val eligibleDeposits = depositsAbsorbed.filter(_ => true).toSet
        val nonEligibleDepositsInBlock = depositsAbsorbed.toSet &~ eligibleDeposits
        if nonEligibleDepositsInBlock.nonEmpty then
            return Invalid(NonEligibleDepositsInBlock(nonEligibleDepositsInBlock))

        // 5.d build mbGenesis
        mbGenesis =
            if depositsAbsorbed.isEmpty then None
            else
                val depositsAbsorbedUtxos = depositUtxos.map
                    .filter((k, _) => depositsAbsorbed.contains(k))
                    .toList.sortWith((a, b) => a._1._1.hash.compareTo(b._1._1.hash) < 0
                )
                val genesis: SimpleGenesis = SimpleGenesis.mkGenesis(depositsAbsorbedUtxos.map(_._2))
                stateL2.submit(AdaSimpleLedger.mkGenesisEvent(genesis)) match
                    case Right(txId, utxos) => Some(txId, genesis)
                    case Left(_, _) => ??? // unreachable, submit for deposits should always succeed

        // 6. If finalizing, there are no deposits in the block

        if finalizing && depositsAbsorbed.nonEmpty
        then return Invalid(FinalBlockContainsDeposits(depositsAbsorbed))

        // and all utxos should be withdrawn
        if finalizing then utxosWithdrawn.addAll(stateL2.flush)

        // 7. Return Invalid if block.blockType is not set according to the first among these to hold:
        // 7.a Final if finalizing is True.
        if finalizing && blockType != Final
        then return Invalid(NotFinalBlockDuringFinalization(blockHeader))
        // 7.b Major if mbGenesis is Some, utxosWithdrawn is non-empty,
        // or: block.timeCreation ≥ previousMajorBlock.timeCreation + multisigRegimeKeepAlive
        // FIXME: in fact, utxosWithdrawn can be non-empty for Final blocks as well (and we already checked it in 3.b)
        // So:
        if (mbGenesis.isDefined) && blockType != Major
        then return Invalid(OnlyMajorBlockCanContainGenesis)
        // Duplicates 3.b
        if (utxosWithdrawn.nonEmpty && blockType == Minor)
        then return Invalid(MinorBlockContainsWithdrawals)
        // TODO: block.timeCreation ≥ previousMajorBlock.timeCreation + multisigRegimeKeepAlive
        val keepAlive = blockType == Major && false
        if (
          mbGenesis.isEmpty && utxosWithdrawn.isEmpty &&
          !(blockType == Minor || keepAlive || finalizing)
        )
        then return Invalid(MinorBlockWasExpected)

        // 8. Return Invalid if any of these fails to hold:
        // 8.a block.blockNum matches (previousBlock.blockNum + 1).
        val expectedBlockNum = prevHeader.blockNum + 1
        val blockNum = blockHeader.blockNum
        if expectedBlockNum != blockNum
        then return Invalid(UnexpectedBlockNumber(expectedBlockNum, blockNum))

        // 8.b block.utxosActive matches the Merkle root hash of utxosActive
        // TODO: TBD

        // 8.(d,c)
        val expectedVersion = blockType match
            case Minor =>
                // 8.d If block.blockType is Minor, both of these hold:
                //   i.block.versionMajor matches previousBlock.versionMajor.
                //  ii.block.versionMinor matches (previousBlock.versionMinor + 1).
                (prevHeader.versionMajor, prevHeader.versionMinor + 1)
            case _ =>
                // 8.c If block.blockType is Major or Final, both of these hold :
                //   i.block.versionMajor matches (previousBlock.versionMajor + 1).
                //  ii.block.versionMinor is zero.
                (prevHeader.versionMajor + 1, 0)

        val blockVersion = (blockHeader.versionMajor, blockHeader.versionMinor)
        if (expectedVersion != blockVersion)
        then return Invalid(UnexpectedBlockVersion(expectedVersion, blockVersion))

        // 9. Return Valid, along with utxosActive, mbGenesis, and utxosWithdrawn.
        Valid(stateL2.getUtxosActive, mbGenesis, utxosWithdrawn.toSet)

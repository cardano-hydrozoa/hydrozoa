package hydrozoa.integration.stage4

import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.HydrozoaTransactionMutator
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis, L2Tx, genesisObligationDecoder}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.event.RequestNumber.increment
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import io.bullet.borer.Cbor
import org.scalacheck.commands.ModelCommand
import scalus.cardano.ledger.Utxos

import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration

object Model {

    private val logger = Logging.logger("Stage4.Model")


    case class Params(
        multiNodeConfig: MultiNodeConfig,
        /** How long after depositAbsorptionStart we assume the deposit is available in L2.
         */
        absorptionSlack: FiniteDuration,
        meanInterArrivalTimes: Map[HeadPeerNumber, FiniteDuration],
    )

    /** A deposit that has been registered but whose L2 UTxOs are not yet available in the model
      * (absorptionStart + absorptionSlack has not been reached by this peer's model time).
      */
    case class PendingDeposit(
        requestId: RequestId,
        expectedAbsorptionTime: QuantizedInstant,
        depositRefundTxSeq: DepositRefundTxSeq,
    )

    case class ModelState(
        // Immutable per-run parameters; kept in state so runState can access them
        params: Params,

        // Pre-init L1 UTxOs per peer (before the init tx is applied); immutable
        preinitPeerUtxosL1: Map[HeadPeerNumber, Utxos],

        // Per-peer simulated clock (used only during generation for deposit absorption)
        currentModelTimes: Map[HeadPeerNumber, QuantizedInstant],

        // Shared (among peers) L2 active UTxO set
        utxosL2Active: Utxos,

        // Per-peer L1 UTxOs (funding sources for deposits)
        peerUtxosL1: Map[HeadPeerNumber, Utxos],

        // Per-peer next request number
        nextRequestNumbers: Map[HeadPeerNumber, RequestNumber],

        // Per-peer deposits registered but not yet absorbed
        pendingDeposits: Map[HeadPeerNumber, List[PendingDeposit]],
    ) {
        override def toString: String = "<stage4 model state (hidden)>"

        def nextRequestId(peerNum: HeadPeerNumber): RequestId =
            RequestId(peerNum = peerNum, requestNum = nextRequestNumbers(peerNum))

        def modelTimeFor(peerNum: HeadPeerNumber): QuantizedInstant =
            currentModelTimes(peerNum)

        def nodeConfigFor(peerNum: HeadPeerNumber): NodeConfig =
            params.multiNodeConfig.nodeConfigs(peerNum)
    }

    // ===================================
    // Helpers
    // ===================================

    /** Promote pending deposits whose expectedAbsorptionTime has been reached.
      * Called as the first step of every runState so the active UTxO set is always up-to-date
      * before the command is evaluated. In a discrete-event model time only advances on command
      * fire, so "just when the time comes" means "when the clock first crosses the threshold."
      */
    private def absorbDeposits(
        peerNum: HeadPeerNumber,
        newTime: QuantizedInstant,
        state: ModelState,
    ): ModelState = {
        val (nowAbsorbed, stillPending) =
            state.pendingDeposits(peerNum).partition { pd =>
                pd.expectedAbsorptionTime <= newTime
            }

        val newL2Utxos = nowAbsorbed.foldLeft(state.utxosL2Active) { (utxos, pd) =>
            val l2Payload = pd.depositRefundTxSeq.depositTx.depositProduced.l2Payload.bytes
            val obligations =
                Cbor.decode(l2Payload).to[Queue[GenesisObligation]].value.toList
            val genesisId =
                L2Genesis.mkGenesisId(pd.depositRefundTxSeq.depositTx.depositProduced.utxoId)
            val genesis = L2Genesis(Queue.from(obligations), genesisId)
            utxos ++ genesis.asUtxos.map((i, o) => i -> o.value)
        }

        state.copy(
          pendingDeposits = state.pendingDeposits + (peerNum -> stillPending),
          utxosL2Active = newL2Utxos,
        )
    }

    // ===================================
    // ModelCommand instances
    // ===================================

    given ModelCommand[DelayCommand, Unit, ModelState] with {
        override def runState(cmd: DelayCommand, state: ModelState): (Unit, ModelState) =
            import cmd.peerNum
            logger.debug(s"MODEL>> DelayCommand(peer=$peerNum, duration=${cmd.duration})")
            val newTime = state.modelTimeFor(peerNum) + cmd.duration
            val absorbed = absorbDeposits(peerNum, newTime, state)
            () -> absorbed.copy(
              currentModelTimes = absorbed.currentModelTimes + (peerNum -> newTime)
            )

        override def delay(cmd: DelayCommand): scala.concurrent.duration.FiniteDuration =
            cmd.duration.finiteDuration
    }

    given ModelCommand[L2TxCommand, ValidityFlag, ModelState] with {
        // interArrivalDelay is the time the SUT fiber sleeps BEFORE issuing this command.
        // Advancing the model clock by the same amount keeps model time in sync with wall time.
        // Even the first command has a non-zero delay: Poisson first-arrival time ~ Exp(λ).
        override def delay(cmd: L2TxCommand): FiniteDuration = cmd.interArrivalDelay

        override def runState(
            cmd: L2TxCommand,
            state: ModelState
        ): (ValidityFlag, ModelState) =
            import cmd.peerNum
            logger.debug(s"MODEL>> L2TxCommand(peer=$peerNum, requestId=${cmd.request.requestId})")

            val newTime = state.modelTimeFor(peerNum) + cmd.interArrivalDelay
            val stateAfterTime = absorbDeposits(peerNum, newTime, state).copy(
              currentModelTimes = state.currentModelTimes + (peerNum -> newTime)
            )

            val nodeConfig = stateAfterTime.nodeConfigFor(peerNum)

            val l2Tx = L2Tx
                .parse(
                  cmd.request.request.body.l2Payload.bytes,
                  nodeConfig.headConfig.cardanoNetwork
                )
                .fold(err => throw RuntimeException(s"Failed to parse L2Tx: $err"), identity)

            val mutatorResult = HydrozoaTransactionMutator.transit(
              config = nodeConfig.headConfig,
              time = stateAfterTime.modelTimeFor(peerNum),
              state = stateAfterTime.utxosL2Active,
              l2Tx = l2Tx
            )

            val (flag, newL2Utxos) = mutatorResult match {
                case Left(err) =>
                    logger.debug(s"L2 tx ${cmd.request.requestId} invalid in model: $err")
                    ValidityFlag.Invalid -> stateAfterTime.utxosL2Active
                case Right(newUtxos) =>
                    ValidityFlag.Valid -> newUtxos
            }

            flag -> stateAfterTime.copy(
              utxosL2Active = newL2Utxos,
              nextRequestNumbers = stateAfterTime.nextRequestNumbers +
                  (peerNum -> stateAfterTime.nextRequestNumbers(peerNum).increment),
            )
    }

    given ModelCommand[RegisterAndSubmitDepositCommand, ValidityFlag, ModelState] with {
        override def delay(cmd: RegisterAndSubmitDepositCommand): FiniteDuration =
            cmd.interArrivalDelay

        override def runState(
            cmd: RegisterAndSubmitDepositCommand,
            state: ModelState
        ): (ValidityFlag, ModelState) =
            import cmd.peerNum
            logger.debug(
              s"MODEL>> RegisterAndSubmitDepositCommand(peer=$peerNum, requestId=${cmd.request.requestId})"
            )

            val newTime = state.modelTimeFor(peerNum) + cmd.interArrivalDelay
            val stateAfterTime = absorbDeposits(peerNum, newTime, state).copy(
              currentModelTimes = state.currentModelTimes + (peerNum -> newTime)
            )

            val pending = PendingDeposit(
              requestId = cmd.request.requestId,
              expectedAbsorptionTime = cmd.expectedAbsorptionTime,
              depositRefundTxSeq = cmd.depositRefundTxSeq,
            )

            val spentL1Inputs = cmd.depositTxBytesSigned.body.value.inputs.toSet
            val updatedPeerL1 = stateAfterTime.peerUtxosL1(peerNum) -- spentL1Inputs

            ValidityFlag.Valid -> stateAfterTime.copy(
              pendingDeposits = stateAfterTime.pendingDeposits +
                  (peerNum -> (stateAfterTime.pendingDeposits(peerNum) :+ pending)),
              nextRequestNumbers = stateAfterTime.nextRequestNumbers +
                  (peerNum -> stateAfterTime.nextRequestNumbers(peerNum).increment),
              peerUtxosL1 = stateAfterTime.peerUtxosL1 + (peerNum -> updatedPeerL1),
            )
    }

}

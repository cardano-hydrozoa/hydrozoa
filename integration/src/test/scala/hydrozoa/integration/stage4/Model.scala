package hydrozoa.integration.stage4

import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import cats.implicits.toContravariantOps
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, debug}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.HydrozoaTransactionMutator
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis, L2Tx, genesisObligationDecoder}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.event.RequestNumber.increment
import io.bullet.borer.Cbor
import org.scalacheck.commands.ModelCommand
import scalus.cardano.ledger.{TransactionInput, Utxos}
import scalus.uplc.builtin.ByteString

import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration

object Model {

    private val logger: ContraTracer[cats.Id, Slf4jMsg] =
        Slf4jTracer.sink
            .contramap(Slf4jMsgFormat.humanFormat("Stage4.Model"))
            .natTracer(Slf4jTracer.ioToId)

    case class Params(
        multiNodeConfig: MultiNodeConfig,
        /** How long after depositAbsorptionStart we assume the deposit is available in L2.
          */
        absorptionSlack: FiniteDuration,
        meanInterArrivalTimes: Map[HeadPeerNumber, FiniteDuration],
        /** Coil follower node configs (each hubbed by head 0). Empty for a pure-head run. */
        coilNodeConfigs: List[NodeConfig] = List.empty,
    )

    /** A deposit that has been registered but whose L2 UTxOs are not yet available in the model
      * (absorptionStart + absorptionSlack has not been reached by this peer's model time).
      */
    case class PendingDeposit(
        requestId: RequestId,
        // Protocol-level deposit maturity time (= depositAbsorptionStartTime).
        absorptionStartTime: QuantizedInstant,
        // absorptionStartTime + Params.absorptionSlack — model's view of when the deposit's L2
        // UTxOs become available in `utxosL2Active`.
        expectedAbsorptionTime: QuantizedInstant,
        l2Payload: ByteString,
        depositProduced: TransactionInput
    )

    case class ModelState(
        // Immutable per-run parameters; kept in state so runState can access them
        params: Params,

        // Pre-init L1 UTxOs per peer (before the init tx is applied); immutable
        preinitPeerUtxosL1: Map[HeadPeerNumber, Utxos],

        // Single cumulative model clock. Advances by `cmd.interArrivalDelay` (or `cmd.duration`
        // for DelayCommand) on every command's runState. Stays in lockstep with the SUT virtual
        // clock since the framework also calls IO.sleep with the same value before submission.
        currentModelTime: QuantizedInstant,

        // Real-clock anchor for non-TestControl runs. `Some(t)` means `genInitialState`
        // computed `t = Instant.now() + 60s` and pinned the head's initial block end-time to
        // it; `startupSut` will sleep until the wall clock reaches `t` (or abort if late).
        // `None` for TestControl runs — virtual clock jumps instantly. See
        // package.scala "Why TestControl is mandatory" for the full rationale.
        takeoffTime: Option[java.time.Instant],

        // Shared (among peers) L2 active UTxO set
        utxosL2Active: Utxos,

        // Per-peer L1 UTxOs (funding sources for deposits)
        peerUtxosL1: Map[HeadPeerNumber, Utxos],

        // Per-peer next request number
        nextRequestNumbers: Map[HeadPeerNumber, RequestNumber],

        // Per-peer deposits registered but not yet absorbed
        pendingDeposits: Map[HeadPeerNumber, List[PendingDeposit]],

        // Validity flag the model assigned to each user request, in submission order. Used at
        // shutdown to compare model's submission-order verdict against SUT's block-order verdict.
        modelFlags: Map[RequestId, ValidityFlag],

        // Every deposit ever registered, by RequestId. Retained even after absorption so that
        // shutdownSut analysis has access to timing metadata (expectedAbsorptionTime, etc.).
        registeredDeposits: Map[RequestId, PendingDeposit],
    ) {
        override def toString: String = "<stage4 model state (hidden)>"

        def nextRequestId(peerNum: HeadPeerNumber): RequestId =
            RequestId(peerNum = peerNum, requestNum = nextRequestNumbers(peerNum))

        def nodeConfigFor(peerNum: HeadPeerNumber): NodeConfig =
            params.multiNodeConfig.nodeConfigs(peerNum)
    }

    // ===================================
    // Helpers
    // ===================================

    /** Promote pending deposits whose expectedAbsorptionTime has been reached. Called as the first
      * step of every runState so the active UTxO set is always up-to-date before the command is
      * evaluated. With a single global clock, absorption is checked across all peers' pending
      * deposits — once the clock crosses a deposit's expectedAbsorptionTime, every peer sees it.
      */
    private def absorbDeposits(
        newTime: QuantizedInstant,
        state: ModelState,
    ): ModelState = {
        val (newPendingByPeer, allAbsorbed) =
            state.pendingDeposits.foldLeft(
              Map.empty[HeadPeerNumber, List[PendingDeposit]] -> List.empty[PendingDeposit]
            ) { case ((accPending, accAbsorbed), (peer, list)) =>
                val (nowAbsorbed, stillPending) =
                    list.partition(_.expectedAbsorptionTime <= newTime)
                (accPending + (peer -> stillPending), accAbsorbed ++ nowAbsorbed)
            }

        val newL2Utxos = allAbsorbed.foldLeft(state.utxosL2Active) { (utxos, pd) =>
            val l2Payload = pd.l2Payload.bytes
            val obligations =
                Cbor.decode(l2Payload).to[Queue[GenesisObligation]].value.toList
            val genesisId = L2Genesis.mkGenesisId(pd.depositProduced)
            val genesis = L2Genesis(Queue.from(obligations), genesisId)
            utxos ++ genesis.asUtxos.map((i, o) => i -> o.value)
        }

        state.copy(
          pendingDeposits = newPendingByPeer,
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
            val newTime = state.currentModelTime + cmd.duration
            val absorbed = absorbDeposits(newTime, state)
            () -> absorbed.copy(currentModelTime = newTime)

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

            val newTime = state.currentModelTime + cmd.interArrivalDelay
            val stateAfterTime = absorbDeposits(newTime, state).copy(
              currentModelTime = newTime,
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
              time = stateAfterTime.currentModelTime,
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
              modelFlags = stateAfterTime.modelFlags + (cmd.request.requestId -> flag),
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

            val newTime = state.currentModelTime + cmd.interArrivalDelay
            val stateAfterTime = absorbDeposits(newTime, state).copy(
              currentModelTime = newTime,
            )

            val pending = PendingDeposit(
              requestId = cmd.request.requestId,
              absorptionStartTime = cmd.absorptionStartTime,
              expectedAbsorptionTime = cmd.expectedAbsorptionTime,
              l2Payload = cmd.l2Payload,
              depositProduced = cmd.depositProduced
            )

            val spentL1Inputs = cmd.depositTxBytesSigned.body.value.inputs.toSet
            val updatedPeerL1 = stateAfterTime.peerUtxosL1(peerNum) -- spentL1Inputs

            ValidityFlag.Valid -> stateAfterTime.copy(
              pendingDeposits = stateAfterTime.pendingDeposits +
                  (peerNum -> (stateAfterTime.pendingDeposits(peerNum) :+ pending)),
              nextRequestNumbers = stateAfterTime.nextRequestNumbers +
                  (peerNum -> stateAfterTime.nextRequestNumbers(peerNum).increment),
              peerUtxosL1 = stateAfterTime.peerUtxosL1 + (peerNum -> updatedPeerL1),
              modelFlags =
                  stateAfterTime.modelFlags + (cmd.request.requestId -> ValidityFlag.Valid),
              registeredDeposits =
                  stateAfterTime.registeredDeposits + (cmd.request.requestId -> pending),
            )
    }

}

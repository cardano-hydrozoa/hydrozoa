package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.*
import hydrozoa.infra.Piper
import hydrozoa.l1.genesisUtxos
import hydrozoa.l1.multisig.state.{DepositUtxos, MultisigHeadStateL1}
import hydrozoa.l2.block.{Block, zeroBlock}
import hydrozoa.l2.ledger.L2Event
import hydrozoa.model.PeersNetworkPhase.NewlyCreated
import hydrozoa.node.TestPeer
import hydrozoa.node.state.*

/** This should be immutable. So the original idea of using NodeState/HeadStateGlobal won't work. We
  * need another thing here.
  */
case class HydrozoaState(
    peersNetworkPhase: PeersNetworkPhase,
    knownPeers: Set[TestPeer],
    pp: ProtocolParams,

    // Head
    headPhase: Option[HeadPhase] = None,
    initiator: Option[TestPeer] = None,
    headPeers: Set[TestPeer] = Set.empty,
    headAddressBech32: Option[AddressBechL1] = None,
    headMultisigScript: Option[NativeScript] = None,
    depositUtxos: DepositUtxos = TaggedUtxoSet.apply(),
    treasuryUtxoId: Option[UtxoIdL1] = None,

    // Node
    poolEvents: Seq[L2Event] = Seq.empty,

    // L1
    knownTxs: Map[TxId, TxL1] = Map.empty,
    utxosActive: Map[UtxoIdL1, OutputL1],

    // L2
    utxosActiveL2: Map[UtxoIdL2, OutputL2] = Map.empty,
    l2Tip: Block = zeroBlock
):
    override def toString: String =
        "Hydrozoa state:" +
            s"\tNetwork phase: ${peersNetworkPhase.toString}\n" +
            s"\tKnown peers: ${knownPeers.toString()}\n" +
            s"\tProtocol parameters: [SKIPPED]\n" +
            s"\tHead phase: ${headPhase.toString}\n" +
            s"\tInitiator: ${initiator.toString}\n" +
            s"\tHead peers: ${headPeers.toString()}\n" +
            s"\tHead address ${headAddressBech32.toString}\n" +
            s"\tHead multisig script: [SKIPPED]\n" +
            s"\tDeposits utxo: ${depositUtxos.toString}\n" +
            s"\tTreasury UTxO id: ${treasuryUtxoId.toString}\n" +
            s"\tPool events: ${poolEvents.toString()}\n" +
            s"\tL1: [SKIPPED]\n" +
            s"\tL2: [SKIPPED]\n"

object HydrozoaState:
    def apply(
        pp: ProtocolParams,
        knownPeers: Set[TestPeer]
    ): HydrozoaState =
        new HydrozoaState(
          peersNetworkPhase = NewlyCreated,
          knownPeers = knownPeers,
          pp = pp,
          utxosActive = Map.from(genesisUtxos)
        )

enum PeersNetworkPhase derives CanEqual:
    case NewlyCreated
    case RunningHead
    case Freed
    case Shutdown

class NodeStateReaderMock(s: HydrozoaState) extends HeadStateReader:

    override def currentPhase: HeadPhase = s.headPhase.get

    override def multisigRegimeReader[A](foo: MultisigRegimeReader => A): A =
        (new MultisigRegimeReader:
            def headAddress: AddressBechL1 = s.headAddressBech32.get

            override def headPeers: Set[WalletId] = ???

            override def headNativeScript: NativeScript = s.headMultisigScript.get

            override def headMintingPolicy: CurrencySymbol = ???

            override def beaconTokenName: TokenName = ???

            override def seedAddress: AddressBechL1 = ???

            override def treasuryUtxoId: UtxoIdL1 = s.treasuryUtxoId.get

            override def stateL1: MultisigHeadStateL1 = ???
        ) |> foo

    override def initializingPhaseReader[A](foo: InitializingPhaseReader => A): A = ???

    override def openPhaseReader[A](foo: OpenPhaseReader => A): A =
        (new OpenPhaseReader:
            def immutablePoolEventsL2: Seq[L2Event] = ???
            def immutableBlocksConfirmedL2: Seq[BlockRecord] = ???
            def immutableEventsConfirmedL2: Seq[(L2Event, Int)] = ???
            def l2Tip: Block = ???
            def l2LastMajorRecord: BlockRecord = ???
            def l2LastMajor: Block = ???
            def lastKnownTreasuryUtxoId: UtxoIdL1 = s.treasuryUtxoId.get
            def peekDeposits: DepositUtxos = ???
            def depositTimingParams: (UDiffTimeMilli, UDiffTimeMilli, UDiffTimeMilli) = ???
            def blockLeadTurn: Int = ???
            def isBlockLeader: Boolean = ???
            def isBlockPending: Boolean = ???
            def pendingOwnBlock: OwnBlock = ???
            def isQuitConsensusImmediately: Boolean = ???
            def beaconTokenName: hydrozoa.TokenName = ???
            def headAddress: hydrozoa.AddressBechL1 = ???
            def headMintingPolicy: hydrozoa.CurrencySymbol = ???
            def headNativeScript: hydrozoa.NativeScript = ???
            def headPeers: Set[hydrozoa.node.state.WalletId] = ???
            def seedAddress: hydrozoa.AddressBechL1 = ???
            def stateL1: hydrozoa.l1.multisig.state.MultisigHeadStateL1 = ???
            def treasuryUtxoId: hydrozoa.UtxoIdL1 = ???
        ) |> foo

    override def finalizingPhaseReader[A](foo: FinalizingPhaseReader => A): A = ???

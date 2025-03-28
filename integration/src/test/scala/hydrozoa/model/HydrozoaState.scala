package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.*
import hydrozoa.infra.Piper
import hydrozoa.l1.genesisUtxos
import hydrozoa.l1.multisig.state.{DepositUtxos, MultisigHeadStateL1}
import hydrozoa.l2.block.Block
import hydrozoa.l2.ledger.NonGenesisL2
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
    depositUtxos: DepositUtxos = UtxoSet.apply(),

    // Node
    poolEvents: Seq[NonGenesisL2] = Seq.empty,

    // L1
    knownTxs: Map[TxId, TxL1] = Map.empty,
    utxosActive: Map[UtxoIdL1, OutputL1],

    // L2
    utxosActiveL2: Map[UtxoIdL2, OutputL2] = Map.empty,
    l2Tip: Option[Block] = None
)

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

enum PeersNetworkPhase:
    case NewlyCreated
    case RunningHead
    case Freed
    case Shutdown

class NodeStateReaderMock(s: HydrozoaState) extends HeadStateReader:

    override def currentPhase: HeadPhase = s.headPhase.get

    override def multisigRegimeReader[A](foo: MultisigRegimeReader => A): A =
        (new MultisigRegimeReader:
            def headBechAddress: AddressBechL1 = s.headAddressBech32.get

            override def headPeers: Set[WalletId] = ???

            override def headNativeScript: NativeScript = s.headMultisigScript.get

            override def beaconTokenName: String = ???

            override def seedAddress: AddressBechL1 = ???

            override def currentTreasuryRef: UtxoIdL1 = ???

            override def stateL1: MultisigHeadStateL1 = ???
        ) |> foo

    override def initializingPhaseReader[A](foo: InitializingPhaseReader => A): A = ???

    override def openPhaseReader[A](foo: OpenPhaseReader => A): A = ???

    override def finalizingPhaseReader[A](foo: FinalizingPhaseReader => A): A = ???

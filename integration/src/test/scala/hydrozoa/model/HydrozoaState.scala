package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.*
import hydrozoa.l1.genesisUtxos
import hydrozoa.model.PeersNetworkPhase.NewlyCreated
import hydrozoa.node.TestPeer

/** This should be immutable. So the original idea of using NodeState/HeadStateGlobal won't work. We
  * need another thing here.
  */
case class HydrozoaState(
    peersNetworkPhase: PeersNetworkPhase,
    knownPeers: Set[TestPeer],
    pp: ProtocolParams,

    // Head
    initiator: Option[TestPeer] = None,
    headPeers: Set[TestPeer] = Set.empty,

    // L1
    knownTxs: Map[TxId, TxL1] = Map.empty,
    utxosActive: Map[UtxoIdL1, Output[L1]]
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

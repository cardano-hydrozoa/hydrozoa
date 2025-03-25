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
    networkPeers: Seq[TestPeer],
    // L1
    knownTxs: Map[TxId, TxL1],
    utxosActive: Map[UtxoIdL1, Output[L1]]
)

object HydrozoaState:
    def apply(
        pp: ProtocolParams,
        knownPeers: Seq[TestPeer]
    ): HydrozoaState =
        new HydrozoaState(
          NewlyCreated,
          knownPeers,
          Map.empty,
          Map.from(genesisUtxos)
        )

enum PeersNetworkPhase:
    case NewlyCreated
    case RunningHead
    case Freed
    case Shutdown

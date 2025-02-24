package hydrozoa.node.server
import hydrozoa.{L1Tx, TxId}

// TODO: revise
enum HeadState:
    // Hydrozoa node is running and can be asked to prepare the init tx.
    case Free(knownPeers: Array[Peer])
    // The init tx is generated and ready to be signed by a user.
    case ReadyToInit(initTx: L1Tx)
    // The init transaction has been submitted but not yet confirmed.
    case Initialization(initTxId: TxId)
    // The init transaction is settled.
    case MultisigRegime
    // FIXME:
    case Finalization
    case RuleBasedRegime
    case DisputeResolution

// Represents a peer node
case class Peer()

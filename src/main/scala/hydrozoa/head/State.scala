package hydrozoa.head

enum HeadState:
    // Hydrozoa node is running and can be asked to prepare the TxBuilder.scala tx.
    case Free(knownPeers: Array[Peer])
    // The TxBuilder.scala tx is generated and ready to be signed by a user.
    case ReadyToInit(initTx: L1Tx)
    // The TxBuilder.scala transaction has been submitted but not yet confirmed.
    case Initialization(initTxId: TxId)
    // The TxBuilder.scala transaction is settled.
    case MultisigRegime
    // FIXME: elaborate...
    case Finalization
    case RuleBasedRegime
    case DisputeResolution

// Represents a peer node
case class Peer()

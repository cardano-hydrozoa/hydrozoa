package hydrozoa.node.state

import com.typesafe.scalalogging.Logger
import hydrozoa.VerificationKeyBytes
import hydrozoa.node.TestPeer
import hydrozoa.infra.{Piper, sequence}
import scala.collection.mutable

/** The class that provides read-write and read-only access to the state of the node.
  */
class NodeState():

    val log: Logger = Logger(getClass)

    private var ownPeer: TestPeer = _

    // All known peers in a peer network (not to confuse with head's peers)
    private val knownPeers: mutable.Set[WalletId] = mutable.Set.empty

    private val knownPeersVKeys: mutable.Map[WalletId, VerificationKeyBytes] = mutable.Map.empty

    def getKnownPeers: Set[WalletId] = knownPeers.toSet

    def saveKnownPeersVKeys(keys: Map[WalletId, VerificationKeyBytes]): Unit =
        log.info(s"Saving learned verification keys for known peers: $keys")
        knownPeersVKeys.addAll(keys)

    def getVerificationKeys(peers: Set[WalletId]): Option[Set[VerificationKeyBytes]] =
        val mbList = peers.toList.map(knownPeersVKeys.get) |> sequence
        mbList.map(_.toSet)

    // The head state. Currently, we support only one head per a [set] of nodes.
    private var headState: Option[HeadStateGlobal] = None

    // FIXME: seedUtxo: ???
    // FIXME: why list, should be set?
    def initializeHead(peers: List[WalletId]): Unit =
        headState match
            case None =>
                log.info(s"Initializing a new head for peers: $peers")
                this.headState = Some(HeadStateGlobal(peers))
            // FIXME:
            // case Some(Finalized) => this.headState = Some(HeadStateGlobal())
            case Some(_) =>
                val err = "The head is already initialized."
                log.warn(err)
                throw new IllegalStateException(err)

    // Returns read-write API for head state.
    def head: HeadState = getOrThrow

    // Returns read-only API for head state.
    val reader: HeadStateReader = new {
        override def currentPhase: HeadPhase = getOrThrow.currentPhase
        override def multisigRegimeReader[A](foo: MultisigRegimeReader => A): A =
            getOrThrow.multisigRegimeReader(foo)
        override def initializingPhaseReader[A](foo: InitializingPhaseReader => A): A =
            getOrThrow.initializingPhaseReader(foo)
        override def openPhaseReader[A](foo: OpenPhaseReader => A): A =
            getOrThrow.openPhaseReader(foo)
        override def finalizingPhaseReader[A](foo: FinalizingPhaseReader => A): A =
            getOrThrow.finalizingPhaseReader(foo)

    }

    private def getOrThrow = {
        headState match
            case Some(v) => v
            case None =>
                val err = "The head is missing."
                log.warn(err)
                throw IllegalStateException(err)
    }

    val knownVerificationKeys: mutable.Map[TestPeer, VerificationKeyBytes] = mutable.Map.empty

// FIXME: add pub key
case class WalletId(
    name: String
)

object NodeState:
    def apply(knownPeers: Set[WalletId]): NodeState =
        // TODO: @atlanter: is there a way to make it more concise?
        val nodeState = new NodeState()
        nodeState.knownPeers.addAll(knownPeers)
        nodeState

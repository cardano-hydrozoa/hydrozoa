package hydrozoa.node.state

import com.typesafe.scalalogging.Logger
import hydrozoa.*

import scala.collection.mutable

/** The class that provides read-write and read-only access to the state of the node.
  */
class NodeState():

    val log: Logger = Logger(getClass)

    // All known peers in a peer network (not to confuse with head's pears)
    private var knownPeers: mutable.Set[Peer] = mutable.Set.empty

    // The head state. Currently, we support only one head per a [set] of nodes.
    private var headState: Option[HeadStateGlobal] = None

    // seedUtxo: ???
    def initializeHead(peers: List[Peer]): Unit =
        headState match
            case None =>
                log.info("Initializing a new head.")
                this.headState = Some(HeadStateGlobal(peers))
            // FIXME
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

/** Represent a node (Hydrozoa process) run by a peer (a user/operator).
  */
case class Peer()

package hydrozoa.integration.fallbackhandoff

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.commands.{CommandLabel, CommandProp}

object Commands:

    /** Cut the given peer off from the head: the suite interprets this by issuing the
      * corresponding CRUD ops to the live [[Firewall]] (Add rules dropping that peer's outbound
      * peer-transport sigs and outbound L1 tx submissions). With N=2 this is sufficient to
      * deprive the head of quorum progress and deterministically drive both peers to
      * `FallbackToRuleBased`.
      */
    final case class IsolatePeerCommand(peerNum: HeadPeerNumber):
        override def toString: String = s"IsolatePeerCommand(peer=$peerNum)"

    given CommandProp[IsolatePeerCommand, Unit, ModelState] with {}

    given CommandLabel[IsolatePeerCommand] with
        override def label(cmd: IsolatePeerCommand): String =
            s"IsolatePeer(${cmd.peerNum: Int})"

end Commands

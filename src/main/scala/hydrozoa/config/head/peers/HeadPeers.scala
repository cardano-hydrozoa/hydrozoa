package hydrozoa.config.head.peers

import hydrozoa.VerificationKeyBytes
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{PeerId, PeerNumber}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import scalus.cardano.address.ShelleyAddress

final case class HeadPeers(
    peerVKeys: IArray[VerificationKeyBytes]
) extends HeadPeers.Section {
    require(peerVKeys.size > 0)

    override transparent inline def headPeers: HeadPeers = this

    def apply(p: PeerId): VerificationKeyBytes = {
        require(p.nPeers == nPeers)
        peerVKeys(p.peerNum)
    }

    override def peerVKey(p: PeerId): VerificationKeyBytes = apply(p)

    override val nPeers: PositiveInt = PositiveInt.unsafeApply(peerVKeys.size)

    override lazy val headMultisigScript: HeadMultisigScript = HeadMultisigScript(this)
}

object HeadPeers {
    trait Section {
        def headPeers: HeadPeers

        def peerVKey(p: PeerId): VerificationKeyBytes

        def nPeers: PositiveInt

        def headMultisigScript: HeadMultisigScript
    }

    extension (config: HeadPeers.Section & CardanoNetwork.Section)
        def headMultisigAddress: ShelleyAddress =
            config.headMultisigScript.mkAddress(config.network)
}

package hydrozoa.config.head.peers

import cats.data.NonEmptyList
import hydrozoa.VerificationKeyBytes
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import scalus.cardano.address.ShelleyAddress

final case class HeadPeers private (
    override val headPeerVKeys: NonEmptyList[VerificationKeyBytes]
) extends HeadPeers.Section {
    require(headPeerVKeys.size > 0)

    override transparent inline def headPeers: HeadPeers = this

    def apply(p: HeadPeerNumber): Option[VerificationKeyBytes] = {
        Option.when(p < nHeadPeers)(headPeerVKeys.toList(p))
    }

    def apply(p: HeadPeerId): Option[VerificationKeyBytes] = {
        Option.when(p.nHeadPeers == nHeadPeers)(headPeerVKeys.toList(p.peerNum))
    }

    override def headPeerIds: NonEmptyList[HeadPeerId] =
        NonEmptyList.fromListUnsafe(
          Range.Exclusive(0, nHeadPeers, 1).map(HeadPeerId(_, nHeadPeers)).toList
        )

    override def headPeerVKey(p: HeadPeerNumber): Option[VerificationKeyBytes] = apply(p)
    override def headPeerVKey(p: HeadPeerId): Option[VerificationKeyBytes] = apply(p)

    override lazy val headMultisigScript: HeadMultisigScript = HeadMultisigScript(this)

    override val nHeadPeers: PositiveInt = PositiveInt.unsafeApply(headPeerVKeys.size)
}

object HeadPeers {
    def apply(headPeerVKeys: NonEmptyList[VerificationKeyBytes]): HeadPeers =
        new HeadPeers(headPeerVKeys)

    def apply(headPeerVKeys: List[VerificationKeyBytes]): Option[HeadPeers] = for {
        neHeadPeerVKeys <- NonEmptyList.fromList(headPeerVKeys)
    } yield new HeadPeers(neHeadPeerVKeys)

    trait Section {
        def headPeers: HeadPeers

        def headPeerIds: NonEmptyList[HeadPeerId]

        def headPeerVKeys: NonEmptyList[VerificationKeyBytes]

        def headPeerVKey(p: HeadPeerNumber): Option[VerificationKeyBytes]
        def headPeerVKey(p: HeadPeerId): Option[VerificationKeyBytes]

        def headMultisigScript: HeadMultisigScript

        def nHeadPeers: PositiveInt
    }

    extension (config: HeadPeers.Section & CardanoNetwork.Section)
        def headMultisigAddress: ShelleyAddress =
            config.headMultisigScript.mkAddress(config.network)
}

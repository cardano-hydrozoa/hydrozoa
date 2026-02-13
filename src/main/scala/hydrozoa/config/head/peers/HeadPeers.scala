package hydrozoa.config.head.peers

import cats.data.{NonEmptyList, NonEmptyMap}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import scalus.builtin.Builtins.blake2b_224
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.AddrKeyHash
import scalus.crypto.ed25519.VerificationKey

final case class HeadPeers private (
    override val headPeerVKeys: NonEmptyList[VerificationKey]
) extends HeadPeers.Section {
    require(headPeerVKeys.size > 0)

    override transparent inline def headPeers: HeadPeers = this

    override def headPeerNums: NonEmptyList[HeadPeerNumber] =
        NonEmptyList.fromListUnsafe(
          Range.Exclusive(0, nHeadPeers, 1).map(HeadPeerNumber.apply).toList
        )

    override def headPeerIds: NonEmptyList[HeadPeerId] =
        headPeerNums.map(HeadPeerId(_, nHeadPeers))

    override def headPeerVKey(p: HeadPeerNumber): Option[VerificationKey] =
        Option.when(p < nHeadPeers)(headPeerVKeys.toList(p))

    override def headPeerVKey(p: HeadPeerId): Option[VerificationKey] =
        Option.when(p.nHeadPeers == nHeadPeers)(headPeerVKeys.toList(p.peerNum))

    override lazy val headMultisigScript: HeadMultisigScript = HeadMultisigScript(this)

    override val nHeadPeers: PositiveInt = PositiveInt.unsafeApply(headPeerVKeys.size)
}

object HeadPeers {
    def apply(headPeerVKeys: NonEmptyList[VerificationKey]): HeadPeers =
        new HeadPeers(headPeerVKeys)

    def apply(headPeerVKeys: List[VerificationKey]): Option[HeadPeers] = for {
        neHeadPeerVKeys <- NonEmptyList.fromList(headPeerVKeys)
    } yield new HeadPeers(neHeadPeerVKeys)

    trait Section {
        def headPeers: HeadPeers

        def headPeerNums: NonEmptyList[HeadPeerNumber]
        def headPeerIds: NonEmptyList[HeadPeerId]

        def headPeerVKeys: NonEmptyList[VerificationKey]

        def headPeerVKey(p: HeadPeerNumber): Option[VerificationKey]
        def headPeerVKey(p: HeadPeerId): Option[VerificationKey]

        def headMultisigScript: HeadMultisigScript

        def nHeadPeers: PositiveInt
    }

    extension (config: HeadPeers.Section & CardanoNetwork.Section)
        def headMultisigAddress: ShelleyAddress =
            config.headMultisigScript.mkAddress(config.network)

        def headPeerAddresses: NonEmptyMap[HeadPeerNumber, ShelleyAddress] = {

            config.headPeerNums
                .zip(config.headPeerVKeys)
                .map((pNum, vKey) =>
                    pNum ->
                        ShelleyAddress(
                          network = config.network,
                          payment = ShelleyPaymentPart.Key(AddrKeyHash(blake2b_224(vKey))),
                          delegation = ShelleyDelegationPart.Null
                        )
                )
                .toNem
        }
}

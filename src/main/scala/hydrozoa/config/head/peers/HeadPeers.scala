package hydrozoa.config.head.peers

import cats.data.{NonEmptyList, NonEmptyMap}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import io.circe.*
import io.circe.generic.semiauto.*
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.AddrKeyHash
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.blake2b_224

import HeadPeerNumber.given

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
    given headPeersEncoder: Encoder[HeadPeers] =
        Encoder
            .encodeMap(using headPeerNumberKeyEncoder, given_Encoder_VerificationKey)
            .contramap(peerKeys =>
                Map.from(
                  peerKeys.headPeerVKeys.zipWithIndex
                      .map((key, idx) => (HeadPeerNumber(idx), key))
                      .toList
                )
            )

    given headPeersDecoder: Decoder[HeadPeers] = {
        def isContiguous(ns: List[Int]): Boolean =
            ns.sorted == Range(0, ns.max + 1)

        Decoder
            .decodeMap(using headPeerNumberKeyDecoder, given_Decoder_VerificationKey)
            .emap(m =>
                for {
                    _ <- Either.cond(
                      test = m.nonEmpty,
                      right = (),
                      left = "headPeers cannot be empty"
                    )
                    _ <- Either.cond(
                      test = isContiguous(m.keys.map(_.toInt).toList),
                      right = (),
                      left = "headPeers does not contain contiguous peer indices starting from zero"
                    )
                } yield HeadPeers(NonEmptyList.fromListUnsafe(m.values.toList))
            )
    }

    def apply(headPeerVKeys: NonEmptyList[VerificationKey]): HeadPeers =
        new HeadPeers(headPeerVKeys)

    def apply(headPeerVKeys: List[VerificationKey]): Option[HeadPeers] = for {
        neHeadPeerVKeys <- NonEmptyList.fromList(headPeerVKeys)
    } yield new HeadPeers(neHeadPeerVKeys)

    trait Section {
        def headPeers: HeadPeers

        def headPeerNums: NonEmptyList[HeadPeerNumber] = headPeers.headPeerNums
        def headPeerIds: NonEmptyList[HeadPeerId] = headPeers.headPeerIds

        def headPeerVKeys: NonEmptyList[VerificationKey] = headPeers.headPeerVKeys

        def headPeerVKey(p: HeadPeerNumber): Option[VerificationKey] = headPeers.headPeerVKey(p)
        def headPeerVKey(p: HeadPeerId): Option[VerificationKey] = headPeers.headPeerVKey(p)

        def headMultisigScript: HeadMultisigScript = headPeers.headMultisigScript

        def nHeadPeers: PositiveInt = headPeers.nHeadPeers
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

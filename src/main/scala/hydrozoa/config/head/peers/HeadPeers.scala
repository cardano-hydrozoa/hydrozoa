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

/** @param webSocketAddress
  *   The connection address for the head peer, i.e. "ws://192.168.10.8081"
  */
final case class HeadPeerData(verificationKey: VerificationKey, webSocketAddress: String)

/** Invariant: Peer numbers must be contiguous, starting from 0.
  * @param headPeerData
  */
final case class HeadPeers private (headPeerData: NonEmptyMap[HeadPeerNumber, HeadPeerData])
    extends HeadPeers.Section {

    override transparent inline def headPeers: HeadPeers = this
}

object HeadPeers {

    def apply(headPeerDataMap: NonEmptyMap[HeadPeerNumber, HeadPeerData]): Option[HeadPeers] = {
        def isContiguous(ns: List[Int]): Boolean =
            ns.sorted == Range(0, ns.max + 1)

        val predicate: Boolean = isContiguous(
          headPeerDataMap.keys.toNonEmptyList.toList.map(_.toInt)
        )

        if predicate
        then Some(new HeadPeers(headPeerDataMap))
        else None
    }

    // Encode a single field of the HeadPeerData as a list from VKey to the field
    private def headPeerDataFieldEncoder[A](
        headPeerDataField: HeadPeerData => A
    )(using encoderA: Encoder[A]): Encoder[HeadPeers] =
        Encoder
            .encodeMap(using headPeerNumberKeyEncoder, encoderA)
            .contramap((headPeers: HeadPeers) =>
                headPeers.headPeerData.map(headPeerDataField).toSortedMap
            )

    private def headPeerDataFieldDecoder[A](using
        decoderA: Decoder[A]
    ): Decoder[NonEmptyMap[HeadPeerNumber, A]] =
        Decoder.decodeNonEmptyMap

    given headPeersEncoder: Encoder[HeadPeers] =
        Encoder.instance(headPeers =>
            Json.obj(
              "headPeerVKeys" -> headPeerDataFieldEncoder(_.verificationKey)(headPeers),
              "headPeerAddresses" -> headPeerDataFieldEncoder(_.webSocketAddress)(headPeers)
            )
        )

    given headPeersDecoder: Decoder[HeadPeers] = {

        Decoder.instance(c =>
            for {
                vKeys <- headPeerDataFieldDecoder[VerificationKey].tryDecode(
                  c.downField("headPeerVKeys")
                )
                addresses <- headPeerDataFieldDecoder[String].tryDecode(
                  c.downField("headPeerAddresses")
                )

                _ <-
                    if vKeys.keys == addresses.keys
                    then Right(())
                    else
                        Left(
                          io.circe.DecodingFailure(
                            "Could not decode HeadPeers: HeadPeerNumbers do not match between" +
                                " fields.",
                            c.history
                          )
                        )

                rawDataMap = vKeys.mapBoth((hpn, vKey) =>
                    (hpn, HeadPeerData(vKey, addresses(hpn).get))
                )

                headPeers <- HeadPeers(rawDataMap) match {
                    case None =>
                        Left(
                          io.circe.DecodingFailure(
                            "Could not decode HeadPeers: HeadPeerNumbers must" +
                                "be contiguous starting from zero.",
                            c.history
                          )
                        )
                    case Some(headPeers: HeadPeers) => Right(headPeers)
                }
            } yield headPeers
        )
    }

    trait Section {
        def headPeers: HeadPeers

        final def headPeerNums: NonEmptyList[HeadPeerNumber] =
            NonEmptyList.fromListUnsafe(headPeers.headPeerData.toSortedMap.keys.toList)

        final def headPeerIds: NonEmptyList[HeadPeerId] =
            headPeerNums.map(HeadPeerId(_, nHeadPeers))

        final def headPeerVKeys: NonEmptyList[VerificationKey] =
            NonEmptyList.fromListUnsafe(
              headPeers.headPeerData.map(hpd => hpd.verificationKey).toSortedMap.values.toList
            )

        final def headPeerVKey(p: HeadPeerNumber): Option[VerificationKey] =
            Option.when(p < nHeadPeers)(headPeerVKeys.toList(p))

        final def headPeerVKey(p: HeadPeerId): Option[VerificationKey] =
            Option.when(p.nHeadPeers == nHeadPeers)(headPeerVKeys.toList(p.peerNum))

        final def headMultisigScript: HeadMultisigScript = HeadMultisigScript(this)

        final def nHeadPeers: PositiveInt = PositiveInt.unsafeApply(headPeerVKeys.size)
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

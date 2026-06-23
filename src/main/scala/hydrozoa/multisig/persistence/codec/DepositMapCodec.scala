package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.persistence.codec.DepositUtxoCodec.given
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codec for [[DepositsMap]] — the value stored at `StoreKey.DepositMap`,
  * JointLedger's deposits snapshot at `softAcked` (§5.2).
  *
  * `DepositsMap` is order-bearing (a `TreeMap` keyed by absorption start time) behind a private
  * constructor, so it is encoded as the flat in-order [[DepositsMap.Entry]] list (`flatten`) and
  * rebuilt with `append` — which recomputes each entry's key from its deposit, restoring the same
  * map. Entries reuse the canonical `RequestId` codec (on its companion) and [[DepositUtxoCodec]].
  */
object DepositMapCodec:

    private given entryEncoder(using CardanoNetwork.Section): Encoder[DepositsMap.Entry] =
        deriveEncoder[DepositsMap.Entry]

    private given entryDecoder(using CardanoNetwork.Section): Decoder[DepositsMap.Entry] =
        deriveDecoder[DepositsMap.Entry]

    given depositsMapEncoder(using CardanoNetwork.Section): Encoder[DepositsMap] =
        Encoder.encodeList[DepositsMap.Entry].contramap(_.flatten.toList)

    given depositsMapDecoder(using CardanoNetwork.Section): Decoder[DepositsMap] =
        Decoder.decodeList[DepositsMap.Entry].map(_.foldLeft(DepositsMap.empty)(_.append(_)))

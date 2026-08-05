package hydrozoa.multisig.persistence

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** One request's local-processing record ([[Cf.RequestBlockIndex]]): the block that included the
  * request and the validity verdict it received there.
  */
final case class RequestBlockEntry(
    blockNum: BlockNumber,
    validity: RequestId.ValidityFlag
)

object RequestBlockEntry:
    given Codec[RequestBlockEntry] = deriveCodec[RequestBlockEntry]

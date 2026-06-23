package hydrozoa.multisig.ledger.joint

import scalus.uplc.builtin.ByteString

/** Key identifying a UTxO slot in the evacuation map. Wraps a raw [[ByteString]]. */
final case class EvacuationKey private (byteString: ByteString)

object EvacuationKey:
    def apply(bytes: ByteString): Option[EvacuationKey] = Some(new EvacuationKey(bytes))

package hydrozoa.multisig.ledger.event

import cats.implicits.catsSyntaxOrder
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import scala.annotation.targetName

type RequestId = RequestId.Id

object RequestId {
    def fromI64(requestIdI64: Long): RequestId = {
        val peerNum = (requestIdI64 >> 40).intValue
        val requestNum = requestIdI64 & ((1L << 40) - 1)
        apply(HeadPeerNumber(peerNum), RequestNumber(requestNum))
    }

    /** Used in the transient fields to indicate whether an event is valid or invalid. (This type is
      * solely here to avoid boolean blindness)
      */
    enum ValidityFlag:
        case Valid
        case Invalid

    opaque type Id = (HeadPeerNumber, RequestNumber)

    def apply(peerNum: HeadPeerNumber, requestNum: RequestNumber): Id = (peerNum, requestNum)

    @targetName("apply_Int")
    def apply(peerNum: Int, requestNum: Long): Id =
        (HeadPeerNumber(peerNum), RequestNumber(requestNum))

    def unapply(self: Id): (HeadPeerNumber, RequestNumber) =
        (HeadPeerNumber(self._1), RequestNumber(self._2))

    /*
      1. Compiler elaborates given Conversion[Id, (Int, Long)] = identity
      2. It infers identity[Id], giving it type Id => Id
      3. Expected type is Id => (Int, Long), so return type Id must coerce to (Int, Long)
      4. Compiler searches for Conversion[Id, (Int, Long)] in implicit scope
      5. It finds the given being defined right now — Scala 3 puts a given in scope during
      elaboration of its own body
      6. To use it, the compiler must elaborate it — goes back to step 1
     */
    // given Conversion[Id, (Int, Long)] = identity

    /*
      When the compiler sees id._1: Int inside the lambda body:
      - id._1 gives HeadPeerNumber, which doesn't conform to Int
      - It needs a conversion. Two candidates:
        - Conversion[HeadPeerNumber, Int] — in implicit scope (remote companion)
        - Conversion[Id, (Int, Long)] — in local scope (current object)
      - Local scope wins. The compiler rewrites id._1: Int as given_Conversion.apply(id)._1 —
      applying the whole conversion to id to get a (Int, Long), then ._1: Int directly
      - That calls the lambda again with the same id → infinite loop
     */
    // given Conversion[Id, (Int, Long)] = id => id._1 -> id._2
    given Conversion[Id, (Int, Long)] = id => id._1.convert -> id._2.convert

    given Ordering[Id] with {
        override def compare(x: Id, y: Id): Int =
            x.convert.compare(y.convert)
    }

    extension (self: Id)
        def asI64: Long = (self._1.toLong << 40) + self.requestNum

        def increment: Id = RequestId(self._1, self._2 + 1)
        def peerNum: HeadPeerNumber = HeadPeerNumber(self._1)
        def requestNum: RequestNumber = RequestNumber(self._2)

        def precedes(other: Id): Boolean =
            self.peerNum == other.peerNum &&
                self.requestNum.increment == other.requestNum
}

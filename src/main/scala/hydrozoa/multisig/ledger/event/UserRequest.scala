//package hydrozoa.multisig.ledger.event
//
//import cats.syntax.all.*
//import hydrozoa.multisig.consensus.peer.HeadPeerNumber
//import scalus.cardano.ledger.{Coin, Value}
//
///** This is just a wrapper that adds event id. */
//sealed trait UserRequest {
//    def requestId: RequestId
//
//    final transparent inline def eventNum: RequestNumber = requestId.requestNum
//    final transparent inline def peerNum: HeadPeerNumber = requestId.peerNum
//}
//
//object UserRequest {
//
//    final case class L2Event(
//                                override val requestId: RequestId,
//                                l2Payload: Array[Byte]
//    ) extends UserRequest
//
//    // TODO: factor out a true request type - depositTxBytes + refundTxBytes + virtualOutputsBytes + depositFee
//    // TODO: See also: EventSequencer.DepositRequest
//    final case class DepositEvent(
//                                     override val requestId: RequestId,
//                                     depositTxBytes: Array[Byte],
//                                     refundTxBytes: Array[Byte],
//                                     l2Payload: Array[Byte],
//                                     l2Value: Value,
//                                     // TODO: explain the name, previously was known as donationToTreasury
//                                     depositFee: Coin,
//    ) extends UserRequest
//
//}

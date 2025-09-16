package hydrozoa.multisig.protocol

import hydrozoa.multisig.consensus.block.Block
import hydrozoa.multisig.consensus.peer.Peer

object Identifiers {
    object LedgerEventNum:
        opaque type LedgerEventNum = Int

        def apply(i: Int): LedgerEventNum = i

        given Conversion[LedgerEventNum, Int] = identity

        given Ordering[LedgerEventNum] with {
            override def compare(x: LedgerEventNum, y: LedgerEventNum): Int =
                x.compare(y)
        }

        extension (self: LedgerEventNum) def increment: LedgerEventNum = LedgerEventNum(self + 1)

    type LedgerEventNum = LedgerEventNum.LedgerEventNum
    

    type LedgerEventId = (Peer.Number, LedgerEventNum)

}

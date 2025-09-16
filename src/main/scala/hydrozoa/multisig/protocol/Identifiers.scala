package hydrozoa.multisig.protocol

import hydrozoa.multisig.consensus.block.Block

object Identifiers {
    object AckNum:
        opaque type AckNum = Int

        def apply(i: Int): AckNum = i

        given Conversion[AckNum, Int] = identity

        given Ordering[AckNum] with {
            override def compare(x: AckNum, y: AckNum): Int =
                x.compare(y)
        }

        extension (self: AckNum) def increment: AckNum = AckNum(self + 1)

    type AckNum = AckNum.AckNum

    object BatchNum:
        opaque type BatchNum = Int

        def apply(i: Int): BatchNum = i

        given Conversion[BatchNum, Int] = identity

        given Ordering[BatchNum] with {
            override def compare(x: BatchNum, y: BatchNum): Int =
                x.compare(y)
        }

        extension (self: BatchNum) def increment: BatchNum = BatchNum(self + 1)

    type BatchNum = BatchNum.BatchNum

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

    object LedgerCallbackNum:
        opaque type LedgerCallbackNum = Int

        def apply(i: Int): LedgerCallbackNum = i

        given Conversion[LedgerCallbackNum, Int] = identity

        given Ordering[LedgerCallbackNum] with {
            override def compare(x: LedgerCallbackNum, y: LedgerCallbackNum): Int =
                x.compare(y)
        }

        extension (self: LedgerCallbackNum)
            def increment: LedgerCallbackNum = LedgerCallbackNum(self + 1)

    type LedgerCallbackNum = LedgerCallbackNum.LedgerCallbackNum

    object PeerNum:
        opaque type PeerNum = Int

        def apply(i: Int): PeerNum = i

        given Ordering[PeerNum] with {
            override def compare(x: PeerNum, y: PeerNum): Int =
                x.compare(y)
        }

        given Conversion[PeerNum, Int] = identity

        extension (self: PeerNum) def increment: PeerNum = PeerNum(self + 1)

    type PeerNum = PeerNum.PeerNum
    
    type PeerId = PeerNum
    type AckId = (PeerId, AckNum)
    type BatchId = (PeerId, BatchNum)
    type LedgerEventId = (PeerId, LedgerEventNum)
    type LedgerCallbackId = (Block.Number, LedgerCallbackNum)

}

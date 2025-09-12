package hydrozoa.multisig.protocol

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

    object BlockNum:
        opaque type BlockNum = Int

        def apply(i: Int): BlockNum = i

        given Conversion[BlockNum, Int] = identity

        given Ordering[BlockNum] with {
            override def compare(x: BlockNum, y: BlockNum): Int =
                x.compare(y)
        }

        extension (self: BlockNum) def increment: BlockNum = BlockNum(self + 1)

    type BlockNum = BlockNum.BlockNum

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

    type BlockId = BlockNum
    type PeerId = PeerNum
    type AckId = (PeerId, AckNum)
    type BatchId = (PeerId, BatchNum)
    type LedgerEventId = (PeerId, LedgerEventNum)
    type LedgerCallbackId = (BlockId, LedgerCallbackNum)

    object BlockVersionMajor:
        opaque type BlockVersionMajor = Int

        def apply(i: Int): BlockVersionMajor = i

        given Conversion[BlockVersionMajor, Int] = identity

    type BlockVersionMajor = BlockVersionMajor.BlockVersionMajor

    object BlockVersionMinor:
        opaque type BlockVersionMinor = Int

        def apply(i: Int): BlockVersionMinor = i

        given Conversion[BlockVersionMinor, Int] = identity

    type BlockVersionMinor = BlockVersionMinor.BlockVersionMinor

    enum BlockType:
        case BlockInit
        case BlockMinor
        case BlockMajor
        case BlockFinal

}

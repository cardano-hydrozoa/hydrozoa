package hydrozoa.l2.block

import hydrozoa.*
import hydrozoa.infra.CryptoHash.H32
import hydrozoa.l2.block.BlockTypeL2.{Major, Minor}
import hydrozoa.l2.block.EventType.{Genesis, Transaction, Withdrawal}
import hydrozoa.l2.event.{L2Event, L2Genesis, L2Transaction_, L2Withdrawal_}
import hydrozoa.l2.ledger.state.Utxos

import scala.collection.mutable

case class Block(
    blockHeader: BlockHeader,
    blockBody: BlockBody
)

val zeroBlock =
    Block(BlockHeader(0, Major, timeCurrent, 0, 0, RH32UtxoSetL2.dummy), BlockBody.empty)

case class BlockHeader(
    blockNum: Int,
    blockType: BlockTypeL2,
    timeCreation: PosixTime,
    versionMajor: Int,
    versionMinor: Int,
    utxosActive: RH32UtxoSetL2
)

enum BlockTypeL2:
    case Minor
    case Major
    case Final

case class BlockBody(
    eventsValid: Seq[(EventType, TxId)],
    eventsInvalid: Seq[(EventType, TxId)],
    depositsAbsorbed: Seq[OutputRef[L1]]
)

object BlockBody:
    def empty: BlockBody = BlockBody(Seq.empty, Seq.empty, Seq.empty)

enum EventType:
    case Genesis
    case Transaction
    case Withdrawal

opaque type RH32UtxoSetL2 = H32[UtxoSetL2]

object RH32UtxoSetL2:
    def dummy: RH32UtxoSetL2 = H32.hash(IArray())

type UtxoSetL2 = Utxos

class BlockBuilder:

    private var timeCreation: Option[PosixTime] = None
    private val eventsValid: mutable.Buffer[(EventType, TxId)] = mutable.Buffer()
    private val eventsInvalid: mutable.Buffer[(EventType, TxId)] = mutable.Buffer()
    private var depositsAbsorbed: Set[OutputRef[L1]] = Set.empty
    private var blockType: Option[BlockTypeL2] = None
    private var blockNum: Int = 0
    private var utxosActive: RH32UtxoSetL2 = RH32UtxoSetL2.dummy
    private var versionMajor, versionMinor: Int = 0

    def withTimeCreation(timeCurrent: PosixTime) =
        this.timeCreation = Some(timeCurrent)
        this

    def withConfirmedEvent(txId: TxId, l2Event: L2Event) =
        eventsValid.appendAll(mutable.Buffer((eventTypeTag(l2Event), txId))) // FIXME:
        this

    def withInvalidEvent(txId: TxId, l2Event: L2Event) =
        eventsInvalid.appendAll(mutable.Buffer((eventTypeTag(l2Event), txId))) // FIXME:
        this

    def withDeposits(ds: Set[OutputRef[L1]]) =
        this.depositsAbsorbed = ds
        this

    def withBlockType(ty: BlockTypeL2) =
        this.blockType = Some(ty)
        this

    def withBlockNum(num: Int) =
        this.blockNum = num
        this

    def withUtxosActive(hash: RH32UtxoSetL2) =
        this.utxosActive = hash
        this

    def withPreviousVersions(major: Int, minor: Int) =
        blockType match
            case Some(Minor) =>
                versionMajor = major
                versionMinor = minor + 1
            case Some(_) =>
                versionMajor = major + 1
                versionMinor = 0
            case _ => throw IllegalStateException("block type is not known")

    def build: Block =
        Block(
          BlockHeader(
            blockNum,
            blockType.get,
            timeCreation.get,
            versionMajor,
            versionMinor,
            utxosActive
          ),
          BlockBody(this.eventsValid.toSeq, this.eventsInvalid.toSeq, this.depositsAbsorbed.toSeq)
        )

def eventTypeTag(e: L2Event): EventType = e match
    case _: L2Genesis      => Genesis
    case _: L2Transaction_ => Transaction
    case _: L2Withdrawal_  => Withdrawal

package hydrozoa.l2.block

import hydrozoa.infra.CryptoHash.H32
import hydrozoa.l2.block.EventType.{Genesis, Transaction, Withdrawal}
import hydrozoa.l2.event.{L2Event, L2Genesis, L2Transaction_, L2Withdrawal_}
import hydrozoa.l2.ledger.state.Utxos
import hydrozoa.{L1, OutputRef, PosixTime, TxId}

import scala.collection.mutable

case class Block(
    blockHeader: BlockHeader,
    blockBody: BlockBody
)

case class BlockHeader(
    blockNum: Int,
    blockType: BlockTypeL2,
    timeCreation: BigInt,
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
    depositsAbsorbed: Seq[OutputRef[L1]]
)

enum EventType:
    case Genesis
    case Transaction
    case Withdrawal

opaque type RH32UtxoSetL2 = H32[UtxoSetL2]
type UtxoSetL2 = Utxos

//def majorDummyBlock(major: Int, depositsAbsorbed: Set[AwaitingDeposit]): Block =
//    Block(
//      BlockHeader(0, BlockTypeL2.Major, 0, major, 0, H32.hash(IArray())),
//      BlockBody(Seq.empty, depositsAbsorbed)
//    )
//
//def finalDummyBlock(major: Int): Block =
//    Block(
//      BlockHeader(0, BlockTypeL2.Final, 0, major, 0, H32.hash(IArray())),
//      BlockBody(Seq.empty, Set.empty)
//    )

class BlockBuilder:

    private var timeCreation: Option[PosixTime] = None
    private val eventsValid: mutable.Buffer[(EventType, TxId)] = mutable.Buffer()
    private val eventsInvalid: mutable.Buffer[(EventType, TxId)] = mutable.Buffer()

    def withTimeCreation(timeCurrent: PosixTime): BlockBuilder =
        this.timeCreation = Some(timeCurrent)
        this

    def withConfirmedEvent(txId: TxId, l2Event: L2Event): BlockBuilder =
        eventsValid.appendAll(mutable.Buffer((eventTypeTag(l2Event), txId))) // FIXME:
        this

    def withInvalidEvent(txId: TxId, l2Event: L2Event): BlockBuilder =
        eventsInvalid.appendAll(mutable.Buffer((eventTypeTag(l2Event), txId))) // FIXME:
        this

    def withDeposits(ds: Set[OutputRef[L1]]): BlockBuilder = // FIXME: type
        // ???
        this

    def withBlockType(ty: BlockTypeL2): BlockBuilder =
        // ???
        this

    def build: Block = ???

def eventTypeTag(e: L2Event): EventType = e match
    case _: L2Genesis      => Genesis
    case _: L2Transaction_ => Transaction
    case _: L2Withdrawal_  => Withdrawal

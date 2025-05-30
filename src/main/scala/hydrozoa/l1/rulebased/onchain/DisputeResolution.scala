package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.rulebased.state.RH32UtxoSetL2
import hydrozoa.l2.block.BlockTypeL2
import scalus.builtin.ByteString
import scalus.ledger.api.v1.PosixTime

enum DisputeRedeemer:
    case Vote(voteRedeemer: Option[MinorBlockL1Effect])
    case Tally
    case Resolve

type Signature = ByteString

case class MinorBlockL1Effect(
    blockHeader: BlockHeader,
    multisig: List[Signature]
)

case class BlockHeader(
    blockNum: BigInt,
    // TODO: should we re-use Hydrozoa's type broadly?
    blockType: BlockTypeL2,
    timeCreation: PosixTime,
    versionMajor: BigInt,
    versionMinor: BigInt,
    utxosActive: RH32UtxoSetL2,
)

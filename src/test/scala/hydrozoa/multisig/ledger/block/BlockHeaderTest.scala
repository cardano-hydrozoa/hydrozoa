package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.block.BlockHeader.Minor
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genOnchainBlockHeader
import org.scalacheck.*
import org.scalacheck.util.Pretty

object BlockHeaderTest extends Properties("Block header test") {
    given ppBlockHeadOnchain: (List[BlockHeader.Minor.Onchain] => Pretty) = headers =>
        Pretty(_ =>
            headers
                .map(header =>
                    "Onchain(" +
                        s"\n\tblockNum: ${header.blockNum}," +
                        s"\n\tstartTime: ${header.startTime}," +
                        s"\n\tversionMajor: ${header.versionMajor}," +
                        s"\n\tversionMinor: ${header.versionMinor}," +
                        s"\n\tcommitment: ${header.commitment.take(8)}(...)" +
                        "\n)"
                )
                .mkString("[", ", ", "]")
        )

    val _ = property("Fixed size block headers") = Prop.forAllNoShrink(
      Gen.nonEmptyListOf(for {
          versionMajor <- Gen.posNum[Int]
          header <- genOnchainBlockHeader(versionMajor)
      } yield header)
    )(listOfHeaders =>
        val lengths = listOfHeaders.map(Minor.Onchain.Serialized(_).length)
        lengths.forall(_ == lengths.head)
    )
}

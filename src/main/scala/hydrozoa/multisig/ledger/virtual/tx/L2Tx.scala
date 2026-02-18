package hydrozoa.multisig.ledger.virtual.tx

import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.joint.obligation.Payout
import scala.util.Try
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.Metadatum.Int
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Metadatum, Transaction, TransactionInput, TransactionOutput, Word64}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

// TODO: Refactor it using our usual style
// TODO: Run L2 conformance during parsing? - yes

final case class L2Tx(
    tx: Transaction,
    l1utxos: List[(TransactionInput, Babbage)],
    l2utxos: List[(TransactionInput, Babbage)],
    // TODO: do we need it?
    resolvedUtxos: ResolvedUtxos
) {
    // TODO: do we need it? tokens?
    def volume: Long = tx.body.value.outputs.map(sto => sto.value.value.coin.value).sum

    def payoutObligations: Vector[Payout.Obligation] =
        Vector.from(l1utxos.map(utxo => Payout.Obligation(utxo._1, utxo._2)))
}

object L2Tx:
    export L2TxOps.build
    export L2TxOps.parse

private object L2TxOps:

    // TODO: the code is in the stage command generation
    //  - give me inputs, outputs and their destination and I will give you transaction
    def build: Void = ???

    // TODO: use Either
    def parse(bs: Array[Byte]): Either[String, L2Tx] = for {
        tx <- Try(Transaction.fromCbor(bs)).toEither.left.map(_.toString)
        up <- utxoPartition(tx)
    } yield L2Tx(
      tx = tx,
      l1utxos = up.l1Utxos,
      l2utxos = up.l2Utxos,
      // TODO:
      resolvedUtxos = ResolvedUtxos.empty
    )

    final case class UtxoPartition(
        l1Utxos: List[(TransactionInput, Babbage)],
        l2Utxos: List[(TransactionInput, Babbage)]
    )

    /** @param tx
      * @return
      *   An error if the metadata cant be parsed, or a pair of lists indicating (l1Bound, l2Bound)
      */
    def utxoPartition(
        tx: Transaction
    ): Either[String, UtxoPartition] =
        for {
            metadataMap <- tx.auxiliaryData match {
                case Some(keepRawM) =>
                    keepRawM.value match {
                        case Metadata(m) => Right(m)
                        case _           => Left("metadata not list")
                    }
                case _ => Left("Malformed metadata")
            }
            // Should we use a different tag here to indicate its L2?
            metaDatum <- metadataMap
                .get(Word64(CIP67.Tags.head))
                .toRight(
                  s"Head tag ${CIP67.Tags.head} not" +
                      "found in metadata map"
                )

            outputs <- {
                val outputs = tx.body.value.outputs.map(_.value)
                if outputs.forall(_.isInstanceOf[Babbage])
                then Right(outputs.map(_.asInstanceOf[Babbage]))
                else Left("Non-babbage output found in utxo partition")
            }

            // TODO: This is an idiot-proof way to do it. A better way might be a bitmask -- 0 for L1, 1 for L2
            l1OrL2 <- metaDatum match {
                case Metadatum.List(il: IndexedSeq[Metadatum])
                    if il.length == outputs.length
                        && il.forall(elem => elem == Int(1) || elem == Int(2)) =>
                    Right(il)
                case _ => Left("Malformed index list in L2 transaction")
            }

            partition = {
                // NOTE/FIXME: there are multiple traversals here, but the transformation is a little bit
                // tricky. This can be refactored to do it in one pass if it becomes a bottleneck.

                // Format: (output, l1OrL2, index)
                val zippedOutputs =
                    outputs.zip(l1OrL2).zipWithIndex.map(x => (x._1._1, x._1._2, x._2))

                // format: ((input, output), l1orL2)
                val utxosWithDesignation =
                    zippedOutputs.map(x => ((TransactionInput(tx.id, x._3), x._1), x._2))

                // format: ([((l1Input, l1Output), l1orL2)] , [((l2Input, l2Output), l1orL2)])
                val partitionWithDesignation =
                    utxosWithDesignation.partition(x => if x._2 == Int(1) then true else false)

                UtxoPartition(
                  partitionWithDesignation._1.map(_._1).toList,
                  partitionWithDesignation._2.map(_._1).toList
                )
            }

        } yield partition

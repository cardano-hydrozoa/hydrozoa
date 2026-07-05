package hydrozoa.multisig.ledger.eutxol2.tx

import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.token.CIP67
import scala.annotation.unused
import scala.util.Try
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.Metadatum.Int as MInt
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{KeepRaw, Metadatum, MultiAsset, Sized, Transaction, TransactionInput, TransactionOutput, Word64}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

// TODO: Refactor it using our usual style
// TODO: Run L2 conformance during parsing? - yes

final case class L2Tx(
    tx: Transaction,
    l1utxos: List[(TransactionInput, TransactionOutput)],
    l2utxos: List[(TransactionInput, Babbage)],
    /** Declared transient token content per output index (the `transientOutputs` metadata field).
      * Indices absent from the map carry no transient tokens; only L2-bound outputs may appear.
      */
    transientOutputs: Map[Int, MultiAsset],
    // TODO: do we need it?
    resolvedUtxos: ResolvedUtxos
) {
    // TODO: do we need it? tokens?
    def volume: Long = tx.body.value.outputs.map(sto => sto.value.value.coin.value).sum

    def payoutObligations(
        network: CardanoNetwork.Section
    ): Either[Payout.Obligation.MinAdaViolation, Vector[Payout.Obligation]] =
        Vector
            .from(
              l1utxos.map(utxo =>
                  Payout.Obligation(KeepRaw(utxo._2.asInstanceOf[TransactionOutput]), network)
              )
            )
            .sequence

    /** The transient-token compartment entries this transaction creates, keyed by the new utxo ids
      * (all L2-bound outputs).
      */
    def mkTransientUtxos: Map[TransactionInput, MultiAsset] =
        transientOutputs.map { case (index, bundle) =>
            TransactionInput(tx.id, index) -> bundle
        }

    /** The L1 projection of this transaction: the mint field stripped and each output's value
      * reduced by its declared transient bundle. Balancing the projection against the main
      * compartment alone proves the post-transaction state stays L1-remittable — and makes minting
      * or burning main-compartment (L1-native) tokens impossible by arithmetic, with no policy-id
      * checks anywhere. The projection's changed serialized id is irrelevant: it is fed only to the
      * value-conservation rule, never signed or hashed against.
      */
    def projectToL1: Transaction = {
        val body = tx.body.value
        val projectedOutputs = body.outputs.zipWithIndex.map { case (sized, index) =>
            transientOutputs.get(index) match {
                case Some(bundle) =>
                    val output = sized.value
                    Sized(
                      output.withValue(
                        output.value.copy(assets = output.value.assets - bundle)
                      )
                    )
                case None => sized
            }
        }
        tx.copy(body = KeepRaw(body.copy(mint = None, outputs = projectedOutputs)))
    }
}

object L2Tx:
    export L2TxOps.build
    export L2TxOps.parse

private object L2TxOps:

    // TODO: the code is in the stage command generation
    //  - give me inputs, outputs and their destination and I will give you transaction
    def build: Void = ???

    // TODO: use Either
    def parse(bs: Array[Byte], @unused network: CardanoNetwork.Section): Either[String, L2Tx] =
        for {
            tx <- Try(Transaction.fromCbor(bs)).toEither.left.map(_.toString)
            parsed <- parseOutputsMetadata(tx)
            (up, transientOutputs) = parsed
            _ <- validateTransientDeclarations(tx, up, transientOutputs)
        } yield L2Tx(
          tx = tx,
          l1utxos = up.l1Utxos,
          l2utxos = up.l2Utxos,
          transientOutputs = transientOutputs,
          // TODO:
          resolvedUtxos = ResolvedUtxos.empty
        )

    final case class UtxoPartition(
        l1Utxos: List[(TransactionInput, Babbage)],
        l2Utxos: List[(TransactionInput, Babbage)]
    )

    /** Parse the head-label metadata into the output partition (L1-bound vs L2-bound) and the
      * transient-token declarations. Two metadatum shapes are accepted at the head label:
      *
      *   - legacy: a bare `List` of per-output `Int(1)` (L1-bound) / `Int(2)` (L2-bound) markers —
      *     no transient outputs;
      *   - current: a `Map` with the required `Text("outputs") -> List(...)` marker list and an
      *     optional `Text("transientOutputs")` field (see [[TransientOutputs]]).
      */
    def parseOutputsMetadata(
        tx: Transaction
    ): Either[String, (UtxoPartition, Map[Int, MultiAsset])] =
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

            shapes <- metaDatum match {
                case markers: Metadatum.List => Right((markers, None))
                case Metadatum.Map(entries) =>
                    for {
                        markers <- entries.get(Metadatum.Text("outputs")) match {
                            case Some(markers: Metadatum.List) => Right(markers)
                            case other =>
                                Left(s"Metadata field 'outputs' must be a List, got $other")
                        }
                    } yield (markers, entries.get(Metadatum.Text("transientOutputs")))
                case _ => Left("Malformed head-label metadatum in L2 transaction")
            }
            (markers, transientOutputsMetadatum) = shapes

            // TODO: This is an idiot-proof way to do it. A better way might be a bitmask -- 0 for L1, 1 for L2
            l1OrL2 <- markers match {
                case Metadatum.List(il: IndexedSeq[Metadatum])
                    if il.length == outputs.length
                        && il.forall(elem => elem == MInt(1) || elem == MInt(2)) =>
                    Right(il)
                case _ => Left("Malformed index list in L2 transaction")
            }

            transientOutputs <- transientOutputsMetadatum match {
                case Some(metadatum) => TransientOutputs.decodeMetadatum(metadatum)
                case None            => Right(Map.empty[Int, MultiAsset])
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
                    utxosWithDesignation.partition(x => if x._2 == MInt(1) then true else false)

                UtxoPartition(
                  partitionWithDesignation._1.map(_._1).toList,
                  partitionWithDesignation._2.map(_._1).toList
                )
            }

        } yield (partition, transientOutputs)

    /** Check the transient declarations against the transaction's outputs:
      *
      *   - every declared index refers to an existing output;
      *   - L1-bound (withdrawal-marked) outputs declare nothing — transient tokens cannot leave the
      *     head, so a withdrawal carrying them is rejected outright rather than stripped;
      *   - each declared bundle is a sub-value of its output's assets (component-wise `<=`). This
      *     per-output check is independent of the projection's conservation rule: a negative asset
      *     in one projected output could otherwise offset a positive excess in another.
      */
    private def validateTransientDeclarations(
        tx: Transaction,
        partition: UtxoPartition,
        transientOutputs: Map[Int, MultiAsset]
    ): Either[String, Unit] = {
        val outputs = tx.body.value.outputs
        val l1BoundIndices =
            partition.l1Utxos.map { case (input, _) => input.index }.toSet
        transientOutputs.toList.traverse_ { case (index, bundle) =>
            for {
                _ <- Either.cond(
                  index < outputs.length,
                  (),
                  s"transientOutputs: declared index $index out of range (${outputs.length} outputs)"
                )
                _ <- Either.cond(
                  !l1BoundIndices.contains(index),
                  (),
                  s"transientOutputs: L1-bound output $index cannot carry transient tokens"
                )
                _ <- Either.cond(
                  (outputs(index).value.value.assets - bundle).negativeAssets.isEmpty,
                  (),
                  s"transientOutputs: declared bundle for output $index exceeds the output's assets"
                )
            } yield ()
        }
    }

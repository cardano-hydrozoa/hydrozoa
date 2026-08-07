package hydrozoa.multisig.ledger.eutxol2.tx

import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.joint.obligation.Payout
import scala.annotation.unused
import scala.util.Try
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{KeepRaw, MultiAsset, Sized, Transaction, TransactionInput, TransactionOutput}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

// TODO: Refactor it using our usual style
// TODO: Run L2 conformance during parsing? - yes

final case class L2Tx(
    tx: Transaction,
    /** The head this transaction is pinned to, parsed from the L2 head-label metadata. */
    headId: HeadId,
    l1utxos: List[(TransactionInput, TransactionOutput)],
    l2utxos: List[(TransactionInput, Babbage)],
    /** Declared transient token content per output index (the `l2TransientTokens` metadata field).
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

    /** The projection of this transaction to the main compartment: the mint field stripped and each
      * output's value reduced by its declared transient bundle. Balancing the projection against
      * the main compartment alone proves the post-transaction state stays L1-remittable — and makes
      * minting or burning main-compartment (L1-native) tokens impossible by arithmetic, with no
      * policy-id checks anywhere. The projection's changed serialized id is irrelevant: it is fed
      * only to the value-conservation rule, never signed or hashed against.
      */
    def projectMain: Transaction = {
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
            (headId, up, transientOutputs) = parsed
            _ <- validateTransientDeclarations(tx, up, transientOutputs)
        } yield L2Tx(
          tx = tx,
          headId = headId,
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

    /** Parse the head-label metadata ([[L2Metadata]]) into the pinned headId, the output partition
      * (L1-bound vs L2-bound), and the transient-token declarations. Outputs whose index is listed
      * in `l1BoundOutputs` are L1-bound (withdrawals); every other output stays on L2.
      */
    def parseOutputsMetadata(
        tx: Transaction
    ): Either[String, (HeadId, UtxoPartition, Map[Int, MultiAsset])] =
        for {
            parsed <- L2Metadata.parse(tx)
            (headId, metadata) = parsed

            outputs <- {
                val outputs = tx.body.value.outputs.map(_.value)
                if outputs.forall(_.isInstanceOf[Babbage])
                then Right(outputs.map(_.asInstanceOf[Babbage]))
                else Left("Non-babbage output found in utxo partition")
            }

            _ <- metadata.l1BoundOutputs.traverse_ { index =>
                Either.cond(
                  index < outputs.length,
                  (),
                  s"l1BoundOutputs: index $index out of range (${outputs.length} outputs)"
                )
            }

            partition = {
                val l1BoundIndices = metadata.l1BoundOutputs.toSet
                val utxos = outputs.zipWithIndex.map { case (output, index) =>
                    TransactionInput(tx.id, index) -> output
                }
                val (l1Bound, l2Bound) =
                    utxos.partition { case (input, _) => l1BoundIndices.contains(input.index) }
                UtxoPartition(l1Bound.toList, l2Bound.toList)
            }

        } yield (headId, partition, metadata.l2TransientTokens)

    /** Check the transient declarations against the transaction's outputs:
      *
      *   - every declared index refers to an existing output;
      *   - L1-bound outputs declare nothing — transient tokens cannot leave the head, so a
      *     withdrawal carrying them is rejected outright rather than stripped;
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

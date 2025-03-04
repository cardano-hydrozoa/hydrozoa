package hydrozoa.infra

import com.bloxbean.cardano.client.api.model.Amount.{ada, lovelace}
import com.bloxbean.cardano.client.api.model.Utxo.UtxoBuilder
import com.bloxbean.cardano.client.api.model.{Result, Utxo}
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput
import com.bloxbean.cardano.client.util.HexUtil

import scala.jdk.CollectionConverters.*

extension [A](result: Result[A])
    def toEither: Either[String, A] =
        if result.isSuccessful then Right(result.getValue)
        else Left(result.getResponse)

    // TODO: we don't handle errors properly so far
    def force: A =
        if result.isSuccessful then result.getValue
        else throw RuntimeException("Unexpected left")

// Make an Utxo from an output reference + TransactionOutput
// For now has some limitations:
// * no datum hashes
// * no scripts
// * FIXME: only ada
def txOutputToUtxo(txHash: String, txIx: Int, output: TransactionOutput): Utxo =
    Utxo(
      txHash,
      txIx,
      output.getAddress,
      List(lovelace(output.getValue.getCoin)).asJava,
      null, // no datum hashes
      output.getInlineDatum.serializeToHex,
      null // no scripts
    )

package hydrozoa.infra

import com.bloxbean.cardano.client.api.model.Amount.lovelace
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

extension [A](option: Option[A])
    def toResult(err: String): Result[A] = option match
        case Some(a) =>
            val result = Result.success("dummy").asInstanceOf[Result[A]]
            result.withValue(a).asInstanceOf[Result[A]]
        case None => Result.error(err).asInstanceOf[Result[A]]

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

def encodeHex(bytes: IArray[Byte]): String =
    HexUtil.encodeHexString(IArray.genericWrapArray(bytes).toArray)

def decodeHex(hex: String): IArray[Byte] = IArray.from(HexUtil.decodeHexString(hex))

// Piper!
implicit class Piper[A](val x: A) extends AnyVal {
    def |>[B](f: A => B): B = f(x)
}

// PS-style pair constructor
implicit final class PSStyleAssoc[A](private val self: A) extends AnyVal {
    @inline def /\[B](y: B): (A, B) = (self, y)
}

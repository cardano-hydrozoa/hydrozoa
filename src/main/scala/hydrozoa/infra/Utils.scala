package hydrozoa.infra

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.api.model.{Amount, Result, Utxo}
import com.bloxbean.cardano.client.crypto.KeyGenUtil.getKeyHash
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput
import com.bloxbean.cardano.client.util.HexUtil
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import hydrozoa.{AnyLayer, L1, L2, Network, UtxoId, VerificationKeyBytes}
import scalus.builtin.ByteString
import scalus.ledger.api.v3.TxId
import scalus.ledger.api.v3.TxOutRef

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

extension [A](result: Result[A])
    def toEither: Either[String, A] =
        if result.isSuccessful then Right(result.getValue)
        else Left(result.getResponse)

    // TODO: we don't handle errors properly so far
    def force: A =
        if result.isSuccessful then result.getValue
        else
            println
            throw RuntimeException(s"Unexpected: ${result.getResponse}")

object ResultUtils:
    def mkResult[A](value: A): Result[A] =
        val result = Result.success("dummy").asInstanceOf[Result[A]]
        result.withValue(value).asInstanceOf[Result[A]]

    def mkResultError[A]: Result[A] =
        Result.error().asInstanceOf[Result[A]]

extension [A](option: Option[A])
    def toResult(err: String): Result[A] = option match
        case Some(a) => ResultUtils.mkResult[A](a)
        case None    => Result.error(err).asInstanceOf[Result[A]]

// Make an Utxo from an output reference + TransactionOutput.
// Used in BloxBean builders.
// For now has some limitations:
// * no datum hashes
// * no scripts
def txOutputToUtxo(txHash: String, txIx: Int, output: TransactionOutput): Utxo =
    val assets = mutable.Buffer[Amount]()
    output.getValue.toMap.forEach((policy, inner) =>
        inner.forEach((name, amount) => assets.append(Amount.asset(policy, name, amount)))
    )
    Utxo(
      txHash,
      txIx,
      output.getAddress,
      (List(Amount.lovelace(output.getValue.getCoin)) ++ assets.toList).asJava,
      null, // TODO: no datum hashes
      output.getInlineDatum.serializeToHex,
      null // TODO: no scripts
    )

def encodeHex(bytes: IArray[Byte]): String =
    HexUtil.encodeHexString(IArray.genericWrapArray(bytes).toArray)

def decodeHex(hex: String): IArray[Byte] = IArray.from(HexUtil.decodeHexString(hex))

extension (vkb: VerificationKeyBytes) def verKeyHash: String = getKeyHash(vkb.bytes)

// Piper!
implicit class Piper[A](val x: A) extends AnyVal {
    def |>[B](f: A => B): B = f(x)
}

// PS-style pair constructor
implicit final class PSStyleAssoc[A](private val self: A) extends AnyVal {
    @inline def /\[B](y: B): (A, B) = (self, y)
}

def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case h :: t =>
        h match {
            case None => None
            case Some(head) =>
                sequence(t) match {
                    case None       => None
                    case Some(list) => Some(head :: list)
                }
        }
}

extension [L <: AnyLayer](self: UtxoId[L])
    def toTxOutRefV3: TxOutRef = {
        val txId = TxId.apply(ByteString.fromHex(self.txId.hash))
        val txIx = BigInt(self.outputIx.ix)
        TxOutRef.apply(txId, txIx)
    }

extension (self: Network) def toBB: BBNetwork = BBNetwork(self.networkId, self.protocolMagic)

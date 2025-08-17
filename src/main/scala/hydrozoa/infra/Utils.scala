package hydrozoa.infra

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.api.model.{Amount, Result, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.KeyGenUtil.getKeyHash
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import scalus.bloxbean.Interop
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.Data.fromData
import scalus.builtin.{ByteString, FromData}
import scalus.cardano.address.Network
import scalus.cardano.ledger.{AddrKeyHash, Hash}
import scalus.ledger.api.v3.{TxId, TxOutRef}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

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

extension (vkb: VerificationKeyBytes) def verKeyHash: AddrKeyHash = Hash(blake2b_224(vkb.bytes))

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
        val txId = self.transactionId
        TxOutRef.apply(TxId(txId), self.index)
    }

// FIXME: This is a static value of 42. Scalus network doesn't expose the protocl magic.
extension (self: Network) def toBB: BBNetwork = BBNetwork(self.value.toInt, 42)

def getUtxoWithDatum[T](using
    FromData[T]
)(utxoId: UtxoIdL1, backendService: BackendService): Either[String, (Utxo, T)] =
    for
        utxo <- backendService.getUtxoService
            .getTxOutput(utxoId.transactionId.toHex, utxoId.index)
            .toEither

        datum <- Try(
          fromData[T](
            Interop.toScalusData(
              PlutusData
                  .deserialize(HexUtil.decodeHexString(utxo.getInlineDatum))
            )
          )
        ) match {
            case Success(s) => Right(s)
            case Failure(e) =>
                Left(
                  e.toString
                )
        }
    yield (utxo, datum)

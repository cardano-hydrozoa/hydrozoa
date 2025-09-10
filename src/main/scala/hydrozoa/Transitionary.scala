package hydrozoa

// These types are (mostly) temporary bridging between hydrozoa, scalus, and bloxbean types.
// Eventually, we want to move exclusively to the scalus types
// TODO: Not tests for this module yet

import com.bloxbean.cardano.client.api.model.Utxo as BBUtxo
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.transaction.spec.{Transaction as BBTransaction, Value as BBValue}
import com.bloxbean.cardano.client.util.HexUtil
import scalus.|>

import scala.::
//import hydrozoa.infra.{Piper, toEither, valueTokens}
import hydrozoa.{Address, *}
import io.bullet.borer.Cbor
import scalus.bloxbean.Interop
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.ledger.*
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.toLedgerValue
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Transaction.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.StakingCredential.StakingHash
import scalus.ledger.api.v2.OutputDatum.{NoOutputDatum, OutputDatum}
import scalus.ledger.api.{v1, v3}
import scalus.prelude.Option as ScalusOption
import scalus.{ledger, prelude}

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.language.implicitConversions

import com.bloxbean.cardano.client.api.model.{Amount, Result, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput as BBTO
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import scalus.bloxbean.Interop
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.Data.fromData
import scalus.builtin.FromData
import scalus.cardano.address.Network
import scalus.cardano.ledger.{AddrKeyHash, Hash}
import scalus.ledger.api.v3.{TxId, TxOutRef}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

//////////////////////////////////
// "Empty" values used for building up real values and for testing

val emptyTxBody: TransactionBody = TransactionBody(
  inputs = Set.empty,
  outputs = IndexedSeq.empty,
  fee = Coin(0)
)
//
//val emptyTransaction: Transaction = {
//    Transaction(
//        body = KeepRaw(emptyTxBody),
//        witnessSet = TransactionWitnessSet.empty,
//        isValid = false,
//        auxiliaryData = None
//    )
//}
//
//val emptyContext: Context =
//    Context(fee = Coin(0L), env = UtxoEnv.default, slotConfig = SlotConfig.Preprod)
//
//val emptyState: State = State(utxo = Map.empty, certState = CertState.empty)
//
//////////////////////////////////////////////////////////////////////////////////////////////////////
//// Conversions
//
////////////////////////////////////////////////////////
//// Transaction Output
//
//extension (to: v3.TxOut) {
//    def toScalusLedger: TransactionOutput = {
//        Babbage(
//            address = to.address.toScalusLedger,
//            value = to.value.toScalusLedger,
//            datumOption = to.datum match {
//                case NoOutputDatum  => None
//                case OutputDatum(d) => Some(Inline(d))
//                case _              => throw new RuntimeException("invalid datum")
//            },
//            scriptRef = to.referenceScript match {
//                case ScalusOption.None => None
//                // The v3TxOut contains a script hash, but the ledger type contains the actual script.
//                // We can't recover the full script from the hash, so we throw.
//                // Maybe in the future we can pass in the script separately and ensure the hashes match
//                case ScalusOption.Some(sr) =>
//                    throw new RuntimeException("v3 TxOut has a reference script hash, but can't")
//            }
//        )
//    }
//}
//
//////////////////////////////////////////////////////
//// Address
//
//// FIXME: This isn't a full translation. We don't care about delegation, so we drop them.
//extension (addr: v3.Address) {
//    def toScalusLedger[L <: AnyLayer]: Address[L] =
//        Address[L](
//            ShelleyAddress(
//                // FIXME: We use the static network here
//                network = networkL1static,
//                payment = addr.credential match {
//                    case PubKeyCredential(pkc) =>
//                        ShelleyPaymentPart.Key(Hash(ByteString.fromArray(pkc.hash.bytes)))
//                    case ScriptCredential(sc) =>
//                        ShelleyPaymentPart.Script(Hash(ByteString.fromArray(sc.bytes)))
//                },
//                delegation = ShelleyDelegationPart.Null
//            )
//        )
//
//}
//
///** Convert scalus.ledger.api.v1.Address to scalus.cardano.address.Address .
// *
// * This function converts between the simplified address representation used in Plutus script
// * contexts and the comprehensive address representation used in the domain model.
// */
//
//// TODO: Needs tests
//def v1AddressToLedger(address: v1.Address, network: Network): ShelleyAddress = {
//
//    val paymentPart: ShelleyPaymentPart = address.credential match {
//        case ledger.api.v1.Credential.PubKeyCredential(v1hash) =>
//            ShelleyPaymentPart.Key(AddrKeyHash(v1hash.hash))
//        case ledger.api.v1.Credential.ScriptCredential(v1hash) =>
//            ShelleyPaymentPart.Script(Hash(v1hash))
//    }
//
//    val delegationPart: ShelleyDelegationPart = address.stakingCredential match {
//        case prelude.Option.None => Null
//        case prelude.Option.Some(sc) =>
//            sc match {
//                case sh: StakingHash =>
//                    sh match {
//                        case ledger.api.v1.StakingCredential.StakingHash(v1Hash) =>
//                            v1Hash match {
//                                case ledger.api.v1.Credential.PubKeyCredential(v1Key) =>
//                                    ShelleyDelegationPart.Key(Hash(v1Key.hash))
//                                case ledger.api.v1.Credential.ScriptCredential(v1Script) =>
//                                    ShelleyDelegationPart.Script(Hash(v1Script))
//                            }
//                    }
//                case ledger.api.v1.StakingCredential.StakingPtr(a, b, c) =>
//                    ShelleyDelegationPart.Pointer(Pointer(Slot(a.toLong), b.toLong, c.toLong))
//            }
//    }
//    ShelleyAddress(network = network, payment = paymentPart, delegation = delegationPart)
//}
//
////////////////////////////////////////////////////
//// Hashes/PolicyId
//
//extension [HF, P](hash: Hash[HF, P]) {
//    def toIArray: IArray[Byte] =
//        IArray.from(hash.bytes)
//}
//
//def csToPolicyId(cs: v3.CurrencySymbol): PolicyId = {
//    Hash(ByteString.fromArray(cs.bytes))
//}
//
////////////////////////////////////////////////////
//// Token Name
//
//def tnToAssetName(tn: v3.TokenName): AssetName = AssetName.fromHex(tn.toHex)
//
/////////////////////////////////////////
//// Value/MultiAsset Map
//
//
//def valueTokens[L <: AnyLayer](tokens: BBValue): MultiAsset = MultiAsset({
//    SortedMap.from(
//        tokens.toMap.asScala.toMap.map((k, v) =>
//            Hash[Blake2b_224, HashPurpose.ScriptHash](ByteString.fromHex(k))
//                -> SortedMap.from(
//                v.asScala.toMap.map((k, v) =>
//                    AssetName(ByteString.fromHex(k.drop(2))) -> v.longValue()
//                )
//            )
//        )
//    )
//})
//
//
//extension (v: BBValue) {
//    def toScalus: Value = {
//        val coin = Coin(v.getCoin.longValue())
//        val tokens = valueTokens(v)
//        Value(coin = coin, multiAsset = tokens)
//    }
//}
//
//extension (v: v3.Value) {
//    def toScalusLedger: Value = {
//        val coins: Coin = Coin(v.flatten.head._3.toLong)
//        val ma0: prelude.List[(PolicyId, prelude.List[(AssetName, Long)])] =
//            v.toSortedMap.toList.tail.map((cs, assocMap) =>
//                (csToPolicyId(cs), assocMap.toList.map((tn, bi) => (tnToAssetName(tn), bi.toLong)))
//            )
//
//        // Note: The reason I don't go directly to a SortedMap here is because the compiler gets confused
//        // about ambiguous instances. Doing it in the definition of ma1 helps inference.
//        def listToSeq[A](l: prelude.List[A]): Seq[A] =
//            l.foldLeft(Seq.empty)(_.appended(_))
//
//        val ma1 = MultiAsset(
//            SortedMap.from(listToSeq(ma0.map(x => (x._1, SortedMap.from(listToSeq(x._2))))))
//        )
//
//        Value(coin = coins, multiAsset = ma1)
//    }
//}
//
//extension (tx: BBTransaction) {
//    def toScalus: Transaction = {
//        val s = tx.serialize()
//
//        given OriginalCborByteArray = OriginalCborByteArray(s)
//
//        Cbor.decode(s).to[Transaction].value
//    }
//}
//
////////////////////////////////////////////////
//// UTxO Set
//
//def toV3UTxO(utxo: UTxO): Map[v3.TxOutRef, v3.TxOut] = {
//    utxo.map((ti, to) =>
//        (
//            v3.TxOutRef(
//                id = v3.TxId(ByteString.fromArray(ti.transactionId.bytes)),
//                idx = BigInt(ti.index)
//            ),
//            LedgerToPlutusTranslation.getTxOutV2(Sized(to))
//        )
//    )
//}
//
///** Warning: Partial. Assumes a Hydrozoa-valid UTxO; babbage output, shelley address, inline datum
// */
//extension (utxo: BBUtxo) {
//    def toScalus: (TransactionInput, Babbage) = {
//        val txIn: TransactionInput =
//            TransactionInput(
//                Hash[Blake2b_256, HashPurpose.TransactionHash](ByteString.fromHex(utxo.getTxHash)),
//                utxo.getOutputIndex
//            )
//        val addr: ShelleyAddress =
//            Address.unsafeFromBech32(utxo.getAddress).asInstanceOf[ShelleyAddress]
//        val coins = utxo.getAmount.asScala.find(_.getUnit.equals("lovelace")).get.getQuantity
//        val tokens = valueTokens(utxo.toValue)
//        val inlineDatum = Data.fromCbor(HexUtil.decodeHexString(utxo.getInlineDatum))
//
//        (
//            txIn,
//            Babbage(
//                address = addr,
//                value = Value(coin = Coin(coins.longValue()), multiAsset = tokens),
//                datumOption = Some(Inline(inlineDatum)),
//                scriptRef = None
//            )
//        )
//
//    }
//}
//
//////////////////////////////////////////////////
//// TxOut Ref
//
//extension (txor: v3.TxOutRef) {
//    def toScalusLedger: TransactionInput =
//        TransactionInput(transactionId = Hash(txor.id.hash), index = txor.idx.toInt)
//}
//
////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper functions

// Uses the bloxbean backend to query a utxo into a scalus TransactionOutput
def bloxToScalusUtxoQuery(
    backendService: BackendService,
    input: UtxoId[L1]
): Either[String, TransactionOutput] = {
    backendService.getUtxoService
        .getTxOutput(input.transactionId.toHex, input.index)
        .toEither match {
        case Left(err) => Left("[bloxToScalusUtxoQuery]: Querying failed: " ++ err)
        case Right(utxo) =>
            Right({
                val outAddress = Address.unsafeFromBech32(utxo.getAddress)
                val outVal: Value = utxo.toValue.toLedgerValue
                val outDat = scala
                    .Option(utxo.getInlineDatum)
                    .map(hex =>
                        hex |> HexUtil.decodeHexString
                            |> PlutusData.deserialize
                            |> Interop.toScalusData
                            |> DatumOption.Inline.apply
                    )

                TransactionOutput(
                  address = outAddress,
                  value = outVal,
                  datumOption = outDat
                )
            })
    }
}
//
////
//
extension [A](result: Result[A])
    def toEither: Either[String, A] =
        if result.isSuccessful then Right(result.getValue)
        else Left(result.getResponse)

//    // TODO: we don't handle errors properly so far
//    def force: A =
//        if result.isSuccessful then result.getValue
//        else
//            println
//            throw RuntimeException(s"Unexpected: ${result.getResponse}")
//
//object ResultUtils:
//    def mkResult[A](value: A): Result[A] =
//        val result = Result.success("dummy").asInstanceOf[Result[A]]
//        result.withValue(value).asInstanceOf[Result[A]]
//
//    def mkResultError[A]: Result[A] =
//        Result.error().asInstanceOf[Result[A]]
//
//extension [A](option: Option[A])
//    def toResult(err: String): Result[A] = option match
//        case Some(a) => ResultUtils.mkResult[A](a)
//        case None    => Result.error(err).asInstanceOf[Result[A]]
//
//// Make an Utxo from an output reference + (bloxbean) TransactionOutput.
//// Used in BloxBean builders.
//// For now has some limitations:
//// * no datum hashes
//// * no scripts
//def txOutputToUtxo(txHash: String, txIx: Int, output: BBTO): Utxo =
//    val assets = mutable.Buffer[Amount]()
//    output.getValue.toMap.forEach((policy, inner) =>
//        inner.forEach((name, amount) => assets.append(Amount.asset(policy, name, amount)))
//    )
//    Utxo(
//        txHash,
//        txIx,
//        output.getAddress,
//        (List(Amount.lovelace(output.getValue.getCoin)) ++ assets.toList).asJava,
//        null, // TODO: no datum hashes
//        output.getInlineDatum.serializeToHex,
//        null // TODO: no scripts
//    )
//
//def encodeHex(bytes: IArray[Byte]): String =
//    HexUtil.encodeHexString(IArray.genericWrapArray(bytes).toArray)
//
//def decodeHex(hex: String): IArray[Byte] = IArray.from(HexUtil.decodeHexString(hex))
//
//extension (vkb: VerificationKeyBytes) def verKeyHash: AddrKeyHash = Hash(blake2b_224(vkb.bytes))
//
//// Piper!
//implicit class Piper[A](val x: A) extends AnyVal {
//    def |>[B](f: A => B): B = f(x)
//}
//
//// How do we do that canonically?
//object LongCompare:
//    sealed trait Result
//    case object LT extends Result
//    case object EQ extends Result
//    case object GT extends Result
//
//    extension (a: Long)
//        infix def ?(b: Long): Result =
//            val c = java.lang.Long.compare(a, b)
//            if c < 0 then LT
//            else if c == 0 then EQ
//            else GT
//
//// PS-style pair constructor
//implicit final class PSStyleAssoc[A](private val self: A) extends AnyVal {
//    @inline def /\[B](y: B): (A, B) = (self, y)
//}
//
//def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
//    case Nil => Some(Nil)
//    case h :: t =>
//        h match {
//            case None => None
//            case Some(head) =>
//                sequence(t) match {
//                    case None       => None
//                    case Some(list) => Some(head :: list)
//                }
//        }
//}
//
//extension [L <: AnyLayer](self: UtxoId[L])
//    def toTxOutRefV3: TxOutRef = {
//        val txId = self.transactionId
//        TxOutRef.apply(TxId(txId), self.index)
//    }
//
//// FIXME: This is a static value of 42. Scalus network doesn't expose the protocl magic.
//extension (self: Network) def toBB: BBNetwork = BBNetwork(self.value.toInt, 42)
//
//def getUtxoWithDatum[T](using
//                        FromData[T]
//                       )(utxoId: UtxoIdL1, backendService: BackendService): Either[String, (Utxo, T)] =
//    for
//        utxo <- backendService.getUtxoService
//            .getTxOutput(utxoId.transactionId.toHex, utxoId.index)
//            .toEither
//
//        datum <- Try(
//            fromData[T](
//                Interop.toScalusData(
//                    PlutusData
//                        .deserialize(HexUtil.decodeHexString(utxo.getInlineDatum))
//                )
//            )
//        ) match {
//            case Success(s) => Right(s)
//            case Failure(e) =>
//                Left(
//                    e.toString
//                )
//        }
//    yield (utxo, datum)

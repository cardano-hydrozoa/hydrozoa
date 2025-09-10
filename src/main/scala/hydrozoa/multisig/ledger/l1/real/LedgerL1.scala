package hydrozoa.multisig.ledger.l1.real

import cats.effect.{IO, Ref}
import co.nstant.in.cbor.model.{
    Map,
    UnsignedInteger,
    Array as CborArray,
    ByteString as CborByteString
}
import hydrozoa.{Address, L1, Utxo as HUtxo}
import hydrozoa.multisig.ledger.l1.real.LedgerL1.{State, Tx}
import hydrozoa.multisig.ledger.l1.real.token.CIP67Tags
import hydrozoa.multisig.ledger.l1.real.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionMetadatum.Bytes
import scalus.cardano.ledger.{
    AssetName,
    Blake2b_224,
    Hash,
    HashPurpose,
    MultiAsset,
    OriginalCborByteArray,
    PolicyId,
    Sized,
    Transaction,
    TransactionInput,
    TransactionMetadatumLabel,
    VKeyWitness,
    TransactionOutput as STransactionOutput,
    Value as SValue
}
import scalus.ledger.api.Timelock.AllOf

import java.math.BigInteger
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

final case class LedgerL1(headAddress: Address[L1])(
    private val state: Ref[IO, State]
) {

    /** A transaction belongs to the head if it matches on address and currency symbol */
    def txBelongsToHead(tx: LedgerL1.Tx): Boolean =
        tx.tx.auxiliaryData.getOrElse(false) match {
            case Metadata(m) =>
                m.get(TransactionMetadatumLabel(CIP67Tags.head)).fold(false)(_ == headAddress)
            case _ => false
        }

    trait StsError

    /** Check all L1 ledger rules except for the existence of the tx's inputs */
    def txValidateSts(tx: Tx): Either[StsError, Unit] =
        ???

    /** If the transaction passes [[txValidateSts]], apply it to transition the L1 ledger state. */
    def applyTx(tx: Tx): IO[Either[StsError, Unit]] =
        ???
}

object LedgerL1 {
    def create(headAddress: Address[L1]): IO[LedgerL1] =
        for {
            state <- Ref[IO].of(State())
        } yield LedgerL1(headAddress)(state)

    final case class State(
        treasury: Option[TreasuryUtxo] = None,
        deposits: List[DepositUtxo] = List(),
        rollouts: List[RolloutUtxo] = List()
    )

    trait Tx {
        val tx: Transaction
    }

    object Tx {
        type Serialized = Array[Byte]

    }
}

package hydrozoa.multisig.ledger.l1.real

/* Note to George:
Scalus already has a `KeepRaw[_]` type, that _should_ essentially memoize the raw CBOR bytes.
This is, in particular, used in the `body : KeepRaw[TransactionBody]` field of the transaction.
However, it appears that this is not actually working as intended; see the comment on encoder of the KeepRaw object in
scalus. HOWEVER, I don't think keeping a half-working scalus implementation along with our own parallel implementation
of the same functionality is a good idea.

We can access the raw bytes as follows:

```
val tx : Transaction
val txBodyCbor : Array[Byte] = tx.body.raw
val txBody : TransactionBody = tx.body.value
```

Because the KeepRaw type isn't functioning correctly on re-serialization (due to a limitation in the underlying Borer library),
we need to handle this a little bit manually for now.

The way we should do this is by understanding which parts of our data need consistent serialization and manually
extracting the `(_ : KeepRaw[_]).raw)` from it. For example, with a scalus transaction, we would need to do something
like:

// Persist the ORIGINAL transaction body CBOR alongside our re-serialization.
// Note that our original serialization contains another possibly-unequal serialization of the body; we could
// alternately split the `Transaction` into its fields and serialize individually.
def persistTx(tx : Transaction) : IO[PrimaryKey] = {
   val serializedTx : Array[Byte] = Cbor.encode(tx).toByteArray
   val serializedTxWithRawBody : (Array[Byte], Array[Byte]) = (serializedTx, tx.body.raw)
   persist(db, serializedTxWithRawBody)
}

// Get back the re-serialized bytes and the original bytes, and overwrite the raw bytes with the original ones.
def fetchTx(pk : PrimaryKey) : IO[Transaction] =
  for {
    (txBytes, originalBodyBytes) <- fetch(db, pk)
    tx = Cbor.decode(txBytes).to[Transaction].value
    } yield (tx.copy(body = tx.body.copy(raw = originalBodyBytes)))
 */

import cats.effect.{IO, Ref}
import co.nstant.in.cbor.model.{Map, UnsignedInteger, Array as CborArray, ByteString as CborByteString}
import hydrozoa.multisig.ledger.l1.real.LedgerL1.{State, Tx}
import hydrozoa.multisig.ledger.l1.real.state.{CIP67Tags, DepositDatum}
import hydrozoa.{Address, L1, Utxo as HUtxo}
import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionMetadatum.Bytes
import scalus.cardano.ledger.{AssetName, Blake2b_224, Hash, HashPurpose, MultiAsset, OriginalCborByteArray, PolicyId, Sized, Transaction, TransactionInput, TransactionMetadatumLabel, VKeyWitness, TransactionOutput as STransactionOutput, Value as SValue}
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

    sealed trait Utxo(utxo: HUtxo[L1])

    final case class TreasuryUtxo(override val utxo: HUtxo[L1]) extends Utxo(utxo)

    final case class DepositUtxo(override val utxo: HUtxo[L1]) extends Utxo(utxo) {
        // TODO parse datum
        val datum: DepositDatum = ???
    }

    final case class RolloutUtxo(override val utxo: HUtxo[L1]) extends Utxo(utxo)

    final case class State(
        treasury: Option[TreasuryUtxo] = None,
        deposits: List[DepositUtxo] = List(),
        rollouts: List[RolloutUtxo] = List()
    )

    sealed trait Tx {
        val tx: Transaction

        
    }

    object Tx {

        def extractHeadAddress(tx : Transaction): Option[Address[L1]] =
            tx.auxiliaryData match {
                case None => None
                case Some(Metadata(metadataMap)) => {
                    for {
                        metadataValue <- metadataMap.get(TransactionMetadatumLabel(CIP67Tags.head))
                        addressBytes <- metadataValue match {
                            case b: Bytes => Some(b)
                            case _ => None
                        }
                        addressParsed <- Address.fromByteString[L1](addressBytes.value)

                    } yield addressParsed
                }
            }
        
        type Serialized = Array[Byte]

        final case class Initialization(
            treasuryProduced: TreasuryUtxo,
            override val tx: Transaction
        ) extends Tx

        final case class Deposit(
            depositProduced: DepositUtxo,
            override val tx: Transaction
        ) extends Tx

        final case class Refund(
            depositSpent: DepositUtxo,
            override val tx: Transaction
        ) extends Tx

        final case class Settlement(
            treasurySpent: TreasuryUtxo,
            treasuryProduced: TreasuryUtxo,
            depositsSpent: List[DepositUtxo],
            rolloutProduced: Option[Rollout],
            override val tx: Transaction
        ) extends Tx

        final case class Rollout(
            rolloutSpent: RolloutUtxo,
            rolloutProduced: Option[RolloutUtxo],
            override val tx: Transaction
        ) extends Tx

        final case class Fallback(
            treasurySpent: TreasuryUtxo,
            override val tx: Transaction
        ) extends Tx

        final case class Finalization(
            treasurySpent: TreasuryUtxo,
            override val tx: Transaction
        ) extends Tx

    }
}

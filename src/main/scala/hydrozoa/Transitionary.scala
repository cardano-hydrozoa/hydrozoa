package hydrozoa

// TODO: Split up and remove

import monocle.Lens
import monocle.syntax.all.*
import scala.collection.immutable.SortedMap
import scala.language.implicitConversions
import scalus.cardano.ledger
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TxBalancingError.*

/** Adds multiple verification key witnesses to a transaction.
  *
  * This function preserves the original CBOR encoding of the transaction body (via the KeepRaw
  * wrapper), modifying only the witness set by adding all provided witnesses.
  *
  * @param tx
  *   The transaction to add the witnesses to
  * @param witnesses
  *   The VKeyWitnesses to add
  * @return
  *   A new Transaction with all witnesses added
  */
def attachVKeyWitnesses(tx: Transaction, witnesses: Iterable[VKeyWitness]): Transaction =
    tx.focus(_.witnessSet.vkeyWitnesses)
        .modify(w => TaggedSortedSet(w.toSet ++ witnesses))

//////////////////////////////////
// "Empty" values used for building up real values and for testing

val emptyContext: Context =
    Context(fee = Coin.zero, env = UtxoEnv.default, slotConfig = SlotConfig.Preprod)

/** Create a value with the specified policy id, asset name, and quantity (default 1) */
def singleton(policyId: PolicyId, assetName: AssetName, quantity: Int = 1): Value = {
    Value(
      coin = Coin.zero,
      assets = MultiAsset(assets = SortedMap((policyId, SortedMap((assetName, quantity)))))
    )
}

extension (self: TransactionOutput)
    def ensureMinAda(params: ProtocolParams): TransactionOutput =
        TransactionBuilder.ensureMinAda(self, params)

/** A wrapper for [[reportLovelaceDiffHandler]] and [[prebalancedLovelaceDiffHandler]]
  * @param coin
  */
case class WrappedCoin(coin: Coin) extends Throwable

/** A diff handler for [[LowLevelTxBalancer]] that simply reports the difference as a
  * Left(CantBalance(diff)). Note that this handler returns left _even if the diff is zero_.
  *
  * This is a useful hack for doing two things:
  *
  *   - Speculative balancing, where you're trying to determine what the minimum size of an _input_
  *     would need to be
  *   - Getting out the balance as seen by the diff handler
  */
def reportLovelaceDiffHandler: DiffHandler = (diff, _) => Left(Failed(WrappedCoin(diff.coin)))

/** A diff handler for [[LowLevelTxBalancer]] that only succeeds if the transaction is pre-balanced,
  * otherwise returning a Left(InsufficientFunds(diff, tx)).
  *
  * @return
  */
def prebalancedLovelaceDiffHandler: DiffHandler =
    (diff, tx) => if diff.coin.value == 0 then Right(tx) else Left(Failed(WrappedCoin(diff.coin)))

/** Lovelace per tx byte (a): 44 Lovelace per tx (b): 155381 Max tx bytes: 16 * 1024 = 16384
  * Therefore, max non-Plutus tx fee: 16 * 1024 * 44 + 155381 = 720896 + 155381 = 876277
  */
def maxNonPlutusTxFee(params: ProtocolParams): Coin = Coin(
  params.txFeeFixed +
      params.maxTxSize * params.txFeePerByte
)

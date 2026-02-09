package hydrozoa.multisig.backend.cardano

import scalus.builtin.Data
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, PolicyId, Transaction, TransactionHash, Utxos}

/** Notes:
  *   - Only [[ShelleyAddress]] are supported
  *   - The return data types are limited by what is really needed for Hydrozoa, but can be expanded
  *     if needed
  *
  * @tparam F
  */
trait CardanoBackend[F[_]]:
    import CardanoBackend.*

    /** All utxos at the [[address]]. The ordering of items from the point of view of the blockchain -
      * oldest first, newest last.
      *
      * @param address
      * @return
      */
    def utxosAt(address: ShelleyAddress): F[Either[Error, Utxos]]

    /** All the utxos that contain [[asset]] at the [[address]]. The ordering of items from the
      * point of view of the blockchain - oldest first, newest last.
      *
      * @param address
      * @param asset
      * @return
      */
    def utxosAt(address: ShelleyAddress, asset: (PolicyId, AssetName)): F[Either[Error, Utxos]]

    /** Checks whether a tx specified by [[txHash]] is known to the backend ledger.
      *
      * @param txHash
      * @return
      *   true - known, false - unknown or an error
      */
    def isTxKnown(txHash: TransactionHash): F[Either[Error, Boolean]]

    /** This is used for tracking the current treasury state when doing withdrawals in the
      * rule-based regime. Gets all transaction in the reverse order (newest first, oldest last)
      * starting from some known transaction with txHash=[[after]] EXCLUDING it, such that a tx
      * contains a continuing output with an asset (a treasury beacon token is going to be used).
      * Returns the tx hashes along with the spending redeemer for the corresponding input. This is
      * written in terms of common Scalus types, to keep it more general, but maybe we can switch to
      * concrete Hydrozoa types - like HeadMultisigScript/TokenNames/RuleBasedTreasuryDatum.
      *
      * @param asset
      *   the asset id that marks the continuing input
      * @param after
      *   the lower bound of the list, usually the fallback tx.
      * @return
      */
    def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): F[Either[CardanoBackend.Error, List[(TransactionHash, Data)]]]

    /** Submits a transaction.
      * @param tx
      * @return
      */
    def submitTx(tx: Transaction): F[Either[Error, Unit]]

object CardanoBackend:

    enum Error(msg: String) extends Throwable:
        case Timeout(msg: String) extends Error(msg)
        case InvalidTx(msg: String) extends Error(msg)
        case Unexpected(msg: String) extends Error(msg)
        case NoTxInputWithAsset(txId: TransactionHash, asset: String)
            extends Error(s"The tx $txId doesn't contain an input with asset: $asset")
        case NoTxOutputWithAsset(txId: TransactionHash, asset: String)
            extends Error(s"The tx $txId doesn't contain an output with asset: $asset")
        case SpendingRedeemerNotFound(txId: TransactionHash, ix: Int)
            extends Error(s"The redeemer for input with index $ix was not found for tx $txId")
        case ErrorDecodingRedeemerCbor(hex: String)
            extends Error(s"Error decoding redeemer Data from hex: $hex")

        override def toString: String = msg

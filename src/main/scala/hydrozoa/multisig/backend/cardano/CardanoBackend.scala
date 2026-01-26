package hydrozoa.multisig.backend.cardano

import hydrozoa.UtxoSetL1
import scalus.builtin.Data
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, PolicyId, Transaction, TransactionHash}

/** Notes:
  *   - Only [[ShelleyAddress]] are supported
  *   - The return data types are limited, but can be expanded
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
    def utxosAt(address: ShelleyAddress): F[Either[Error, UtxoSetL1]]

    /** All the utxos that contain [[asset]] at the [[address]]. The ordering of items from the
      * point of view of the blockchain - oldest first, newest last.
      *
      * @param address
      * @param asset
      * @return
      */
    def utxosAt(address: ShelleyAddress, asset: (PolicyId, AssetName)): F[Either[Error, UtxoSetL1]]

    /** Get limited (i.e. only that Hydrozoa is interested in) information on a transaction with
      * [[txHash]].
      * @param txHash
      * @return
      */
    def getTxInfo(txHash: TransactionHash): F[Either[Error, GetTxInfo.Response]]

    /** All transaction in the reverse order (newest first, oldest last) starting from some known
      * transaction with txHash=[[after]] EXCLUDING itself.
      *
      * @param asset
      * @param after
      * @return
      */
    def assetTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): F[Either[CardanoBackend.Error, List[TransactionHash]]]

    /** Submits a transaction.
      * @param tx
      * @return
      */
    def submitTx(tx: Transaction): F[Either[Error, Unit]]

object CardanoBackend:

    /** So far we only need to know whether a tx is known or it is not. */
    object GetTxInfo:
        final case class Response(
            isKnown: Boolean,
            spendingRedeemersData: List[Data] = List.empty
        )

    enum Error(msg: String) extends Throwable:
        case Timeout(msg: String) extends Error(msg)
        case InvalidTx(msg: String) extends Error(msg)
        case Unexpected(msg: String) extends Error(msg)

        override def toString: String = msg

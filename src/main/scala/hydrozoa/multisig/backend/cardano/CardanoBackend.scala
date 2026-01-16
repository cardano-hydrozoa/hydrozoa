package hydrozoa.multisig.backend.cardano

import hydrozoa.UtxoSetL1
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

    def utxosAt(address: ShelleyAddress): F[Either[Error, UtxoSetL1]]
    def utxosAt(address: ShelleyAddress, asset: (PolicyId, AssetName)): F[Either[Error, UtxoSetL1]]
    def getTxInfo(txHash: TransactionHash): F[Either[Error, GetTxInfo.Response]]
    def submitTx(tx: Transaction): F[Either[Error, Unit]]

object CardanoBackend:

    /** So far we only need to know whether a tx is known or it is not. */
    object GetTxInfo:
        final case class Response(
            isKnown: Boolean
        )

    enum Error(msg: String) extends Throwable:
        case Timeout(msg: String) extends Error(msg)
        case InvalidTx(msg: String) extends Error(msg)
        case Unknown(msg: String) extends Error(msg)

        override def toString: String = msg

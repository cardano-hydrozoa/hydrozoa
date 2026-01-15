package hydrozoa.multisig.backend.cardano

import hydrozoa.UtxoSetL1
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, PolicyId, Transaction, TransactionHash}

trait CardanoBackend[F[_]]:
    import CardanoBackend.*

    def utxosAt(address: Address): F[Either[Error, UtxoSetL1]]
    def utxosAt(address: Address, asset: (PolicyId, AssetName)): F[Either[Error, UtxoSetL1]]
    def getTxInfo(txHash: TransactionHash): F[Either[Error, GetTxInfo.Response]]
    def submitTx(tx: Transaction): F[Either[Error, Unit]]

object CardanoBackend:

    /** So far we only need to know whether a tx is known or it is not. */
    object GetTxInfo:
        final case class Response(
            isKnown: Boolean
        )

    enum Error extends Throwable:
        case Timeout(msg: String)
        case InvalidTx(msg: String)
        case Unknown(msg: String)

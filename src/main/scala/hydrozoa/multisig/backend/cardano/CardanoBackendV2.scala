package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.uplc.builtin.Data

import scala.concurrent.Future

trait ContinuingTxTracker[F[_]]:
    def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): F[Either[CardanoBackend.Error, List[(TransactionHash, Data)]]]

class CardanoBackendV2(
    val provider: BlockchainProvider,
    val continuingTxTracker: ContinuingTxTracker[IO]
) extends CardanoBackend[IO]:

    override def resolve(input: TransactionInput): IO[Either[CardanoBackend.Error, Option[Utxo]]] =
        IO.fromFuture(IO.delay(provider.findUtxo(input))).map {
            case Right(utxo) => Right(Some(utxo))
            case Left(_: UtxoQueryError.NotFound) => Right(None)
            case Left(err) =>
                Left(CardanoBackend.Error.Unexpected(s"UTxO query error: $err"))
        }

    override def utxosAt(address: ShelleyAddress): IO[Either[CardanoBackend.Error, Utxos]] =
        IO.fromFuture(IO.delay(provider.findUtxos(address))).map {
            _.left.map(err => CardanoBackend.Error.Unexpected(s"UTxO query error: $err"))
        }

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): IO[Either[CardanoBackend.Error, Utxos]] =
        val query = UtxoQuery(
          UtxoSource.FromAddress(address) && UtxoSource.FromAsset(asset._1, asset._2)
        )
        IO.fromFuture(IO.delay(provider.findUtxos(query))).map {
            _.left.map(err => CardanoBackend.Error.Unexpected(s"UTxO query error: $err"))
        }

    override def isTxKnown(txHash: TransactionHash): IO[Either[CardanoBackend.Error, Boolean]] =
        IO.fromFuture(IO.delay(provider.checkTransaction(txHash))).map { status =>
            Right(status == TransactionStatus.Confirmed)
        }.handleError(e =>
            Left(CardanoBackend.Error.Unexpected(e.getMessage))
        )

    override def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): IO[Either[CardanoBackend.Error, List[(TransactionHash, Data)]]] =
        continuingTxTracker.lastContinuingTxs(asset, after)

    override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] =
        IO.fromFuture(IO.delay(provider.submit(tx))).map {
            case Right(_)  => Right(())
            case Left(err) => Left(CardanoBackend.Error.InvalidTx(err.message))
        }.handleError(e =>
            Left(CardanoBackend.Error.Unexpected(e.getMessage))
        )

    override def fetchLatestParams: IO[Either[CardanoBackend.Error, ProtocolParams]] =
        IO.fromFuture(IO.delay(provider.fetchLatestParams))
            .map(Right(_))
            .handleError(e =>
                Left(CardanoBackend.Error.Unexpected(e.getMessage))
            )

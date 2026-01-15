package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import hydrozoa.UtxoSetL1
import hydrozoa.multisig.backend.cardano.CardanoBackend.GetTxInfo
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, PolicyId, Transaction, TransactionHash}

class BlockfrostCardanoBackend(apiKey: String) extends CardanoBackend[IO] {

    override def utxosAt(address: Address): IO[Either[CardanoBackend.Error, UtxoSetL1]] = ???

    override def utxosAt(
        address: Address,
        asset: (PolicyId, AssetName)
    ): IO[Either[CardanoBackend.Error, UtxoSetL1]] = ???

    override def getTxInfo(
        txHash: TransactionHash
    ): IO[Either[CardanoBackend.Error, GetTxInfo.Response]] = ???

    override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] = ???
}

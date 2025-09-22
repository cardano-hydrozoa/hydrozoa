package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.lib.cardano.scalus.ledger.txbuilder.setMinAda
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.Token
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import io.bullet.borer.Cbor
import org.scalacheck.{Gen, Prop, Test as ScalaCheckTest}
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.babbage.ProtocolParams
import scalus.prelude.Option as SOption
import test.*

def genDepositDatum(network: Network = Mainnet): Gen[DepositUtxo.Datum] = {
    for {
        addr <- genPubkeyAddr(network = network).map(
          LedgerToPlutusTranslation.getAddress(_).credential
        )
        datum <- genByteStringData
        deadline <- Gen.posNum[BigInt]
        refundAddr <- genPubkeyAddr(network = network).map(LedgerToPlutusTranslation.getAddress(_))
        refundDatum <- genByteStringData

    } yield DepositUtxo.Datum(
      address = addr,
      datum = SOption.Some(datum),
      deadline = deadline,
      refundAddress = refundAddr,
      refundDatum = SOption.Some(refundDatum)
    )
}

def genDepositUtxo(
    network: Network = Mainnet,
    params: ProtocolParams = blockfrost544Params
): Gen[DepositUtxo] =
    for {
        txId <- genTxId
        headAddr <- genScriptAddr(network)
        dd <- genDepositDatum(network)

        // Mock utxo to calculate minAda
        mockUtxo = Babbage(
          address = headAddr,
          value = Value.zero,
          datumOption = Some(Inline(dd.toData)),
          scriptRef = None
        )

        depositMinAda = setMinAda(mockUtxo, params).value.coin

        depositAmount <- Gen.posNum[Long].map((l: Long) => Coin(l - 1L) + depositMinAda)

    } yield DepositUtxo(
      l1Input = txId,
      l1OutputAddress = headAddr,
      l1OutputDatum = dd,
      l1OutputValue = depositAmount,
      l1RefScript = None
    )

val genHeadTokenName: Gen[AssetName] =
    for {
        txIds <- Gen.nonEmptyListOf(genTxId)
        ne = NonEmptyList.fromListUnsafe(txIds)
    } yield Token.mkHeadTokenName(ne)

val genTreasuryDatum: Gen[TreasuryUtxo.Datum] = {
    for {
        mv <- Gen.posNum[BigInt]
        // Verify that this is the correct length!
        kzg <- genByteStringOfN(32)
        paramsHash <- genByteStringOfN(32)

    } yield TreasuryUtxo.Datum(commit = kzg, versionMajor = mv, paramsHash = paramsHash)
}

/** Generate a treasury utxo with at least minAda */
def genTreasuryUtxo(
    network: Network = Mainnet,
    params: ProtocolParams = blockfrost544Params
): Gen[TreasuryUtxo] =
    for {
        txId <- genTxId
        headTn <- genHeadTokenName
        scriptHash <- genScriptHash
        scriptAddr = ShelleyAddress(network, ShelleyPaymentPart.Script(scriptHash), Null)
        datum <- genTreasuryDatum

        treasuryToken = singleton(scriptHash, headTn)

        treasuryMinAda = setMinAda(
          TreasuryUtxo(
            headTokenName = headTn,
            txId = txId,
            addr = scriptAddr,
            datum = datum,
            value = Value(Coin(0L)) + treasuryToken
          ).toUtxo._2,
          params
        ).value.coin

        treasuryAda <- Gen.posNum[Long].map((l: Long) => Coin(l - 1L) + treasuryMinAda)

    } yield TreasuryUtxo(
      headTokenName = headTn,
      txId = txId,
      datum = datum,
      addr = scriptAddr,
      value = Value(treasuryAda) + treasuryToken
    )

def genSettlementRecipe(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params,
    network: Network = Mainnet
): Gen[SettlementTx.Recipe] =
    for {

        majorVersion <- Gen.posNum[Int]
        deposits <- Gen.listOf(genDepositUtxo(network = network, params = params))
        withdrawalPeers <- Gen.listOf(genTestPeer)
        withdrawals = Map.from(withdrawalPeers.map(genAdaOnlyPubKeyUtxo(_, params).sample.get))
        utxo <- genTreasuryUtxo(network = network)
        peers <- genTestPeers
        hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))

    } yield SettlementTx.Recipe(
      majorVersion = majorVersion,
      deposits = deposits,
      utxosWithdrawn = withdrawals,
      treasuryUtxo = utxo,
      headNativeScript = hns,
      context =
          unsignedTxBuilderContext(utxo = Map.from(deposits.map(_.toUtxo).appended(utxo.toUtxo)))
    )

class SettlementTxTest extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10_000)
    }

    // override def scalaCheckInitialSeed = "SfYvj1tuRnXN2LkzQzKEbLA6LEPVYNSFj2985MfH0ZO="

    property("Build settlement tx")(
      Prop.forAll(genSettlementRecipe()) { recipe =>
          SettlementTx.build(recipe) match {
              case Left(e) => throw RuntimeException(s"Build failed $e")
              case Right(tx) => {
                  val cbor = tx.tx.toCbor
                  given OriginalCborByteArray = OriginalCborByteArray(cbor)
                  val roundTripped = Cbor.decode(cbor).to[Transaction].value
                  assertEquals(obtained = roundTripped, expected = tx.tx)
              }
          }

      }
    )
}

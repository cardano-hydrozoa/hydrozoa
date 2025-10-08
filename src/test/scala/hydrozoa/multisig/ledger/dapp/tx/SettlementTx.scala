package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.lib.tx.TransactionBuilder.setMinAda
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.*
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.prelude.Option as SOption
import cats.*
import cats.data.*
import hydrozoa.multisig.ledger.dapp.token.CIP67
import io.bullet.borer.Cbor
import org.scalacheck.{Gen, Prop, Test as ScalaCheckTest}
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
    params: ProtocolParams = blockfrost544Params,
    headAddr: Option[ShelleyAddress] = None
): Gen[DepositUtxo] =
    for {
        txId <- genTransactionInput
        headAddr_ = headAddr.getOrElse(genScriptAddr(network).sample.get)
        dd <- genDepositDatum(network)

        // Mock utxo to calculate minAda
        mockUtxo = Babbage(
          address = headAddr_,
          value = Value.zero,
          datumOption = Some(Inline(dd.toData)),
          scriptRef = None
        )

        depositMinAda = setMinAda(mockUtxo, params).value.coin

        depositAmount <- Gen.posNum[Long].map((l: Long) => Coin(l - 1L) + depositMinAda)

    } yield DepositUtxo(
      l1Input = txId,
      l1OutputAddress = headAddr_,
      l1OutputDatum = dd,
      l1OutputValue = depositAmount,
      l1RefScript = None
    )

val genHeadTokenName: Gen[AssetName] =
    for {
        ti <- genTransactionInput
    } yield CIP67.TokenNames(ti).headTokenName

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
    params: ProtocolParams = blockfrost544Params,
    headAddr: Option[ShelleyAddress]
): Gen[TreasuryUtxo] =
    for {
        txId <- genTransactionInput
        headTn <- genHeadTokenName

        scriptAddr = headAddr.getOrElse({
            ShelleyAddress(network, ShelleyPaymentPart.Script(genScriptHash.sample.get), Null)
        })
        datum <- genTreasuryDatum

        treasuryToken = singleton(
          scriptAddr.payment.asInstanceOf[ShelleyPaymentPart.Script].hash,
          headTn
        )

        treasuryMinAda = setMinAda(
          TreasuryUtxo(
            headTokenName = headTn,
            txId = txId,
            addr = scriptAddr,
            datum = datum,
            value = Value(Coin(0L)) + treasuryToken
          ).asUtxo._2,
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
): Gen[SettlementTx.Recipe] = {
    (for {
        peers <- genTestPeers
        hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
        majorVersion <- Gen.posNum[Int]
        deposits <- Gen.listOf(
          genDepositUtxo(network = network, params = params, headAddr = Some(hns.address(network)))
        )

        utxo <- genTreasuryUtxo(headAddr = Some(hns.address(network)), network = network)
        treasuryInputAda = utxo.value.coin

        withdrawals <- Gen
            .listOf(genTestPeer)
            .map(_.map(genAdaOnlyPubKeyUtxo(_, params).sample.get))

    } yield SettlementTx.Recipe(
      majorVersion = majorVersion,
      deposits = deposits,
      utxosWithdrawn = Map.from(withdrawals),
      treasuryUtxo = utxo,
      headNativeScript = hns,
      context =
          unsignedTxBuilderContext(utxo = Map.from(deposits.map(_.toUtxo).appended(utxo.asUtxo.toTuple))),
        rolloutTokenName = ???,
        headNativeScriptReferenceInput = ???
    )).suchThat(r => {
        val withdrawnCoin = sumUtxoValues(r.utxosWithdrawn.toList).coin
        val depositedCoin = sumUtxoValues(r.deposits.map(_.toUtxo)).coin
        val treasuryInputAda = r.treasuryUtxo.value.coin
        withdrawnCoin + estimatedFee < treasuryInputAda + depositedCoin
    })
}

class SettlementTxTest extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10_000)
    }

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

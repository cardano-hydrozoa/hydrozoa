package hydrozoa.multisig.ledger.dapp.tx

import cats.data.*
import hydrozoa.*
import hydrozoa.lib.tx.TransactionBuilder.setMinAda
import hydrozoa.lib.tx.TransactionUnspentOutput
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import io.bullet.borer.Cbor
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen, Prop, Test as ScalaCheckTest}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.prelude.Option as SOption
import test.*
import hydrozoa.multisig.ledger.dapp.tx.ArbitraryInstances.{*, given}

import scala.collection.immutable.Queue


// Import as (...).ArbitraryInstances.{*, given}
object ArbitraryInstances {
    given Arbitrary[Payout.Obligation.L2] = Arbitrary {
        for {
            l2Input <- arbitrary[TransactionInput]

            addr <- arbitrary[ShelleyAddress]
            coin <- arbitrary[Coin]
            datum <- arbitrary[ByteString]
            output = Babbage(address = addr,
                value = Value(coin),
                datumOption = Some(Inline(datum.toData)),
                scriptRef = None)
        } yield Payout.Obligation.L2(l2Input = l2Input, output = output)
    }

    given Arbitrary[Payout.Obligation.L1] = Arbitrary {
        for {
            l2 <- arbitrary[Payout.Obligation.L2]
        } yield (Payout.Obligation.L1(l2))
    }
}

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
        txId <- arbitrary[TransactionInput]
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
        ti <- arbitrary[TransactionInput]
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
        txId <- arbitrary[TransactionInput]
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

def genSettlementTxSeqBuilder(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params,
    network: Network = Mainnet
                                 ): Gen[SettlementTxSeq.Builder] = {
    (for {
        peers <- genTestPeers
        hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
        majorVersion <- Gen.posNum[Int]
        deposits <- Gen.listOf(
          genDepositUtxo(
            network = network,
            params = params,
            headAddr = Some(hns.mkAddress(network))
          )
        )

        utxo <- genTreasuryUtxo(headAddr = Some(hns.mkAddress(network)), network = network)
        treasuryInputAda = utxo.value.coin

        payouts <- Gen
            .listOf(arbitrary[Payout.Obligation.L1])

        env = testTxBuilderEnvironment

        multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(hns, env.network)

    } yield SettlementTxSeq.Builder(
        majorVersion = majorVersion,
        // TODO: generating a list and turning it into a queue is suboptimal
        deposits = Queue.from(deposits),
        payouts = Vector.from(payouts),
        treasuryUtxo = utxo,
        headNativeScript = hns,
        headNativeScriptReferenceInput = multisigWitnessUtxo,
        env = testTxBuilderEnvironment,
        validators = testValidators
    ))
        
}

def genFakeMultisigWitnessUtxo(
    script: HeadMultisigScript,
    network: Network
): Gen[TransactionUnspentOutput] = for {
    utxoId <- Arbitrary.arbitrary[TransactionInput]
    output = Babbage(
      script.mkAddress(network),
      Value.ada(2),
      None,
      Some(ScriptRef.apply(script.script))
    )
} yield TransactionUnspentOutput((utxoId, output))


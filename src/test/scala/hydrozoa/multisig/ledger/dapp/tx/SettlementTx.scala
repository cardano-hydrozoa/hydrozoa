package hydrozoa.multisig.ledger.dapp.tx

import cats.data.*
import hydrozoa.*
import hydrozoa.lib.tx.TransactionBuilder.setMinAda
import hydrozoa.lib.tx.TransactionUnspentOutput
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.protocol.types.Block as HBlock
import hydrozoa.multisig.ledger.joint.utxo.Payout
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scala.collection.immutable.Queue
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

/** NOTE: These will generate _fully_ arbitrary data. It is probably not what you want, but may be a
  * good starting point. For example, an arbitrary payout obligation may be for a different network
  * than the one you intend.
  *
  * Import as (...).ArbitraryInstances.{*, given}
  */
object ArbitraryInstances {

    /** NOTE: You can't change the network very easily because this is an opaque type. You should
      * only use this for fuzz testing.
      */
    given Arbitrary[Payout.Obligation.L2] = Arbitrary {
        for {
            l2Input <- arbitrary[TransactionInput]

            address <- arbitrary[ShelleyAddress]
            coin <- arbitrary[Coin]
            datum <- arbitrary[ByteString]
            output = Babbage(
              address = address,
              value = Value(coin),
              datumOption = Some(Inline(datum.toData)),
              scriptRef = None
            )
        } yield Payout.Obligation.L2(l2Input = l2Input, output = output)
    }

    given Arbitrary[Payout.Obligation.L1] = Arbitrary {
        for {
            l2 <- arbitrary[Payout.Obligation.L2]
        } yield Payout.Obligation.L1(l2)
    }
}

def genPayoutObligationL2(network: Network): Gen[Payout.Obligation.L2] =
    for {
        l2Input <- arbitrary[TransactionInput]

        address0 <- arbitrary[ShelleyAddress]
        address = address0.copy(network = network)
        coin <- arbitrary[Coin]
        datum <- arbitrary[ByteString]
        output = Babbage(
          address = address,
          value = Value(coin),
          datumOption = Some(Inline(datum.toData)),
          scriptRef = None
        )
    } yield Payout.Obligation.L2(l2Input = l2Input, output = output)

def genPayoutObligationL1(network: Network): Gen[Payout.Obligation.L1] =
    genPayoutObligationL2(network).map(Payout.Obligation.L1(_))

def genDepositDatum(network: Network = Mainnet): Gen[DepositUtxo.Datum] = {
    for {
        address <- genPubkeyAddress(network = network).map(
          LedgerToPlutusTranslation.getAddress(_).credential
        )
        datum <- genByteStringData
        deadline <- Gen.posNum[BigInt]
        refundAddress <- genPubkeyAddress(network = network).map(
          LedgerToPlutusTranslation.getAddress(_)
        )
        refundDatum <- genByteStringData

    } yield DepositUtxo.Datum(
      address = address,
      datum = SOption.Some(datum),
      deadline = deadline,
      refundAddress = refundAddress,
      refundDatum = SOption.Some(refundDatum)
    )
}

def genDepositUtxo(
    network: Network = Mainnet,
    params: ProtocolParams = blockfrost544Params,
    headAddress: Option[ShelleyAddress] = None
): Gen[DepositUtxo] =
    for {
        txId <- arbitrary[TransactionInput]
        headAddress_ = headAddress.getOrElse(genScriptAddress(network).sample.get)
        dd <- genDepositDatum(network)

        // Mock utxo to calculate minAda
        mockUtxo = Babbage(
          address = headAddress_,
          value = Value.zero,
          datumOption = Some(Inline(dd.toData)),
          scriptRef = None
        )

        depositMinAda = setMinAda(mockUtxo, params).value.coin

        depositAmount <- arbitrary[Coin].map(_ + depositMinAda)

    } yield DepositUtxo(
      l1Input = txId,
      l1OutputAddress = headAddress_,
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
    headAddress: Option[ShelleyAddress],
    coin: Option[Coin]
): Gen[TreasuryUtxo] =
    for {
        txId <- arbitrary[TransactionInput]
        headTn <- genHeadTokenName

        scriptAddress = headAddress.getOrElse({
            ShelleyAddress(network, ShelleyPaymentPart.Script(genScriptHash.sample.get), Null)
        })
        datum <- genTreasuryDatum

        treasuryToken = singleton(
          scriptAddress.payment.asInstanceOf[ShelleyPaymentPart.Script].hash,
          headTn
        )

        treasuryMinAda = setMinAda(
          TreasuryUtxo(
            headTokenName = headTn,
            txId = txId,
            address = scriptAddress,
            datum = datum,
            value = Value(Coin(0L)) + treasuryToken
          ).asUtxo._2,
          params
        ).value.coin

        treasuryAda <- arbitrary[Coin].map(l => l - Coin(1L) + treasuryMinAda)

    } yield TreasuryUtxo(
      headTokenName = headTn,
      txId = txId,
      datum = datum,
      address = scriptAddress,
      value = Value(coin.getOrElse(treasuryAda)) + treasuryToken
    )

def genSettlementTxSeqBuilder(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params,
    network: Network = Mainnet
): Gen[(SettlementTxSeq.Builder, SettlementTxSeq.Builder.Args)] = {
    for {
        peers <- genTestPeers
        hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
        majorVersion <- Gen.posNum[Int]
        deposits <- Gen.listOfN(
          10,
          genDepositUtxo(
            network = network,
            params = params,
            headAddress = Some(hns.mkAddress(network))
          )
        )

        env = testTxBuilderEnvironment
        payouts <- Gen.listOfN(1000, genPayoutObligationL1(network))
        payoutAda = payouts.map(_.output.value.coin).fold(Coin.zero)(_ + _)

        utxo <- genTreasuryUtxo(
          headAddress = Some(hns.mkAddress(network)),
          network = network,
          coin = Some(payoutAda + Coin(1_000_000_000L))
        )

        multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(hns, env.network)

    } yield (
      SettlementTxSeq.Builder(config =
          Tx.Builder.Config(
            headNativeScript = hns,
            headNativeScriptReferenceInput = multisigWitnessUtxo,
            env = testTxBuilderEnvironment,
            validators = testValidators
          )
      ),
      SettlementTxSeq.Builder.Args(
        majorVersionProduced = HBlock.Version.Major(majorVersion),
        // TODO: generating a list and turning it into a queue is suboptimal
        // @Peter from George: Is the above still relevant?
        depositsToSpend = Vector.from(deposits),
        payoutObligationsRemaining = Vector.from(payouts),
        treasuryToSpend = utxo
      )
    )
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

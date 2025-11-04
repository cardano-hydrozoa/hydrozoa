package hydrozoa.multisig.ledger.dapp.tx

import cats.data.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.multisig.protocol.types.Block as HBlock
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.prelude.Option as SOption
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other

def genDepositDatum(network: Network = testNetwork): Gen[DepositUtxo.Datum] = {
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
    network: Network = testNetwork,
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

        depositMinAda = ensureMinAda(mockUtxo, params).value.coin

        depositAmount <- arbitrary[Coin].map(_ + depositMinAda)

    } yield DepositUtxo(
      l1Input = txId,
      l1OutputAddress = headAddress_,
      l1OutputDatum = dd,
      l1OutputValue = depositAmount,
      l1RefScript = None
    )




def genSettlementTxSeqBuilder(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params,
    network: Network = testNetwork
): Gen[(SettlementTxSeq.Builder, SettlementTxSeq.Builder.Args, NonEmptyList[TestPeer])] = {
    // A helper to generator empty, small, medium, large (up to 1000)
    def genHelper[T](gen : Gen[T]) : Gen[Vector[T]] = Gen.sized(size =>
      Gen.frequency(
       (1, Gen.const(Vector.empty)),
       (2, Other.vectorOfN(size, gen)),
       (5, Other.vectorOfN(size * 5, gen)),
        (1, Other.vectorOfN(1, gen))
      ).map(_.take(1000))
    )

    for {
        (config, peers) <- genTxBuilderConfigAndPeers() 
        hns = config.headNativeScript
        majorVersion <- Gen.posNum[Int]

        genDeposit = genDepositUtxo(
          network = network,
          params = params,
          headAddress = Some(hns.mkAddress(network))
        )
        deposits <- genHelper(genDeposit)

        payouts <- genHelper(genPayoutObligationL1(network))
        payoutAda = payouts.map(_.output.value.coin).fold(Coin.zero)(_ + _)
        utxo <- genTreasuryUtxo(
          headAddress = Some(hns.mkAddress(network)),
          network = network,
          coin = Some(payoutAda + Coin(1_000_000_000L))
        )



    } yield (
      SettlementTxSeq.Builder(config),
      SettlementTxSeq.Builder.Args(
        majorVersionProduced = HBlock.Version.Major(majorVersion),
        depositsToSpend = deposits,
        payoutObligationsRemaining = payouts,
        treasuryToSpend = utxo
      ),
      peers
    )
}

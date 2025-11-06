package hydrozoa.multisig.ledger.dapp.tx

import cats.data.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.txseq.FinalizationTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.MultisigRegimeUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.multisig.protocol.types.Block as HBlock
import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other

val genMultisigRegimeTokenName: Gen[AssetName] =
    for {
        ti <- arbitrary[TransactionInput]
    } yield CIP67.TokenNames(ti).multisigRegimeTokenName

def genMultisigRegimeUtxo(
    network: Network = testNetwork,
    params: ProtocolParams = blockfrost544Params,
    headAddress: Option[ShelleyAddress],
    coin: Option[Coin],
    script: HeadMultisigScript
): Gen[MultisigRegimeUtxo] =
    for {
        txId <- arbitrary[TransactionInput]
        multisigRegimeTn <- genMultisigRegimeTokenName

        scriptAddress = headAddress.getOrElse {
            ShelleyAddress(network, ShelleyPaymentPart.Script(genScriptHash.sample.get), Null)
        }

        multisigRegimeToken = singleton(
          scriptAddress.payment.asInstanceOf[ShelleyPaymentPart.Script].hash,
          multisigRegimeTn
        )

        treasuryMinAda = ensureMinAda(
          MultisigRegimeUtxo(
            multisigRegimeTokenName = multisigRegimeTn,
            utxoId = txId,
            address = scriptAddress,
            value = Value(Coin(0L)) + multisigRegimeToken,
            script = script
          ).asUtxo._2,
          params
        ).value.coin

        // TODO: Why?
        treasuryAda <- arbitrary[Coin].map(l => l - Coin(1L) + treasuryMinAda)

    } yield MultisigRegimeUtxo(
      multisigRegimeTokenName = multisigRegimeTn,
      utxoId = txId,
      address = scriptAddress,
      value = Value(coin.getOrElse(treasuryAda)) + multisigRegimeToken,
      script = script
    )

def genFinalizationTxSeqBuilder(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params,
    network: Network = testNetwork
): Gen[(FinalizationTxSeq.Builder, FinalizationTxSeq.Builder.Args, NonEmptyList[TestPeer])] = {
    // A helper to generator empty, small, medium, large (up to 1000)
    def genHelper[T](gen: Gen[T]): Gen[Vector[T]] = Gen.sized(size =>
        Gen.frequency(
          (1, Gen.const(Vector.empty)),
          (2, Other.vectorOfN(size, gen)),
          (5, Other.vectorOfN(size * 5, gen)),
          (1, Other.vectorOfN(1, gen))
        ).map(_.take(1000))
    )

    for {
        peers <- genTestPeers
        hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
        majorVersion <- Gen.posNum[Int]

        env = testTxBuilderEnvironment

        payouts <- genHelper(genPayoutObligationL1(network))
        payoutAda = payouts.map(_.output.value.coin).fold(Coin.zero)(_ + _)

        headAddress = hns.mkAddress(network)
        treasuryUtxo <- genTreasuryUtxo(
          headAddress = Some(headAddress),
          network = network,
          coin = Some(payoutAda + Coin(1_000_000_000L))
        )

        shares <- genEquityShares(peers)

        multisigRegimeUtxo <- genMultisigRegimeUtxo(
          network,
          params,
          Some(headAddress),
          Some(Coin(shares.totalFallbackDeposit.underlying)),
          hns
        )

    } yield (
      FinalizationTxSeq.Builder(config =
          Tx.Builder.Config(
            headNativeScript = hns,
            headNativeScriptReferenceInput = multisigRegimeUtxo.asUtxo,
            env = testTxBuilderEnvironment,
            validators = testValidators
          )
      ),
      FinalizationTxSeq.Builder.Args(
        majorVersionProduced = HBlock.Version.Major(majorVersion),
        treasuryToSpend = treasuryUtxo,
        payoutObligationsRemaining = payouts,
        multisigRegimeUtxoToSpend = multisigRegimeUtxo,
        equityShares = shares
      ),
      peers
    )
}

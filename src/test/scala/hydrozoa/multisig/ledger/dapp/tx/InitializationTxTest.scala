package hydrozoa.multisig.ledger.dapp.tx

//import cats.data.NonEmptyList
//import hydrozoa.maxNonPlutusTxFee
//import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
//import hydrozoa.multisig.ledger.dapp.token.CIP67
//import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
//import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
//import hydrozoa.multisig.ledger.dapp.utxo.MultisigTreasuryUtxo
//import org.scalacheck.{Arbitrary, Gen}
//import scala.collection.immutable.SortedMap
//import scalus.builtin.ByteString
//import scalus.builtin.Data.toData
//import scalus.cardano.address.*
//import scalus.cardano.address.ShelleyPaymentPart.Key
//import scalus.cardano.ledger.*
//import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Coin
//import scalus.cardano.ledger.DatumOption.Inline
//import scalus.cardano.ledger.TransactionOutput.Babbage
//import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
//import test.*
//import test.Generators.Hydrozoa.*
//import test.TestPeer.Alice
//
//// The minimum ada required for the initial treasury utxo
//val minInitTreasuryAda: Coin = {
//    val mockTreasury = Babbage(
//      // This is a pubkey address, but the script address should be the same size
//      address = genPubkeyAddress().sample.get,
//      value = Value(
//        Coin(0L),
//        assets = MultiAsset(
//          SortedMap(
//            (
//              genPolicyId.sample.get,
//              SortedMap(
//                (
//                  CIP67.HeadTokenNames(genAdaOnlyPubKeyUtxo(Alice).sample.get._1).treasuryTokenName,
//                  1L
//                )
//              )
//            )
//          )
//        )
//      ),
//      datumOption = Some(Inline(MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum.toData)),
//      scriptRef = None
//    )
//    ensureMinAda(mockTreasury, blockfrost544Params).value.coin
//}
//
//// NOTE: This generator isn't currently used. It makes more sense to test this transaction
//// as part of its transaction sequence. This generator is provided in case there are bugs
//// discovered and we want to isolate testing specifically to this transaction.
//// See InitializationTxSeqTest.scala
//// TODO: This uses unsafeRunSync to set the initialization time. This should be moved to PropertyM
//// so that it can run IO effects
//val genInitTxRecipe: Gen[InitializationTx.Recipe] =
//    for {
//        peers <- genTestPeers()
//
//        hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
//
//        // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
//        // a max non-plutus fee
//        seedUtxo <- genAdaOnlyPubKeyUtxo(
//          peers.head,
//          minimumCoin = minInitTreasuryAda
//              + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)
//        ).map(x => Utxo(x._1, x._2))
//        otherSpentUtxos <- Gen
//            .listOf(genAdaOnlyPubKeyUtxo(peers.head))
//            .map(_.map(x => Utxo(x._1, x._2)))
//
//        spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)
//
//        tokenNames = HeadTokenNames(seedUtxo.input)
//
//        // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
//        // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
//        // output
//        initialTreasuryCoin <- Gen
//            .choose(
//              minInitTreasuryAda.value,
//              sumUtxoValues(spentUtxos.toList).coin.value
//                  - maxNonPlutusTxFee(testTxBuilderCardanoInfo.protocolParams).value
//                  - minPubkeyAda().value
//            )
//            .map(Coin(_))
//
//        initialTreasury = Value(initialTreasuryCoin)
//
//        hmrwCoin <- Arbitrary.arbitrary[Coin]
//
//    } yield InitializationTx.Recipe(
//      spentUtxos = SpentUtxos(seedUtxo, otherSpentUtxos),
//      initialTreasury = initialTreasury,
//      hmrwCoin = hmrwCoin,
//      changePP = Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte)))
//    )

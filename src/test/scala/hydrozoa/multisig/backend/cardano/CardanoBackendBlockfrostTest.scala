package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import io.github.cdimascio.dotenv.Dotenv
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{AssetName, CardanoInfo, Hash, Transaction, TransactionHash}
import scalus.uplc.builtin.ByteString

object RequiresBlockfrostApiKey extends Tag("requires-blockfrost-api-key")

/** To run those integration tests an API key is needed to access Blockfrost PREVIEW API. To feed it
  * to test you have to pur it into .env file in the root of the project (or from where you run this
  * test suite:
  *
  * ```hex
  * BLOCKFROST_API_KEY=preview...
  * ```
  */
class CardanoBackendBlockfrostTest extends AnyFunSuite {

    def runWithKey[A](io: String => IO[A]): A =
        TestConfig.blockfrostApiKey match {
            case Some(key) => io(key).unsafeRunSync()
            case None      => cancel("BLOCKFROST_API_KEY not set - skipping integration test")
        }

    // TODO: these are random addresses, which may change over time, we need to have our own one
    private val testAddress: ShelleyAddress = Address
        .fromBech32("addr_test1wqt2v8zcpjldyu2zcwz3yuu8p4wpk0hzaqwthh23qgs5xgg7266qn")
        .asInstanceOf[ShelleyAddress]

    private val testAddress2: ShelleyAddress = Address
        .fromBech32(
          "addr_test1qruhen60uwzpwnnr7gjs50z2v8u9zyfw6zunet4k42zrpr54mrlv55f93rs6j48wt29w90hlxt4rvpvshe55k5r9mpvqjv2wt4"
        )
        .asInstanceOf[ShelleyAddress]

    test("Error gracefully when key and network mismatch", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Mainnet), key)
                ret <- backend.utxosAt(testAddress)
            } yield ret
        )
        println(ret)
        assert(ret.isLeft)
    }

    test("Fetch some utxos", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                utxoSet <- backend.utxosAt(testAddress)
            } yield utxoSet
        )
        println(ret)
        assert(ret.isRight && ret.exists(set => set.size == 6))
    }

    test("Fetch some utxos, multi-page", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key, 1)
                utxoSet <- backend.utxosAt(testAddress)
            } yield utxoSet
        )
        println(ret)
        assert(ret.isRight && ret.exists(set => set.size == 6))
    }

    test("Fetch utxos with specific asset 1", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                policyId = Hash.scriptHash(
                  ByteString.fromHex("a217f9484e3b7854ff68242bd37600da6b734c1b467a6d4e902aac07")
                )
                assetName = AssetName.empty
                utxoSet <- backend.utxosAt(testAddress, (policyId, assetName))
            } yield utxoSet
        )
        println(ret)
        assert(ret.isRight && ret.exists(set => set.size == 1))
    }

    test("Fetch utxos with specific asset 2", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                policyId = Hash.scriptHash(
                  ByteString.fromHex("919d4c2c9455016289341b1a14dedf697687af31751170d56a31466e")
                )
                assetName = AssetName.fromHex("745348454e")
                utxoSet <- backend.utxosAt(testAddress2, (policyId, assetName))
            } yield utxoSet
        )
        println(ret)
        assert(ret.isRight && ret.exists(set => set.size == 1))
    }

    test("Known tx is reported correctly", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                txInfo <- backend.isTxKnown(
                  TransactionHash.fromHex(
                    "9844228688a4d0e54ec416bf7aa31fc10888d5845bfb16cbd68fb625ff86bb5f"
                  )
                )
            } yield txInfo
        )
        assert(ret.isRight && ret.exists(x => x))
    }

    test("Fake tx is reported correctly", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                txInfo <- backend.isTxKnown(
                  TransactionHash.fromHex(
                    "8844228688a4d0e54ec416bf7aa31fc10888d5845bfb16cbd68fb625ff86bb5f"
                  )
                )
            } yield txInfo
        )
        println(ret)
        assert(ret.isRight && ret.exists(x => !x))
    }

    test("Wrong URI is indicated as error", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(
                  Right((CardanoNetwork.Custom(CardanoInfo.mainnet), "https://not-blockforst.net")),
                  key
                )
                txInfo <- backend.isTxKnown(
                  TransactionHash.fromHex(
                    "8844228688a4d0e54ec416bf7aa31fc10888d5845bfb16cbd68fb625ff86bb5f"
                  )
                )
            } yield txInfo
        )
        println(ret)
        assert(ret.isLeft)
    }

    test("Submit endpoint works and gives sensible error for empty tx", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                ret <- backend.submitTx(Transaction.empty)
            } yield ret
        )
        print(ret)
        assert(ret.isLeft)
    }

    // TODO: post our own golden tx - it's almost impossible to find such
    //  a tx on the public testnet
    // TODO: update the test
    ignore("Fetch txs with specific asset 1", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                policyId = Hash.scriptHash(
                  ByteString.fromHex("ee492ffb3dd3fb15231920d1db1f66671add1dc48b165f5006a565bd")
                )
                // assetName = AssetName.empty
                assetName = AssetName.fromHex("53656e696f72426f6e64546f6b656e")
                txOnList = TransactionHash.fromHex(
                  "5c22219ef5bebe66b07942ee0dd3c32c0affac529e71b087ee9167dbb637eadc"
                )
                txIds <- backend.lastContinuingTxs((policyId, assetName), txOnList)
            } yield txIds
        )
        println(ret)
        assert(ret.isRight && ret.exists(set => set.size == 16))
    }

    // TODO: post our own golden tx - it's almost impossible to find such
    //  a tx on the public testnet
    // TODO: update the test
    ignore("Fetch txs - empty results", RequiresBlockfrostApiKey) {
        val ret = runWithKey(key =>
            for {
                backend <- CardanoBackendBlockfrost(Left(CardanoNetwork.Preview), key)
                policyId = Hash.scriptHash(
                  ByteString.fromHex("ee492ffb3dd3fb15231920d1db1f66671add1dc48b165f5006a565bd")
                )
                randomAssetName = AssetName.fromHex("deadbeef")
                someTx = TransactionHash.fromHex(
                  "5c22219ef5bebe66b07942ee0dd3c32c0affac529e71b087ee9167dbb637eadc"
                )
                txIds <- backend.lastContinuingTxs((policyId, randomAssetName), someTx)
            } yield txIds
        )
        println(ret)
        assert(ret.isRight && ret.exists(set => set.isEmpty))
    }
}

object TestConfig {
    private val dotenv: Option[Dotenv] =
        Try(Dotenv.configure().ignoreIfMissing().load()).toOption

    def getApiKey(name: String): Option[String] =
        dotenv
            .flatMap(d => Option(d.get(name)))
            .orElse(sys.env.get(name))

    lazy val blockfrostApiKey: Option[String] =
        getApiKey("BLOCKFROST_API_KEY")
}

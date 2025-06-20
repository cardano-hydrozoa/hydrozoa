package hydrozoa.l1

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.api.util.AssetUtil
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{serializeTxHex, txHash, txInputs, txOutputs}
import hydrozoa.node.monitoring.Metrics
import ox.channels.ActorRef
import ox.resilience.RetryConfig
import scalus.ledger.api.v1.PosixTime

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class CardanoL1Mock() extends CardanoL1:

    private val log = Logger(getClass)

    override def setMetrics(metrics: ActorRef[Metrics]): Unit = ()

    private val knownTxs: mutable.Map[TxId, TxL1] = mutable.Map()

    def getKnownTxs: Map[TxId, TxL1] = Map.from(knownTxs)

    private val utxosActive: mutable.Map[UtxoIdL1, Output[L1]] = mutable.Map()

    def getUtxosActive: Map[UtxoIdL1, Output[L1]] = Map.from(utxosActive)

    override def submit(tx: TxL1): Either[SubmissionError, TxId] =
        synchronized {
            val txId = txHash(tx)
            log.info(s"Submitting tx hash $txId, tx: ${serializeTxHex(tx)}")
            if knownTxs.contains(txId) then Right(txId)
            else
                knownTxs.put(txId, tx)
                val setSizeBefore = utxosActive.size
                val inputs = txInputs(tx)
                if (!inputs.toSet.subsetOf(utxosActive.keySet))
                    throw RuntimeException(s"Some inputs are missing: ${inputs.toSet.&~(utxosActive.keySet)}")
                utxosActive.subtractAll(inputs)
                val outputs = txOutputs(tx)
                utxosActive.addAll(outputs)
                val setSizeAfter = utxosActive.size
                if (!outputs.map(_._1).toSet.subsetOf(utxosActive.keySet))
                    throw RuntimeException("")
                // Check number of inputs and outputs
                assert(setSizeBefore - inputs.size + outputs.size == setSizeAfter)
                Right(txId)
        }

    override def awaitTx(
        txId: TxId,
        retryConfig: RetryConfig[Throwable, Option[TxL1]]
    ): Option[TxL1] = knownTxs.get(txId)

    override def network: Network = Network(0, 42)

    override def lastBlockTime: PosixTime = 0

    def utxoIdsByAddress(address: AddressBechL1): Set[UtxoIdL1] =
        utxosActive.filter((_, utxo) => utxo.address == address).keySet.toSet

    def utxoById(utxoId: UtxoIdL1): Option[OutputL1] = utxosActive.get(utxoId)

    override def utxosAtAddress(address: AddressBechL1): List[Utxo] =
        utxosActive
            .filter((_, utxo) => utxo.address == address)
            .map((utxoId, output) =>

                val amounts: mutable.Set[Amount] = mutable.Set.empty

                output.tokens.foreach((policyId, tokens) =>
                    tokens.foreach((tokenName, quantity) =>
                        val unit = AssetUtil.getUnit(policyId.policyId, tokenName.tokenNameHex)
                        amounts.add(Amount.asset(unit, quantity.longValue))
                    )
                )

                Utxo(
                  utxoId.txId.hash,
                  utxoId.outputIx.ix,
                  address.bech32,
                  (List(lovelace(output.coins.bigInteger)) ++ amounts.toList).asJava,
                  null, // no datum hashes
                  output.mbInlineDatum.getOrElse(""),
                  null // no scripts
                )
            )
            .toList

    override def utxoIdsAdaAtAddress(address: AddressBechL1): Map[UtxoIdL1, BigInt] =
        utxosActive
            .filter((_, utxo) => utxo.address == address)
            .view
            .mapValues(_.coins)
            .toMap

object CardanoL1Mock:
    def apply(): CardanoL1Mock =
        val l1Mock = new CardanoL1Mock
        l1Mock.utxosActive.addAll(genesisUtxos)
        l1Mock

    def apply(knownTxs: Map[TxId, TxL1], utxosActive: Map[UtxoIdL1, Output[L1]]): CardanoL1Mock =
        val l1Mock = new CardanoL1Mock
        l1Mock.knownTxs.addAll(knownTxs)
        l1Mock.utxosActive.clear()
        l1Mock.utxosActive.addAll(utxosActive)
        l1Mock

val genesisUtxos: Set[(UtxoIdL1, Output[L1])] =
    List(
      (
        "6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508",
        "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
      ),
      (
        "dc0d0d0a13e683e443c575147ec12136e5ac6a4f994cd4189d4d25bed541c44d",
        "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te"
      ),
      (
        "aae852d5d2b08c0a937a319fec0d9933bc3bc67b9d0a6bfd4001997b169364b3",
        "addr_test1qr9xuxclxgx4gw3y4h4tcz4yvfmrt3e5nd3elphhf00a67xnrv5vjcv6tzehj2nnjj4cth4ndzyuf4asvvkgzeac2hfqk0za93"
      ),
      (
        "145da89c02380f6f72d6acc8194cd9295eb2001c2d88f0b20fef647ec5a18f7f",
        "addr_test1qqra0q073cecs03hr724psh3ppejrlpjuphgpdj7xjwvkqnhqttgsr5xuaaq2g805dldu3gq9gw7gwmgdyhpwkm59ensgyph06"
      ),
      (
        "8cc2f88405991a5dfb8cb6962fe44f56510d93405cfe9ea23baf1f6bf29f3011",
        "addr_test1qp38kfvcm4c39yt8sfgkp3tyqe736fz708xzxuy5s9w9ev43yh3sash5eeq9ngrfuzxrekpvmly52xlmyfy8lz39emhs2spswl"
      ),
      (
        "27bc6c090c20dc1fe66f07ac200d5e97493a11c9554f3fc92f0ea7036fb11df9",
        "addr_test1qrrv7774puml0exvzc0uqrc8axezy6a925kv4ucdx906qy6mhjxtmx44x70ndr7g6dgqcdaf69q8fnrdmtvfud5x7rsqvsuqx5"
      ),
      (
        "5b5b9626afd8846240e3c05de23634b1b6e76620be69b5a249670071b3c3fb60",
        "addr_test1qpgkf2ccvu2uscmcqgy4dkyjeae0va3kk7yk04nuleekq3u3xrwgnnm6n0yfzz0e8x2kkehex2f6mexrjg9h9l2qhm4qkms53s"
      ),
      (
        "979a06c4cc5a1902d68f469d6d6e9a780a2667f5ce8557c199fcfa882e05c92e",
        "addr_test1qq7a8p6zaxzgcmcjcy7ak8u5vn7qec9mjggzw6qg096nzlj6n7rflnv3x43vnv8q7q0h0ef4n6ncp5mljd2ljupwl79s5mqneq"
      ),
      (
        "a2857a9eb8e140c6183137bf8aadbb47eeea23b96b9f4a13e4155c1ef83716a6",
        "addr_test1qzyw0ensk3w2kgezk5vw77m0vmfs4mqdjg7ugclyvuy357g0vaukh8a8zgj09prvaw70f9gvz8sypmsjf5c0dctyhn2slcvsjn"
      ),
      (
        "6793565c7c64e2a979137e7876b2c560c194a0a6372b15f1473b72e64f4e1ab8",
        "addr_test1qrrshpppv9uq95lj89tv4vv40cwnqmx5szzcndqhvr5hjfltl4s98wsjkwpg3v4k6h2vgcvdd2xwt82stq8fcmmsft6s8dzp8f"
      ),
      (
        "08419f77fa20aec0a8b66047d0040be1c5446b2ddbe88e656dc62390260a997c",
        "addr_test1qrqfxwuz8rm0xvewfrp5eudgup24jsan8n22h3f6a7mavyjt0njqm4ykhhqzkdrq9ua8w0lhen8wsuuerexgsehn5u9syjlrxr"
      ),
      (
        "592b36fd2dff5c8ff8c7125b9dd7ac027de661d55b2fc9ea4ef5b22b53cffda7",
        "addr_test1qp5l04egnh30q8x3uqn943d7jsa5za66htsvu6e74s8dacxwnjkm0n0v900d8mu20wlrx55xn07p8pm4fj0wdvtc9kwq7pztl7"
      ),
      (
        "ce5179fbf81eb92907e8e5ca7d2180b561871c87c4136a2deaf9dd75f34b51be",
        "addr_test1qpcf5ursqpwx2tp8maeah00rxxdfpvf8h65k4hk3chac0fvu28duly863yqhgjtl8an2pkksd6mlzv0qv4nejh5u2zjsshr90k"
      ),
      (
        "d9d325322d157b007eb936bbd54968844e6e0f845994ad1843874d82a1521832",
        "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
      ),
      (
        "c408f9103ba7e0f207e211b53f5cca42f79f3e9c9d24024731d648be909497a8",
        "addr_test1qqe5df3su6tjhuuve6rjr8d36ccxre7dxfx2mzxp3egy72vsetrga9el2yjke2fcdql6f6sjzu7h6prajs8mhzpm6r5qpkfq9m"
      ),
      (
        "6712b9e13e7e433397ea2888899b8a5ba57b12c427dc72f1c1c684ac05f32b1a",
        "addr_test1qq5tscksq8n2vjszkdtqe0zn9645246ex3mu88x9y0stnlzjwyqgnrq3uuc3jst3hyy244rrwuxke0m7ezr3cn93u5vq0rfv8t"
      ),
      (
        "1c2498914a63269e7d4ab7f6b7d493d0d41a87b0cacaab7412c591219dcc21c4",
        "addr_test1qp0qu4cypvrwn4c7pu50zf3x9qu2drdsk545l5dnsa7a5gsr6htafuvutm36rm23hdnsw7w7r82q4tljuh55drxqt30q6vm8vs"
      ),
      (
        "8b96680725c4fcf461214054d0b364a86e43d7d6be0475d610e980971b101ad0",
        "addr_test1qqm87edtdxc7vu2u34dpf9jzzny4qhk3wqezv6ejpx3vgrwt46dz4zq7vqll88fkaxrm4nac0m5cq50jytzlu0hax5xqwlraql"
      ),
      (
        "c7565416e7553cdf8fdac8bf054b4b3de19d06b72efd00c47823335d7156ed1f",
        "addr_test1qrzufj3g0ua489yt235wtc3mrjrlucww2tqdnt7kt5rs09grsag6vxw5v053atks5a6whke03cf2qx3h3g2nhsmzwv3sgml3ed"
      ),
      (
        "a6ce90a9a5ef8ef73858effdae375ba50f302d3c6c8b587a15eaa8fa98ddf741",
        "addr_test1qrh3nrahcd0pj6ps3g9htnlw2jjxuylgdhfn2s5rxqyrr43yzewr2766qsfeq6stl65t546cwvclpqm2rpkkxtksgxuq90xn5f"
      )
    ).map((txHash, address) =>
        (
          UtxoIdL1(TxId(txHash), TxIx(0)),
          Output[L1](
            AddressBech[L1](address),
            BigInt("10000000000"),
            emptyTokens
          )
        )
    ).toSet

package hydrozoa.l1

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.api.util.AssetUtil
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{serializeTxHex, txHash, txInputs, txOutputs}
import hydrozoa.node.TestPeer.*
import hydrozoa.node.addressFromPeer
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
                    throw RuntimeException(
                      s"Some inputs are missing: ${inputs.toSet.&~(utxosActive.keySet)}"
                    )
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

/** Two utxos per test peer */
val genesisUtxos: Set[(UtxoIdL1, Output[L1])] =
    List(
      (
        "6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508",
        addressFromPeer(Alice)
      ),
      (
        "dc0d0d0a13e683e443c575147ec12136e5ac6a4f994cd4189d4d25bed541c44d",
        addressFromPeer(Bob)
      ),
      ("aae852d5d2b08c0a937a319fec0d9933bc3bc67b9d0a6bfd4001997b169364b3", addressFromPeer(Carol)),
      (
        "145da89c02380f6f72d6acc8194cd9295eb2001c2d88f0b20fef647ec5a18f7f",
        addressFromPeer(Daniella)
      ),
      ("8cc2f88405991a5dfb8cb6962fe44f56510d93405cfe9ea23baf1f6bf29f3011", addressFromPeer(Erin)),
      (
        "27bc6c090c20dc1fe66f07ac200d5e97493a11c9554f3fc92f0ea7036fb11df9",
        addressFromPeer(Frank)
      ),
      (
        "5b5b9626afd8846240e3c05de23634b1b6e76620be69b5a249670071b3c3fb60",
        addressFromPeer(Gustavo)
      ),
      ("979a06c4cc5a1902d68f469d6d6e9a780a2667f5ce8557c199fcfa882e05c92e", addressFromPeer(Hector)),
      (
        "a2857a9eb8e140c6183137bf8aadbb47eeea23b96b9f4a13e4155c1ef83716a6",
        addressFromPeer(Isabel)
      ),
      ("6793565c7c64e2a979137e7876b2c560c194a0a6372b15f1473b72e64f4e1ab8", addressFromPeer(Julia)),
      ("08419f77fa20aec0a8b66047d0040be1c5446b2ddbe88e656dc62390260a997c", addressFromPeer(Alice)),
      (
        "592b36fd2dff5c8ff8c7125b9dd7ac027de661d55b2fc9ea4ef5b22b53cffda7",
        addressFromPeer(Bob)
      ),
      (
        "ce5179fbf81eb92907e8e5ca7d2180b561871c87c4136a2deaf9dd75f34b51be",
        addressFromPeer(Carol)
      ),
      (
        "d9d325322d157b007eb936bbd54968844e6e0f845994ad1843874d82a1521832",
        addressFromPeer(Daniella)
      ),
      ("c408f9103ba7e0f207e211b53f5cca42f79f3e9c9d24024731d648be909497a8", addressFromPeer(Erin)),
      (
        "6712b9e13e7e433397ea2888899b8a5ba57b12c427dc72f1c1c684ac05f32b1a",
        addressFromPeer(Frank)
      ),
      (
        "1c2498914a63269e7d4ab7f6b7d493d0d41a87b0cacaab7412c591219dcc21c4",
        addressFromPeer(Gustavo)
      ),
      (
        "8b96680725c4fcf461214054d0b364a86e43d7d6be0475d610e980971b101ad0",
        addressFromPeer(Hector)
      ),
      ("c7565416e7553cdf8fdac8bf054b4b3de19d06b72efd00c47823335d7156ed1f", addressFromPeer(Isabel)),
      ("a6ce90a9a5ef8ef73858effdae375ba50f302d3c6c8b587a15eaa8fa98ddf741", addressFromPeer(Julia))
    ).map((txHash, address) =>
        (
          UtxoIdL1(TxId(txHash), TxIx(0)),
          Output[L1](
            AddressBech[L1](address.toBech32.get),
            BigInt("10000000000"),
            emptyTokens
          )
        )
    ).toSet

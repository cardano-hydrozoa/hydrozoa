package hydrozoa.l1

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{serializeTxHex, txInputs, txOutputs}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.monitoring.Metrics
import ox.channels.ActorRef
import ox.resilience.RetryConfig
import scalus.builtin.ByteString
import scalus.cardano.address.Network
import scalus.cardano.address.Network.Testnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v1.PosixTime

import scala.collection.mutable

class CardanoL1Mock() extends CardanoL1:

    private val log = Logger(getClass)
    private val knownTxs: mutable.Map[TransactionHash, TxL1] = mutable.Map()
    private val utxosActive: mutable.Map[UtxoIdL1, Output[L1]] = mutable.Map()

    override def setMetrics(metrics: ActorRef[Metrics]): Unit = ()

    def getKnownTxs: Map[TransactionHash, TxL1] = Map.from(knownTxs)

    def getUtxosActive: UtxoSet[L1] = UtxoSet[L1](Map.from(utxosActive))

    override def submit(tx: TxL1): Either[SubmissionError, TransactionHash] =
        synchronized {
            val txId = tx.id
            log.info(s"Submitting tx hash $txId, tx: ${serializeTxHex(tx)}")
            if knownTxs.contains(txId) then Right(txId)
            else
                val _ = knownTxs.put(txId, tx)
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
        txId: TransactionHash,
        retryConfig: RetryConfig[Throwable, Option[TxL1]]
    ): Option[TxL1] = knownTxs.get(txId)

    override def network: Network = Testnet

    override def lastBlockTime: PosixTime = 0

    def utxoIdsByAddress(address: AddressL1): Set[UtxoIdL1] =
        utxosActive.filter((_, utxo) => utxo.address == address).keySet.toSet

    def utxoById(utxoId: UtxoIdL1): Option[OutputL1] = utxosActive.get(utxoId)

    override def utxosAtAddress(address: AddressL1): List[(Utxo[L1])] =
        utxosActive
            .filter((_, utxo) => utxo.address == address)
            .toList
            .map((in, out) => Utxo(in, out))

    override def utxoIdsAdaAtAddress(address: AddressL1): Map[UtxoIdL1, Coin] =
        utxosActive
            .filter((_, utxo) => utxo.address == address)
            .view
            .mapValues(_.value.coin)
            .toMap

object CardanoL1Mock:
    def apply(): CardanoL1Mock =
        val l1Mock = new CardanoL1Mock
        l1Mock.utxosActive.addAll(genesisUtxos)
        l1Mock

    def apply(knownTxs: Map[TransactionHash, TxL1], utxosActive: UtxoSet[L1]): CardanoL1Mock =
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
        TestPeer.address(Alice)
      ),
      (
        "dc0d0d0a13e683e443c575147ec12136e5ac6a4f994cd4189d4d25bed541c44d",
        TestPeer.address(Bob)
      ),
      ("aae852d5d2b08c0a937a319fec0d9933bc3bc67b9d0a6bfd4001997b169364b3", TestPeer.address(Carol)),
      (
        "145da89c02380f6f72d6acc8194cd9295eb2001c2d88f0b20fef647ec5a18f7f",
        TestPeer.address(Daniella)
      ),
      ("8cc2f88405991a5dfb8cb6962fe44f56510d93405cfe9ea23baf1f6bf29f3011", TestPeer.address(Erin)),
      (
        "27bc6c090c20dc1fe66f07ac200d5e97493a11c9554f3fc92f0ea7036fb11df9",
        TestPeer.address(Frank)
      ),
      (
        "5b5b9626afd8846240e3c05de23634b1b6e76620be69b5a249670071b3c3fb60",
        TestPeer.address(Gustavo)
      ),
      (
        "979a06c4cc5a1902d68f469d6d6e9a780a2667f5ce8557c199fcfa882e05c92e",
        TestPeer.address(Hector)
      ),
      (
        "a2857a9eb8e140c6183137bf8aadbb47eeea23b96b9f4a13e4155c1ef83716a6",
        TestPeer.address(Isabel)
      ),
      ("6793565c7c64e2a979137e7876b2c560c194a0a6372b15f1473b72e64f4e1ab8", TestPeer.address(Julia)),
      ("08419f77fa20aec0a8b66047d0040be1c5446b2ddbe88e656dc62390260a997c", TestPeer.address(Alice)),
      (
        "592b36fd2dff5c8ff8c7125b9dd7ac027de661d55b2fc9ea4ef5b22b53cffda7",
        TestPeer.address(Bob)
      ),
      (
        "ce5179fbf81eb92907e8e5ca7d2180b561871c87c4136a2deaf9dd75f34b51be",
        TestPeer.address(Carol)
      ),
      (
        "d9d325322d157b007eb936bbd54968844e6e0f845994ad1843874d82a1521832",
        TestPeer.address(Daniella)
      ),
      ("c408f9103ba7e0f207e211b53f5cca42f79f3e9c9d24024731d648be909497a8", TestPeer.address(Erin)),
      (
        "6712b9e13e7e433397ea2888899b8a5ba57b12c427dc72f1c1c684ac05f32b1a",
        TestPeer.address(Frank)
      ),
      (
        "1c2498914a63269e7d4ab7f6b7d493d0d41a87b0cacaab7412c591219dcc21c4",
        TestPeer.address(Gustavo)
      ),
      (
        "8b96680725c4fcf461214054d0b364a86e43d7d6be0475d610e980971b101ad0",
        TestPeer.address(Hector)
      ),
      (
        "c7565416e7553cdf8fdac8bf054b4b3de19d06b72efd00c47823335d7156ed1f",
        TestPeer.address(Isabel)
      ),
      ("a6ce90a9a5ef8ef73858effdae375ba50f302d3c6c8b587a15eaa8fa98ddf741", TestPeer.address(Julia))
    ).map((txHash, address) =>
        (
          UtxoIdL1(
            TransactionInput(
              Hash[Blake2b_256, HashPurpose.TransactionHash](ByteString.fromHex(txHash)),
              0
            )
          ),
          Output[L1](Babbage(address = address, value = Value(Coin(10_000_000_000L))))
        )
    ).toSet

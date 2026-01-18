package hydrozoa.multisig.backend.cardano

import hydrozoa.{L1, Output, UtxoIdL1, UtxoSet, UtxoSetL1}
import scalus.builtin.ByteString
import scalus.cardano.address.Network
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Blake2b_256, Hash, HashPurpose, TransactionInput, TransactionOutput, Value}
import test.TestPeer
import test.TestPeer.*

// TODO: move to TestPeer?
/** Two utxos per test peer */
def yaciTestSauceGenesis(network: Network): Map[TestPeer, UtxoSetL1] =
    val mkAddress = TestPeer.address(network)

    List(
      (
        "6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508",
        Alice
      ),
      (
        "dc0d0d0a13e683e443c575147ec12136e5ac6a4f994cd4189d4d25bed541c44d",
        Bob
      ),
      (
        "aae852d5d2b08c0a937a319fec0d9933bc3bc67b9d0a6bfd4001997b169364b3",
        Carol
      ),
      (
        "145da89c02380f6f72d6acc8194cd9295eb2001c2d88f0b20fef647ec5a18f7f",
        Daniella
      ),
      (
        "8cc2f88405991a5dfb8cb6962fe44f56510d93405cfe9ea23baf1f6bf29f3011",
        Erin
      ),
      (
        "27bc6c090c20dc1fe66f07ac200d5e97493a11c9554f3fc92f0ea7036fb11df9",
        Frank
      ),
      (
        "5b5b9626afd8846240e3c05de23634b1b6e76620be69b5a249670071b3c3fb60",
        Gustavo
      ),
      (
        "979a06c4cc5a1902d68f469d6d6e9a780a2667f5ce8557c199fcfa882e05c92e",
        Hector
      ),
      (
        "a2857a9eb8e140c6183137bf8aadbb47eeea23b96b9f4a13e4155c1ef83716a6",
        Isabel
      ),
      (
        "6793565c7c64e2a979137e7876b2c560c194a0a6372b15f1473b72e64f4e1ab8",
        Julia
      ),
      (
        "08419f77fa20aec0a8b66047d0040be1c5446b2ddbe88e656dc62390260a997c",
        Alice
      ),
      (
        "592b36fd2dff5c8ff8c7125b9dd7ac027de661d55b2fc9ea4ef5b22b53cffda7",
        Bob
      ),
      (
        "ce5179fbf81eb92907e8e5ca7d2180b561871c87c4136a2deaf9dd75f34b51be",
        Carol
      ),
      (
        "d9d325322d157b007eb936bbd54968844e6e0f845994ad1843874d82a1521832",
        Daniella
      ),
      (
        "c408f9103ba7e0f207e211b53f5cca42f79f3e9c9d24024731d648be909497a8",
        Erin
      ),
      (
        "6712b9e13e7e433397ea2888899b8a5ba57b12c427dc72f1c1c684ac05f32b1a",
        Frank
      ),
      (
        "1c2498914a63269e7d4ab7f6b7d493d0d41a87b0cacaab7412c591219dcc21c4",
        Gustavo
      ),
      (
        "8b96680725c4fcf461214054d0b364a86e43d7d6be0475d610e980971b101ad0",
        Hector
      ),
      (
        "c7565416e7553cdf8fdac8bf054b4b3de19d06b72efd00c47823335d7156ed1f",
        Isabel
      ),
      (
        "a6ce90a9a5ef8ef73858effdae375ba50f302d3c6c8b587a15eaa8fa98ddf741",
        Julia
      )
    ).map((txHash, peer) =>
        peer ->
            (
              UtxoIdL1(
                TransactionInput(
                  Hash[Blake2b_256, HashPurpose.TransactionHash](ByteString.fromHex(txHash)),
                  0
                )
              ),
              Output[L1](
                TransactionOutput
                    .Babbage(address = mkAddress(peer), value = Value.lovelace(10_000_000_000L))
                    .asInstanceOf[Babbage]
              )
            )
    ).toMap
        .groupBy((peer, _) => peer)
        .view
        .mapValues(a => UtxoSet[L1](a.values.toMap))
        .toMap

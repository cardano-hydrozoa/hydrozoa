package hydrozoa.multisig.ledger

import cats.data.NonEmptyList
import hydrozoa.config.head.network.generateStandardCardanoNetwork
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import io.bullet.borer.Cbor
import org.scalacheck.*
import scalus.cardano.ledger.{Coin, OriginalCborByteArray, TransactionOutput}
import test.Generators.Hydrozoa.genGenesisObligation
import test.TestPeer.Alice

case object GenesisObligationTest extends Properties("Genesis Obligation Properties") {
    val _ = property("Cbor Rount Trip") =
        Prop.forAll(generateStandardCardanoNetwork)(cardanoNetwork =>
            Prop.forAll(
              Gen.nonEmptyListOf(
                genGenesisObligation(cardanoNetwork, Alice, minimumCoin = Coin.ada(5))
              ).map(NonEmptyList.fromListUnsafe)
            )(genesisObligations =>
                val bytes = Cbor
                    .encode(
                      genesisObligations.toList.map(_.toBabbage.asInstanceOf[TransactionOutput])
                    )
                    .toByteArray
                given OriginalCborByteArray = OriginalCborByteArray(bytes)

                val roundTrippedList: List[TransactionOutput] =
                    Cbor
                        .decode(bytes)
                        .to[List[TransactionOutput]]
                        .value

                val roundTripped = NonEmptyList.fromListUnsafe(
                  roundTrippedList.map(GenesisObligation.fromTransactionOutput)
                )
                genesisObligations.map(Right(_)) == roundTripped
            )
        )
}
